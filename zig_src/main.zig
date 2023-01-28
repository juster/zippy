const std = @import("std");
const nif = @import("./nif.zig");
const c = @import("./nif_c.zig");
const decoder = @import("./decoder.zig");

const assert = std.debug.assert;
const moduleName = "jzon";

//var nif_allocator = std.heap.loggingAllocator(nif.allocator).allocator();

fn load(env: ?*c.ErlNifEnv, priv_data: [*c]?*anyopaque, load_opts: c.ERL_NIF_TERM) callconv(.C) c_int {
    _ = priv_data;
    _ = load_opts;
    return if (decoder.load(env)) 0 else 1;
}

fn unload(env: ?*c.ErlNifEnv, priv_data: ?*anyopaque) callconv(.C) void {
    _ = env;
    _ = priv_data;
    std.log.debug("module unload", .{});
}

fn json_to_term(env: ?*c.ErlNifEnv, argc: c_int, argv: [*c]const c.ERL_NIF_TERM) callconv(.C) c.ERL_NIF_TERM {
    if (argc != 1) return c.enif_make_badarg(env);

    var bin: c.ErlNifBinary = undefined;
    if (c.enif_inspect_binary(env, argv[0], &bin) == 0) return c.enif_make_badarg(env);
    //if (c.enif_inspect_iolist_as_binary(env, argv[0], &bin) == 0) return c.enif_make_badarg(env);

    const state = decoder.state_resource.alloc() orelse return nif.raiseAtom(env, "badalloc");
    state.* = decoder.State.init(nif.allocator, nif.binarySlice(bin));

    const start = c.enif_monotonic_time(c.ERL_NIF_USEC);
    return decoder.decode(env, start, argv[0], state) orelse blk: {
        const state_term = c.enif_make_resource(env, state);
        var new_argv = [_]c.ERL_NIF_TERM{ argv[0], state_term };
        break :blk c.enif_schedule_nif(env, "json_to_term1", 0, json_to_term1, 2, &new_argv);
    };
}

fn json_to_term1(env: ?*c.ErlNifEnv, argc: c_int, argv: [*c]const c.ERL_NIF_TERM) callconv(.C) c.ERL_NIF_TERM {
    if (argc != 2) return c.enif_make_badarg(env);
    const state = decoder.state_resource.unwrap(env, argv[1]) orelse return c.enif_make_badarg(env);
    const start = c.enif_monotonic_time(c.ERL_NIF_USEC);
    return decoder.decode(env, start, argv[0], state) orelse c.enif_schedule_nif(env, "json_to_term1", 0, json_to_term1, 2, argv);
}

export fn nif_init() *c.ErlNifEntry {
    return nif.moduleEntry(comptime .{
        .name = "jzon",
        .funcs = &[_]c.ErlNifFunc{
            nif.Func("json_to_term", json_to_term, 1),
            nif.Func("json_to_term1", json_to_term1, 2),
        },
        .load = load,
        .unload = unload
    });
}
