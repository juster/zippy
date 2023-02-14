const std = @import("std");
const nif = @import("./nif.zig");
const c = @import("./nif_c.zig");
const decoder = @import("./decoder.zig");

fn load(env: ?*c.ErlNifEnv, priv_data: [*c]?*anyopaque, load_opts: c.ERL_NIF_TERM) callconv(.C) c_int {
    _ = priv_data;
    _ = load_opts;
    decoder.load(env) catch return 1;
    std.log.debug("module loaded", .{});
    return 0;
}

fn unload(env: ?*c.ErlNifEnv, priv_data: ?*anyopaque) callconv(.C) void {
    _ = env;
    _ = priv_data;
    std.log.debug("module unload", .{});
}

export fn nif_init() *c.ErlNifEntry {
    return nif.moduleEntry(comptime .{
        .name = "zippy",
        .funcs = &[_]c.ErlNifFunc{
            nif.Func("json_to_term", decoder.exec, 1),
        },
        .load = load,
        .unload = unload
    });
}
