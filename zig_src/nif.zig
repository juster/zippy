const std = @import("std");
const Allocator = std.mem.Allocator;
const c = @import("./nif_c.zig");
const assert = std.debug.assert;

pub var allocator = NifAllocator.allocator();

const NifAllocator = struct {
    pub const vtable = Allocator.VTable{
        .alloc = alloc,
        //.resize = resize,
        .resize = Allocator.NoResize(anyopaque).noResize,
        .free = free,
    };

    const Self = @This();
    pub fn allocator() Allocator {
        return Allocator{
            .ptr = undefined,
            .vtable = &vtable,
        };
    }

    const MAX_ALIGN = @sizeOf(c_long);
    fn alloc(ptr: *anyopaque, len: usize, ptr_align: u29, len_align: u29, ret_addr: usize) Allocator.Error![]u8 {
        _ = ptr;
        _ = len_align;
        _ = ret_addr;

        // TODO: alignment?
        if (ptr_align > MAX_ALIGN) return error.OutOfMemory;
        var full_len = std.math.max(len, ptr_align);
        const pad = full_len % len_align;
        if (pad > 0) full_len += pad;
        //std.log.debug("alloc len={} full_len={} ptr_align={} len_align={} ret_addr={}", .{ len, full_len, ptr_align, len_align, ret_addr });

        const any_ptr = c.enif_alloc(full_len) orelse return error.OutOfMemory;

        std.debug.assert(std.mem.isAligned(@ptrToInt(any_ptr), ptr_align));
        const buf = @ptrCast([*]u8, any_ptr);
        return buf[0..len];
    }

    fn resize(ptr: *anyopaque, buf: []u8, buf_align: u29, new_size: usize, len_align: u29, ret_addr: usize) ?usize {
        _ = ptr;
        _ = buf_align;
        _ = len_align;
        _ = ret_addr;

        //std.log.debug("resize buf={x} buf_align={} new_size={} len_align={} ret_addr={}", .{ @ptrToInt(buf.ptr), buf_align, new_size, len_align, ret_addr });

        if (new_size == 0) {
            c.enif_free(buf.ptr);
            return 0;
        }
        if (new_size <= buf.len) {
            return new_size;
        }
        return null;
    }
    fn free(ptr: *anyopaque, buf: []u8, buf_align: u29, ret_addr: usize) void {
        _ = ptr;
        _ = buf_align;
        _ = ret_addr;

        //std.log.debug("free buf={x} buf_align={} ret_addr={}", .{ @ptrToInt(buf.ptr), buf_align, ret_addr });
        c.enif_free(buf.ptr);
        return;
    }
};

pub const ResourceOpts = struct {
    destroy: ?fn (?*c.ErlNifEnv, ?*anyopaque) callconv(.C) void = null,
};

pub const ResourceError = error{ RegistrationFailed };

pub fn Resource(comptime T: type) type {
    return struct {
        resType: *c.ErlNifResourceType,

        const Self = @This();
        pub fn init(env: ?*c.ErlNifEnv, name: []const u8, opt: ResourceOpts) ResourceError!Self {
            if (c.enif_open_resource_type(env, null, name.ptr, opt.destroy, c.ERL_NIF_RT_CREATE, null)) |resType| {
                return Self{ .resType = resType };
            } else {
                return error.RegistrationFailed;
            }
        }

        pub fn alloc(self: *Self) ?*T {
            const ptr = c.enif_alloc_resource(self.resType, @sizeOf(T)) orelse return null;
            return @ptrCast(*T, @alignCast(@alignOf(T), ptr));
        }

        pub fn unwrap(self: *Self, env: ?*c.ErlNifEnv, term: c.ERL_NIF_TERM) ?*T {
            var obj: ?*anyopaque = null;
            if (c.enif_get_resource(env, term, self.resType, &obj) == 0) return null;
            return if (obj) |ptr| @ptrCast(*T, @alignCast(@alignOf(T), ptr)) else null;
        }
    };
}

pub const Module = struct {
    name: []const u8,
    funcs: []const c.ErlNifFunc,
    load: ?fn (?*c.ErlNifEnv, [*c]?*anyopaque, c.ERL_NIF_TERM) callconv(.C) c_int = null,
    reload: ?fn (?*c.ErlNifEnv, [*c]?*anyopaque, c.ERL_NIF_TERM) callconv(.C) c_int = null,
    upgrade: ?fn (?*c.ErlNifEnv, [*c]?*anyopaque, [*c]?*anyopaque, c.ERL_NIF_TERM) callconv(.C) c_int = null,
    unload: ?fn (?*c.ErlNifEnv, ?*anyopaque) callconv(.C) void = null,
};

pub fn Func(comptime name: []const u8, comptime fptr: fn (?*c.ErlNifEnv, c_int, [*c]const c.ERL_NIF_TERM) callconv(.C) c.ERL_NIF_TERM, comptime arity: c_uint) c.ErlNifFunc {
    return c.ErlNifFunc{
        .name = name.ptr,
        .arity = arity,
        .fptr = fptr,
        .flags = 0,
    };
}

pub fn moduleEntry(comptime mod: Module) *c.ErlNifEntry {
    assert(mod.funcs.len > 0);
    const S = struct {
        var funcs = [_]c.ErlNifFunc{undefined} ** mod.funcs.len;
        var entry = c.ErlNifEntry{
            .major = c.ERL_NIF_MAJOR_VERSION,
            .minor = c.ERL_NIF_MINOR_VERSION,
            .name = mod.name.ptr,
            .num_of_funcs = @intCast(c_int, mod.funcs.len),
            .funcs = undefined,
            .load = mod.load,
            .reload = mod.reload,
            .upgrade = mod.upgrade,
            .unload = mod.unload,
            .vm_variant = c.ERL_NIF_VM_VARIANT,
            .options = 1, // always 1 in erl_nif.h
            .sizeof_ErlNifResourceTypeInit = @sizeOf(c.ErlNifResourceTypeInit),
            .min_erts = c.ERL_NIF_MIN_ERTS_VERSION,
        };
    };
    comptime var i: usize = 0;
    inline while (i < mod.funcs.len) : (i += 1) {
        S.funcs[i] = mod.funcs[i];
    }
    S.entry.funcs = &S.funcs;
    return &S.entry;
}

pub fn raiseAtom(env: ?*c.ErlNifEnv, name: []const u8) c.ERL_NIF_TERM {
    return c.enif_raise_exception(env, c.enif_make_atom(env, name.ptr));
}

pub fn binarySlice(bin: c.ErlNifBinary) []u8 {
    const binPtr = @ptrCast([*]u8, bin.data);
    return binPtr[0..bin.size];
}

pub fn shouldYield(env: ?*c.ErlNifEnv, start: c.ErlNifTime) bool {
    var percent = @intCast(c_int, @divFloor(c.enif_monotonic_time(c.ERL_NIF_USEC) - start, 10000));
    percent = std.math.max(1, std.math.min(100, percent));
    return if (c.enif_consume_timeslice(env, percent) == 1) true else false;
}