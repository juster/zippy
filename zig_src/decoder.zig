const std = @import("std");
const nif = @import("./nif.zig");
const c = @import("./nif_c.zig");
const assert = std.debug.assert;

const DecodeError = error{
    Overflow,
    OutOfMemory,
    BadIntStr,
    BadFloatStr,
    BadMapKeys,
};

pub const State = struct {
    env: ?*c.ErlNifEnv,
    stack: std.ArrayList(c.ERL_NIF_TERM),
    stream: std.json.TokenStream,
    n: usize,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, json: []const u8) Self {
        //var log_alloc = std.heap.loggingAllocator(allocator).allocator();
        std.log.debug("{s} init", .{@typeName(Self)});
        return Self{
            .env = c.enif_alloc_env(),
            .stack = std.ArrayList(c.ERL_NIF_TERM).init(allocator),
            .stream = std.json.TokenStream.init(json),
            .n = 0,
        };
    }

    pub fn next(self: *Self) !?std.json.Token {
        return self.stream.next();
    }

    pub fn push(self: *Self, term: c.ERL_NIF_TERM) !void {
        try self.stack.append(term);
        self.n += 1;
    }

    pub fn pop(self: *Self) c.ERL_NIF_TERM {
        self.n -= 1;
        return self.stack.pop();
    }

    pub fn frame(self: *Self) []c.ERL_NIF_TERM {
        const items = self.stack.items;
        return items[(items.len - self.n)..items.len];
    }

    pub fn enter(self: *Self) !void {
        try self.stack.append(@intCast(c.ERL_NIF_TERM, self.n));
        self.n = 0;
    }

    pub fn leave(self: *Self) !void {
        const new_len = self.stack.items.len - self.n;
        try self.stack.resize(new_len);
        self.n = @intCast(@TypeOf(self.n), self.stack.pop());
    }

    pub fn copyResult(self: *Self, env: ?*c.ErlNifEnv) c.ERL_NIF_TERM {
        assert(self.stack.items.len == 1);
        return c.enif_make_copy(env, self.stack.items[0]);
    }

    pub fn deinit(self: *Self) void {
        std.log.debug("{s} deinit", .{@typeName(Self)});
        c.enif_free_env(self.env);
        self.stack.deinit();
    }
};

fn raiseDecodeError(env: ?*c.ErlNifEnv, err: DecodeError) c.ERL_NIF_TERM {
    const name =
        switch (err) {
        error.Overflow, error.OutOfMemory => "badalloc",
        error.BadIntStr => "badinteger",
        error.BadFloatStr => "badfloat",
        error.BadMapKeys => "badmap",
    };
    return c.enif_raise_exception(env, c.enif_make_atom(env, name));
}

const StateResource = nif.Resource(State);
pub var state_resource: StateResource = undefined;

pub fn load(env: ?*c.ErlNifEnv) bool {
    state_resource = StateResource.init(env, "JzonDecoderState", .{ .destroy = freeState }) catch {
        std.log.debug("failed to register decode state resource type", .{});
        return false;
    };
    return true;
}

fn freeState(env: ?*c.ErlNifEnv, state_ptr: ?*anyopaque) callconv(.C) void {
    _ = env;
    if (state_ptr) |ptr| {
        const state = @ptrCast(*State, @alignCast(@alignOf(State), ptr));
        state.deinit();
    } else {
    }
}

pub fn decode(env: ?*c.ErlNifEnv, start: c.ErlNifTime, bin_term: c.ERL_NIF_TERM, state: *State) ?c.ERL_NIF_TERM {
    _ = start;

    std.log.debug("decode env: {x}", .{@ptrToInt(env.?)});
    // var init_i = state.stream.i;
    while (state.next()) |result| {
        const token = result orelse break;
        decodeToken(state, bin_term, token) catch |err| return raiseDecodeError(env, err);
        //if (state.stream.i - init_i >= 1024) {
            if (nif.shouldYield(env, start)) return null; // else std.log.debug("didn't yield, at offset {}", .{state.stream.i});
            // init_i = state.stream.i;
        //}
    } else |_| {
        // TODO: convert error union to atom
        return nif.raiseAtom(env, "badjson");
    }

    var term = state.copyResult(env);
    c.enif_release_resource(state);
    return term;
}

pub fn decodeToken(state: *State, bin_term: c.ERL_NIF_TERM, token: std.json.Token) DecodeError!void {
    _ = bin_term;
    const stream = state.stream;
    const env = state.env;
    switch (token) {
        .True => try state.push(c.enif_make_atom(env, "true")),
        .False => try state.push(c.enif_make_atom(env, "false")),
        .Null => try state.push(c.enif_make_atom(env, "null")),
        .Number => |no| {
            const slice = no.slice(stream.slice, stream.i - 1);
            if (no.is_integer) {
                if (std.fmt.parseInt(c_long, slice, 10)) |i| {
                    return try state.push(c.enif_make_long(env, i));
                } else |err| switch (err) {
                    error.InvalidCharacter => return error.BadIntStr,
                    error.Overflow => return error.Overflow,
                }
            } else {
                if (std.fmt.parseFloat(f64, slice)) |f| {
                    return try state.push(c.enif_make_double(env, f));
                } else |err| switch (err) {
                    error.InvalidCharacter => return error.BadFloatStr,
                }
            }
        },
        .String => |str| {
            // TODO: decode escape sequences
            // const slice = str.slice(stream.slice, stream.i - 1);
            // var term: c.ERL_NIF_TERM = undefined;
            // const buf = c.enif_make_new_binary(env, slice.len, &term) orelse return error.OutOfMemory;
            // var i: usize = 0;
            // while (i < slice.len) : (i += 1) buf[i] = slice[i];
            // TODO: make sub-binaries instead of copying bytes?
            const term = c.enif_make_sub_binary(env, bin_term, stream.i - str.count - 1, str.count);
            return try state.push(term);
        },
        .ArrayBegin => try state.enter(),
        .ArrayEnd => {
            const terms = state.frame();
            const array = c.enif_make_list_from_array(env, terms.ptr, @intCast(c_uint, terms.len));
            try state.leave();
            return try state.push(array);
        },
        .ObjectBegin => try state.enter(),
        .ObjectEnd => {
            const terms = state.frame();
            assert(terms.len % 2 == 0);
            const buf = c.enif_alloc(terms.len * @sizeOf(c.ERL_NIF_TERM)) orelse return error.OutOfMemory;
            defer c.enif_free(buf);
            const mterms = @ptrCast([*]c.ERL_NIF_TERM, @alignCast(@alignOf(c.ERL_NIF_TERM), buf));
            const mid = terms.len / 2;
            var i: usize = 0;
            while (i < mid) : (i += 1) {
                mterms[i] = terms[2 * i];
                mterms[mid + i] = terms[2 * i + 1];
            }
            try state.leave();

            // TODO: allow (ignore) duplicate keys
            var map: c.ERL_NIF_TERM = undefined;
            return if (c.enif_make_map_from_arrays(env, mterms, mterms + mid, mid, &map) == 1)
                try state.push(map)
            else
                error.BadMapKeys;
        },
    }
}