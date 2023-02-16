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
    BadJson
};

// straight copy of std.json.TokenStream but with a chunk_size limit added
pub const ChunkedTokenStream = struct {
    const Token = std.json.Token;
    const StreamingParser = std.json.StreamingParser;
    const Self = @This();

    i: usize,
    max: usize,
    slice: []const u8,
    parser: StreamingParser,
    token: ?Token,

    pub const Error = StreamingParser.Error || error{UnexpectedEndOfJson};

    pub const Streamed = union(enum) {
        Token: std.json.Token,
        EndOfChunk,
        EndOfJson
    };

    pub fn init(slice: []const u8) Self {
        return Self{
            .i = 0,
            .max = slice.len,
            .slice = slice,
            .parser = StreamingParser.init(),
            .token = null,
        };
    }

    fn stackUsed(self: *Self) usize {
        return self.parser.stack.len + if (self.token != null) @as(usize, 1) else 0;
    }

    pub fn limit(self: *Self, n: usize) void {
        self.max = std.math.min(self.slice.len, self.i + n);
    }

    pub fn next(self: *Self) !Streamed {
        if (self.token) |token| {
            self.token = null;
            return Streamed{.Token = token};
        }

        var t1: ?Token = undefined;
        var t2: ?Token = undefined;

        while (self.i < self.max) {
            try self.parser.feed(self.slice[self.i], &t1, &t2);
            self.i += 1;

            if (t1) |token| {
                self.token = t2;
                return Streamed{.Token = token};
            }
        }

        // Without this a bare number fails, the streaming parser doesn't know the input ended
        // (sic)
        if (self.i >= self.slice.len) {
            try self.parser.feed(' ', &t1, &t2);
            self.i += 1;
        }

        if (t1) |token| {
            return Streamed{.Token = token};
        } else if (self.i >= self.slice.len) {
            return if (self.parser.complete) .EndOfJson
                else error.UnexpectedEndOfJson;
        } else {
            return .EndOfChunk;
        }
    }
};

const State = struct {
    env: ?*c.ErlNifEnv,
    stack: std.ArrayList(c.ERL_NIF_TERM),
    frame_size: usize = 0,
    stream: ChunkedTokenStream,
    //arena_allocator: ArenaAllocator,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, json: []const u8) Self {
        std.log.debug("{s} init", .{@typeName(Self)});
        return Self{
            .env = c.enif_alloc_env(),
            .stack = std.ArrayList(c.ERL_NIF_TERM).init(allocator),
            .stream = ChunkedTokenStream.init(json),
        };
    }

    pub fn push(self: *Self, term: c.ERL_NIF_TERM) !void {
        try self.stack.append(term);
        self.frame_size += 1;
    }

    pub fn pop(self: *Self) c.ERL_NIF_TERM {
        self.frame_size -= 1;
        return self.stack.pop();
    }

    pub fn frame(self: *Self) []c.ERL_NIF_TERM {
        const items = self.stack.items;
        return items[(items.len - self.frame_size)..items.len];
    }

    pub fn enter(self: *Self) !void {
        try self.stack.append(@intCast(c.ERL_NIF_TERM, self.frame_size));
        self.frame_size = 0;
    }

    pub fn leave(self: *Self) !void {
        const new_len = self.stack.items.len - self.frame_size;
        try self.stack.resize(new_len);
        self.frame_size = @intCast(@TypeOf(self.frame_size), self.stack.pop());
    }

    pub fn copyResult(self: *Self, env: ?*c.ErlNifEnv) c.ERL_NIF_TERM {
        assert(self.stack.items.len == 1);
        const start = std.time.milliTimestamp();
        var result = c.enif_make_copy(env, self.stack.items[0]);
        std.log.debug("enif_make_copy took {} millis", .{std.time.milliTimestamp() - start});
        return result;
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
        error.BadJson => "badjson",
    };
    return c.enif_raise_exception(env, c.enif_make_atom(env, name));
}

const StateResource = nif.ResourceType(State);
var state_resource: StateResource = undefined;

fn freeState(env: ?*c.ErlNifEnv, state_ptr: ?*anyopaque) callconv(.C) void {
    _ = env;
    if (state_ptr) |ptr| {
        const state = @ptrCast(*State, @alignCast(@alignOf(State), ptr));
        state.deinit();
    }
}

fn decode(env: ?*c.ErlNifEnv, argc: c_int, argv: [*c]const c.ERL_NIF_TERM, count: usize) ?c.ERL_NIF_TERM {
    if (argc != 2) return c.enif_make_badarg(env);
    const state = state_resource.unwrap(env, argv[1]) orelse return c.enif_make_badarg(env);

    if (c.enif_is_binary(env, argv[0]) == 0) {
        c.enif_release_resource(state);
        return c.enif_make_badarg(env);
    }

    state.stream.limit(count);
    if (decodeErr(state, argv[0])) |done| {
        if (done) {
            const term = state.copyResult(env);
            c.enif_release_resource(state);
            return term;
        } else {
            // keep state for next iteration
            return null;
        }
    } else |err| {
        c.enif_release_resource(state);
        return raiseDecodeError(env, err);
    }
}

fn decodeErr(state: *State, binary: c.ERL_NIF_TERM) DecodeError!bool {
    while (state.stream.next()) |result| {
        switch (result) {
            .Token => |token| try decodeToken(state, binary, token),
            .EndOfChunk => return false,
            .EndOfJson => break,
        }
    } else |_| {
        // TODO: preserve specific errors
        return error.BadJson;
    }

    return true;
}

fn decodeToken(state: *State, bin_term: c.ERL_NIF_TERM, token: std.json.Token) DecodeError!void {
    const env = state.env;
    switch (token) {
        .True => try state.push(c.enif_make_atom(env, "true")),
        .False => try state.push(c.enif_make_atom(env, "false")),
        .Null => try state.push(c.enif_make_atom(env, "null")),
        .Number => |no| {
            const slice = no.slice(state.stream.slice, state.stream.i - 1);
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
            // const slice = str.slice(state.stream.slice, state.stream.i - 1);
            // var term: c.ERL_NIF_TERM = undefined;
            // const buf = c.enif_make_new_binary(env, slice.len, &term) orelse return error.OutOfMemory;
            // var i: usize = 0;
            // while (i < slice.len) : (i += 1) buf[i] = slice[i];
            const term = c.enif_make_sub_binary(env, bin_term, state.stream.i - str.count - 1, str.count);
            return try state.push(term);
        },
        .ArrayBegin => try state.enter(),
        .ArrayEnd => {
            const terms = state.frame();
            const array = c.enif_make_list_from_array(env, terms.ptr, @intCast(c_uint, terms.len));
            try state.leave();
            return try state.push(array);
        },
        .ObjectBegin => { std.log.debug("ObjectBegin", .{}); try state.enter(); },
        .ObjectEnd => {
            std.log.debug("ObjectEnd", .{});
            const terms = state.frame();
            assert(terms.len % 2 == 0);
            const buf = c.enif_alloc(terms.len * @sizeOf(c.ERL_NIF_TERM)) orelse return error.OutOfMemory;
            defer c.enif_free(buf);
            const mterms = @ptrCast([*]c.ERL_NIF_TERM, @alignCast(@alignOf(c.ERL_NIF_TERM), buf));
            const count = terms.len / 2;
            const keys = mterms[0..count];
            const values = mterms[count..terms.len];
            std.log.debug("terms.len:{} bufsize:{} count:{}", .{terms.len, terms.len * @sizeOf(c.ERL_NIF_TERM), count});
            var i: usize = 0;
            while (i < count) : (i += 1) {
                keys[i] = terms[2 * i];
                values[i] = terms[2 * i + 1];
            }
            try state.leave();

            // TODO: allow (ignore) duplicate keys
            var map: c.ERL_NIF_TERM = undefined;
            return if (c.enif_make_map_from_arrays(env, keys.ptr, values.ptr, count, &map) == 1)
                try state.push(map)
            else
                return error.BadMapKeys;
        },
    }
}

pub fn load(env: ?*c.ErlNifEnv) !void {
    state_resource = StateResource.init(env, "ZippyDecoderState", .{ .destroy = freeState }) catch |err| {
        std.log.debug("failed to register decode state resource type", .{});
        return err;
    };
}

const YieldingDecode = nif.Yielding("json_to_term", decode);

pub fn exec(env: ?*c.ErlNifEnv, argc: c_int, argv: [*c]const c.ERL_NIF_TERM) callconv (.C) c.ERL_NIF_TERM {
    var bin: c.ErlNifBinary = undefined;
    if (argc != 1) return c.enif_make_badarg(env);
    if (c.enif_inspect_binary(env, argv[0], &bin) == 0) return c.enif_make_badarg(env);

    const chunk_size = 4 * 1024;
    const state = state_resource.alloc() orelse return nif.raiseAtom(env, "badalloc");
    state.* = State.init(nif.allocator, nif.binarySlice(bin));
    const state_term = c.enif_make_resource(env, state);
    const argv1 = [_]c.ERL_NIF_TERM{ argv[0], state_term };
    return YieldingDecode.nif(env, 2, &argv1, chunk_size);
}
