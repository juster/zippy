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

// straight copy of std.json.TokenStream but with a chunk_size limit added
pub const ChunkedTokenStream = struct {
    const Token = std.json.Token;
    const StreamingParser = std.json.StreamingParser;
    const Self = @This();

    i: usize,
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
            .slice = slice,
            .parser = StreamingParser.init(),
            .token = null,
        };
    }

    fn stackUsed(self: *Self) usize {
        return self.parser.stack.len + if (self.token != null) @as(usize, 1) else 0;
    }

    pub fn next(self: *Self, n: usize) !Streamed {
        if (self.token) |token| {
            self.token = null;
            return Streamed{.Token = token};
        }

        const len = std.math.min(self.slice.len, self.i + n);

        var t1: ?Token = undefined;
        var t2: ?Token = undefined;

        while (self.i < len) {
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

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, json: []const u8) Self {
        //var log_alloc = std.heap.loggingAllocator(allocator).allocator();
        std.log.debug("{s} init", .{@typeName(Self)});
        return Self{
            .env = c.enif_alloc_env(),
            .stack = std.ArrayList(c.ERL_NIF_TERM).init(allocator),
            .stream = ChunkedTokenStream.init(json),
        };
    }

    pub fn next(self: *Self, n: usize) !ChunkedTokenStream.Streamed {
        return self.stream.next(n);
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
    std.log.debug("decode env: {x}", .{@ptrToInt(env.?)});
    if (argc != 2) return c.enif_make_badarg(env);
    const state = state_resource.unwrap(env, argv[1]) orelse return c.enif_make_badarg(env);
    if (c.enif_is_binary(env, argv[0]) == 0) {
        c.enif_release_resource(state);
        return c.enif_make_badarg(env);
    }

    while (state.next(count)) |result| {
        switch (result) {
            .Token => |token| {
                decodeToken(state, argv[0], token) catch |err| {
                    c.enif_release_resource(state);
                    return raiseDecodeError(env, err);
                };
            },
            .EndOfChunk => return null,
            .EndOfJson => break,
        }
    } else |_| {
        // TODO: convert error union to atom
        c.enif_release_resource(state);
        return nif.raiseAtom(env, "badjson");
    }

    var term = state.copyResult(env);
    c.enif_release_resource(state);
    return term;
}

fn decodeToken(state: *State, bin_term: c.ERL_NIF_TERM, token: std.json.Token) DecodeError!void {
    _ = bin_term;
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
            // TODO: make sub-binaries instead of copying bytes?
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

pub fn load(env: ?*c.ErlNifEnv) !void {
    state_resource = StateResource.init(env, "JzonDecoderState", .{ .destroy = freeState }) catch |err| {
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
