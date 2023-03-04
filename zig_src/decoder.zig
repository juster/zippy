const std = @import("std");
const nif = @import("./nif.zig");
const c = @import("./nif_c.zig");
const assert = std.debug.assert;

const DecodeError = error{
    Overflow,
    OutOfMemory,
    BadIntStr,
    BadFloatStr,
    BadUnicode,
    Utf8CannotEncodeSurrogateHalf,
    CodepointTooLarge,
    BadJson,
    BadAlloc
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
    null_atom: c.ERL_NIF_TERM,
    true_atom: c.ERL_NIF_TERM,
    false_atom: c.ERL_NIF_TERM,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, json: []const u8) Self {
        std.log.debug("{s} init", .{@typeName(Self)});
        const env = c.enif_alloc_env();
        return Self{
            .env = env,
            .null_atom = c.enif_make_atom(env, "null"),
            .true_atom = c.enif_make_atom(env, "true"),
            .false_atom = c.enif_make_atom(env, "false"),
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
        error.BadJson => "badjson",
        error.BadAlloc => "badalloc",
        error.BadUnicode,
        error.Utf8CannotEncodeSurrogateHalf,
        error.CodepointTooLarge => "badunicode",
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

fn decodeErr(state: *State, bin_term: c.ERL_NIF_TERM) DecodeError!bool {
    while (state.stream.next()) |result| {
        switch (result) {
            .Token => |token| try decodeToken(state, bin_term, token),
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
        .True => try state.push(state.true_atom),
        .False => try state.push(state.false_atom),
        .Null => try state.push(state.null_atom),
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
            const offset = state.stream.i - str.count - 1;
            try decodeString(state, bin_term, offset, str);
        },
        .ArrayBegin => try state.enter(),
        .ArrayEnd => {
            const terms = state.frame();
            const array = c.enif_make_list_from_array(env, terms.ptr, @intCast(c_uint, terms.len));
            try state.leave();
            return try state.push(array);
        },
        .ObjectBegin => { std.log.debug("ObjectBegin", .{}); try state.enter(); },
        .ObjectEnd => try decodeObject(state),
    }
}

fn decodeString(state: *State, bin_term: c.ERL_NIF_TERM, offset: usize, str: anytype) DecodeError!void {
    switch (str.escapes) {
        .None => {
            const term = c.enif_make_sub_binary(state.env, bin_term, offset, str.count);
            return try state.push(term);
        },
        // fall through
        .Some => {}
    }

    const slice = state.stream.slice[offset .. offset + str.count];
    var new_bin_term: c.ERL_NIF_TERM = undefined;
    const new_len = str.decodedLength();
    //const new_len = str.count;
    const new_bin = c.enif_make_new_binary(state.env, new_len, &new_bin_term)
        orelse return error.BadAlloc;
    const new_slice = new_bin[0 .. new_len];

    var i: usize = 0;
    var j: usize = 0;
    while (i < str.count) {
        assert(j < new_len);
        if (slice[i] != '\\') {
            new_slice[j] = slice[i];
            i += 1;
            j += 1;
            continue;
        }
        if (slice[i+1] != 'u') {
            new_slice[j] = switch (slice[i+1]) {
                '\\' => '\\',
                '/' => '/',
                'n' => 10,
                'r' => 13,
                't' => 9,
                'f' => 12,
                'b' => 8,
                '"' => '"',
                else => slice[i+1],
            };
            i += 2;
            j += 1;
            continue;
        }
        assert(i+6 <= slice.len);
        const cp1 = std.fmt.parseUnsigned(u16, slice[i+2 .. i+6], 16) catch |err| {
            return switch(err){
                error.InvalidCharacter => error.BadUnicode,
                error.Overflow => unreachable
            };
        };

        const k = i+6;
        if (cp1 < 0xD800 or 0xDFFF < cp1) {
            // this codepoint cannot be a surrogate pair
            i = k;
            j += try std.unicode.utf8Encode(@intCast(u21, cp1), new_slice[j .. new_len]);
            continue;
        }
        if (0xDC00 <= cp1 and cp1 <= 0xDFFF) {
            // this is a low surrogate and should not come before a high surrogate
            return error.BadUnicode;
        }
        if (k+6 > slice.len or (slice[k] != '\\' and slice[k+1] != 'u')) {
            // this codepoint can only be used as a surrogate but the second half is missing
            return error.BadUnicode;
        }

        const cp2 = std.fmt.parseUnsigned(u16, slice[k+2 .. k+6], 16) catch |err| {
            return switch(err){
                error.InvalidCharacter => error.BadUnicode,
                error.Overflow => unreachable
            };
        };
        i = k+6;

        if (0xDC00 < cp2 or cp2 > 0xDFFF) {
            // this is supposed to be a low surrogate but is outside the range
            return error.BadUnicode;
        }

        // Yes, these two codepoints are officially a surrogate pair.
        // https://www.unicode.org/faq/utf_bom.html#utf16-3
        const cp_x = (@intCast(u21, cp1) & ((1 << 6)-1) << 10) | (@intCast(u21, cp2) & ((1 << 10)-1));
        const cp_w = (@intCast(u21, cp1) >> 6) & ((1 << 5) - 1);
        const cp = ((cp_w + 1) << 16) | cp_x;
        j += try std.unicode.utf8Encode(cp, new_slice[j .. new_len]);
    }

    try state.push(new_bin_term);
}

fn decodeObject(state: *State) DecodeError!void {
    std.log.debug("ObjectEnd", .{});
    const env = state.env;
    const terms = state.frame();
    // JSON parser should prevent this
    assert(terms.len % 2 == 0);

    var map: c.ERL_NIF_TERM = c.enif_make_new_map(env);
    var unused: c.ERL_NIF_TERM = undefined;
    var i: usize = 0;
    while (i < terms.len) : (i += 2) {
        if (c.enif_get_map_value(env, map, terms[i], &unused) == 0) {
            if (c.enif_make_map_put(env, map, terms[i], terms[i+1], &map) == 0) {
                return error.BadAlloc;
            }
        }
    }
    try state.leave();
    try state.push(map);
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
