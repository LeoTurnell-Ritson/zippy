const std = @import("std");

pub const Token = struct {
    kind: Kind,
    loc: Loc,

    pub const Loc = struct {
        start: usize,
        end: usize,
    };

    pub const keywords = std.StaticStringMap(Kind).initComptime(.{
        .{ "and", .keyword_and },
        .{ "fn", .keyword_fn },
        .{ "pub", .keyword_pub },
    });

    pub const Kind = enum {
        identifier,
        string_literal,
        number_literal,
        float,
        whitespace,
        eof,
        tab,
        newline,
        l_paren,
        r_paren,
        l_brace,
        r_brace,
        l_square,
        r_square,
        comma,
        bang,
        pipe,
        pipe_pipe,
        equal,
        equal_equal,
        plus,
        plus_plus,
        plus_equal,
        minus,
        minus_minus,
        minus_equal,
        keyword_and,
        keyword_fn,
        keyword_pub,

        pub fn lexeme(kind: Kind) ?[]const u8 {
            return switch (kind) {
                .eof => "EOF",

                .keyword_and => "and",
                .keyword_fn => "fn",
                .keyword_pub => "pub",
            };
        }
    };

    pub fn symbol(kind: Kind) []const u8 {
        return kind.lexeme() orelse switch (kind) {
            .identifier => "identifier",
            .string_literal => "string_literal",
            else => unreachable,
        };
    }
};

pub const Tokenizer = struct {
    buffer: [:0]const u8,
    index: usize,

    pub fn init(buffer: [:0]const u8) Tokenizer {
        return Tokenizer{
            .buffer = buffer,
            .index = 0,
        };
    }

    const State = enum {
        start,
        string_literal,
        whitespace,
        identifier,
        equal,
        bang,
        pipe,
        invalid,
    };

    pub fn next(self: *Tokenizer) Token {
        var result: Token = .{
            .kind = undefined,
            .loc = .{
                .start = self.index,
                .end = undefined,
            },
        };
        state: switch (State.start) {
            .start => switch (self.buffer[self.index]) {
                0 => {
                    if (self.index == self.buffer.len) {
                        return .{
                            .kind = .eof,
                            .loc = .{
                                .start = self.index,
                                .end = self.index,
                            },
                        };
                    } else {
                        continue :state .invalid;
                    }
                },
                '"' => {
                    self.index += 1;
                    result.kind = .string_literal;
                    continue :state .string_literal;
                },
                ' ', '\r' => {
                    result.kind = .whitespace;
                    continue :state .whitespace;
                },
                'a'...'z', 'A'...'Z', '_' => {
                    result.kind = .identifier;
                    continue :state .identifier;
                },
                '(' => {
                    self.index += 1;
                    result.kind = .l_paren;
                },
                ')' => {
                    self.index += 1;
                    result.kind = .r_paren;
                },
                '{' => {
                    self.index += 1;
                    result.kind = .l_brace;
                },
                '}' => {
                    self.index += 1;
                    result.kind = .r_brace;
                },
                '[' => {
                    self.index += 1;
                    result.kind = .l_square;
                },
                ']' => {
                    self.index += 1;
                    result.kind = .r_square;
                },
                ',' => {
                    self.index += 1;
                    result.kind = .comma;
                },
                '\t' => {
                    self.index += 1;
                    result.kind = .tab;
                },
                '\n' => {
                    self.index += 1;
                    result.kind = .newline;
                },
                else => continue :state .invalid,
            },

            .whitespace => switch (self.buffer[self.index]) {
                ' ', '\r' => {
                    self.index += 1;
                    continue :state .whitespace;
                },
                else => {
                    result.kind = .whitespace;
                    break :state;
                },
            },

            .identifier => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    'a'...'z', 'A'...'Z', '_', '0'...'9' => continue :state .identifier,
                    else => {
                        const id = self.buffer[result.loc.start..self.index];
                        if (Token.keywords.get(id)) |kind| {
                            result.kind = kind;
                        }
                    },
                }
            },

            .string_literal => switch (self.buffer[self.index]) {
                '"' => {
                    self.index += 1;
                    break :state;
                },
                0 => continue :state .invalid,
                else => {
                    self.index += 1;
                    continue :state .string_literal;
                },
            },

            else => continue :state .invalid,
        }

        result.loc.end = self.index;
        return result;
    }
};

test "Valid tokenization" {
    try testTokenize("", &.{});
    try testTokenize("   \t", &.{ .whitespace, .tab });
    try testTokenize("\t\t", &.{ .tab, .tab });
    try testTokenize("\n\n", &.{ .newline, .newline });
    try testTokenize("pub fn foo", &.{ .keyword_pub, .whitespace, .keyword_fn, .whitespace, .identifier });
    try testTokenize("()", &.{ .l_paren, .r_paren });
    try testTokenize("[]", &.{ .l_square, .r_square });
    try testTokenize("{}", &.{ .l_brace, .r_brace });
    try testTokenize("\"abc\"", &.{.string_literal});
}

/// Utility function to test the tokenizer.
fn testTokenize(source: [:0]const u8, expected: []const Token.Kind) !void {
    var tokenizer = Tokenizer.init(source);
    for (expected) |kind| {
        const token = tokenizer.next();
        try std.testing.expectEqual(kind, token.kind);
    }

    // As is zig, the last token should always be eof
    // regardless of the input.
    const last = tokenizer.next();
    try std.testing.expectEqual(Token.Kind.eof, last.kind);
    try std.testing.expectEqual(source.len, last.loc.start);
    try std.testing.expectEqual(source.len, last.loc.end);
}
