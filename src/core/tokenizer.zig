//! Tokenizer for the core language grammar.

const std = @import("std");

pub const Token = struct {
    kind: Kind,
    loc: Loc,

    /// Source byte offsets for a token.
    pub const Loc = struct {
        start: usize,
        end: usize,
    };

    /// Keywords recognized by the tokenizer.
    pub const keywords = std.StaticStringMap(Kind).initComptime(.{
        .{ "and", .keyword_and },
        .{ "fn", .keyword_fn },
        .{ "pub", .keyword_pub },
    });

    /// Token categories.
    pub const Kind = enum {
        invalid,
        identifier,
        string_literal,
        number_literal,
        whitespace,
        eof,
        tab,
        dot,
        newline,
        l_paren,
        r_paren,
        l_brace,
        r_brace,
        l_square,
        r_square,
        comma,
        bang,
        bang_equal,
        pipe,
        pipe_pipe,
        equal,
        equal_equal,
        equal_angle_bracket_right,
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

    /// Human-readable symbol for a kind, useful in error messages.
    pub fn symbol(kind: Kind) []const u8 {
        return kind.lexeme() orelse switch (kind) {
            .identifier => "identifier",
            .string_literal => "string_literal",
            else => unreachable,
        };
    }
};

/// Stateful tokenizer over a zero-terminated buffer.
pub const Tokenizer = struct {
    buffer: [:0]const u8,
    index: usize,

    /// Create a tokenizer for the given zero-terminated buffer.
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
        minus,
        plus,
        invalid,
        int,
        int_dot,
        float,
    };

    /// Scan and return the next token in the input.
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
                '|' => continue :state .pipe,
                '=' => continue :state .equal,
                '!' => continue :state .bang,
                '+' => continue :state .plus,
                '-' => continue :state .minus,
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
                '0'...'9' => {
                    result.kind = .number_literal;
                    self.index += 1;
                    continue :state .int;
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
                '.' => {
                    self.index += 1;
                    result.kind = .dot;
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

            .invalid => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => if (self.index == self.buffer.len) {
                        result.kind = .invalid;
                    } else {
                        continue :state .invalid;
                    },
                    // Stolen from zig, we won't propagate the invalid state
                    // past a newline.
                    '\n' => result.kind = .invalid,
                    else => continue :state .invalid,
                }
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

            .equal => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '=' => {
                        self.index += 1;
                        result.kind = .equal_equal;
                    },
                    '>' => {
                        self.index += 1;
                        result.kind = .equal_angle_bracket_right;
                    },
                    else => result.kind = .equal,
                }
            },

            .minus => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '-' => {
                        self.index += 1;
                        result.kind = .minus_minus;
                    },
                    '=' => {
                        self.index += 1;
                        result.kind = .minus_equal;
                    },
                    else => result.kind = .minus,
                }
            },

            .plus => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '+' => {
                        self.index += 1;
                        result.kind = .plus_plus;
                    },
                    '=' => {
                        self.index += 1;
                        result.kind = .plus_equal;
                    },
                    else => result.kind = .plus,
                }
            },

            .pipe => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '|' => {
                        self.index += 1;
                        result.kind = .pipe_pipe;
                    },
                    else => result.kind = .pipe,
                }
            },

            .bang => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '=' => {
                        self.index += 1;
                        result.kind = .bang_equal;
                    },
                    else => result.kind = .bang,
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

            .int => switch (self.buffer[self.index]) {
                '.' => continue :state .int_dot,
                '0'...'9' => {
                    self.index += 1;
                    continue :state .int;
                },
                else => {},
            },
            .int_dot => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '0'...'9' => {
                        self.index += 1;
                        continue :state .float;
                    },
                    else => {
                        // Rollback to before the dot
                        self.index -= 1;
                    },
                }
            },
            .float => switch (self.buffer[self.index]) {
                '0'...'9' => {
                    self.index += 1;
                    continue :state .float;
                },
                else => {},
            },
        }

        result.loc.end = self.index;
        return result;
    }
};

test "symbols" {
    try testTokenize("", &.{});
    try testTokenize("   \t", &.{ .whitespace, .tab });
    try testTokenize("\t\t", &.{ .tab, .tab });
    try testTokenize("\n\n", &.{ .newline, .newline });
    try testTokenize("()", &.{ .l_paren, .r_paren });
    try testTokenize("[]", &.{ .l_square, .r_square });
    try testTokenize("{}", &.{ .l_brace, .r_brace });
    try testTokenize("- -- -=", &.{ .minus, .whitespace, .minus_minus, .whitespace, .minus_equal });
    try testTokenize("+ ++ +=", &.{ .plus, .whitespace, .plus_plus, .whitespace, .plus_equal });
    try testTokenize("= == =>", &.{ .equal, .whitespace, .equal_equal, .whitespace, .equal_angle_bracket_right });
    try testTokenize("! !=", &.{ .bang, .whitespace, .bang_equal });
    try testTokenize("| ||", &.{ .pipe, .whitespace, .pipe_pipe });
    try testTokenize(".", &.{.dot});
}

test "identifiers" {
    try testTokenize("pub fn foo", &.{ .keyword_pub, .whitespace, .keyword_fn, .whitespace, .identifier });
}

test "string literals" {
    try testTokenize("\"abc\"", &.{.string_literal});
    try testTokenize("\"\"", &.{.string_literal});
}

test "number literals" {
    try testTokenize("0", &.{.number_literal});
    try testTokenize("1", &.{.number_literal});
    try testTokenize("0.0", &.{.number_literal});
    try testTokenize("1.0", &.{.number_literal});
    try testTokenize("1.", &.{ .number_literal, .dot });
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
