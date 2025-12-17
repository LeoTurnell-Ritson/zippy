//! Tokenization utilities for the core language syntax.
//!
//! The tokenizer emits a flat stream of tokens, including indentation tokens
//! for leading tabs at the start of a line. Mid-line whitespace is ignored.

const std = @import("std");
const core = @import("core.zig");

/// Errors produced by lexical analysis.
const TokenizeParseError = error{
    /// An unexpected character was encountered.
    UnexpectedCharacter,
    /// A decimal literal was malformed.
    InvalidDecimal,
};

/// All errors that can occur during tokenization.
const TokenizeError = TokenizeParseError || std.mem.Allocator.Error;

/// Token produced by the tokenizer.
const Token = struct {
    kind: core.enums.TokenKind,
    /// Slice into the original input buffer.
    value: []const u8,
    /// Zero-based line index.
    line: usize = 0,
    /// Zero-based column index.
    column: usize = 0,
};

/// Iterator over a token list.
const TokenListIterator = struct {
    token_list: *TokenList,
    current_index: usize,

    /// Return the next token, or null when exhausted.
    pub fn next(self: *TokenListIterator) ?Token {
        if (self.current_index >= self.token_list.len()) {
            return null;
        }
        const token = self.token_list.list.items[self.current_index];
        self.current_index += 1;
        return token;
    }
};

/// A mutable list of tokens with convenience helpers.
const TokenList = struct {
    list: std.ArrayList(Token),

    /// Convert the list to an owned slice.
    pub fn toOwnedSlice(self: *TokenList, allocator: std.mem.Allocator) ![]Token {
        return self.list.toOwnedSlice(allocator);
    }

    /// Free resources associated with the list.
    pub fn deinit(self: *TokenList, allocator: std.mem.Allocator) void {
        self.list.deinit(allocator);
    }

    /// Number of tokens currently in the list.
    pub fn len(self: *TokenList) usize {
        return self.list.items.len;
    }

    /// Create an iterator over the list.
    pub fn asIterator(self: *TokenList) TokenListIterator {
        return TokenListIterator{
            .token_list = self,
            .current_index = 0,
        };
    }

    /// Create a new token list from an input string.
    pub fn fromString(
        allocator: std.mem.Allocator,
        input: []const u8,
    ) TokenizeError!TokenList {
        var tokens = try TokenList.initCapacity(allocator, 16);
        errdefer tokens.list.deinit(allocator);

        var i: usize = 0;
        var line_start: usize = 0;
        var line_number: usize = 0; // we zero index lines internally
        var at_line_start = true;
        var pending_indent_tabs: usize = 0;
        while (i < input.len) : (i += 1) {
            const c = input[i];

            // Consume indentation at start of line
            if (at_line_start) {
                if (c == '\t') {
                    pending_indent_tabs += 1;
                    continue;
                } else if (c == ' ') {
                    return TokenizeParseError.UnexpectedCharacter;
                } else if (c == '\n') {
                    line_start = i + 1;
                    line_number += 1;
                    pending_indent_tabs = 0;
                    continue;
                }

                if (pending_indent_tabs > 0) {
                    var indent_index: usize = 0;
                    while (indent_index < pending_indent_tabs) : (indent_index += 1) {
                        const indent_start = line_start + indent_index;
                        const indent_end = indent_start + 1;
                        try tokens.appendTokenRange(allocator, .indent, indent_start, indent_end, line_start, line_number, input);
                    }
                }
                pending_indent_tabs = 0;
                at_line_start = false;
            }

            if (isNumberOrDecimalStart(c)) {
                const number = try scanNumberOrDecimal(input, i);
                try tokens.appendTokenRange(allocator, number.kind, i, number.end, line_start, line_number, input);
                i = number.end - 1;
            } else if (isIdentifierStart(c)) {
                const end = scanIdentifier(input, i);
                try tokens.appendTokenRange(allocator, .identifier, i, end, line_start, line_number, input);
                i = end - 1;
            } else switch (c) {
                '+' => try tokens.appendSingleToken(allocator, .plus, i, line_start, line_number, input),
                '-' => try tokens.appendSingleToken(allocator, .minus, i, line_start, line_number, input),
                '*' => try tokens.appendSingleToken(allocator, .multiply, i, line_start, line_number, input),
                '/' => try tokens.appendSingleToken(allocator, .divide, i, line_start, line_number, input),
                '=' => try tokens.appendSingleToken(allocator, .equal, i, line_start, line_number, input),
                '(' => try tokens.appendSingleToken(allocator, .open_paren, i, line_start, line_number, input),
                ')' => try tokens.appendSingleToken(allocator, .close_paren, i, line_start, line_number, input),
                '[' => try tokens.appendSingleToken(allocator, .open_square, i, line_start, line_number, input),
                ']' => try tokens.appendSingleToken(allocator, .close_square, i, line_start, line_number, input),
                '{' => try tokens.appendSingleToken(allocator, .open_curly, i, line_start, line_number, input),
                '}' => try tokens.appendSingleToken(allocator, .close_curly, i, line_start, line_number, input),
                '.' => try tokens.appendSingleToken(allocator, .dot, i, line_start, line_number, input),
                ' ', '\t' => {}, // skip whitespace
                '\n' => {
                    line_start = i + 1; // Update line start for future tokens
                    line_number += 1;
                    at_line_start = true;
                    pending_indent_tabs = 0;
                },
                else => return TokenizeParseError.UnexpectedCharacter,
            }
        }

        return tokens;
    }

    inline fn initCapacity(allocator: std.mem.Allocator, capacity: usize) !TokenList {
        return TokenList{
            .list = try std.ArrayList(Token).initCapacity(allocator, capacity),
        };
    }

    inline fn append(self: *TokenList, allocator: std.mem.Allocator, token: Token) !void {
        try self.list.append(allocator, token);
    }

    inline fn appendSingleToken(
        self: *TokenList,
        allocator: std.mem.Allocator,
        kind: core.enums.TokenKind,
        index: usize,
        line_start: usize,
        line_number: usize,
        input: []const u8,
    ) !void {
        try self.appendTokenRange(allocator, kind, index, index + 1, line_start, line_number, input);
    }

    inline fn appendTokenRange(
        self: *TokenList,
        allocator: std.mem.Allocator,
        kind: core.enums.TokenKind,
        start: usize,
        end: usize,
        line_start: usize,
        line_number: usize,
        input: []const u8,
    ) !void {
        try self.append(allocator, Token{
            .kind = kind,
            .value = input[start..end],
            .line = line_number,
            .column = start - line_start,
        });
    }
};

/// True when a character is valid inside an identifier.
inline fn isIdentifierChar(c: u8) bool {
    return std.ascii.isAlphanumeric(c) or c == '_';
}

/// True when a character can begin an identifier.
inline fn isIdentifierStart(c: u8) bool {
    return std.ascii.isAlphabetic(c) or c == '_';
}

/// True when a character can begin a number or decimal literal.
inline fn isNumberOrDecimalStart(c: u8) bool {
    return std.ascii.isDigit(c);
}

/// Return the exclusive end index for an identifier starting at `start`.
inline fn scanIdentifier(input: []const u8, start: usize) usize {
    var i = start;
    while (i + 1 < input.len and isIdentifierChar(input[i + 1])) : (i += 1) {}
    return i + 1; // exclusive end
}

/// Scan a number or decimal literal beginning at `start`.
inline fn scanNumberOrDecimal(
    input: []const u8,
    start: usize,
) TokenizeParseError!struct {
    kind: core.enums.TokenKind,
    end: usize,
} {
    var i = start;
    while (i + 1 < input.len and std.ascii.isDigit(input[i + 1])) : (i += 1) {}

    if (i + 1 < input.len and input[i + 1] == '.') {
        i += 1; // consume '.'
        if (i + 1 < input.len and input[i + 1] == '.') {
            // two dots in a row is not a valid decimal
            // e.g., "12..34"
            return TokenizeParseError.InvalidDecimal;
        }
        if (i + 1 == input.len) {
            // dot at end of input is valid decimal
            return .{ .kind = .decimal, .end = i + 1 };
        } else if (i + 1 < input.len and !(std.ascii.isDigit(input[i + 1]))) {
            // dot not followed by digit is valid decimal
            // e.g., "12. ", "12.+", "12.a"
            return .{ .kind = .decimal, .end = i + 1 };
        } else if (i + 1 < input.len and std.ascii.isDigit(input[i + 1])) {
            // consume digits after the dot
            // e.g., "12.34", "12.3+" or "12.a" are valid
            while (i + 1 < input.len and std.ascii.isDigit(input[i + 1])) : (i += 1) {}
            return .{ .kind = .decimal, .end = i + 1 };
        } else {
            return TokenizeParseError.InvalidDecimal;
        }
    }

    return .{ .kind = .number, .end = i + 1 };
}

test "tokenise: empty input" {
    var list = try TokenList.fromString(std.testing.allocator, "");
    defer list.deinit(std.testing.allocator);

    try std.testing.expect(list.len() == 0);
}

test "tokenise: integer" {
    var list = try TokenList.fromString(std.testing.allocator, "1234");
    defer list.deinit(std.testing.allocator);
    var it = list.asIterator();
    const token = it.next() orelse return error.TestExpectedToken;

    try std.testing.expect(list.len() == 1);
    try std.testing.expect(token.kind == .number);
    try std.testing.expectEqualStrings(token.value, "1234");

    // check position
    try std.testing.expect(token.line == 0);
    try std.testing.expect(token.column == 0);
}

test "tokenise: identifier" {
    const allowed_inputs = [_][]const u8{
        "abc",
        "a_bc123",
        "_abc",
        "abc_123",
    };
    for (allowed_inputs) |input| {
        var list = try TokenList.fromString(std.testing.allocator, input);
        defer list.deinit(std.testing.allocator);
        var it = list.asIterator();
        const token = it.next() orelse return error.TestExpectedToken;

        try std.testing.expect(list.len() == 1);
        try std.testing.expect(token.kind == .identifier);
        try std.testing.expectEqualStrings(token.value, input);

        // check position
        try std.testing.expect(token.line == 0);
        try std.testing.expect(token.column == 0);
    }
}

test "tokenise: decimal" {
    const allowed_inputs = [_][]const u8{
        "12. ",
        "12.+", // two tokens
        "12.",
        "12.3",
        "12.34",
        "12.a", // two tokens
        "12.3.3", // multiple tokens (will be treated as an error by the parser)
    };
    for (allowed_inputs) |input| {
        var list = try TokenList.fromString(std.testing.allocator, input);
        defer list.deinit(std.testing.allocator);
        var it = list.asIterator();
        const token = it.next() orelse return error.TestExpectedToken;

        try std.testing.expect(list.len() > 0);
        try std.testing.expect(token.kind == .decimal);
        try std.testing.expectEqualStrings(token.value, input[0..token.value.len]);

        // check position
        try std.testing.expect(token.line == 0);
        try std.testing.expect(token.column == 0);
    }
}

test "tokenise: indent" {
    const input = "\tabc"; // tab indent
    var list = try TokenList.fromString(std.testing.allocator, input);
    defer list.deinit(std.testing.allocator);
    const tokens = try list.toOwnedSlice(std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    try std.testing.expect(tokens.len == 2);
    try std.testing.expect(tokens[0].kind == .indent);
    try std.testing.expect(tokens[1].kind == .identifier);
    try std.testing.expectEqualStrings(tokens[1].value, "abc");

    // check position
    try std.testing.expect(tokens[0].line == 0);
    try std.testing.expect(tokens[0].column == 0);
    try std.testing.expect(tokens[1].line == 0);
    try std.testing.expect(tokens[1].column == 1);
}

test "tokenise: complex indent" {
    const input = "\t\tabc\n\tdef";
    var list = try TokenList.fromString(std.testing.allocator, input);
    defer list.deinit(std.testing.allocator);
    const tokens = try list.toOwnedSlice(std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    try std.testing.expect(tokens.len == 5);
    try std.testing.expect(tokens[0].kind == .indent);
    try std.testing.expect(tokens[1].kind == .indent);
    try std.testing.expect(tokens[2].kind == .identifier);
    try std.testing.expect(tokens[3].kind == .indent);
    try std.testing.expect(tokens[4].kind == .identifier);
    try std.testing.expectEqualStrings(tokens[2].value, "abc");
    try std.testing.expectEqualStrings(tokens[4].value, "def");

    // check position
    try std.testing.expect(tokens[0].column == 0);
    try std.testing.expect(tokens[1].column == 1);
    try std.testing.expect(tokens[2].column == 2);
    try std.testing.expect(tokens[3].column == 0);
    try std.testing.expect(tokens[4].column == 1);
}

test "tokenise: ignores mid-line spacing" {
    var list = try TokenList.fromString(std.testing.allocator, "abc    + 2");
    defer list.deinit(std.testing.allocator);
    const tokens = try list.toOwnedSlice(std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    try std.testing.expect(tokens.len == 3);
    try std.testing.expect(tokens[0].kind == .identifier);
    try std.testing.expect(tokens[1].kind == .plus);
    try std.testing.expect(tokens[2].kind == .number);
}

test "tokenise: rejects leading spaces" {
    try std.testing.expectError(TokenizeParseError.UnexpectedCharacter, TokenList.fromString(std.testing.allocator, " abc"));
    try std.testing.expectError(TokenizeParseError.UnexpectedCharacter, TokenList.fromString(std.testing.allocator, "abc\n def"));
}

test "tokenise: single symbols" {
    const input = "+-*/=()[]{}.";
    const expected_kinds = [_]core.enums.TokenKind{
        .plus,
        .minus,
        .multiply,
        .divide,
        .equal,
        .open_paren,
        .close_paren,
        .open_square,
        .close_square,
        .open_curly,
        .close_curly,
        .dot,
    };

    var list = try TokenList.fromString(std.testing.allocator, input);
    defer list.deinit(std.testing.allocator);
    const tokens = try list.toOwnedSlice(std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    try std.testing.expect(tokens.len == expected_kinds.len);
    for (expected_kinds, tokens) |expected, tok| {
        try std.testing.expect(tok.kind == expected);
    }
}
