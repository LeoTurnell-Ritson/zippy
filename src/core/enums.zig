//! Token kind definitions used by the tokenizer and parser.

/// Token categories for the language grammar.
pub const TokenKind = enum {
    // multi-char tokens
    /// A sequence of digits without a decimal point.
    number,
    /// A numeric literal containing a decimal point.
    decimal,
    /// An identifier starting with a letter or underscore.
    identifier,
    /// A leading indentation level (tab at line start).
    indent,

    // single-char tokens
    /// The `+` operator.
    plus,
    /// The `-` operator.
    minus,
    /// The `*` operator.
    multiply,
    /// The `/` operator.
    divide,
    /// The `=` operator.
    equal,
    /// The `(` delimiter.
    open_paren,
    /// The `)` delimiter.
    close_paren,
    /// The `[` delimiter.
    open_square,
    /// The `]` delimiter.
    close_square,
    /// The `{` delimiter.
    open_curly,
    /// The `}` delimiter.
    close_curly,
    /// The `.` token.
    dot,
};
