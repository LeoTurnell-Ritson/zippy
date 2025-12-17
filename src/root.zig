//! Zippy modules root file.
pub const core = @import("core/core.zig");

// Zippy test suite aggregation.
test {
    _ = core;
}
