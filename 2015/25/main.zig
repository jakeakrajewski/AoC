const std = @import("std");
const builtin = @import("builtin");
const ArrayList = @import("list.zig").ArrayList;


const row: usize = 3010;
const column: usize = 3019;
var value: usize = 20151125;

pub fn main () !void {
    const steps: usize = if (row <= 1)
        1
    else
        (1 + ((row * row) - row) / 2) + 
        (row * (column - 1)) + (((column * column) - column) / 2);

    for (0..steps - 1) |_| {
        value = (value * 252533) % 33554393;
    }

    std.debug.print("\n{}", .{value});
}



