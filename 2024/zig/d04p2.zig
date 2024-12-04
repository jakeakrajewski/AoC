const std = @import("std");
const parser = @import("helpers/parser.zig");

pub fn main() !void {
    const start_time = std.time.milliTimestamp();
    var stdout = std.io.getStdOut().writer();
    var allocator = std.heap.page_allocator;
    const file_path = "../input/day4.txt";
    var lines = std.ArrayList([]u8).init(allocator);
    try parser.parseFile(&allocator, &lines, file_path);

    var count: u32 = 0;

    for (1..lines.items.len - 1) |i| {
        const line = lines.items[i];

        for (1..line.len - 1) |c| {
            if (line[c] != 'A') {
                continue;
            }

            var mas_count: u32 = 0;

            if (lines.items[i - 1][c - 1] == 'M' and lines.items[i + 1][c + 1] == 'S') mas_count += 1;
            if (lines.items[i - 1][c + 1] == 'M' and lines.items[i + 1][c - 1] == 'S') mas_count += 1;
            if (lines.items[i + 1][c - 1] == 'M' and lines.items[i - 1][c + 1] == 'S') mas_count += 1;
            if (lines.items[i + 1][c + 1] == 'M' and lines.items[i - 1][c - 1] == 'S') mas_count += 1;

            if (mas_count == 2) count += 1;
        }
    }

    try stdout.print("{}", .{count});
    const end_time = std.time.milliTimestamp();
    std.debug.print("\nTotal Time: {}ms\n", .{end_time - start_time});
}
