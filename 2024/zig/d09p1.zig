const std = @import("std");
const parser = @import("helpers/parser.zig");

pub fn main() !void {
    const start_time = std.time.milliTimestamp();
    var stdout = std.io.getStdOut().writer();
    var allocator = std.heap.page_allocator;
    const file_path = "../input/day9.txt";
    var lines = std.ArrayList([]u8).init(allocator);
    try parser.parseFile(&allocator, &lines, file_path);

    var disc_blocks = std.ArrayList(bool).init(allocator);
    var file_blocks = std.ArrayList(u64).init(allocator);

    var c: usize = 0;
    var id: usize = 0;

    for (0..lines.items[0].len) |i| {
        const char = lines.items[0][i];
        const num = try std.fmt.parseInt(u32, &[_]u8{char}, 10);
        if (i % 2 == 0) {
            for (0..num) |_| {
                try file_blocks.append(id);
                try disc_blocks.append(false);
                c += 1;
            }
            id += 1;
        } else {
            for (0..num) |_| {
                try disc_blocks.append(true);
                c += 1;
            }
        }
    }

    var sum: u64 = 0;

    for (0..file_blocks.items.len) |i| {
        if (i >= file_blocks.items.len) break;
        if (disc_blocks.items[i]) {
            const fb = file_blocks.orderedRemove(file_blocks.items.len - 1);
            try file_blocks.insert(i, fb);
        }

        sum += file_blocks.items[i] * i;
    }

    try stdout.print("{}", .{sum});
    const end_time = std.time.milliTimestamp();
    std.debug.print("\nTotal Time: {}ms\n", .{end_time - start_time});
}
