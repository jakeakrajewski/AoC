const std = @import("std");
const parser = @import("helpers/parser.zig");

pub fn main() !void {
    const start_time = std.time.milliTimestamp();
    var stdout = std.io.getStdOut().writer();
    var allocator = std.heap.page_allocator;
    const file_path = "../input/day1.txt";
    var lines = std.ArrayList([]u8).init(allocator);
    try parser.parseFile(&allocator, &lines, file_path);

    var first_nums = std.ArrayList(u32).init(allocator);
    var second_nums = std.ArrayList(u32).init(allocator);

    var map = std.AutoHashMap(u32, u32).init(allocator);

    for (0..lines.items.len) |i| {
        const line = lines.items[i];
        var split = std.mem.split(u8, line, "   ");

        const first = try std.fmt.parseInt(u32, split.first(), 10);
        try first_nums.append(first);

        if (split.next()) |s| {
            const second = try std.fmt.parseInt(u32, s, 10);
            try second_nums.append(second);
            if (map.contains(second)) {
                const v = map.get(second);
                if (v) |val| {
                    try map.put(second, val + 1);
                }
            } else {
                try map.put(second, 1);
            }
        }
    }

    var sum: u64 = 0;

    for (0..first_nums.items.len) |i| {
        const val = first_nums.items[i];

        const v = map.get(val);
        if (v) |value| {
            sum += val * value;
        }
    }

    try stdout.print("{}", .{sum});
    const end_time = std.time.milliTimestamp();
    std.debug.print("\nTotal Time: {}ms\n", .{end_time - start_time});
}
