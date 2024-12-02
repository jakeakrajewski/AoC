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

    for (0..lines.items.len) |i| {
        const line = lines.items[i];
        var split = std.mem.split(u8, line, "   ");

        const first = try std.fmt.parseInt(u32, split.first(), 10);
        try first_nums.append(first);

        if (split.next()) |s| {
            const second = try std.fmt.parseInt(u32, s, 10);
            try second_nums.append(second);
        }
    }

    const first = try first_nums.toOwnedSlice();
    const second = try second_nums.toOwnedSlice();

    std.mem.sort(u32, first, {}, comptime std.sort.asc(u32));
    std.mem.sort(u32, second, {}, comptime std.sort.asc(u32));

    var sum: u64 = 0;

    for (0..first.len) |i| {
        const f: i32 = @intCast(first[i]);
        const s: i32 = @intCast(second[i]);
        sum += @abs(f - s);
    }

    try stdout.print("{}", .{sum});
    const end_time = std.time.milliTimestamp();
    std.debug.print("\nTotal Time: {}ms\n", .{end_time - start_time});
}
