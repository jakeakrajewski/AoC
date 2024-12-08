const std = @import("std");
const parser = @import("helpers/parser.zig");

pub fn main() !void {
    const start_time = std.time.milliTimestamp();
    var stdout = std.io.getStdOut().writer();
    var allocator = std.heap.page_allocator;
    const file_path = "../input/day7.txt";
    var lines = std.ArrayList([]u8).init(allocator);
    try parser.parseFile(&allocator, &lines, file_path);

    var sum: u64 = 0;

    for (0..lines.items.len) |i| {
        const line = lines.items[i];
        var split = std.mem.split(u8, line, ":");
        const target = try std.fmt.parseInt(u64, split.first(), 10);
        var nums_split = std.mem.split(u8, split.next().?, ":");
        var nums = std.ArrayList(u32).init(allocator);
        defer nums.deinit();
        const components = nums_split.next();

        if (components) |c| {
            var num_arr = std.mem.split(u8, c, " ");
            while (true) {
                if (num_arr.next()) |n| {
                    if (n.len == 0) continue;
                    try nums.append(try std.fmt.parseInt(u32, n, 10));
                } else {
                    break;
                }
            }
        }

        const exp: u64 = @intCast(nums.items.len - 1);
        const permuatations: u64 = std.math.pow(u64, 2, exp);

        var total: u64 = nums.items[0];
        var match = false;

        for (0..permuatations) |p| {
            total = nums.items[0];
            for (0..nums.items.len - 1) |item| {
                const operation = (p >> @intCast(item)) & 1;
                if (operation == 1) {
                    total += nums.items[item + 1];
                } else {
                    total *= nums.items[item + 1];
                }
            }

            if (total == target) match = true;
        }

        if (match) {
            sum += target;
        }
    }

    try stdout.print("{}", .{sum});
    const end_time = std.time.milliTimestamp();
    std.debug.print("\nTotal Time: {}ms\n", .{end_time - start_time});
}
