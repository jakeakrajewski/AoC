const std = @import("std");
const parser = @import("helpers/parser.zig");

pub fn main() !void {
    const start_time = std.time.milliTimestamp();
    var stdout = std.io.getStdOut().writer();
    var allocator = std.heap.page_allocator;
    const file_path = "../input/day5.txt";
    var lines = std.ArrayList([]u8).init(allocator);
    try parser.parseFile(&allocator, &lines, file_path);

    var map = std.AutoHashMap(u32, std.ArrayList(u32)).init(allocator);

    var sum: u32 = 0;

    for (0..lines.items.len) |i| {
        const line = lines.items[i];

        if (line.len == 5 and line[2] == '|') {
            var split = std.mem.split(u8, line, "|");
            const first = try std.fmt.parseInt(u32, split.first(), 10);
            if (map.contains(first)) {
                var arr = map.getPtr(first);
                try arr.?.append(try std.fmt.parseInt(u32, split.next().?, 10));
            } else {
                var arr = std.ArrayList(u32).init(allocator);
                try arr.append(try std.fmt.parseInt(u32, split.next().?, 10));
                try map.put(first, arr);
            }
        } else {
            var split = std.mem.split(u8, line, ",");
            var nums = std.ArrayList(u32).init(allocator);
            var line_good = true;

            while (true) {
                const num = split.next();
                if (num) |n| {
                    const key = std.fmt.parseInt(u32, n, 10) catch {
                        line_good = false;
                        break;
                    };
                    try nums.append(key);

                    const val_arr = map.get(key);

                    for (0..val_arr.?.items.len) |val_ind| {
                        const val = val_arr.?.items[val_ind];

                        if (std.mem.indexOf(u32, nums.items, &[_]u32{val})) |ind| {
                            if (ind < i) {
                                line_good = false;
                                break;
                            }
                        }
                    }
                } else {
                    break;
                }
            }

            if (line_good) {
                sum += nums.items[nums.items.len / 2];
            }
        }
    }

    try stdout.print("{}", .{sum});
    const end_time = std.time.milliTimestamp();
    std.debug.print("\nTotal Time: {}ms\n", .{end_time - start_time});
}
