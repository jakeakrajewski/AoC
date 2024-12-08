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

        const total: u64 = nums.items[0];

        if (testLine(nums, 0, 0, total, target)) {
            sum += target;
        } else if (testLine(nums, 0, 1, total, target)) {
            sum += target;
        } else if (testLine(nums, 0, 2, total, target)) {
            sum += target;
        }
    }

    try stdout.print("{}", .{sum});
    const end_time = std.time.milliTimestamp();
    std.debug.print("\nTotal Time: {}ms\n", .{end_time - start_time});
}

pub fn testLine(list: std.ArrayList(u32), index: usize, operation: u2, acc: u64, total: u64) bool {
    var running_total = acc;
    if (acc > total) return false;
    if (index == list.items.len - 1) {
        if (acc == total) {
            return true;
        } else {
            return false;
        }
    }

    if (operation == 0) {
        running_total += list.items[index + 1];
    } else if (operation == 1) {
        running_total *= list.items[index + 1];
    } else if (operation == 2) {
        const digits = countDigits(list.items[index + 1]);
        running_total = running_total * std.math.pow(u64, 10, digits) + list.items[index + 1];
    }

    if (testLine(list, index + 1, 0, running_total, total)) {
        return true;
    } else if (testLine(list, index + 1, 1, running_total, total)) {
        return true;
    } else if (testLine(list, index + 1, 2, running_total, total)) {
        return true;
    }

    return false;
}

fn countDigits(n: u64) u64 {
    var count: u64 = 0;
    var value = n;
    while (value > 0) {
        count += 1;
        value /= 10;
    }
    return count;
}
