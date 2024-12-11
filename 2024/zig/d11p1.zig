const std = @import("std");
const parser = @import("helpers/parser.zig");

pub fn main() !void {
    const start_time = std.time.milliTimestamp();
    var stdout = std.io.getStdOut().writer();
    var allocator = std.heap.page_allocator;
    const file_path = "../input/day11.txt";
    var lines = std.ArrayList([]u8).init(allocator);
    try parser.parseFile(&allocator, &lines, file_path);

    var cache = std.AutoHashMap(CacheEntry, u64).init(allocator);
    var sum: u64 = 0;

    var split = std.mem.split(u8, lines.items[0], " ");

    while (true) {
        if (split.next()) |i| {
            const stone: u64 = try std.fmt.parseInt(u64, i, 10);
            sum += try calculate(stone, 75, &cache, allocator);
        } else {
            break;
        }
    }

    try stdout.print("{}", .{sum});
    const end_time = std.time.milliTimestamp();
    std.debug.print("\nTotal Time: {}ms\n", .{end_time - start_time});
}

const CacheEntry = struct {
    value: u64,
    blinks: u8,
};

fn calculate(stone: u64, blinks: u8, cache: *std.AutoHashMap(CacheEntry, u64), allocator: std.mem.Allocator) !u64 {
    if (blinks == 0) {
        // std.debug.print("\n{}", .{stone});
        return 1;
    }
    var count: u64 = 0;

    if (cache.get(.{ .value = stone, .blinks = blinks })) |entry| {
        // std.debug.print("\n{}", .{stone});
        return entry;
    } else {
        if (stone == 0) {
            count += try calculate(1, blinks - 1, cache, allocator);
        } else {
            var buffer: [20]u8 = undefined;
            const num_string = try std.fmt.bufPrint(&buffer, "{}", .{stone});
            if (num_string.len % 2 == 0) {
                const mid = num_string.len / 2;
                const first_num = try std.fmt.parseInt(u64, num_string[0..mid], 10);
                const second_num = try std.fmt.parseInt(u64, num_string[mid..], 10);
                count += try calculate(first_num, blinks - 1, cache, allocator);
                count += try calculate(second_num, blinks - 1, cache, allocator);
            } else {
                count += try calculate(stone * 2024, blinks - 1, cache, allocator);
            }
        }

        try cache.put(.{ .value = stone, .blinks = blinks }, count);
    }

    return count;
}
