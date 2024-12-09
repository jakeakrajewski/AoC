const std = @import("std");
const parser = @import("helpers/parser.zig");

pub fn main() !void {
    const start_time = std.time.milliTimestamp();
    var stdout = std.io.getStdOut().writer();
    var allocator = std.heap.page_allocator;
    const file_path = "../input/day8.txt";
    var lines = std.ArrayList([]u8).init(allocator);
    try parser.parseFile(&allocator, &lines, file_path);

    var map = std.AutoHashMap(u8, std.ArrayList(Location)).init(allocator);
    var antinode_map: [50][50]bool = undefined;

    //Construct Map
    for (0..50) |i| {
        for (0..50) |j| {
            const key = lines.items[i][j];
            if (key == '.') continue;
            const vals = map.getPtr(key);
            if (vals) |v| {
                try v.append(.{ .x = @intCast(j), .y = @intCast(i) });
            } else {
                var list = std.ArrayList(Location).init(allocator);
                // defer list.deinit();
                try list.append(.{ .x = @intCast(j), .y = @intCast(i) });
                try map.put(key, list);
            }
        }
    }

    for (0..50) |i| {
        for (0..50) |j| {
            const key = lines.items[i][j];
            if (key == '.') continue;
            const vals = map.getPtr(key).?;
            for (0..vals.items.len) |v| {
                const loc = vals.items[v];
                const i_j: i64 = @intCast(j);
                const i_i: i64 = @intCast(i);
                const x_dist: i64 = loc.x - i_j;
                const y_dist: i64 = loc.y - i_i;

                if (x_dist == 0 and y_dist == 0) continue;
                const x_target = x_dist + loc.x;
                const y_target = y_dist + loc.y;

                if (x_target < 0 or x_target >= 50 or y_target < 0 or y_target >= 50) continue;

                antinode_map[@intCast(x_target)][@intCast(y_target)] = true;
            }
        }
    }

    var sum: u64 = 0;

    for (0..50) |i| {
        for (0..50) |j| {
            if (antinode_map[j][i]) {
                sum += 1;
            }
        }
    }
    try stdout.print("{}", .{sum});
    const end_time = std.time.milliTimestamp();
    std.debug.print("\nTotal Time: {}ms\n", .{end_time - start_time});
}

pub const Location = struct {
    x: i64,
    y: i64,
};
