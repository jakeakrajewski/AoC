const std = @import("std");
const parser = @import("helpers/parser.zig");

pub fn main() !void {
    const start_time = std.time.milliTimestamp();
    var stdout = std.io.getStdOut().writer();
    var allocator = std.heap.page_allocator;
    const file_path = "../input/day10.txt";
    var lines = std.ArrayList([]u8).init(allocator);
    try parser.parseFile(&allocator, &lines, file_path);

    var topo_map: [45][46]usize = undefined;

    for (0..lines.items.len) |i| {
        const line = lines.items[i];
        for (0..line.len) |j| {
            const num = try std.fmt.parseInt(u32, &[_]u8{line[j]}, 10);
            topo_map[j][i] = num;
        }
    }

    var sum: u64 = 0;
    for (0..46) |y| {
        for (0..45) |x| {
            if (topo_map[x][y] == 0) {
                var trails = std.ArrayList(Location).init(allocator);
                try search(.{ .x = x, .y = y }, topo_map, &trails);
                sum += trails.items.len;
            }
        }
    }

    try stdout.print("{}", .{sum});
    const end_time = std.time.milliTimestamp();
    std.debug.print("\nTotal Time: {}ms\n", .{end_time - start_time});
}

pub fn search(loc: Location, map: [45][46]usize, list: *std.ArrayList(Location)) !void {
    const val = map[loc.x][loc.y];
    if (val == 9) {
        try list.append(.{ .x = loc.x, .y = loc.y });
    } else {
        if (loc.x > 0) {
            if (map[loc.x - 1][loc.y] == val + 1) {
                try search(.{ .x = loc.x - 1, .y = loc.y }, map, list);
            }
        }
        if (loc.y > 0) {
            if (map[loc.x][loc.y - 1] == val + 1) {
                try search(.{ .x = loc.x, .y = loc.y - 1 }, map, list);
            }
        }
        if (loc.x < 44) {
            if (map[loc.x + 1][loc.y] == val + 1) {
                try search(.{ .x = loc.x + 1, .y = loc.y }, map, list);
            }
        }
        if (loc.y < 45) {
            if (map[loc.x][loc.y + 1] == val + 1) {
                try search(.{ .x = loc.x, .y = loc.y + 1 }, map, list);
            }
        }
    }
}

pub const Node = struct {
    value: usize,
    loc: Location,
};

pub const Location = struct {
    x: usize,
    y: usize,
};
