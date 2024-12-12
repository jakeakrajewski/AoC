const std = @import("std");
const parser = @import("helpers/parser.zig");

pub fn main() !void {
    const start_time = std.time.milliTimestamp();
    var stdout = std.io.getStdOut().writer();
    var allocator = std.heap.page_allocator;
    const file_path = "../input/day12.txt";
    var lines = std.ArrayList([]u8).init(allocator);
    try parser.parseFile(&allocator, &lines, file_path);

    var sum: u64 = 0;
    var visited: [140][140]bool = undefined;

    for (0..lines.items.len) |y| {
        for (0..lines.items.len) |x| {
            if (!visited[y][x]) {
                var region: Region = .{ .area = 0, .per = 0 };
                calculate(lines, .{ .x = x, .y = y }, &region, &visited);
                sum += region.area * region.per;
            }
        }
    }

    try stdout.print("{}", .{sum});
    const end_time = std.time.milliTimestamp();
    std.debug.print("\nTotal Time: {}ms\n", .{end_time - start_time});
}

const Location = struct {
    x: u64,
    y: u64,
};

const Region = struct {
    area: u64 = 0,
    per: u64 = 0,
};

fn calculate(map: std.ArrayList([]u8), loc: Location, region: *Region, visited: *[140][140]bool) void {
    if (visited[loc.y][loc.x]) {
        return;
    }
    region.area += 1;
    region.per += countCorners(map, loc);
    visited[loc.y][loc.x] = true;

    const char = map.items[loc.y][loc.x];

    if (loc.x < map.items[0].len - 1) {
        if (map.items[loc.y][loc.x + 1] == char) {
            calculate(map, .{ .x = loc.x + 1, .y = loc.y }, region, visited);
        }
    }
    if (loc.x > 0) {
        if (map.items[loc.y][loc.x - 1] == char) {
            calculate(map, .{ .x = loc.x - 1, .y = loc.y }, region, visited);
        }
    }
    if (loc.y < map.items.len - 1) {
        if (map.items[loc.y + 1][loc.x] == char) {
            calculate(map, .{ .x = loc.x, .y = loc.y + 1 }, region, visited);
        }
    }
    if (loc.y > 0) {
        if (map.items[loc.y - 1][loc.x] == char) {
            calculate(map, .{ .x = loc.x, .y = loc.y - 1 }, region, visited);
        }
    }
}

fn countCorners(map: std.ArrayList([]u8), loc: Location) u8 {
    const char = map.items[loc.y][loc.x];
    var NE: bool = false;
    var E: bool = false;
    var SE: bool = false;
    var S: bool = false;
    var SW: bool = false;
    var W: bool = false;
    var NW: bool = false;
    var N: bool = false;

    if (loc.x > 0) {
        W = map.items[loc.y][loc.x - 1] == char;
        if (loc.y > 0) {
            NW = map.items[loc.y - 1][loc.x - 1] == char;
        }
        if (loc.y < map.items.len - 1) {
            SW = map.items[loc.y + 1][loc.x - 1] == char;
        }
    }
    if (loc.x < map.items[0].len - 1) {
        E = map.items[loc.y][loc.x + 1] == char;
        if (loc.y > 0) {
            NE = map.items[loc.y - 1][loc.x + 1] == char;
        }
        if (loc.y < map.items.len - 1) {
            SE = map.items[loc.y + 1][loc.x + 1] == char;
        }
    }
    if (loc.y > 0) {
        N = map.items[loc.y - 1][loc.x] == char;
    }
    if (loc.y < map.items.len - 1) {
        S = map.items[loc.y + 1][loc.x] == char;
    }

    // No Neighbors
    if (!N and !S and !E and !W) return 4;
    // One Neighbor
    if (E and !W and !N and !S) return 2;
    if (!E and W and !N and !S) return 2;
    if (!E and !W and N and !S) return 2;
    if (!E and !W and !N and S) return 2;
    // Sandwiched
    if ((E and W and !N and !S) or (N and S and !E and !W)) return 0;
    // Two Neighbors
    if ((S and E) and !N and !W) {
        if (!SE) return 2;
        return 1;
    }
    if ((S and W) and !N and !E) {
        if (!SW) return 2;
        return 1;
    }
    if ((N and E) and !S and !W) {
        if (!NE) return 2;
        return 1;
    }
    if ((N and W) and !S and !E) {
        if (!NW) return 2;
        return 1;
    }

    //Three or Four Neighbors
    var count: u8 = 0;
    if (N and E and S and W) {
        if (!NE) count += 1;
        if (!SE) count += 1;
        if (!NW) count += 1;
        if (!SW) count += 1;
    } else if (N and E and S) {
        if (!NE) count += 1;
        if (!SE) count += 1;
    } else if (N and W and S) {
        if (!NW) count += 1;
        if (!SW) count += 1;
    } else if (E and W and N) {
        if (!NE) count += 1;
        if (!NW) count += 1;
    } else if (E and W and S) {
        if (!SE) count += 1;
        if (!SW) count += 1;
    }
    return count;
}
