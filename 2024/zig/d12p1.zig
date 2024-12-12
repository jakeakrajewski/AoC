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
    visited[loc.y][loc.x] = true;

    const char = map.items[loc.y][loc.x];

    if (loc.x < 139) {
        if (map.items[loc.y][loc.x + 1] == char) {
            calculate(map, .{ .x = loc.x + 1, .y = loc.y }, region, visited);
        } else {
            region.per += 1;
        }
    } else region.per += 1;
    if (loc.x > 0) {
        if (map.items[loc.y][loc.x - 1] == char) {
            calculate(map, .{ .x = loc.x - 1, .y = loc.y }, region, visited);
        } else {
            region.per += 1;
        }
    } else region.per += 1;
    if (loc.y < 139) {
        if (map.items[loc.y + 1][loc.x] == char) {
            calculate(map, .{ .x = loc.x, .y = loc.y + 1 }, region, visited);
        } else {
            region.per += 1;
        }
    } else region.per += 1;
    if (loc.y > 0) {
        if (map.items[loc.y - 1][loc.x] == char) {
            calculate(map, .{ .x = loc.x, .y = loc.y - 1 }, region, visited);
        } else {
            region.per += 1;
        }
    } else region.per += 1;
}
