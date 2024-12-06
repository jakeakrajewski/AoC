const std = @import("std");
const parser = @import("helpers/parser.zig");

pub fn main() !void {
    const start_time = std.time.milliTimestamp();
    var stdout = std.io.getStdOut().writer();
    var allocator = std.heap.page_allocator;
    const file_path = "../input/day6.txt";
    var lines = std.ArrayList([]u8).init(allocator);
    try parser.parseFile(&allocator, &lines, file_path);

    var visited: [130][130]bool = undefined;
    var occupancy: [130][130]bool = undefined;

    const height: usize = 130;
    const width: usize = 130;

    var obj_line: usize = 0;
    var obj_col: usize = 0;

    var dir: Direction = .North;

    for (0..height) |line| {
        for (0..width) |col| {
            if (lines.items[line][col] == '^') {
                obj_line = line;
                obj_col = col;
                visited[line][col] = true;
            } else if (lines.items[line][col] == '#') {
                occupancy[line][col] = true;
            }
        }
    }

    while (true) {
        if (dir == .North) {
            if (obj_line == 0) break;
            if (!occupancy[obj_line - 1][obj_col]) {
                visited[obj_line - 1][obj_col] = true;
                obj_line -= 1;
            } else {
                dir = .East;
            }
        }
        if (dir == .South) {
            if (obj_line >= height - 1) break;
            if (!occupancy[obj_line + 1][obj_col]) {
                visited[obj_line + 1][obj_col] = true;
                obj_line += 1;
            } else {
                dir = .West;
            }
        }
        if (dir == .East) {
            if (obj_col >= width - 1) break;
            if (!occupancy[obj_line][obj_col + 1]) {
                visited[obj_line][obj_col + 1] = true;
                obj_col += 1;
            } else {
                dir = .South;
            }
        }
        if (dir == .West) {
            if (obj_col == 0) break;
            if (!occupancy[obj_line][obj_col - 1]) {
                visited[obj_line][obj_col - 1] = true;
                obj_col -= 1;
            } else {
                dir = .North;
            }
        }
    }

    var sum: u32 = 0;

    for (0..height) |line| {
        for (0..width) |col| {
            if (visited[line][col]) sum += 1;
        }
    }

    try stdout.print("{}", .{sum});
    const end_time = std.time.milliTimestamp();
    std.debug.print("\nTotal Time: {}ms\n", .{end_time - start_time});
}

const Direction = enum { North, South, East, West };
