const std = @import("std");
const parser = @import("helpers/parser.zig");

pub fn main() !void {
    const start_time = std.time.milliTimestamp();
    var stdout = std.io.getStdOut().writer();
    var allocator = std.heap.page_allocator;
    const file_path = "../input/day6.txt";
    var lines = std.ArrayList([]u8).init(allocator);
    try parser.parseFile(&allocator, &lines, file_path);

    var nodes: [130][130]Node = undefined;
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
                nodes[line][col].visited = true;
                nodes[line][col].direction = .North;
            } else if (lines.items[line][col] == '#') {
                occupancy[line][col] = true;
            }
        }
    }

    var sum: u32 = 0;

    const start_line = obj_line;
    const start_col = obj_col;

    while (true) {
        if (dir == .North) {
            if (obj_line == 0) break;
            if (!occupancy[obj_line - 1][obj_col]) {
                nodes[obj_line - 1][obj_col].visited = true;
                nodes[obj_line - 1][obj_col].direction = .North;
                obj_line -= 1;
            } else {
                dir = .East;
            }
        }
        if (dir == .South) {
            if (obj_line >= height - 1) break;
            if (!occupancy[obj_line + 1][obj_col]) {
                nodes[obj_line + 1][obj_col].visited = true;
                nodes[obj_line + 1][obj_col].direction = .South;
                obj_line += 1;
            } else {
                dir = .West;
            }
        }
        if (dir == .East) {
            if (obj_col >= width - 1) break;
            if (!occupancy[obj_line][obj_col + 1]) {
                nodes[obj_line][obj_col + 1].visited = true;
                nodes[obj_line][obj_col + 1].direction = .East;
                obj_col += 1;
            } else {
                dir = .South;
            }
        }
        if (dir == .West) {
            if (obj_col == 0) break;
            if (!occupancy[obj_line][obj_col - 1]) {
                nodes[obj_line][obj_col - 1].visited = true;
                nodes[obj_line][obj_col - 1].direction = .West;
                obj_col -= 1;
            } else {
                dir = .North;
            }
        }
    }

    for (0..130) |l| {
        for (0..130) |c| {
            if (nodes[l][c].visited) {
                var occ = occupancy;
                occ[l][c] = true;

                var old_line = start_line;
                var old_col = start_col;
                var old_dir = nodes[start_line][start_col].direction;
                var old_nodes = nodes;
                while (true) {
                    if (old_nodes[old_line][old_col].directionTraveled(old_dir)) {
                        sum += 1;
                        break;
                    }
                    if (old_dir == .North) {
                        if (old_line == 0) break;
                        if (!occ[old_line - 1][old_col]) {
                            old_nodes[old_line - 1][old_col].visited = true;
                            old_nodes[old_line - 1][old_col].direction = .North;
                            old_nodes[old_line][old_col].north = true;
                            old_line -= 1;
                        } else {
                            old_dir = .East;
                        }
                    } else if (old_dir == .South) {
                        if (old_line >= height - 1) break;
                        if (!occ[old_line + 1][old_col]) {
                            old_nodes[old_line + 1][old_col].visited = true;
                            old_nodes[old_line + 1][old_col].direction = .South;
                            old_nodes[old_line][old_col].south = true;
                            old_line += 1;
                        } else {
                            old_dir = .West;
                        }
                    } else if (old_dir == .East) {
                        if (old_col >= width - 1) break;
                        if (!occ[old_line][old_col + 1]) {
                            old_nodes[old_line][old_col + 1].visited = true;
                            old_nodes[old_line][old_col + 1].direction = .East;
                            old_nodes[old_line][old_col].east = true;
                            old_col += 1;
                        } else {
                            old_dir = .South;
                        }
                    } else if (old_dir == .West) {
                        if (old_col == 0) break;
                        if (!occ[old_line][old_col - 1]) {
                            old_nodes[old_line][old_col - 1].visited = true;
                            old_nodes[old_line][old_col - 1].direction = .West;
                            old_nodes[old_line][old_col].west = true;
                            old_col -= 1;
                        } else {
                            old_dir = .North;
                        }
                    }
                }
            }
        }
    }

    try stdout.print("{}", .{sum});
    const end_time = std.time.milliTimestamp();
    std.debug.print("\nTotal Time: {}ms\n", .{end_time - start_time});
}

const Direction = enum { North, South, East, West, Univisited };

const Node = struct {
    visited: bool = true,
    direction: Direction = .Univisited,
    north: bool = false,
    south: bool = false,
    east: bool = false,
    west: bool = false,

    fn directionTraveled(self: *Node, dir: Direction) bool {
        switch (dir) {
            .North => return self.north,
            .East => return self.east,
            .South => return self.south,
            .West => return self.west,
            else => return false,
        }
    }
};
