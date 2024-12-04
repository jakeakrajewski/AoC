const std = @import("std");
const parser = @import("helpers/parser.zig");

pub fn main() !void {
    const start_time = std.time.milliTimestamp();
    var stdout = std.io.getStdOut().writer();
    var allocator = std.heap.page_allocator;
    const file_path = "../input/day4.txt";
    var lines = std.ArrayList([]u8).init(allocator);
    try parser.parseFile(&allocator, &lines, file_path);

    var count: u32 = 0;

    for (0..lines.items.len) |i| {
        const line = lines.items[i];

        for (0..line.len) |c| {
            if (line[c] != 'X') {
                continue;
            }

            var E: [4]u8 = [4]u8{ 'X', 0, 0, 0 };
            var S: [4]u8 = [4]u8{ 'X', 0, 0, 0 };
            var W: [4]u8 = [4]u8{ 'X', 0, 0, 0 };
            var N: [4]u8 = [4]u8{ 'X', 0, 0, 0 };
            var NE: [4]u8 = [4]u8{ 'X', 0, 0, 0 };
            var SE: [4]u8 = [4]u8{ 'X', 0, 0, 0 };
            var SW: [4]u8 = [4]u8{ 'X', 0, 0, 0 };
            var NW: [4]u8 = [4]u8{ 'X', 0, 0, 0 };

            for (1..4) |d| {
                //East
                if (c + d < line.len) {
                    E[d] = line[c + d];
                }
                //South
                if (i + d < lines.items.len) {
                    S[d] = lines.items[i + d][c];
                }
                //West
                if (c >= 3) {
                    W[d] = line[c - d];
                }
                //North
                if (i >= 3) {
                    N[d] = lines.items[i - d][c];
                }
                //NorthEast
                if (i >= 3 and c + d < line.len) {
                    NE[d] = lines.items[i - d][c + d];
                }
                //SouthEast
                if (i + d < lines.items.len and c + d < line.len) {
                    SE[d] = lines.items[i + d][c + d];
                }
                //SouthWest
                if (i + d < lines.items.len and c >= 3) {
                    SW[d] = lines.items[i + d][c - d];
                }
                //NorthWest
                if (i >= 3 and c >= 3) {
                    NW[d] = lines.items[i - d][c - d];
                }
            }

            if (std.mem.eql(u8, &E, "XMAS")) count += 1;
            if (std.mem.eql(u8, &S, "XMAS")) count += 1;
            if (std.mem.eql(u8, &W, "XMAS")) count += 1;
            if (std.mem.eql(u8, &N, "XMAS")) count += 1;
            if (std.mem.eql(u8, &NE, "XMAS")) count += 1;
            if (std.mem.eql(u8, &SE, "XMAS")) count += 1;
            if (std.mem.eql(u8, &SW, "XMAS")) count += 1;
            if (std.mem.eql(u8, &NW, "XMAS")) count += 1;
        }
    }

    try stdout.print("{}", .{count});
    const end_time = std.time.milliTimestamp();
    std.debug.print("\nTotal Time: {}ms\n", .{end_time - start_time});
}
