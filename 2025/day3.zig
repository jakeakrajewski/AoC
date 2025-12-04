const std = @import("std");

pub fn main () !void {
    var allocator: std.mem.Allocator = undefined;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    allocator = arena.allocator();
    defer arena.deinit();

    var list = try std.ArrayListUnmanaged([]const u8).initCapacity(allocator, 5000);
    try parseFile(allocator, &list, "inputs/3.txt");
    const lines = try list.toOwnedSlice(allocator);
    defer allocator.free(lines);

    try part1(lines);

    const start = std.time.microTimestamp();
    try part2(lines);
    const stop = std.time.microTimestamp();
    std.debug.print("{}", .{stop - start});
}

const nums = "987654321";

pub fn part1(lines: [][]const u8) !void {
    var total: usize = 0;

    for (0..lines.len) |l| {
        const line = lines[l];
        var found: bool = false;
        for (nums) |n| {
            const index = std.mem.indexOf(u8, line, &[1]u8{ n });
            if (index == line.len - 1) continue;
            if (index) |i| {
                const slc = line[i + 1..];
                for (nums) |n2| {
                    const index2 = std.mem.indexOf(u8, slc, &[1]u8{ n2 });
                    if (index2) |_|{
                        const num_one = try std.fmt.parseInt(usize, &[1]u8{n}, 10);
                        const num_two = try std.fmt.parseInt(usize, &[1]u8{n2}, 10);
                        const val = num_one * 10 + num_two;
                        total += val;
                        found = true;
                        break;
                    }
                }
            }
            if (found) break; 
        }
    }

    std.debug.print("{}\n", .{ total });
}

pub fn part2(lines: [][]const u8) !void {
    var total: usize = 0;

    for (lines) |line| {
        var digits: usize = 12;
        var num = [12]usize{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
        var last_index: usize = 0;

        while( digits > 0 ){
            const left = line[last_index..];
            for (nums) |n| {
                const index = std.mem.indexOf(u8, left, &[1]u8{ n });
                if (index) |i| {
                    if (left.len - (i + 1) >= digits - 1) {
                        num[12 - digits] = try std.fmt.parseInt(usize, &[1]u8{n}, 10);
                        last_index = last_index + 1 + i;
                        digits -= 1;
                        break;
                    }
                }
            }
        }
        var result: usize = 0;
        for (num) |n| {
            result = result * 10 + n;
        }
        total += result;
    }

    std.debug.print("{}\n", .{ total });
}

// fn concat(allocator: std.mem.Allocator, a: []const u8, b: []const u8) ![]const u8 {
//     var out = try allocator.alloc(u8, a.len + b.len);
//     std.mem.copy(u8, out[0..a.len], a);
//     std.mem.copy(u8, out[a.len..], b);
//     return out;
// }


pub fn parseFile(allocator: std.mem.Allocator, list: *std.ArrayListUnmanaged([]const u8), file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    var buffer: [40000]u8 = undefined;
    var reader_wrapper = file.reader(&buffer); 
    const reader = &reader_wrapper.interface;

    while (reader.takeDelimiterExclusive('\n')) |line| {
        const trimmed_line = std.mem.trimRight(u8, line, "\r");
        
        try list.append(allocator, trimmed_line);
    } else |err| {
        switch (err) {
            error.EndOfStream => return,
            else => {
                std.debug.print("{}\n", .{err});
                @panic("Failed to parse input");
            },
        }
    }
}

