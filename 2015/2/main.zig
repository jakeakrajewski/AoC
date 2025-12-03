const std = @import("std");
const builtin = @import("builtin");
const ArrayList = @import("list.zig").ArrayList;

pub fn parseFile(allocator: std.mem.Allocator, list: *ArrayList([]const u8), file_path: []const u8) !void {
    _ = allocator;
    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    var buffer: [40000]u8 = undefined;
    var reader_wrapper = file.reader(&buffer); 
    const reader = &reader_wrapper.interface;

    while (reader.takeDelimiterExclusive('\n')) |line| {
        // Trim the trailing carriage return ('\r') if present
        const trimmed_line = std.mem.trimRight(u8, line, "\r");
        
        // Append the trimmed line to your list
        try list.append(trimmed_line);
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

pub const pairs = [_][]const u8 {"aa", "bb", "cc", "dd", "ee", "ff", "gg", "hh", "ii", "jj", "kk", "ll", "mm", "nn", "oo", "pp", "qq", "rr", "ss", "tt", "uu", "vv", "ww", "xx", "yy", "zz"};

pub const cannot = [_][]const u8{"ab", "cd", "pq", "xy"};

pub fn main () !void {
    var allocator: std.mem.Allocator = undefined;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    allocator = arena.allocator();
    defer arena.deinit();

    var list = ArrayList([]const u8).init(allocator);
    defer list.deinit();

    try parseFile(allocator, &list, "input.txt");
    const lines = try list.toOwnedSlice();
    defer allocator.free(lines);

    try part1(lines);
    try part2(lines);
}

fn part1 (list: [][]const u8) !void {
    var total: usize = 0;
    for (list) |line| {
        var vowels: u8 = 0;
        for (line) |c| {
            if (c == 'a' or c == 'e' or c == 'i' or c == 'o' or c == 'u'){
                vowels += 1;
                if (vowels >= 3) break;
            }
        }
        if (vowels < 3) continue;

        var cannot_found = false;
        for (cannot) |not| {
            if (std.mem.containsAtLeast(u8, line, 1, not)){
              cannot_found = true;
              break;
            }
        }        
        if (cannot_found) continue;

        var pair_found = false;
        for (pairs) |pair| {
            if (std.mem.containsAtLeast(u8, line, 1, pair)){
              pair_found = true;
              break;
            }
        }
        
        if (!pair_found) continue;
        total += 1;
    }

    std.debug.print("\nTotal Good: {}", .{ total });
}

fn part2 (list: [][]const u8) !void {
    var total: usize = 0;
    for (list) |line| {
        var pair = false;
        var sandwich = false;
        for (0..line.len) |i| {
            if (!pair and i < line.len - 2){
                for (i + 2..line.len) |j| {
                    if (j < line.len - 1){
                        if (line[i] == line[j] and line[i + 1] == line[j + 1]) pair = true;
                    }
                }
            }

            if (!sandwich and i < line.len - 2){
                if (line[i] == line[i + 2] and line[i] != line[i + 1]) sandwich = true;
            }
            if (pair and sandwich){
                break;
            }
        }
        if (pair and sandwich){
            total += 1;
        }
    }

    std.debug.print("\nTotal Good: {}", .{ total });
}

