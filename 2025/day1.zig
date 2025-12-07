const std = @import("std");

pub fn main () !void {
    var allocator: std.mem.Allocator = undefined;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    allocator = arena.allocator();
    defer arena.deinit();

    var list = try std.ArrayListUnmanaged([]const u8).initCapacity(allocator, 5000);
    try parseFile(allocator, &list, "inputs/1.txt");
    const lines = try list.toOwnedSlice(allocator);
    defer allocator.free(lines);

    const start = std.time.microTimestamp();
    try part1(lines);
    try part2(lines);
    const stop = std.time.microTimestamp();
    std.debug.print("{}", .{stop - start});
}

pub fn part1(lines: [][]const u8) !void {
    var count: usize = 0;
    var pos: usize = 50;
    
    for (lines) |line| {
        const num = try std.fmt.parseInt(usize, line[1..], 10);
        const movement = num % 100;
        
        if (line[0] == 'L') {
            pos = if (movement > pos) pos + 100 - movement else pos - movement;
        } else {
            pos = (pos + movement) % 100;
        }
        
        if (pos == 0) count += 1;
    }
    
    std.debug.print("{}\n", .{count});
}

pub fn part2(lines: [][]const u8) !void {
    var clicks: usize = 0;
    var pos: isize = 50;

    for (lines) |line| {
        const num = try std.fmt.parseInt(usize, line[1..],  10);
        const dir: i2 = if (line[0] == 'L') -1 else 1;
        for (0..num) |_| {
           pos += dir;
           if (pos < 0) pos = 99;
           if (pos > 99) pos = 0;
           if (pos == 0)  clicks += 1;
        }

    }

    std.debug.print("{}\n", .{ clicks });
}

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

