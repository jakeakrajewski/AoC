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

    try parts(lines);
}

fn parts (list: [][]const u8) !void {
    var total: usize = 0;
    var ribbon: usize = 0;

    for (list) |line| {
        std.debug.print("Parsing: {s}\n", .{ line });
        var split = std.mem.splitAny(u8, line, "x");

        const l = try std.fmt.parseInt(usize, split.first(),  10);
        const w = try std.fmt.parseInt(usize, split.next().?,  10);
        const h = try std.fmt.parseInt(usize, split.next().?,  10);

        total += 2 * l * w + 2 * l * h + 2 * w * h;
        total += @min(l * w, w * h, l * h);

        const ribbon_length = @min(2 * l + 2 * w, 2 * l + 2 * h, 2 * w + 2 * h);
        const bow_length = l * w * h;

        ribbon += ribbon_length + bow_length;
    }

    std.debug.print("\nTotal Paper: {}", .{ total });
    std.debug.print("\nTotal Ribbon: {}", .{ ribbon });
}

