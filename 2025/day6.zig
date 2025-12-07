const std = @import("std");

pub fn main () !void {
    var allocator: std.mem.Allocator = undefined;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    allocator = arena.allocator();
    defer arena.deinit();

    var list = try std.ArrayListUnmanaged([]const u8).initCapacity(allocator, 5000);
    try parseFile(allocator, &list, "inputs/6.txt");
    const lines = try list.toOwnedSlice(allocator);
    defer allocator.free(lines);

    var line_0 = try std.ArrayListUnmanaged(?usize).initCapacity(allocator, 5000);
    var line_1 = try std.ArrayListUnmanaged(?usize).initCapacity(allocator, 5000);
    var line_2 = try std.ArrayListUnmanaged(?usize).initCapacity(allocator, 5000);
    var line_3 = try std.ArrayListUnmanaged(?usize).initCapacity(allocator, 5000);

    var part1: usize = 0;
    var part2: usize = 0;

    const start = std.time.microTimestamp();
    var operation: Operation = if (lines[4][0] == '+') .ADD else .MULT;
    for (0..lines[0].len) |i| {
        if ((lines[4][i] != ' ' and i != 0) or i == lines[0].len - 1)  {
            if (i == lines[0].len - 1) {
                try line_0.append(allocator, if (lines[0][i] == ' ') null else try std.fmt.parseInt(usize, &[1]u8{ lines[0][i] }, 10));
                try line_1.append(allocator, if (lines[1][i] == ' ') null else try std.fmt.parseInt(usize, &[1]u8{ lines[1][i] }, 10));
                try line_2.append(allocator, if (lines[2][i] == ' ') null else try std.fmt.parseInt(usize, &[1]u8{ lines[2][i] }, 10));
                try line_3.append(allocator, if (lines[3][i] == ' ') null else try std.fmt.parseInt(usize, &[1]u8{ lines[3][i] }, 10));
            }
            const row_0 = try line_0.toOwnedSlice(allocator);
            const row_1 = try line_1.toOwnedSlice(allocator);
            const row_2 = try line_2.toOwnedSlice(allocator);
            const row_3 = try line_3.toOwnedSlice(allocator);

            var h0 = combineDigits(row_0);
            var h1 = combineDigits(row_1);
            var h2 = combineDigits(row_2);
            var h3 = combineDigits(row_3);

            if (operation == .ADD) {
                const val = h0 + h1 + h2 + h3; 
                part1 += val; 
            } else if (operation == .MULT){
              if (h0 == 0) h0 = 1;
              if (h1 == 0) h1 = 1;
              if (h2 == 0) h2 = 1;
              if (h3 == 0) h3 = 1;

              const val = h0 * h1 * h2 * h3; 
              part1 += val;
            }

            var vert_total: usize = 0;
            for (0..row_0.len) |j| {
                const array = [4]?usize{ row_0[j], row_1[j], row_2[j], row_3[j] };
                if ( row_0[j] == null and row_1[j] == null and row_2[j] == null and row_3[j] == null) continue;
                const val = combineDigits(&array);
                if (j == 0){ vert_total = val;
                } else {
                    if (operation == .ADD) vert_total += val else vert_total *= val;
                }
            }
            part2 += vert_total;

            line_0.clearAndFree(allocator);
            line_1.clearAndFree(allocator);
            line_2.clearAndFree(allocator);
            line_3.clearAndFree(allocator);

            try line_0.append(allocator, if (lines[0][i] == ' ') null else try std.fmt.parseInt(usize, &[1]u8{ lines[0][i] }, 10));
            try line_1.append(allocator, if (lines[1][i] == ' ') null else try std.fmt.parseInt(usize, &[1]u8{ lines[1][i] }, 10));
            try line_2.append(allocator, if (lines[2][i] == ' ') null else try std.fmt.parseInt(usize, &[1]u8{ lines[2][i] }, 10));
            try line_3.append(allocator, if (lines[3][i] == ' ') null else try std.fmt.parseInt(usize, &[1]u8{ lines[3][i] }, 10));

            if (lines[4][i] == '+') operation = .ADD else operation = .MULT;
        } else {
            try line_0.append(allocator, if (lines[0][i] == ' ') null else try std.fmt.parseInt(usize, &[1]u8{ lines[0][i] }, 10));
            try line_1.append(allocator, if (lines[1][i] == ' ') null else try std.fmt.parseInt(usize, &[1]u8{ lines[1][i] }, 10));
            try line_2.append(allocator, if (lines[2][i] == ' ') null else try std.fmt.parseInt(usize, &[1]u8{ lines[2][i] }, 10));
            try line_3.append(allocator, if (lines[3][i] == ' ') null else try std.fmt.parseInt(usize, &[1]u8{ lines[3][i] }, 10));
        }
    }

    const stop = std.time.microTimestamp();

    std.debug.print("Part 1: {}\n", .{ part1 });
    std.debug.print("Part 2: {}\n", .{ part2 });
    std.debug.print("Total Time: {}us\n", .{ stop - start });
}

const Operation = enum { ADD, MULT };

pub fn combineDigits(digits: []const ?usize) usize {
    var result: usize = 0;
    for (digits) |d| {
        if (d) |digit| {
            result = result * 10 + digit;
        }
    }
    return result;
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

