const std = @import("std");
const ArrayList = @import("list.zig").ArrayList;


pub fn main() !void {
    var allocator: std.mem.Allocator = undefined;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    allocator = arena.allocator();
    defer arena.deinit();

    var list = ArrayList([]const u8).init(allocator);
    defer list.deinit();
    try parseFile(allocator, &list, "input.txt");

    const input = try parseLines(allocator, &list);
    part1(input);
    part2(input);

}

fn part1(lines: []Line) void {
    var i: usize = 0;
    var register_a: usize = 0;
    var register_b: usize = 0;

    while (i < lines.len){
        const line = lines[i];
        std.debug.print("\nLine:{} CMD: {s} Reg: {s} Num: {}", .{ i, @tagName(line.cmd), @tagName(line.reg), line.num});
        const reg = if (line.reg == .A) &register_a else &register_b;

        switch (line.cmd) {
            .hlf => {
                reg.* /= 2;
                i += 1;
            },
            .tpl => {
                reg.* *= 3;
                i += 1;
            },
            .inc => {
                reg.* += 1;
                i += 1;
            },
            .jmp => {
                i = @intCast(@as(isize, @intCast(i)) + line.num);
            },
            .jie => {
                if ((line.reg == .A and register_a % 2 == 0) or (line.reg == .B and register_b % 2 == 0)){
                    i = @intCast(@as(isize, @intCast(i)) + line.num);
                } else {
                    i += 1;
                }
            },
            .jio => {
                if ((line.reg == .A and register_a == 1) or (line.reg == .B and register_b == 1)){
                    i = @intCast(@as(isize, @intCast(i)) + line.num);
                } else {
                    i += 1;
                }
            },
        }
    }

    std.debug.print("\nReg B: {}", .{register_b});
}

fn part2(lines: []Line) void {
    var i: usize = 0;
    var register_a: usize = 1;
    var register_b: usize = 0;

    while (i < lines.len){
        const line = lines[i];
        std.debug.print("\nLine:{} CMD: {s} Reg: {s} Num: {}", .{ i, @tagName(line.cmd), @tagName(line.reg), line.num});
        const reg = if (line.reg == .A) &register_a else &register_b;

        switch (line.cmd) {
            .hlf => {
                reg.* /= 2;
                i += 1;
            },
            .tpl => {
                reg.* *= 3;
                i += 1;
            },
            .inc => {
                reg.* += 1;
                i += 1;
            },
            .jmp => {
                i = @intCast(@as(isize, @intCast(i)) + line.num);
            },
            .jie => {
                if ((line.reg == .A and register_a % 2 == 0) or (line.reg == .B and register_b % 2 == 0)){
                    i = @intCast(@as(isize, @intCast(i)) + line.num);
                } else {
                    i += 1;
                }
            },
            .jio => {
                if ((line.reg == .A and register_a == 1) or (line.reg == .B and register_b == 1)){
                    i = @intCast(@as(isize, @intCast(i)) + line.num);
                } else {
                    i += 1;
                }
            },
        }
    }

    std.debug.print("\nReg B: {}", .{register_b});
}

const Command = enum{ hlf, tpl, inc, jmp, jie, jio };

const Register = enum { A, B };

const Line = struct {
    cmd: Command,
    reg: Register = .A,
    num: i8 = 0,
};

fn parseLines(allocator: std.mem.Allocator, list: *ArrayList([]const u8)) ![]Line {
    var output = ArrayList(Line).init(allocator);
    defer output.deinit();

    for (list.items) |line| {
       std.debug.print("\n{s}", .{line});
       var split = std.mem.splitAny(u8, line, " ");
       const command = split.first();
       std.debug.print("\n{s}", .{command});
       const command_enum = std.meta.stringToEnum(Command, command);
       const second = split.next();
       const reg: Register = if (second.?[0] == 'b') .B else .A;
       const num = if (command_enum != .inc and command_enum != .hlf and command_enum != .tpl) if (command_enum == .jmp) try std.fmt.parseInt(i8, second.?, 10) else try std.fmt.parseInt(i8, split.next().?, 10) else 0;
       try output.append(.{ .cmd = command_enum.?, .reg = reg, .num = num });
    }

    return try output.toOwnedSlice();
}

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

