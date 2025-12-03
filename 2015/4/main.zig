const std = @import("std");
const crypto = std.crypto;
const builtin = @import("builtin");
const ArrayList = @import("list.zig").ArrayList;

const input = "iwrupvqb";

pub fn main () !void {
    var allocator: std.mem.Allocator = undefined;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    allocator = arena.allocator();
    defer arena.deinit();

    var i: usize = 0;
    while (true){
        const message =  try generateHashString(allocator, i);
        i += 1;
        const processed = crypto.hash.Md5.hashResult(message);
        if (processed[0] != 0) continue;
        if (processed[1] != 0) continue;
        if (processed[2] & @as(u8, 0xF0) == 0 ) break;
    }

    std.debug.print("\nFive Zeroes: {}", .{i - 1});

    while (true){
        const message =  try generateHashString(allocator, i);
        i += 1;
        const processed = crypto.hash.Md5.hashResult(message);
        if (processed[0] != 0) continue;
        if (processed[1] != 0) continue;
        if (processed[2] == 0 ) break;
    }

    std.debug.print("\nSix Zeroes: {}", .{i - 1});
}

fn generateHashString(allocator: std.mem.Allocator, i: usize) ![]const u8{
    return try std.fmt.allocPrint(allocator, "{s}{}", .{input, i});
}


