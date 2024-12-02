const std = @import("std");

pub fn parseFile(allocator: *std.mem.Allocator, list: *std.ArrayList([]u8), path: []const u8) !void {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    var reader = std.io.bufferedReader(file.reader());

    var line = try reader.reader().readUntilDelimiterOrEofAlloc(allocator.*, '\n', 4096);

    while (true) {
        if (line) |l| {
            try list.append(l);
            line = try reader.reader().readUntilDelimiterOrEofAlloc(allocator.*, '\n', 4096);
        } else {
            break;
        }
    }
}
