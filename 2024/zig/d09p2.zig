const std = @import("std");
const parser = @import("helpers/parser.zig");

pub fn main() !void {
    const start_time = std.time.milliTimestamp();
    var stdout = std.io.getStdOut().writer();
    var allocator = std.heap.page_allocator;
    const file_path = "../input/day9.txt";
    var lines = std.ArrayList([]u8).init(allocator);
    try parser.parseFile(&allocator, &lines, file_path);

    var disc_blocks = std.ArrayList(DiscBlock).init(allocator);

    var id: usize = 0;

    for (0..lines.items[0].len) |i| {
        const char = lines.items[0][i];
        const num = try std.fmt.parseInt(u32, &[_]u8{char}, 10);
        if (i % 2 == 0) {
            try disc_blocks.append(.{ .empty = false, .len = num, .id = id });
            id += 1;
        } else {
            try disc_blocks.append(.{ .empty = true, .len = num, .id = null });
        }
    }

    var sorted = false;
    var disc_index: usize = 0;

    while (!sorted) {
        disc_index = 0;
        sorted = true;
        while (disc_index < disc_blocks.items.len) {
            const index = disc_blocks.items.len - 1 - disc_index;
            if (!disc_blocks.items[index].empty and !disc_blocks.items[index].moved) {
                for (0..index) |j| {
                    if (!disc_blocks.items[j].empty) continue;
                    if (disc_blocks.items[j].len >= disc_blocks.items[index].len) {
                        sorted = false;
                        const new_len = disc_blocks.items[index].len;
                        const new_id = disc_blocks.items[index].id.?;
                        disc_blocks.items[index].empty = true;
                        if (disc_blocks.items[j].len > new_len) {
                            try disc_blocks.insert(j + 1, .{ .empty = true, .len = disc_blocks.items[j].len - new_len, .id = null });
                        }
                        disc_blocks.items[j].len = new_len;
                        disc_blocks.items[j].id = new_id;
                        disc_blocks.items[j].moved = true;
                        disc_blocks.items[j].empty = false;
                        break;
                    }
                }
            }
            disc_index += 1;
        }
    }

    var sum: u64 = 0;
    var c: u64 = 0;

    for (0..disc_blocks.items.len) |i| {
        const disc_block = disc_blocks.items[i];
        if (!disc_block.empty) {
            for (0..disc_block.len) |_| {
                sum += disc_block.id.? * c;
                c += 1;
            }
        } else {
            for (0..disc_block.len) |_| {}
            c += disc_block.len;
        }
    }

    try stdout.print("{}", .{sum});
    const end_time = std.time.milliTimestamp();
    std.debug.print("\nTotal Time: {}ms\n", .{end_time - start_time});
}

pub const DiscBlock = struct {
    empty: bool = true,
    len: u64 = 0,
    id: ?u64,
    moved: bool = false,
};
