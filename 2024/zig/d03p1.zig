const std = @import("std");
const parser = @import("helpers/parser.zig");

pub fn main() !void {
    const start_time = std.time.milliTimestamp();
    var stdout = std.io.getStdOut().writer();
    var allocator = std.heap.page_allocator;
    const file_path = "../input/day3.txt";
    var lines = std.ArrayList([]u8).init(allocator);
    try parser.parseFile(&allocator, &lines, file_path);

    var sum: u32 = 0;
    for (0..lines.items.len) |i| {
        const line = lines.items[i];
        var split = std.mem.split(u8, line, "mul(");

        while (true) {
            const txt = split.next();
            if (txt) |t| {
                var paren_split = std.mem.split(u8, t, ")");
                const num_pair = paren_split.first();
                var num_split = std.mem.split(u8, num_pair, ",");
                const first = std.fmt.parseInt(u32, num_split.first(), 10) catch {
                    continue;
                };
                const next = num_split.next();

                if (next) |n| {
                    const second = std.fmt.parseInt(u32, n, 10) catch {
                        continue;
                    };
                    sum += first * second;
                }
            } else {
                break;
            }
        }
    }

    try stdout.print("{}", .{sum});
    const end_time = std.time.milliTimestamp();
    std.debug.print("\nTotal Time: {}ms\n", .{end_time - start_time});
}
