const std = @import("std");
const parser = @import("helpers/parser.zig");

pub fn main() !void {
    const start_time = std.time.milliTimestamp();
    var stdout = std.io.getStdOut().writer();
    var allocator = std.heap.page_allocator;
    const file_path = "../input/day2.txt";
    var lines = std.ArrayList([]u8).init(allocator);
    try parser.parseFile(&allocator, &lines, file_path);

    var nums = std.ArrayList(u32).init(allocator);

    var count: u32 = 0;

    for (0..lines.items.len) |i| {
        const line = lines.items[i];
        var split = std.mem.split(u8, line, " ");

        const first = try std.fmt.parseInt(u32, split.first(), 10);
        try nums.append(first);

        while (true) {
            if (split.next()) |s| {
                const next = try std.fmt.parseInt(u32, s, 10);
                try nums.append(next);
            } else {
                break;
            }
        }

        var nums_cp = nums;
        const arr = try nums_cp.toOwnedSlice();
        var diff_sort = std.sort.isSorted(u32, arr, {}, comptime cmpByValue);
        if (diff_sort and (std.sort.isSorted(u32, arr, {}, comptime std.sort.asc(u32)) or std.sort.isSorted(u32, arr, {}, comptime std.sort.desc(u32)))) {
            count += 1;
        } else {
            var arr_list = std.ArrayList(u32).init(allocator);
            for (0..arr.len) |l| {
                try arr_list.appendSlice(nums.items);
                _ = arr_list.orderedRemove(l);
                const arr2 = try arr_list.toOwnedSlice();

                diff_sort = std.sort.isSorted(u32, arr2, {}, comptime cmpByValue);
                if (diff_sort and (std.sort.isSorted(u32, arr2, {}, comptime std.sort.asc(u32)) or std.sort.isSorted(u32, arr2, {}, comptime std.sort.desc(u32)))) {
                    count += 1;
                    break;
                }
                arr_list.clearAndFree();
            }
        }

        nums.clearAndFree();
    }
    try stdout.print("\n{}", .{count});
    const end_time = std.time.milliTimestamp();
    std.debug.print("\nTotal Time: {}ms\n", .{end_time - start_time});
}

fn cmpByValue(context: void, a: u32, b: u32) bool {
    if (@TypeOf(context) != void) unreachable;
    const f: i32 = @intCast(a);
    const s: i32 = @intCast(b);
    const diff = @abs(f - s);
    return !(diff > 0 and diff < 4);
}
