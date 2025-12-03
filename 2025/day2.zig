const std = @import("std");

const input = "5529687-5587329,50-82,374-560,83-113,226375-287485,293169-368713,2034-2634,9945560-9993116,4872472-4904227,3218-5121,1074-1357,15451-26093,483468003-483498602,51513-85385,1466-1992,7600-13034,710570-789399,407363-480868,3996614725-3996662113,3-17,5414907798-5414992881,86274-120443,828669-909588,607353-700604,4242340614-4242556443,28750-44009,935177-1004747,20-41,74678832-74818251,8484825082-8484860878,2784096938-2784156610,5477-7589,621-952,2424167145-2424278200,147085-217900,93043740-93241586";

const mins = [12]usize{ 0, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 10000000000};
const maxs = [12]usize{ 0, 9, 99, 999, 9999, 99999, 999999, 9999999, 99999999, 999999999, 9999999999, 99999999999};

const Range = struct {
    min: usize,
    max: usize,
};

pub fn main () !void {
    var allocator: std.mem.Allocator = undefined;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    allocator = arena.allocator();
    defer arena.deinit();

    var list = try std.ArrayListUnmanaged(Range).initCapacity(allocator, 100);
    try getRanges(allocator, &list);

    try part1(allocator, list);
    const start = std.time.milliTimestamp();
    try part2(allocator, list);
    const stop = std.time.milliTimestamp();
    std.debug.print("{}", .{stop - start});
}

pub fn part1(allocator: std.mem.Allocator, list: std.ArrayListUnmanaged(Range)) !void {
    var total: usize = 0;

    for (list.items) |range| {
        const min_len = try getLength(allocator, range.min);
        const max_len = try getLength(allocator, range.max);

        if (min_len % 2 != 0 and max_len % 2 != 0) continue; 

        const min = if (min_len % 2 == 0) 
            try splitNum(allocator, range.min, 2) else
            try splitNum(allocator, mins[max_len], 2);

        const max = if (max_len % 2 == 0) 
            try splitNum(allocator, range.max, 2) else
            try splitNum(allocator, maxs[min_len], 2);

        for (min..(max + 1)) |n| {
            const joined = try joinNum(allocator, n, 2);
            if (joined >= range.min and joined <= range.max) total += joined;
        }
    }

    std.debug.print("{}\n", .{total});
}

pub fn part2(allocator: std.mem.Allocator, list: std.ArrayListUnmanaged(Range)) !void {
    var total: usize = 0;

    for (list.items) |range| {
        // const min_len = try getLength(allocator, range.min);
        const max_len = try getLength(allocator, range.max);
        var hash_map = std.AutoHashMap(usize, usize).init(allocator);
        defer hash_map.deinit();
        for (2..11) |x|{
            if (x > max_len) break;
            const split_max = maxs[max_len / x];

            for (0..split_max + 1) |n| {
                const joined = try joinNum(allocator, n, x);
                if (joined < range.min) continue;
                if (joined > range.max) break;
                if (joined >= range.min and joined <= range.max) {
                    if (!hash_map.contains(joined)){
                        total += joined;
                        try hash_map.put(joined, 0);
                    }
                }
            }
        }
    }

    std.debug.print("{}\n", .{total});
}

fn getRanges(allocator: std.mem.Allocator, list: *std.ArrayListUnmanaged(Range)) !void {
    var ranges = std.mem.splitAny(u8, input, ",");

    while (ranges.next()) |range| {
        var vals = std.mem.splitAny(u8, range, "-");
        try list.append(allocator, Range{
            .min = try std.fmt.parseInt(usize, vals.first(),  10),
            .max = try std.fmt.parseInt(usize, vals.next().?,  10),
        });
    }
}

fn getLength(allocator: std.mem.Allocator, num: usize) !usize {
    const str = try std.fmt.allocPrint(allocator, "{}", .{num});
    defer allocator.free(str);
    return str.len;
}

fn splitNum(allocator: std.mem.Allocator, num: usize, split: usize) !usize {
    const str = try std.fmt.allocPrint(allocator, "{}", .{num});
    defer allocator.free(str);
    return try std.fmt.parseInt(usize, str[0..str.len / split], 10);
}

fn firstChars(allocator: std.mem.Allocator, num: usize, split: usize) !usize {
    const str = try std.fmt.allocPrint(allocator, "{}", .{num});
    defer allocator.free(str);
    return std.fmt.parseInt(usize, str[0..split], 10) catch @panic(try std.fmt.allocPrint(allocator, "Failed to parse num: {} len: {}", .{num, split}));
}

fn joinNum(allocator: std.mem.Allocator, num: usize, joins: usize) !usize {
    var start: usize = num;
    for (0..joins - 1) |_|{
        const str = try std.fmt.allocPrint(allocator, "{}{}", .{start, num});
        defer allocator.free(str);
        start = try std.fmt.parseInt(usize, str, 10);
    }
    return start;
}


