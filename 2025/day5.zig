const std = @import("std");

pub fn main () !void {
    var allocator: std.mem.Allocator = undefined;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    allocator = arena.allocator();
    defer arena.deinit();

    var list = try std.ArrayListUnmanaged([]const u8).initCapacity(allocator, 5000);
    try parseFile(allocator, &list, "inputs/5.txt");
    const lines = try list.toOwnedSlice(allocator);
    defer allocator.free(lines);

    var ranges = try std.ArrayListUnmanaged(Range).initCapacity(allocator, 5000);
    var ingredients = try std.ArrayListUnmanaged(usize).initCapacity(allocator, 5000);

    const start = std.time.microTimestamp();
    for (lines) |line| {
        if (line.len == 0) continue;

        if (std.mem.containsAtLeast(u8, line, 1, "-")){
            var split = std.mem.splitAny(u8, line, "-");
            const min = try std.fmt.parseInt(usize, split.first(), 10);
            const max = try std.fmt.parseInt(usize, split.next().?, 10);
            var range = Range{ .lower = min, .upper = max };

            var combined: bool = false;
            for (ranges.items) |*rng| {
                if (rng.checkAndCombine(&range)){
                    combined = true;
                    break;
                }
            }
            
            if (!combined) try ranges.append(allocator, range);
        } else{
            try ingredients.append(allocator, try std.fmt.parseInt(usize, line, 10));
        }
    }

    var combined = true;
    while (combined) {
        combined = false;
        for (0..ranges.items.len) |i| {
            if (i > ranges.items.len - 1) break;
            for (0..ranges.items.len) |j| {
                if (i == j) continue;
                if (j > ranges.items.len - 1) break;
                if (ranges.items[i].checkAndCombine(&ranges.items[j])) {
                    _ = ranges.orderedRemove(j);
                    combined = true;
                }
            }
        }
    }

    // for (ranges.items) |rng| {
    //     std.debug.print("{} - {}\n", .{ rng.lower, rng.upper });
    // }

    try part1(ranges, ingredients);
    try part2(ranges);
    const stop = std.time.microTimestamp();
    std.debug.print("{}", .{stop - start});
}

const Range = struct {
    lower: usize,
    upper: usize,

    fn checkAndCombine(self: *Range, other: *Range) bool {
        if (other.upper < self.lower or other.lower > self.upper) return false;
        if (other.lower >= self.lower and other.upper <= self.upper) return true; //Contains full range
        if (other.lower >= self.lower and other.upper >= self.upper){ //Expand upper
            self.upper = other.upper;
            return true;
        } 
        if (other.lower <= self.lower and other.upper >= self.upper){ //Expand full range
            self.upper = other.upper;
            self.lower = other.lower;
            return true;
        } 
        if (other.lower >= self.lower and other.upper <= self.upper){ //Expand lower
            self.lower = other.lower;     
            return true;
        }
        return false;
    }

    fn containsIngredient(self: *Range, val: usize) bool {
        return (val >= self.lower and val <= self.upper);
    }
};

pub fn part1(ranges: std.ArrayListUnmanaged(Range), ingredients: std.ArrayListUnmanaged(usize)) !void {
    var total: usize = 0;
    for (ingredients.items) |i| {
        for (ranges.items) |*r| {
            if (r.containsIngredient(i)){
                total += 1;
                // std.debug.print("{} in range: {} - {}\n", .{ i, r.lower, r.upper });
                break;
            }
        }
    }

    std.debug.print("{}\n", .{ total });
}

pub fn part2(ranges: std.ArrayListUnmanaged(Range)) !void {
    var total: usize = 0;
    for (ranges.items) |range| {
        total += (range.upper + 1) - range.lower;
    }

    std.debug.print("{}\n", .{ total });
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

