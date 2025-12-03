const std = @import("std");
const builtin = @import("builtin");
const ArrayList = @import("list.zig").ArrayList;

pub fn main () !void {
    var total: usize = 0;
    for (packages) |p| {
        total += p;
    }

    var weight = total / 3;
    var result = try recurse(0, 0, weight, 0, 1, 163245522776190722);
    std.debug.print("\nGroups of Three: {}", .{result});

    weight = total / 4;
    result = try recurse(0, 0, weight, 0, 1, 163245522776190722);
    std.debug.print("\nGroups of Four: {}", .{result});
}

pub fn recurse(index: usize, sum: usize, weight: usize, total_packages: usize, product: usize, min_result: usize) !usize {
    var mr: usize = min_result;
    if (sum > weight) return 163245522776190722;
    if (product > mr) return 163245522776190722;

    if (sum == weight) {
        return product;
    }

    if (index == packages.len) return 163245522776190722;

    for (index..packages.len) |i| {
        if (product > 163245522776190722) return 163245522776190722;
        const result = try recurse(i + 1, sum + packages[i], weight, total_packages + 1, product * packages[i], mr);
        if (result < mr) {
            mr = result;
        }
    }

    return mr;
}


const packages = [_]usize{1, 3, 5, 11, 13, 17, 19, 23, 29, 31, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113};


