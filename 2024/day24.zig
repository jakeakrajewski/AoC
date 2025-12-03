const std = @import("std");

const input = [4]u8{ "459A", "671A", "846A", "285A", "083A" };

pub fn main() !void {
    var allocator: std.mem.Allocator = undefined;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    allocator = arena.allocator();
    defer arena.deinit();

    const key_pad = try KeyPad.initNumberPad(allocator, null);

    const path = key_pad.goToKey(7);
    if (path) |p|{
        for (p) |c|{
           if (c != null) std.debug.print("{c}", .{c.?}) else std.debug.print("x", .{});
        }
    }
}

pub const Key = struct {
    id: u8,
    up: ?*Key,
    down: ?*Key,
    left: ?*Key,
    right: ?*Key,

    fn setNeightbors(self: *Key, up: ?*Key, left: ?*Key, down: ?*Key, right: ?*Key) void{
        self.up = up;
        self.left = left;
        self.right  = right;
        self.down = down;
    }

};

fn search(current: *Key, depth: usize, goal: u8, path: [8]?u8) [8]?u8 {
   if (depth > 6) return path;

   var up_path: [8]?u8 = path;
   var down_path: [8]?u8 = path;
   var left_path: [8]?u8 = path;
   var right_path: [8]?u8 = path;

   if (current.up) |up| {
     up_path[depth] = '^';
     if (up.id == goal){
        up_path[depth + 1] = 'A';
        return up_path;
     } else {
        up_path = search(up, depth + 1, goal, up_path);
     }
   } 
   if (current.left) |left| {
     left_path[depth] = '<';
     if (left.id == goal){
        left_path[depth + 1] = 'A';
        return left_path;
     } else {
        left_path = search(left, depth + 1, goal, left_path);
     }
   }
   if (current.down) |down| {
     down_path[depth] = 'v';
     if (down.id == goal){
        down_path[depth + 1] = 'A';
        return down_path;
     } else {
        down_path = search(down, depth + 1, goal, down_path);
     }
   }
   if (current.right) |right| {
    right_path[depth] = '>';
     if (right.id == goal){
        right_path[depth + 1] = 'A';
        return right_path;
     } else {
        right_path = search(right, depth + 1, goal, right_path);
     }
   }

   var new_path = path;
   var best_depth: usize = 6;

   for ([4][8]?u8{ up_path, down_path, left_path, right_path }) |p| {
       for (0..p.len) |i| {
           if (p[i] == 'A'){
               if (i < best_depth) {
                   best_depth = i;
                   new_path = p;
               }
           }
       }
   }

   return new_path;
}

const PadType = enum { Number, Directional };

pub const KeyPad = struct {
    current_key: *Key,
    child: ?*KeyPad,
    pad_type: PadType,

    fn handleInput(self: *KeyPad, char: u8) void {
        if (char == 'A') return;
        const next = switch (char) {
            '^' => self.current_key.up,
            '<' => self.current_key.left,
            'v' => self.current_key.down,
            '>' => self.current_key.right,
            else => null,
        };

        if (next) |n| {
            self.current_key = n;
        } else {
            @panic("Invalid key");
        }
    }

    fn goToKey(self: *KeyPad, goal: u8) ?[]const ?u8 {
        const search_path = [8]?u8{ null, null, null, null, null, null, null, null };
        const path = search(self.current_key, 0, goal, search_path);
        for (0..path.len) |i| {
            if (path[i] == null) return path[0..i];
            self.handleInput(path[i].?);
        }

        return &path;
    }


    fn initNumberPad(allocator: std.mem.Allocator, child: ?*KeyPad) !*KeyPad {
        const key_pad = try allocator.create(KeyPad);

        const key_a = try allocator.create(Key);
        const key_1 = try allocator.create(Key);
        const key_2 = try allocator.create(Key);
        const key_3 = try allocator.create(Key);
        const key_4 = try allocator.create(Key);
        const key_5 = try allocator.create(Key);
        const key_6 = try allocator.create(Key);
        const key_7 = try allocator.create(Key);
        const key_8 = try allocator.create(Key);
        const key_9 = try allocator.create(Key);
        const key_0 = try allocator.create(Key);

        key_a.id = 'A';
        key_1.id = 1;
        key_2.id = 2;
        key_3.id = 3;
        key_4.id = 4;
        key_5.id = 5;
        key_6.id = 6;
        key_7.id = 7;
        key_8.id = 8;
        key_9.id = 9;
        key_0.id = 0;

        key_7.setNeightbors(null, null, key_4, key_8);  key_8.setNeightbors(null, key_7, key_5, key_9);  key_9.setNeightbors(null,  key_8, key_6, null);
        key_4.setNeightbors(key_7, null, key_1, key_5); key_5.setNeightbors(key_8, key_4, key_2, key_6); key_6.setNeightbors(key_9, key_5, key_3, null);
        key_1.setNeightbors(key_4, null, null, key_2);  key_2.setNeightbors(key_5, key_1, key_0, key_3); key_3.setNeightbors(key_6, key_2, key_a, null);
                                                        key_0.setNeightbors(key_2, null, null, key_a);   key_a.setNeightbors(key_3, key_0, null, null);

        key_pad.current_key = key_a;
        key_pad.child = child;
        key_pad.pad_type = .Number;
        return key_pad;
    }

    fn initDirectionalPad(allocator: std.mem.Allocator, child: ?*KeyPad) !*KeyPad {
        const key_pad = try allocator.create(KeyPad);

        const key_a = try allocator.create(Key);
        const key_up = try allocator.create(Key);
        const key_down = try allocator.create(Key);
        const key_left = try allocator.create(Key);
        const key_right = try allocator.create(Key);

        key_a.id = 'A';
        key_up.id = '^';
        key_down.id = 'v';
        key_left.id = '<';
        key_right.id = '>';

        key_pad.current_key = key_a;
        key_pad.child = child;
        key_pad.pad_type = .Directional;

        return key_pad;
    }
};
