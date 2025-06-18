const std = @import("std");
const print = std.debug.print;
const String = []const u8;

// Функция, которую мы хотим протестировать
fn add(a: i32, b: i32) i32 {
    return a + b;
}

// Тестовая функция
test "проверка функции add" {
    try std.testing.expect(add(2, 2) == 4);
    try std.testing.expect(add(-1, 1) == 0);
}

// const MapEntry = struct {
//     buf: [100_000]u8 = undefined,
//     fba: std.heap.FixedBufferAllocator = undefined,
//     allocator: std.mem.Allocator = undefined,
//     map: std.AutoHashMap(i32, u32) = undefined,

//     // pub fn init(self: *MapEntry, comptime BufSize: usize) void {
//     pub fn init(self: *MapEntry) void {
//         // var mapbuf: [BufSize]u8 = undefined;
//         // self.buf = &mapbuf;
//         self.fba = std.heap.FixedBufferAllocator.init(&self.buf);
//         self.allocator = self.fba.allocator();
//         self.map = std.AutoHashMap(i32, u32).init(self.allocator);
//     }

//     pub fn deinit(self: *MapEntry) void {
//         self.map.deinit();
//     }
// };

// const MapWithBuf = struct {
//     map: std.AutoHashMap(i32, u32),
//     allocator: std.mem.Allocator,
//     fba: std.heap.FixedBufferAllocator,
//     // buf: [100_000]u8,
// };

// fn makeMap(buf: []u8) MapWithBuf {
//     var fba = std.heap.FixedBufferAllocator.init(buf);
//     const allocator = fba.allocator();
//     return .{
//         .map = std.AutoHashMap(i32, u32).init(allocator),
//         .allocator = allocator,
//         .fba = fba,
//         // .buf = buf, // нужно сохранить буфер, чтобы он не умер
//     };
// }

const MapWithBuffer = struct {
    const Self = @This();
    const buffer_size = 100000;
    const MapType = std.AutoHashMap(i32, u32);

    buffer: [buffer_size]u8 = undefined,
    fba: std.heap.FixedBufferAllocator = undefined,
    map: std.AutoHashMap(i32, u32) = undefined,

    pub fn init() Self {
        var self = Self{};

        self.fba = std.heap.FixedBufferAllocator.init(&self.buffer);
        const allocator = self.fba.allocator();
        self.map = std.AutoHashMap(i32, u32).init(allocator);

        return self;
    }

    pub fn init2(self: *Self) void {
        self.fba = std.heap.FixedBufferAllocator.init(&self.buffer);
        const allocator = self.fba.allocator();
        self.map = std.AutoHashMap(i32, u32).init(allocator);
    }

    pub fn deinit(self: *Self) void {
        self.map.deinit();
    }

    // // Способ 3: Универсальный форвардинг с comptime (продвинутый)
    // pub fn forward(self: *Self, comptime method_name: []const u8, args: anytype) !@TypeOf(@call(.auto, @field(MapType, method_name), .{&self.map} ++ args)) {
    //     return @call(.auto, @field(MapType, method_name), .{&self.map} ++ args);
    // }

    pub fn put(self: *Self, key: i32, value: u32) !void {
        return @call(.auto, MapType.put, .{ &self.map, key, value });
    }

    pub fn get(self: *Self, key: i32) ?u32 {
        return self.map.get(key);
    }

    pub fn iterator(self: *Self) MapType.Iterator {
        return self.map.iterator();
    }
};

fn fnMapWithBuffer(comptime buffer_size: usize) type {
    return struct {
        const Self = @This();
        const MapType = std.AutoHashMap(i32, u32);

        buffer: [buffer_size]u8 = undefined,
        fba: std.heap.FixedBufferAllocator = undefined,
        map: MapType = undefined,

        pub fn init() Self {
            var self = Self{};

            self.fba = std.heap.FixedBufferAllocator.init(&self.buffer);
            const allocator = self.fba.allocator();
            self.map = MapType.init(allocator);

            return self;
        }

        pub fn deinit(self: *Self) void {
            self.map.deinit();
        }

        // Форвардинг методов
        pub fn put(self: *Self, key: i32, value: u32) !void {
            return self.map.put(key, value);
        }

        pub fn get(self: *Self, key: i32) ?u32 {
            return self.map.get(key);
        }

        pub fn remove(self: *Self, key: i32) bool {
            return self.map.remove(key);
        }

        pub fn iterator(self: *Self) MapType.Iterator {
            return self.map.iterator();
        }

        pub fn contains(self: *Self, key: i32) bool {
            return self.map.contains(key);
        }

        pub fn count(self: *Self) u32 {
            return self.map.count();
        }

        pub fn getBufferSize() usize {
            return buffer_size;
        }
    };
}

fn createMap(comptime buffer_size: usize) fnMapWithBuffer(buffer_size) {
    return fnMapWithBuffer(buffer_size).init();
}

pub fn main() !void {
    // // Let's do something interesting and use fixed buffer allocator
    var buffer: [1000]u8 = undefined;
    // // var fba = std.heap.FixedBufferAllocator.init(&buffer);
    // // const allocator = fba.allocator();

    const filename: String = if (std.os.argv.len > 1) std.mem.span(std.os.argv[1]) else "input_test.txt";
    var f = try std.fs.cwd().openFile(filename, .{ .mode = .read_only });
    defer f.close();

    // var mapbuf1: [100_000]u8 = undefined;
    // var map1 = makeMap(&mapbuf1);
    // defer map1.map.deinit();

    // var mapbuf2: [100_000]u8 = undefined;
    // var map2 = makeMap(&mapbuf2);
    // defer map2.map.deinit();

    // // var maps: [2]MapEntry = undefined;
    // // for (&maps) |*m| {
    // //     m.init();
    // //     defer m.deinit();
    // // }

    // try map1.map.put(1, 42);
    // try map2.map.put(2, 84);

    // print("value = {}\n", .{map1.map.get(1).?});

    // var map1 = MapWithBuffer.init();
    var map1 = MapWithBuffer.init();
    defer map1.deinit();

    // var map2 = MapWithBuffer{};
    // map2.init2();
    var map2 = fnMapWithBuffer(100_000).init();
    defer map2.deinit();

    // try map1.put(10, 1000);
    // try map2.put(20, 2000);

    // print("value = {}\n", .{map1.get(10).?});
    // print("value = {}\n", .{map2.get(20).?});

    // var mapbuf1: [100000]u8 = undefined;
    // var fba1 = std.heap.FixedBufferAllocator.init(&mapbuf1);
    // const allocator1 = fba1.allocator();
    // var mapbuf2: [100000]u8 = undefined;
    // var fba2 = std.heap.FixedBufferAllocator.init(&mapbuf2);
    // const allocator2 = fba2.allocator();

    // var map1 = std.AutoHashMap(i32, u32).init(allocator1);
    // defer map1.deinit();

    // var map2 = std.AutoHashMap(i32, u32).init(allocator2);
    // defer map2.deinit();

    while (try f.reader().readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        // print("Line: {s}\n", .{line});
        var it = std.mem.tokenizeAny(u8, line, " ");
        const val1: i32 = try std.fmt.parseInt(i32, it.next() orelse return error.InvalidFormat, 10);
        const val2: i32 = try std.fmt.parseInt(i32, it.next() orelse return error.InvalidFormat, 10);

        try map1.put(val1, (map1.get(val1) orelse 0) + 1);
        try map2.put(val2, (map2.get(val2) orelse 0) + 1);

        // print("Processed: {} {}\n", .{ val1, val2 });
        // var i: u4 = 0;
        // while (it.next()) |y| {
        //     if (i == 0) {
        //         i += 1;
        //         print("value 1: {d}\n", .{});
        //     } else {
        //         print("value 2: {d}\n\n", .{try std.fmt.parseInt(i32, y, 10)});
        //     }
        // }
    }

    var it = map1.iterator();
    var result: u32 = 0;
    while (it.next()) |entry| {
        const k = entry.key_ptr.*;
        const k1: u32 = @intCast(k);
        const v1 = entry.value_ptr.*;
        const v2 = map2.get(k) orelse 0;
        result += k1 * v1 * v2;
        // print("entry: {d} -> {d}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
    }
    print("Total: {d}\n", .{result});
}
