const std = @import("std");
const print = std.debug.print;
const String = []const u8;

const P = struct {
    r: i16,
    c: i16,

    pub fn init(r: i16, c: i16) P {
        return .{ .r = r, .c = c };
    }

    pub fn move(self: P, dir: V) P {
        return P{ .r = self.r + dir.r, .c = self.c + dir.c };
    }

    pub fn vec(self: P, p2: P) V {
        return V{ .r = p2.r - self.r, .c = p2.c - self.c };
    }

    pub fn format(
        self: P,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt; // Игнорируем формат строку в этом примере
        _ = options; // Игнорируем опции форматирования

        try writer.print("(r: {d}, c: {d})", .{ self.r, self.c });
    }
};

const V = struct {
    r: i16,
    c: i16,

    pub fn scale(self: V, lambda: usize) V {
        const l: i16 = @intCast(lambda);
        return .{ .r = self.r * l, .c = self.c * l };
    }
};

const Map = std.AutoHashMap(u8, std.ArrayList(P));
const Set = std.AutoHashMap(P, void);

const Puzzle = struct {
    rows: usize,
    cols: usize,
    map: Map,

    const Self = @This();

    pub fn read_input(comptime T: type, io_reader: T, allocator: std.mem.Allocator) !Self {
        var buf_reader = std.io.bufferedReader(io_reader);
        const reader = buf_reader.reader();

        var row: usize = 1;
        var col: usize = 1;
        var cols: usize = 0;
        var map: Map = Map.init(allocator);
        while (true) {
            const char = reader.readByte() catch |err| switch (err) {
                error.EndOfStream => break,
                else => return err,
            };
            switch (char) {
                '\n' => {
                    row += 1;
                    cols = col;
                    col = 1;
                },
                '.' => {
                    col += 1;
                },
                else => {
                    if (map.contains(char)) {
                        var list = map.get(char).?;
                        try list.append(P{ .r = @intCast(row), .c = @intCast(col) });
                        try map.put(char, list);
                    } else {
                        var list = std.ArrayList(P).init(allocator);
                        try list.append(P{ .r = @intCast(row), .c = @intCast(col) });
                        try map.put(char, list);
                    }
                    col += 1;
                },
            }
        }

        return .{ .rows = row - 1, .cols = cols - 1, .map = map };
    }

    fn gen_point(self: *const Self, p1: P, p2: P, k: usize) ?P {
        const p = p2.move(p1.vec(p2).scale(k));
        // print("P1: {}, P2: {}, P: {}\n", .{ p1, p2, p });
        if (p.r <= 0) return null;
        if (p.c <= 0) return null;
        if (p.r > self.rows) return null;
        if (p.c > self.cols) return null;

        return p;
    }

    fn items_count(self: *const Self, items: []P, set: *Set) !void {
        var j: usize = 0;
        for (0..items.len) |i| {
            j = i + 1;
            const p1 = items[i];
            while (j < items.len) {
                const p2 = items[j];
                var k: usize = 0;
                while (true) {
                    const pa = self.gen_point(p1, p2, k) orelse break;
                    try set.put(pa, {});
                    k += 1;
                }

                k = 0;
                while (true) {
                    const pa = self.gen_point(p2, p1, k) orelse break;
                    try set.put(pa, {});
                    k += 1;
                }
                j += 1;
            }
        }
    }

    pub fn count(self: *const Self, allocator: std.mem.Allocator) !usize {
        var it = self.map.iterator();
        var set = Set.init(allocator);
        while (it.next()) |entry| {
            try self.items_count(entry.value_ptr.*.items, &set);
        }

        // var it2 = set.iterator();
        // while (it2.next()) |v| {
        //     print("{}\n", .{v});
        // }

        return set.count();
    }
};

pub fn main() !void {
    // var buffer: [1000]u8 = undefined;
    const filename: String = if (std.os.argv.len > 1) std.mem.span(std.os.argv[1]) else "input_test.txt";
    var f = try std.fs.cwd().openFile(filename, .{ .mode = .read_only });
    defer f.close();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const aalloc = arena.allocator();

    const puzzle = try Puzzle.read_input(std.fs.File.Reader, f.reader(), aalloc);
    print("Result: {d}\n", .{try puzzle.count(aalloc)});

    // Example on how to read such an object
    // var it = puzzle.map.iterator();
    // while (it.next()) |entry| {
    //     print("{any}\n", .{entry.key_ptr.*});
    //     for (entry.value_ptr.*.items) |p| {
    //         print("{any} ", .{p});
    //     }
    //     print("\n", .{});
    // }
}
