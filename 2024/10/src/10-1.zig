const std = @import("std");
const print = std.debug.print;
const String = []const u8;

const Dir = enum {
    Up,
    Right,
    Down,
    Left,
};

const Dirs = std.meta.tags(Dir);

const Summary = struct {
    nrows: u32,
    ncols: u32,
    n9: u32,

    pub fn format(
        self: Summary,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt; // Игнорируем формат строку в этом примере
        _ = options; // Игнорируем опции форматирования

        try writer.print("(nrows: {d}, ncols: {d}, 9s: {d})", .{ self.nrows, self.ncols, self.n9 });
    }
};

const Index = struct {
    nrows: u32,
    ncols: u32,
    rows: u32 = 0,
    cols: u32 = 0,
    idx: u32 = 0,
};

const Matrix = struct {
    nrows: u8,
    ncols: u8,
    grid: []u8,
    paths: []u256,
    // idx: Index,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, reader: anytype) Self {
        var buf: [2000]u8 = undefined;
        const bytes_read = reader.readAll(&buf) catch unreachable;
        var grid = allocator.alloc(u8, bytes_read) catch unreachable;
        var paths = allocator.alloc(u256, bytes_read) catch unreachable;

        var nrows: u8 = 0;
        var ncols: u8 = 0;
        var cols: u8 = 0;
        var nine_id: u256 = 1;
        var len: u32 = 0;
        for (0..bytes_read) |i| {
            const c = buf[i];
            switch (c) {
                '\n' => {
                    nrows += 1;
                    if (ncols == 0) {
                        ncols = cols;
                    } else {
                        std.debug.assert(ncols == cols);
                    }
                    cols = 0;
                },
                '9' => {
                    grid[len] = c - '0';
                    paths[len] = nine_id;
                    nine_id = nine_id << 1;
                    cols += 1;
                    len += 1;
                },
                else => {
                    grid[len] = c - '0';
                    paths[len] = 0;
                    cols += 1;
                    len += 1;
                },
            }
        }

        return .{ .nrows = nrows, .ncols = ncols, .grid = grid[0..len], .paths = paths[0..len] };
    }

    fn is_level(self: Self, index: u32, level: u8) bool {
        return self.grid[index] == level;
    }

    fn get_pos(self: Self, c: usize, r: usize, index: u32, dir: Dir) ?u32 {
        switch (dir) {
            .Up => {
                if (r == 0) return null;
                return index - self.ncols;
            },
            .Right => {
                if (c >= self.ncols - 1) return null;
                return index + 1;
            },
            .Down => {
                if (r >= self.nrows - 1) return null;
                return index + self.ncols;
            },
            .Left => {
                if (c == 0) return null;
                return index - 1;
            },
        }
    }

    pub fn step(self: *Self, level: u8) void {
        const next_level = level + 1;
        var k: u32 = 0;

        // while (self.next()) |idx| {
        //     if (!self.is_level(idx.idx, level)) continue;
        //     for (Dirs) |dir| {
        //         const pos = self.get_pos(idx.cols, idx.rows, idx.idx, dir) orelse continue;
        //         if (!self.is_level(pos, next_level)) continue;
        //         self.paths[idx.idx] = self.paths[idx.idx] | self.paths[pos];
        //     }
        // }

        for (0..self.nrows) |j| {
            for (0..self.ncols) |i| {
                defer k += 1;
                if (!self.is_level(k, level)) continue;
                for (Dirs) |dir| {
                    const pos = self.get_pos(i, j, k, dir) orelse continue;
                    if (!self.is_level(pos, next_level)) continue;
                    self.paths[k] = self.paths[k] | self.paths[pos];
                }
            }
        }
    }

    pub fn build_paths(self: *Self) void {
        for (0..9) |i| {
            const level: u8 = @intCast(8 - i);
            self.step(level);
        }
    }

    pub fn score(self: *Self) u64 {
        var total: u64 = 0;
        const len = self.paths.len;
        for (0..len) |i| {
            const goo: u32 = @intCast(i);
            if (!self.is_level(goo, 0)) continue;
            total += @popCount(self.paths[i]);
        }
        return total;
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        allocator.free(self.grid);
        allocator.free(self.paths);
    }
};

fn summarize(reader: anytype) Summary {
    var nrows: u32 = 0;
    var ncols: u32 = 0;
    var n9: u32 = 0;
    var cols: u32 = 0;

    while (reader.readByte() catch null) |c| {
        switch (c) {
            '\n' => {
                nrows += 1;
                if (ncols == 0) {
                    ncols = cols;
                } else {
                    std.debug.assert(ncols == cols);
                }
                cols = 0;
            },
            '9' => {
                cols += 1;
                n9 += 1;
            },
            else => cols += 1,
        }
    }

    return .{ .nrows = nrows, .ncols = ncols, .n9 = n9 };
}

pub fn main() !void {
    // var buffer: [1000]u8 = undefined;
    const filename: String = if (std.os.argv.len > 1) std.mem.span(std.os.argv[1]) else "input_test.txt";
    var f = try std.fs.cwd().openFile(filename, .{ .mode = .read_only });
    defer f.close();

    const main_reader = f.reader();
    // var buf_reader = std.io.bufferedReader(main_reader);
    // const reader = buf_reader.reader();

    // print("{}\n", .{summarize(reader)});

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const aalloc = arena.allocator();

    var matrix = Matrix.init(aalloc, main_reader);
    defer matrix.deinit(aalloc);

    // print("{any}\n", .{matrix});

    matrix.build_paths();
    print("Result: {d}\n", .{matrix.score()});
    // print("{any}\n", .{matrix});
}
