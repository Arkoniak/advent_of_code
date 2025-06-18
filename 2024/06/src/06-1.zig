const std = @import("std");
const print = std.debug.print;
const String = []const u8;

const P = struct {
    r: i32,
    c: i32,

    pub fn init(r: i32, c: i32) P {
        return .{ .r = r, .c = c };
    }

    pub fn move(self: P, dir: Direction) P {
        const d = dir.toPoint();
        return P{ .r = self.r + d.r, .c = self.c + d.c };
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

const Direction = enum {
    up,
    right,
    down,
    left,

    fn toPoint(self: Direction) P {
        return switch (self) {
            .right => P{ .r = 0, .c = 1 },
            .down => P{ .r = 1, .c = 0 },
            .left => P{ .r = 0, .c = -1 },
            .up => P{ .r = -1, .c = 0 },
        };
    }
};

const Matrix = struct {
    data: []i8,
    rows: usize,
    cols: usize,

    pub fn get(self: *Matrix, p: P) ?i8 {
        if ((p.r <= 0) or (p.r > self.rows)) return null;
        if ((p.c <= 0) or (p.c > self.cols)) return null;

        const cols: i32 = @intCast(self.cols);
        const location: usize = @intCast(p.c - 1 + (p.r - 1) * cols);

        return self.data[location];
    }

    pub fn set(self: *Matrix, p: P, v: i8) void {
        if ((p.r <= 0) or (p.r > self.rows)) return;
        if ((p.c <= 0) or (p.c > self.cols)) return;

        const cols: i32 = @intCast(self.cols);
        const location: usize = @intCast(p.c - 1 + (p.r - 1) * cols);

        self.data[location] = v;
    }

    pub fn deinit(self: *Matrix, allocator: std.mem.Allocator) void {
        allocator.free(self.data);
    }
};

const Game = struct {
    m: Matrix,
    p: P,
    d: Direction,

    pub fn init(allocator: std.mem.Allocator, file_descriptor: std.fs.File) !Game {
        var data = try allocator.alloc(i8, 1_000_000);
        const buffer = try allocator.alloc(u8, 1_000);
        defer allocator.free(buffer);

        var p = P{ .r = 0, .c = 0 };
        const dir = .up;

        var cols: usize = 0;
        var rows: usize = 0;
        var position: usize = 0;

        while (try file_descriptor.reader().readUntilDelimiterOrEof(buffer, '\n')) |line| {
            rows += 1;
            cols = line.len;
            for (line, 1..) |c, i| {
                switch (c) {
                    '.' => data[position] = 0,
                    '#' => data[position] = -1,
                    '^' => {
                        data[position] = 1;
                        p = P{ .r = @intCast(rows), .c = @intCast(i) };
                    },
                    else => {},
                }
                position += 1;
            }
        }

        return .{ .m = Matrix{ .data = data, .rows = rows, .cols = cols }, .p = p, .d = dir };
    }

    pub fn deinit(game: *Game, allocator: std.mem.Allocator) void {
        game.m.deinit(allocator);
    }

    pub fn step(game: *Game) bool {
        const next = game.p.move(game.d);

        const v = game.m.get(next) orelse return false;
        switch (v) {
            0 => {
                game.p = next;
                game.m.set(next, 1);
            },
            1 => {
                game.p = next;
            },
            -1 => {
                game.d = rotate(game.d);
            },
            else => {},
        }

        return true;
    }

    pub fn totalize(game: *Game) usize {
        var total: usize = 0;

        for (0..game.m.rows * game.m.cols) |i| {
            if (game.m.data[i] > 0) total += 1;
        }

        return total;
    }
};

pub fn rotate(d: Direction) Direction {
    return switch (d) {
        .up => .right,
        .right => .down,
        .down => .left,
        .left => .up,
    };
}

pub fn main() !void {
    // var buffer: [1000]u8 = undefined;
    const filename: String = if (std.os.argv.len > 1) std.mem.span(std.os.argv[1]) else "input_test.txt";
    var f = try std.fs.cwd().openFile(filename, .{ .mode = .read_only });
    defer f.close();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        const deinit_status = gpa.deinit();
        //fail test; can't try in defer as defer is executed after we return
        if (deinit_status == .leak) print("LEAK DETECTED!!!\n", .{});
    }

    var game = try Game.init(allocator, f);
    defer game.deinit(allocator);

    print("Position: {}\n", .{game.p});

    while (game.step()) {}
    print("Result: {d}\n", .{game.totalize()});
}
