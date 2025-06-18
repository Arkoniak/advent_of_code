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

    fn toFlag(self: Direction) i8 {
        return switch (self) {
            .up => 1,
            .right => 1 << 1,
            .down => 1 << 2,
            .left => 1 << 3,
        };
    }
};

fn Matrix(comptime T: type) type {
    return struct {
        data: []T,
        rows: usize,
        cols: usize,

        const Self = @This();

        pub fn get(self: *Self, p: P) ?T {
            if ((p.r <= 0) or (p.r > self.rows)) return null;
            if ((p.c <= 0) or (p.c > self.cols)) return null;

            const cols: i32 = @intCast(self.cols);
            const location: usize = @intCast(p.c - 1 + (p.r - 1) * cols);

            return self.data[location];
        }

        pub fn set(self: *Self, p: P, v: T) void {
            if ((p.r <= 0) or (p.r > self.rows)) return;
            if ((p.c <= 0) or (p.c > self.cols)) return;

            const cols: i32 = @intCast(self.cols);
            const location: usize = @intCast(p.c - 1 + (p.r - 1) * cols);

            self.data[location] = v;
        }

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            allocator.free(self.data);
        }

        pub fn clone(self: *Self, allocator: std.mem.Allocator) !Self {
            return .{ .data = try std.mem.Allocator.dupe(allocator, T, self.data), .rows = self.rows, .cols = self.cols };
        }
    };
}

const MatrixI8 = Matrix(i8);

const State = enum {
    regular,
    outbound,
    cycled,
};

const Game = struct {
    m: MatrixI8,
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

        return .{ .m = .{ .data = data, .rows = rows, .cols = cols }, .p = p, .d = dir };
    }

    pub fn deinit(game: *Game, allocator: std.mem.Allocator) void {
        game.m.deinit(allocator);
    }

    pub fn clone(self: *Game, allocator: std.mem.Allocator) !Game {
        return .{ .m = try self.m.clone(allocator), .p = self.p, .d = self.d };
    }

    pub fn step(game: *Game) State {
        const next = game.p.move(game.d);

        const v = game.m.get(next) orelse return .outbound;
        switch (v) {
            -1 => {
                game.d = rotate(game.d);

                const df = game.d.toFlag();
                const curv = game.m.get(game.p).?;
                if ((curv & df) != 0) return .cycled;
                game.m.set(game.p, curv | df);
            },
            else => {
                game.p = next;
                const df = game.d.toFlag();
                if ((v & df) != 0) return .cycled;
                game.m.set(next, v | df);
            },
        }

        return .regular;
    }

    pub fn isFinite(game: *Game) bool {
        while (true) {
            const state = game.step();
            if (state == .outbound) return true;
            if (state == .cycled) return false;
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

const MIter = struct {
    rows: usize,
    cols: usize,
    position: P = .{ .r = 0, .c = 0 },

    pub fn init(matrix: *MatrixI8) MIter {
        return .{ .rows = matrix.rows, .cols = matrix.cols };
    }

    pub fn next(self: *MIter) ?P {
        const p = self.position;
        if (p.r == 0) {
            self.position = P.init(1, 1);
            return self.position;
        }
        if ((p.r == self.rows) and (p.c == self.cols)) return null;
        self.position = if (p.c == self.cols) P.init(p.r + 1, 1) else P.init(p.r, p.c + 1);
        return self.position;
    }
};

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

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const aalloc = arena.allocator();

    var it = MIter.init(&game.m);
    var total: usize = 0;
    while (it.next()) |p| {
        var game2 = try game.clone(aalloc);
        defer _ = arena.reset(.retain_capacity);
        if (game2.m.get(p) != 0) continue;
        game2.m.set(p, -1);
        if (!game2.isFinite()) total += 1;
        // print("Obstacle: {}, Finity: {}\n", .{ p, game2.isFinite() });
    }

    print("Result: {}\n", .{total});
    // const isf = game.isFinite();
    // print("Finitiy: {}\n", .{isf});

    // game2.m.set(.{ .r = 7, .c = 4 }, -1);
    // const isf2 = game2.isFinite();
    // print("Finitiy: {}\n", .{isf2});

    // while (true) {
    //     const state = game.step();
    //     if (state == .outbound) break;
    // }
    // print("Position: {}\n", .{game.p});
    // print("Result: {d}\n", .{game.totalize()});

    // print("Position: {}\n", .{game2.p});
    // print("Result: {d}\n", .{game2.totalize()});
}
