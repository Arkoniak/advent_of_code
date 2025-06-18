const std = @import("std");
const print = std.debug.print;
const String = []const u8;

const P = struct {
    r: i32,
    c: i32,

    pub fn init(r: i32, c: i32) P {
        return .{ .r = r, .c = c };
    }

    pub fn move(self: P, dir: P) P {
        return P{ .r = self.r + dir.r, .c = self.c + dir.c };
    }
};

const Matrix = struct {
    data: []u8,
    rows: usize,
    cols: usize,

    pub fn init(allocator: std.mem.Allocator, file_descriptor: std.fs.File) !Matrix {
        var data = try allocator.alloc(u8, 1_000_000);
        const buffer = try allocator.alloc(u8, 1_000);
        defer allocator.free(buffer);

        var cols: usize = 0;
        var rows: usize = 0;
        var position: usize = 0;

        while (try file_descriptor.reader().readUntilDelimiterOrEof(buffer, '\n')) |line| {
            rows += 1;
            cols = line.len;
            @memcpy(data[position .. position + cols], line);
            position += cols;
        }

        return Matrix{ .data = data, .rows = rows, .cols = cols };
    }

    pub fn get(self: *Matrix, p: P) ?u8 {
        if ((p.r <= 0) or (p.r > self.rows)) return null;
        if ((p.c <= 0) or (p.c > self.cols)) return null;

        const cols: i32 = @intCast(self.cols);
        const location: usize = @intCast(p.c - 1 + (p.r - 1) * cols);

        return self.data[location];
    }

    pub fn deinit(self: *Matrix, allocator: std.mem.Allocator) void {
        allocator.free(self.data);
    }
};

const MIter = struct {
    rows: usize,
    cols: usize,
    position: P = P.init(0, 0),

    pub fn init(matrix: *Matrix) MIter {
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

const DIRS = [_]P{
    P.init(0, 1),
    P.init(0, -1),
    P.init(1, 0),
    P.init(-1, 0),
    P.init(1, 1),
    P.init(-1, -1),
    P.init(1, -1),
    P.init(-1, 1),
};

fn readWord(buffer: *[4]u8, m: *Matrix, p0: P, dir: P) bool {
    var p = p0;
    for (0..4) |i| {
        const c = m.get(p) orelse return false;
        buffer[i] = c;

        p = p.move(dir);
    }

    return true;
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

    var matrix = try Matrix.init(allocator, f);
    defer matrix.deinit(allocator);

    print("{d} x {d}\n", .{ matrix.rows, matrix.cols });

    print("{c}, {c}, {any}\n", .{ matrix.get(P.init(1, 1)).?, matrix.get(P.init(10, 10)).?, matrix.get(P.init(0, 11)) });

    var buf: [4]u8 = undefined;
    _ = readWord(&buf, &matrix, P.init(1, 6), DIRS[0]);
    print("{s}\n", .{buf});

    if (std.mem.eql(u8, &buf, "XMAS")) print("YES!\n", .{});

    var result: u32 = 0;
    var it = MIter.init(&matrix);
    while (it.next()) |p| {
        for (DIRS) |dir| {
            if (readWord(&buf, &matrix, p, dir)) {
                if (std.mem.eql(u8, &buf, "XMAS")) result += 1;
            }
        }
    }

    print("Result: {d}\n", .{result});
    // var it = MIter.init(&matrix);
    // while (it.next()) |p| {
    //     print("{c}\n", .{matrix.get(p).?});
    // }
}
