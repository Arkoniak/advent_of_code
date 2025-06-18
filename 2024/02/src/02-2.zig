const std = @import("std");
const print = std.debug.print;
const String = []const u8;
const Buf = makeCircularBuf(i32, 3);

// fn count(iter: void, comptime predicate: *const fn () bool) u64 {
//     var counter: u64 = 0;
//     while (iter.next()) |item| {
//         if (predicate(item)) {
//             counter += 1;
//         }
//     }
//     return counter;
// }

fn makeCircularBuf(comptime T: type, comptime N: usize) type {
    return struct {
        buf: [N]T = undefined,
        fill: usize = 0,

        const Self = @This();
        const Last3 = struct { a: T, b: T, c: T };

        fn push(self: Self, item: T) Self {
            var buf2: [N]T = self.buf;
            if (self.fill < N) {
                buf2[self.fill] = item;
                return Self{ .fill = self.fill + 1, .buf = buf2 };
            } else {
                for (0..N - 1) |i| {
                    buf2[i] = self.buf[i + 1];
                }
                buf2[N - 1] = item;
                return Self{ .fill = self.fill, .buf = buf2 };
            }
        }

        fn last3(self: Self) Last3 {
            return Last3{
                .a = self.buf[self.fill - 3],
                .b = self.buf[self.fill - 2],
                .c = self.buf[self.fill - 1],
            };
        }
    };
}

test "verification of circular buf" {
    const buf = Buf{};

    try std.testing.expect(buf.buf.len == 3);
    try std.testing.expect(buf.fill == 0);

    const buf2 = buf.push(10);
    try std.testing.expect(buf2.fill == 1);
    try std.testing.expect(buf2.buf[0] == 10);

    const buf3 = buf2.push(5);
    try std.testing.expect(buf3.fill == 2);
    try std.testing.expect(buf3.buf[0] == 10);
    try std.testing.expect(buf3.buf[1] == 5);

    const buf4 = buf3.push(20);
    try std.testing.expect(buf4.fill == 3);
    try std.testing.expect(buf4.buf[0] == 10);
    try std.testing.expect(buf4.buf[1] == 5);
    try std.testing.expect(buf4.buf[2] == 20);

    const buf5 = buf4.push(25);
    try std.testing.expect(buf4.fill == 3);
    try std.testing.expect(buf4.buf[0] == 10);
    try std.testing.expect(buf4.buf[1] == 5);
    try std.testing.expect(buf4.buf[2] == 20);

    try std.testing.expect(buf5.fill == 3);
    try std.testing.expect(buf5.buf[0] == 5);
    try std.testing.expect(buf5.buf[1] == 20);
    try std.testing.expect(buf5.buf[2] == 25);
}

fn is_delta(buf: Buf) bool {
    if (buf.fill < 2) return true;
    const delta = @abs(buf.buf[buf.fill - 1] - buf.buf[buf.fill - 2]);
    if (delta < 1) return false;
    if (delta > 3) return false;

    return true;
}

fn is_ordered(buf: Buf) bool {
    if (buf.fill < 3) return true;
    const t = buf.last3();
    if ((t.a < t.b) and (t.b > t.c)) return false;
    if ((t.a > t.b) and (t.b < t.c)) return false;

    return true;
}

fn predicate_single(line: []i32, skip: i32) !bool {
    var buf = Buf{};
    // print("{any}\n", .{line});
    for (line, 0..) |val, i| {
        if (i == skip) continue;
        const buf2 = buf.push(val);
        if (!is_delta(buf2)) return false;
        if (!is_ordered(buf2)) return false;

        buf = buf2;
    }

    return true;
}

fn predicate(line: []i32) !bool {
    var i: i32 = -1;
    while (i < line.len) {
        if (try predicate_single(line, i)) return true;
        i += 1;
    }

    return false;
}

// fn predicate(line: String) !bool {
//     var buf = Buf{};
//     var it = std.mem.tokenizeAny(u8, line, " ");
//     while (it.next()) |token| {
//         const val = try get_val(token);
//         const buf2 = buf.push(val);
//         if (!is_delta(buf2)) return false;
//         if (!is_ordered(buf2)) return false;

//         buf = buf2;
//     }

//     return true;
// }

fn get_val(x: ?String) !i32 {
    return try std.fmt.parseInt(i32, x orelse return error.InvalidFormat, 10);
}

fn read_line(v: *[10]i32, line: String) ![]i32 {
    var i: usize = 0;
    var it = std.mem.tokenizeAny(u8, line, " ");
    while (it.next()) |token| {
        v[i] = try get_val(token);
        i += 1;
    }

    // print("{s}; {any}\n", .{ line, v[0..i] });
    return v[0..i];
}

pub fn main() !void {
    var buffer: [1000]u8 = undefined;
    const filename: String = if (std.os.argv.len > 1) std.mem.span(std.os.argv[1]) else "input_test.txt";
    var f = try std.fs.cwd().openFile(filename, .{ .mode = .read_only });
    defer f.close();
    var result: u32 = 0;
    var linebuf: [10]i32 = undefined;
    while (try f.reader().readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        const vals = try read_line(&linebuf, line);
        if (try predicate(vals)) {
            result += 1;
        }
    }

    print("Result: {d}", .{result});
}
