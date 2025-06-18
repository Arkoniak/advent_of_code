const std = @import("std");
const print = std.debug.print;
const String = []const u8;
const Buf = makeCircularBuf(i32, 3);

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

// 587 is too low
fn predicate(line: String, skip_first: bool) !bool {
    var buf = Buf{};
    var it = std.mem.tokenizeAny(u8, line, " ");
    var is_dumped = false;
    if (skip_first) {
        _ = it.next();
        is_dumped = true;
    }
    while (it.next()) |token| {
        const val = try get_val(token);
        const buf2 = buf.push(val);
        if ((!is_delta(buf2)) or (!is_ordered(buf2))) {
            if (is_dumped) return false;
            is_dumped = true;
            continue;
        }

        buf = buf2;
    }

    return true;
}

fn get_val(x: ?String) !i32 {
    return try std.fmt.parseInt(i32, x orelse return error.InvalidFormat, 10);
}

pub fn main() !void {
    var buffer: [1000]u8 = undefined;
    const filename: String = if (std.os.argv.len > 1) std.mem.span(std.os.argv[1]) else "input_test.txt";
    var f = try std.fs.cwd().openFile(filename, .{ .mode = .read_only });
    defer f.close();
    var result: u32 = 0;
    while (try f.reader().readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        if (try predicate(line, false)) {
            result += 1;
            continue;
        }
        if (try predicate(line, true)) {
            result += 1;
            continue;
        }
        print("Bad line? {s}\n", .{line});
    }

    print("Result: {d}", .{result});
}
