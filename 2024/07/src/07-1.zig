const std = @import("std");
const print = std.debug.print;
const String = []const u8;
const testing = std.testing;

pub fn validate(result: u64, acc: u64, data: []const u64) bool {
    if (data.len == 0) return acc == result;

    if (validate(result, acc + data[0], data[1..])) return true;

    return validate(result, acc * data[0], data[1..]);
}

fn get_val(x: ?String) !u64 {
    return try std.fmt.parseInt(u64, x orelse return error.InvalidFormat, 10);
}

pub fn main() !void {
    var buffer: [1000]u8 = undefined;
    var data: [1000]u64 = undefined;
    const filename: String = if (std.os.argv.len > 1) std.mem.span(std.os.argv[1]) else "input_test.txt";
    var f = try std.fs.cwd().openFile(filename, .{ .mode = .read_only });
    defer f.close();

    var total: usize = 0;

    while (try f.reader().readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        var it = std.mem.tokenizeAny(u8, line, ": ");
        const result = try get_val(it.next());

        const acc = try get_val(it.next());
        var i: usize = 0;
        while (it.next()) |token| {
            data[i] = try get_val(token);
            i += 1;
        }

        // print("Result: {}, acc: {}, i: {}, data: {any}\n", .{ result, acc, i, data[0..i] });

        if (validate(result, acc, data[0..i])) {
            total += result;
        }
    }

    print("Result: {}\n", .{total});
}

test "verify recursion" {
    const d1 = [_]u64{19};
    try testing.expect(validate(190, 10, &d1));

    const d2 = [_]u64{ 40, 27 };
    try testing.expect(validate(3267, 81, &d2));

    // 83: 17 5
    const d3 = [_]u64{5};
    try testing.expect(!validate(83, 17, &d3));

    // 21037: 9 7 18 13
    const d4 = [_]u64{ 7, 18, 13 };
    try testing.expect(!validate(21037, 9, &d4));

    // 292: 11 6 16 20
    const d5 = [_]u64{ 6, 16, 20 };
    try testing.expect(validate(292, 11, &d5));
}
