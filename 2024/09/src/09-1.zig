const std = @import("std");
const print = std.debug.print;
const String = []const u8;

const Blocks = std.ArrayList(i64);

// length ~100k + 10k ids

pub fn read_input(comptime T: type, io_reader: T, alloc: std.mem.Allocator) Blocks {
    var buf_reader = std.io.bufferedReader(io_reader);
    const reader = buf_reader.reader();

    var total: usize = 0;
    var cnt: usize = 0;
    var buf = Blocks.initCapacity(alloc, 100000) catch unreachable;
    // var buf = std.ArrayList(i16).init(alloc);

    var id: i64 = 0;
    var is_zero: bool = false;

    while (true) {
        const char = reader.readByte() catch |err| switch (err) {
            error.EndOfStream => break,
            else => unreachable,
        };

        if (char == '\n') break;
        const len = char - '0';
        if (is_zero) {
            buf.appendNTimes(-1, len) catch unreachable;
        } else {
            buf.appendNTimes(id, len) catch unreachable;
            id += 1;
        }
        is_zero = !is_zero;
        total += char - '0';
        cnt += 1;
    }
    // print("Items: {any}\n", .{buf.items});
    // print("Length: {d}, size: {d}\n", .{ total, cnt });

    return buf;
}

pub fn postprocess(buf: Blocks) void {
    var idx1: usize = 0;
    var idx2: usize = buf.items.len - 1;

    while (idx1 < idx2) {
        while (buf.items[idx1] != -1) {
            idx1 += 1;
        }
        while (buf.items[idx2] == -1) {
            idx2 -= 1;
        }
        if (idx1 >= idx2) break;
        buf.items[idx1] = buf.items[idx2];
        buf.items[idx2] = -1;
        idx1 += 1;
        idx2 -= 1;
    }
}

pub fn score(buf: Blocks) u128 {
    var total: u128 = 0;

    for (buf.items, 0..) |v, idx| {
        if (v == -1) break;
        const v1: u128 = @intCast(v);
        total += v1 * idx;
    }
    return total;
}

pub fn main() void {
    // var buffer: [1000]u8 = undefined;
    const filename: String = if (std.os.argv.len > 1) std.mem.span(std.os.argv[1]) else "input_test.txt";
    var f = std.fs.cwd().openFile(filename, .{ .mode = .read_only }) catch unreachable;
    defer f.close();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const aalloc = arena.allocator();

    var buf = read_input(std.fs.File.Reader, f.reader(), aalloc);
    defer buf.deinit();
    postprocess(buf);
    const res = score(buf);
    // print("Items: {any}\n", .{buf.items});
    print("Result: {d}\n", .{res});
}
