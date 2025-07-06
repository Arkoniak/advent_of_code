const std = @import("std");
const print = std.debug.print;
const String = []const u8;
const testing = std.testing;

const Blocks = std.ArrayList(usize);
const Spaces = std.AutoHashMap(u8, Blocks);
const File = struct {
    idx: usize,
    len: u8,
    id: u64,

    pub fn score(file: File) usize {
        const term1 = (file.len * (file.len - 1)) >> 1;
        const term2 = file.len * file.idx;
        return (term1 + term2) * file.id;
    }
};

const Files = std.ArrayList(File);
fn totalScore(files: Files) usize {
    var total: usize = 0;
    for (files.items) |file| {
        total += file.score();
    }

    return total;
}

fn validate(files: *Files) bool {
    std.mem.sort(File, files.items, {}, struct {
        fn compare(context: void, a: File, b: File) bool {
            _ = context;
            return a.idx < b.idx;
        }
    }.compare);

    var last_idx: usize = 0;
    for (files.items, 0..) |file, i| {
        if (i == 0) {
            last_idx = file.len;
            continue;
        }

        if (file.idx < last_idx) return false;
        last_idx = file.idx + file.len;
    }

    return true;
}

const Drive = struct {
    s: Spaces,
    f: Files,

    pub fn solve(d: *Drive) void {
        for (0..d.f.items.len) |i| {
            const ni = d.f.items.len - i - 1;
            const item = d.f.items[ni];
            const si = d.find_min(item.idx, item.len) orelse continue;
            const idx = d.s.get(si).?.getLast();
            d.f.items[ni] = .{ .idx = idx, .len = item.len, .id = item.id };

            d.update_spaces(si, item.len);
        }
    }

    fn find_min(d: Drive, idx: usize, len: u8) ?u8 {
        var i: u8 = len;
        var res: ?u8 = null;
        var min_idx: usize = 0;
        while (i < 10) {
            defer i += 1;
            const b = d.s.get(i) orelse continue;
            // print("i = {d}; b = {any}\n", .{ i, b.items });
            const item = b.getLastOrNull() orelse continue;
            // print("i = {d}; item = {d}; (item > idx?) = {}; res = {any}; b = {any}\n", .{ i, item, item > idx, res, b.items });
            if (item > idx) continue;
            if (res == null) {
                res = i;
                min_idx = item;
            } else {
                // print("res is not null; item < res.? = {}\n", .{item < res.?});
                if (item < min_idx) {
                    res = i;
                    min_idx = item;
                }
            }

            // print("res = {any}\n", .{res});
        }

        return res;
    }

    fn update_spaces(d: *Drive, i: u8, len: u8) void {
        const newi = i - len;
        var old_blocks = d.s.get(i).?;
        const idx = old_blocks.pop().? + len;
        d.s.put(i, old_blocks) catch unreachable;

        if (newi == 0) return;
        var blocks = d.s.get(newi).?;
        blocks.append(idx) catch unreachable;
        std.mem.sort(usize, blocks.items, {}, std.sort.desc(usize));

        d.s.put(newi, blocks) catch unreachable;
    }

    fn score(d: Drive) usize {
        return totalScore(d.f);
    }
};

fn printSpaces(spaces: *const Spaces) void {
    var iterator = spaces.iterator();

    while (iterator.next()) |entry| {
        const key = entry.key_ptr.*;
        const blocks = entry.value_ptr;

        if (blocks.items.len == 0) continue;

        std.debug.print("{}: [", .{key});

        for (blocks.items, 0..) |block, i| {
            if (i > 0) std.debug.print(", ", .{});
            std.debug.print("{}", .{block});
        }

        std.debug.print("]\n", .{});
    }
}

fn printFiles(files: *const Files) void {
    for (files.items) |file| {
        print(" {d}-({d}:{d}) ", .{ file.id, file.idx, file.idx + file.len - 1 });
    }
    print("\n", .{});
}

fn printFiles2(files: *const Files) void {
    var last_idx: usize = 0;
    for (files.items) |file| {
        while (last_idx < file.idx) {
            print(".", .{});
            last_idx += 1;
        }
        while (last_idx < (file.idx + file.len)) {
            print("{d}", .{file.id});
            last_idx += 1;
        }
    }
    print("\n", .{});
}
// length ~100k + 10k ids

pub fn read_input(comptime T: type, io_reader: T, alloc: std.mem.Allocator) Drive {
    var buf_reader = std.io.bufferedReader(io_reader);
    const reader = buf_reader.reader();

    var s: Spaces = Spaces.init(alloc);
    var f: Files = Files.init(alloc);
    var is_zero: bool = false;
    var idx: usize = 0;
    var id: u64 = 0;

    for (0..10) |i| {
        const pf = Blocks.init(alloc);
        const ii: u8 = @intCast(i);
        s.put(ii, pf) catch unreachable;
    }
    while (true) {
        const char = reader.readByte() catch |err| switch (err) {
            error.EndOfStream => break,
            else => unreachable,
        };

        if (char == '\n') break;
        const len = char - '0';
        if (is_zero) {
            var blocks = s.get(len).?;
            blocks.append(idx) catch unreachable;
            s.put(len, blocks) catch unreachable;
        } else {
            f.append(.{ .idx = idx, .len = len, .id = id }) catch unreachable;
            id += 1;
        }

        is_zero = !is_zero;
        idx += len;
    }

    var iterator = s.iterator();

    while (iterator.next()) |entry| {
        const blocks = entry.value_ptr;
        std.mem.sort(usize, blocks.items, {}, std.sort.desc(usize));
    }

    return .{ .s = s, .f = f };
}

pub fn main() void {
    // var buffer: [1000]u8 = undefined;
    const filename: String = if (std.os.argv.len > 1) std.mem.span(std.os.argv[1]) else "input_test.txt";
    var f = std.fs.cwd().openFile(filename, .{ .mode = .read_only }) catch unreachable;
    defer f.close();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const aalloc = arena.allocator();

    var drive = read_input(std.fs.File.Reader, f.reader(), aalloc);
    // printFiles(&drive.f);

    // print("\n{any}\n", .{drive.find_min(74, 1)});

    // print("files: {any}\n", .{drive.f.items});
    // print("spaces: {any}\n", .{drive.s});
    // printSpaces(&drive.s);
    drive.solve();
    // print("files: {any}\n", .{drive.f.items});
    print("Result: {d}\n", .{drive.score()});

    // print("Validation: {}\n", .{validate(&drive.f)});
    // printFiles2(&drive.f);
    // print("files: {any}\n", .{drive.f.items});
}

test "scoring" {
    const file1: File = .{ .idx = 2, .len = 2, .id = 9 };
    try testing.expect(file1.score() == 2 * 9 + 3 * 9);

    const file2: File = .{ .idx = 2, .len = 3, .id = 9 };
    try testing.expect(file2.score() == 2 * 9 + 3 * 9 + 4 * 9);

    const file3: File = .{ .idx = 2, .len = 4, .id = 9 };
    try testing.expect(file3.score() == 2 * 9 + 3 * 9 + 4 * 9 + 5 * 9);

    const file4: File = .{ .idx = 2, .len = 5, .id = 9 };
    try testing.expect(file4.score() == 2 * 9 + 3 * 9 + 4 * 9 + 5 * 9 + 6 * 9);

    const file5: File = .{ .idx = 2, .len = 6, .id = 9 };
    try testing.expect(file5.score() == 2 * 9 + 3 * 9 + 4 * 9 + 5 * 9 + 6 * 9 + 7 * 9);
}
