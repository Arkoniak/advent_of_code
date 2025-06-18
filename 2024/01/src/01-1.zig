const std = @import("std");
const print = std.debug.print;
// Defining this type made it easier for me to understand allocating a
// string array.
const String = []const u8;

// const stdout_file = std.io.getStdOut().writer();
// var bw = std.io.bufferedWriter(stdout_file);
// const stdout = bw.writer();

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    const cwd = std.fs.cwd();
    const filename: []const u8 = if (std.os.argv.len > 1) std.mem.span(std.os.argv[1]) else "input_test.txt";
    const stats = try cwd.statFile(filename);
    // print("file size = {d}", .{stats.size});
    const fileContents = try cwd.readFileAlloc(allocator, filename, stats.size + 1);
    defer allocator.free(fileContents);

    // Print file contents
    // print("{s}", .{fileContents});
    var list1 = std.ArrayList(i32).init(allocator);
    defer list1.deinit();
    var list2 = std.ArrayList(i32).init(allocator);
    defer list2.deinit();

    var it = std.mem.tokenizeScalar(u8, fileContents, '\n');
    while (it.next()) |x| {
        // print("Line: {s}\n", .{x});
        var it2 = std.mem.tokenizeAny(u8, x, " ");
        var i: u4 = 0;
        while (it2.next()) |y| {
            if (i == 0) {
                i += 1;
                try list1.append(try std.fmt.parseInt(i32, y, 10));
            } else {
                try list2.append(try std.fmt.parseInt(i32, y, 10));
            }
            // print("Token: {d}\n", .{try std.fmt.parseInt(i32, y, 10)});
        }
        // break;
    }

    std.mem.sort(i32, list1.items, {}, std.sort.asc(i32));
    std.mem.sort(i32, list2.items, {}, std.sort.asc(i32));

    var total: u32 = 0;
    for (list1.items, list2.items) |item1, item2| {
        total += @abs(item1 - item2);
    }
    print("Distance is {d}\n", .{total});

    // print("List1: {any}\n", .{list1.items});
    // print("List2: {any}\n", .{list2.items});

    // // This was first version of the first approach. I'll leave it here for historical reasons
    // print("Hello world\n", .{});

    // // Allocate a large enough buffer to store the cwd
    // var buf: [std.fs.max_path_bytes]u8 = undefined;

    // // getcwd writes the path of the cwd into buf and returns a slice of buf with the len of cwd
    // const cwd = try std.posix.getcwd(&buf);

    // var paths = [_]String{ cwd, "input.txt" };

    // const file_path = try std.fs.path.join(allocator, &paths);
    // defer allocator.free(file_path);

    // // Print out the cwd
    // print("cwd: {s}; length = {d}\n", .{ cwd, cwd.len });
    // print("input file: {s}\n", .{file_path});

    // buf[cwd.len] = '/';
    // print("cwd2: {s}; length = {d}\n", .{ buf[0 .. cwd.len + 1], cwd.len });

    // print("There are {d} args:\n", .{std.os.argv.len});
    // for (std.os.argv) |arg| {
    //     print("  {s}\n", .{arg});
    // }

    // // try stdout.print("Hello world\n", .{});
    // // try bw.flush();

    // // std.log.info("hello world!\n", .{});
}
