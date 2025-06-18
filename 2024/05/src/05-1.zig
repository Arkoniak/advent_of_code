const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const HashMap = std.HashMap;
const String = []const u8;

const Reader = std.fs.File.Reader;

// const P = struct { u32, u32 };

const Context = struct {
    pub fn hash(self: Context, t: struct { u32, u32 }) u64 {
        _ = self;
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(std.mem.asBytes(&t[0]));
        hasher.update(std.mem.asBytes(&t[1]));
        return hasher.final();
    }

    pub fn eql(self: Context, t1: struct { u32, u32 }, t2: struct { u32, u32 }) bool {
        _ = self;
        return t1[0] == t2[0] and t1[1] == t2[1];
    }
};

const KeyContext = struct {
    pub fn hash(self: KeyContext, t: u32) u64 {
        _ = self;
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(std.mem.asBytes(&t));
        return hasher.final();
    }

    pub fn eql(self: KeyContext, t1: u32, t2: u32) bool {
        _ = self;
        return t1 == t2;
    }
};

fn get_val(x: ?String) !u32 {
    return try std.fmt.parseInt(u32, x orelse return error.InvalidFormat, 10);
}

const Graph = struct {
    const MapType = std.HashMap(struct { u32, u32 }, void, Context, std.hash_map.default_max_load_percentage);
    const KeysType = std.HashMap(u32, void, KeyContext, std.hash_map.default_max_load_percentage);

    connectivity: MapType,
    vertices: KeysType,

    pub fn init(allocator: Allocator) Graph {
        const ctx = Context;
        const map = std.HashMap(struct { u32, u32 }, void, ctx, std.hash_map.default_max_load_percentage).init(allocator);

        const kctx = KeyContext;
        const keys = std.HashMap(u32, void, kctx, std.hash_map.default_max_load_percentage).init(allocator);

        return .{ .connectivity = map, .vertices = keys };
    }

    pub fn deinit(self: *Graph) void {
        self.connectivity.deinit();
        self.vertices.deinit();
    }

    pub fn put(self: *Graph, p1: u32, p2: u32) !void {
        try self.connectivity.put(.{ p1, p2 }, {});
        try self.vertices.put(p1, {});
        try self.vertices.put(p2, {});
    }

    pub fn contains(self: *const Graph, p1: u32, p2: u32) bool {
        return self.connectivity.contains(.{ p1, p2 });
    }

    pub fn buildTransitions(self: *Graph, allocator: Allocator) !void {
        const vertices = try allocator.alloc(u32, self.vertices.count());
        defer allocator.free(vertices);
        var it = self.vertices.keyIterator();
        var ii: usize = 0;
        while (it.next()) |k| {
            vertices[ii] = k.*;
            ii += 1;
        }

        for (vertices) |i| {
            for (vertices) |j| {
                for (vertices) |k| {
                    if (self.contains(i, k) and self.contains(k, j)) {
                        try self.put(i, j);
                    }
                }
            }
        }
    }
};

fn readGraph(allocator: Allocator, reader: Reader) !Graph {
    // const buffer = try allocator.alloc(u8, 1_000);
    var buffer: [1000]u8 = undefined;
    var graph = Graph.init(allocator);
    while (try reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        if (line.len == 0) break;
        var it = std.mem.tokenizeAny(u8, line, "|");
        const p1 = try get_val(it.next());
        const p2 = try get_val(it.next());

        try graph.put(p1, p2);
    }

    // try graph.buildTransitions(allocator);
    return graph;
}

fn readLine(allocator: Allocator, line: String) ![]u32 {
    var pages = std.ArrayList(u32).init(allocator);
    var it = std.mem.tokenizeAny(u8, line, ",");
    while (it.next()) |num| {
        const n = try get_val(num);
        try pages.append(n);
    }

    return pages.items;

    // print("Result: {any}\n", .{pages.items});
}

fn validateLine(items: []u32, graph: *const Graph) bool {
    for (items, 0..) |item, i| {
        var j = i + 1;
        while (j < items.len) {
            const item2 = items[j];
            if (graph.contains(item2, item)) {
                // print("Line: {any}, contradiction: {d}, {d}\n", .{ items, item2, item });
                return false;
            }
            j += 1;
        }
    }

    return true;
}

fn analyzeLine(items: []u32, graph: *const Graph) ?u32 {
    return if (validateLine(items, graph)) items[(items.len - 1) >> 1] else null;
}

fn analyzePages(reader: Reader, graph: *const Graph) !u32 {
    var buffer: [1000]u8 = undefined;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var result: u32 = 0;
    while (try reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        defer _ = arena.reset(.retain_capacity);
        const items = try readLine(allocator, line);
        const page = analyzeLine(items, graph) orelse continue;
        print("Line: {s}, value: {d}\n", .{ line, page });
        result += page;
    }

    return result;
}

pub fn main() !void {
    // var buffer: [1000]u8 = undefined;
    const filename: String = if (std.os.argv.len > 1) std.mem.span(std.os.argv[1]) else "input_test.txt";
    var f = try std.fs.cwd().openFile(filename, .{ .mode = .read_only });
    defer f.close();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    // const allocator = gpa.allocator();
    // defer {
    //     const deinit_status = gpa.deinit();
    //     if (deinit_status == .leak) print("LEAK DETECTED!!!\n", .{});
    // }

    const reader = f.reader();
    var graph = try readGraph(allocator, reader);
    // defer graph.deinit();

    print("{any}\n", .{graph.contains(1, 3)});
    print("{any}\n", .{graph.contains(53, 47)});

    print("Result: {d}\n", .{try analyzePages(reader, &graph)});

    // try graph.buildTransitions(allocator);
    // var buffer: [1000]u8 = undefined;
    // const line = try reader.readUntilDelimiterOrEof(&buffer, '\n');
    // print("{s}\n", .{line.?});
}
