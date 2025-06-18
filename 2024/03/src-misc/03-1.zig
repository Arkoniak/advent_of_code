const std = @import("std");
const print = std.debug.print;
const String = []const u8;
const c = @cImport({
    @cDefine("PCRE2_CODE_UNIT_WIDTH", "8");
    @cInclude("pcre2.h");
});

fn get_val(x: ?String) !i32 {
    return try std.fmt.parseInt(i32, x orelse return error.InvalidFormat, 10);
}

pub fn main() !void {
    var buffer: [100000]u8 = undefined;
    const filename: String = if (std.os.argv.len > 1) std.mem.span(std.os.argv[1]) else "input_test.txt";
    var f = try std.fs.cwd().openFile(filename, .{ .mode = .read_only });
    defer f.close();

    const num_bytes_read = try f.reader().readAll(&buffer);
    // const rbuf = buffer[0..num_bytes_read];
    // print("{s}\n", .{rbuf});

    // const text = "asfoo5xcv\nfoo3";
    const pattern = "mul\\(([0-9]+),([0-9]+)\\)";

    // Компиляция регулярного выражения
    var error_number: c_int = 0;
    var error_offset: usize = 0;

    const re = c.pcre2_compile_8(pattern.ptr, pattern.len, 0, // опции
        &error_number, &error_offset, null // контекст
    );

    if (re == null) {
        print("Ошибка компиляции регулярного выражения\n", .{});
        return;
    }
    defer c.pcre2_code_free_8(re);

    // Создание match data
    const match_data = c.pcre2_match_data_create_from_pattern_8(re, null);
    if (match_data == null) {
        print("Ошибка создания match data\n", .{});
        return;
    }
    defer c.pcre2_match_data_free_8(match_data);

    // Поиск всех совпадений
    var start_offset: usize = 0;
    // var match_count: u32 = 0;

    // print("Ищем '{s}' в строке: '{s}'\n\n", .{ pattern, rbuf });

    var result: i64 = 0;
    while (start_offset < num_bytes_read) {
        // const rc = c.pcre2_match_8(re, text.ptr, text.len, start_offset, 0, // опции
        const rc = c.pcre2_match_8(re, &buffer, num_bytes_read, start_offset, 0, // опции
            match_data, null // контекст
        );

        if (rc < 0) break; // Больше совпадений нет

        const ovector = c.pcre2_get_ovector_pointer_8(match_data);
        const match_start = ovector[0];
        const match_end = ovector[1];

        // const group1_start = ovector[2];
        // const group1_end = ovector[3];

        const val1 = @as(i64, try get_val(buffer[ovector[2]..ovector[3]]));
        const val2 = @as(i64, try get_val(buffer[ovector[4]..ovector[5]]));

        // match_count += 1;
        // print("Совпадение #{d}: позиция {d}-{d}, group {s}\n", .{ match_count, match_start, match_end, buffer[group1_start..group1_end] });

        result += val1 * val2;
        // print("{d}, {d} * {d}\n", .{ result, val1, val2 });

        // Переходим к следующей позиции
        start_offset = match_end;
        if (start_offset == match_start) start_offset += 1; // избегаем бесконечного цикла
    }

    print("{d}\n", .{result});
    // print("\nВсего найдено: {d} совпадений\n", .{match_count});
}
