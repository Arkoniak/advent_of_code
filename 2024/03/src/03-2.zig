const std = @import("std");
const print = std.debug.print;
const String = []const u8;
// const c = @cImport({
//     @cDefine("PCRE2_CODE_UNIT_WIDTH", "8");
//     @cInclude("pcre2.h");
// });

fn get_val(x: ?String) !i32 {
    return try std.fmt.parseInt(i32, x orelse return error.InvalidFormat, 10);
}

const State = enum { Do, Dont, TryMul, TryDo, TryDont, Num1, Num2 };

const MulFSM = struct {
    state: State,
    data: []const u8,
    position: usize,
    num1: i64 = 0,
    num2: i64 = 0,
    result: i64 = 0,
    marker: usize = undefined,

    pub fn init(data: []const u8) MulFSM {
        return MulFSM{ .data = data, .state = .Do, .position = 0 };
    }

    pub fn getCurrentToken(self: *MulFSM) ?u8 {
        if (self.position >= self.data.len) return null else return self.data[self.position];
    }

    fn isMul(self: *MulFSM) bool {
        return std.mem.eql(u8, self.data[self.marker..self.position], "mul");
    }

    fn isDo(self: *MulFSM) bool {
        return std.mem.eql(u8, self.data[self.marker..self.position], "do(");
    }

    fn isDont(self: *MulFSM) bool {
        // print("\n\n{s}\n\n", .{self.data[self.marker..self.position]});
        return std.mem.eql(u8, self.data[self.marker..self.position], "don't(");
    }

    pub fn transition(self: *MulFSM) bool {
        const istoken = self.getCurrentToken();
        if (istoken == null) return false;
        const token = istoken.?;

        // print("token = {c}, state = {}\n", .{ token, self.state });

        switch (self.state) {
            .Do => {
                switch (token) {
                    'm' => {
                        self.marker = self.position;
                        self.state = .TryMul;
                    },
                    'd' => {
                        self.marker = self.position;
                        self.state = .TryDont;
                    },
                    else => {},
                }
            },
            .Dont => {
                switch (token) {
                    'd' => {
                        self.marker = self.position;
                        self.state = .TryDo;
                    },
                    else => {},
                }
            },
            .TryDont => {
                switch (token) {
                    'm' => {
                        self.marker = self.position;
                        self.state = .TryMul;
                    },
                    'd' => {
                        self.marker = self.position;
                        self.state = .TryDont;
                    },
                    ')' => {
                        if (self.isDont()) {
                            self.state = .Dont;
                        } else {
                            self.state = .Do;
                        }
                    },
                    else => {},
                }
            },
            .TryDo => {
                switch (token) {
                    'd' => {
                        self.marker = self.position;
                        self.state = .TryDo;
                    },
                    ')' => {
                        if (self.isDo()) {
                            self.state = .Do;
                        } else {
                            self.state = .Dont;
                        }
                    },
                    else => {},
                }
            },
            .TryMul => {
                switch (token) {
                    'm' => {
                        self.marker = self.position;
                    },
                    'd' => {
                        self.marker = self.position;
                        self.state = .TryDont;
                    },
                    '(' => {
                        if (self.isMul()) {
                            self.state = .Num1;
                            self.num1 = 0;
                        } else {
                            self.state = .Do;
                        }
                    },
                    else => {},
                }
            },
            .Num1 => {
                switch (token) {
                    '0'...'9' => {
                        self.num1 = 10 * self.num1 + @as(i64, token - '0');
                    },
                    ',' => {
                        self.state = .Num2;
                        self.num2 = 0;
                    },
                    else => self.state = .Do,
                }
            },
            .Num2 => {
                switch (token) {
                    '0'...'9' => {
                        self.num2 = 10 * self.num2 + @as(i64, token - '0');
                    },
                    ')' => {
                        self.state = .Do;
                        self.result += self.num1 * self.num2;
                    },
                    else => self.state = .Do,
                }
            },
        }

        self.position += 1;

        return true;
    }
};

pub fn main() !void {
    var buffer: [100000]u8 = undefined;
    const filename: String = if (std.os.argv.len > 1) std.mem.span(std.os.argv[1]) else "input_test.txt";
    var f = try std.fs.cwd().openFile(filename, .{ .mode = .read_only });
    defer f.close();

    const num_bytes_read = try f.reader().readAll(&buffer);
    const data = buffer[0..num_bytes_read];

    var fsm = MulFSM.init(data);
    var run = true;
    while (run) run = fsm.transition();

    print("Result = {d}\n", .{fsm.result});
}
