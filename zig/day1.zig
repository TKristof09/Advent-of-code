const std = @import("std");
pub fn main() !void {
    var file = try std.fs.cwd().openFile("../../inputs/day1", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var left = std.ArrayList(i32).init(allocator);
    var right = std.ArrayList(i32).init(allocator);
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var splits = std.mem.tokenizeScalar(u8, line, ' ');
        try left.append(try std.fmt.parseInt(i32, splits.next().?, 10));
        try right.append(try std.fmt.parseInt(i32, splits.next().?, 10));
    }

    var res: i32 = 0;

    //std.mem.sort(i32, left.items, {}, comptime std.sort.asc(i32));
    //std.mem.sort(i32, right.items, {}, comptime std.sort.asc(i32));

    var map = std.AutoHashMap(i32, i32).init(allocator);

    for (right.items) |i| {
        try map.put(i, 1 + (map.get(i) orelse 0));
    }

    for (left.items) |i| {
        res += i * (map.get(i) orelse 0);
    }
    std.debug.print("\n", .{});
    std.debug.print("{}\n", .{res});
}
