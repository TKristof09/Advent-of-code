const std = @import("std");

fn parseRow(T: type, s: []const u8) ?T {
    var value: T = 0;

    for (s) |c| {
        value <<= 1;
        switch (c) {
            '^' => value |= 1,
            '.' => {},
            else => unreachable,
        }
    }

    if (value == 0) {
        return null;
    } else {
        return value;
    }
}
fn solve(T: type, start: u8, rows: []T) u32 {
    var cur_rays: T = @as(T, 1) << @intCast(start);
    // std.debug.print("{b:0>256}\n", .{cur_rays});
    var count: u32 = 0;
    for (rows) |splitter_encoding| {
        const hits = (cur_rays & splitter_encoding);
        count += @popCount(hits);
        cur_rays = (cur_rays & ~hits) | (hits << 1 | hits >> 1);
        // std.debug.print("{b:0>256}\n", .{cur_rays});
    }
    return count;
}
// noinline fn solve_v(start: u8, rows: []align(256) u256) u32 {
//     var cur_rays: u256 = @as(u256, 1) << start;
//     // std.debug.print("{b:0>256}\n", .{cur_rays});
//     var count: u32 = 0;
//     for (rows) |splitter_encoding| {
//         const cur_rays_v: @Vector(4, u64) = @bitCast(cur_rays);
//         const splitter_v: @Vector(4, u64) = @bitCast(splitter_encoding);
//         const hits_v = (cur_rays_v & splitter_v);
//         const hits: u256 = @bitCast(hits_v);
//         count += @popCount(hits);
//         cur_rays = @as(u256, @bitCast(cur_rays & ~hits)) | (hits << 1 | hits >> 1);
//         // std.debug.print("{b:0>256}\n", .{cur_rays});
//     }
//     return count;
// }
pub fn main() !void {
    const f = try std.fs.cwd().openFile("/mnt/H/Programming/aoc/inputs/day7", .{});
    var reader_buf: [4096]u8 = undefined;
    var reader = f.reader(&reader_buf);

    const start_row = try reader.interface.takeDelimiterExclusive('\n');
    reader.interface.toss(1);

    std.debug.assert(start_row.len < 160);
    const Int = u160;

    // std.debug.print("{s}\n", .{start_row});
    const start: u8 = @intCast(std.mem.indexOfScalar(u8, start_row, 'S').?);

    var rows: [256]Int = undefined;
    var i: usize = 0;
    while (true) {
        const row = reader.interface.takeDelimiterExclusive('\n') catch |e| switch (e) {
            error.EndOfStream => break,
            else => return e,
        };
        if (row.len == 0) break;
        reader.interface.toss(1);
        if (parseRow(Int, row)) |encoding| {
            rows[i] = encoding;
            i += 1;
        }
    }
    const splitters = rows[0..i];
    std.debug.print("Array length: {}\n", .{splitters.len});
    const count_splits = solve(Int, start, splitters);
    std.debug.print("{d}\n", .{count_splits});

    {
        // otherwise compiler optimises away the loops below beaceuse constant arguments to fn?
        var start_volatile = start;
        var splitters_volatile = splitters;

        const s = @as(*volatile @TypeOf(start), @ptrCast(&start_volatile)).*;
        const sp = @as(*volatile @TypeOf(splitters), @ptrCast(&splitters_volatile)).*;
        std.mem.doNotOptimizeAway(@call(.never_inline, solve, .{ Int, s, sp }));
    }

    const num_runs = 100_000;
    {
        const start_cycles = rdtsc();
        var j: usize = 0;
        while (j < num_runs) : (j += 1) {
            std.mem.doNotOptimizeAway(@call(.never_inline, solve, .{ Int, start, splitters }));
        }
        const end_cycles = rdtsc();
        const elapsed_cycles = end_cycles - start_cycles;
        const avg_cycles = @divFloor(elapsed_cycles, num_runs);
        std.debug.print("Average: {} cycles\n", .{avg_cycles});
        std.debug.print("Total: {} cycles\n", .{elapsed_cycles});
    }
    // std.debug.print("VECTOR\n", .{});
    // {
    //     const start_cycles = rdtsc();
    //     var j: usize = 0;
    //     while (j < num_runs) : (j += 1) {
    //         std.mem.doNotOptimizeAway(@call(.never_inline, solve_v, .{ start, splitters }));
    //     }
    //     const end_cycles = rdtsc();
    //     const elapsed_cycles = end_cycles - start_cycles;
    //     const avg_cycles = @divFloor(elapsed_cycles, num_runs);
    //     std.debug.print("Average: {} cycles\n", .{avg_cycles});
    //     std.debug.print("Total: {} cycles\n", .{elapsed_cycles});
    // }
}
inline fn rdtsc() u64 {
    var lo: u32 = undefined;
    var hi: u32 = undefined;
    asm volatile ("rdtsc"
        : [lo] "={eax}" (lo),
          [hi] "={edx}" (hi),
    );
    return (@as(u64, hi) << 32) | lo;
}
