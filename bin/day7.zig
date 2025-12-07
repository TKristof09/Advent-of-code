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
fn solve(T: type, start: u8, rows: []const T) u32 {
    var cur_rays: T = @as(T, 1) << @intCast(start);
    var count: u32 = 0;
    for (rows) |splitter_encoding| {
        const hits = (cur_rays & splitter_encoding);
        count += @popCount(hits);
        cur_rays = (cur_rays & ~hits) | (hits << 1 | hits >> 1);
    }
    return count;
}
fn solve2(Vec: type, T: type, start: u8, rows: []const T) u64 {
    var counts: Vec = @splat(0);
    counts[start] = 1;
    const Width = @typeInfo(Vec).vector.len;

    const zeros: Vec = @splat(0);
    const shift_left_mask = comptime blk: {
        var m: [Width]i32 = undefined;
        // shift up items in 0..N-1
        for (0..Width - 1) |i| m[i] = i + 1;
        // last element from zeros Vector
        m[Width - 1] = -1;
        break :blk m;
    };
    const shift_right_mask = comptime blk: {
        var m: [Width]i32 = undefined;
        m[0] = -1;
        for (1..Width) |i| m[i] = i - 1;
        break :blk m;
    };

    for (rows) |splitter_bits| {
        const split_mask: @Vector(Width, bool) = @bitCast(splitter_bits);
        const hits = @select(u64, split_mask, counts, zeros);
        const pass = @select(u64, split_mask, zeros, counts);

        const left_move = @shuffle(u64, hits, zeros, shift_left_mask);
        const right_move = @shuffle(u64, hits, zeros, shift_right_mask);
        counts = pass + left_move + right_move;
    }

    return @reduce(.Add, counts);
}
pub fn main() !void {
    const f = try std.fs.cwd().openFile("/mnt/H/Programming/aoc/inputs/day7", .{});
    var reader_buf: [4096]u8 = undefined;
    var reader = f.reader(&reader_buf);

    const start_row = try reader.interface.takeDelimiterExclusive('\n');
    reader.interface.toss(1);

    std.debug.assert(start_row.len < 160);
    const Int = u160;
    const Vector = @Vector(160, u64);

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
        // Parse splitters into integers
        if (parseRow(Int, row)) |encoding| {
            rows[i] = encoding;
            i += 1;
        }
    }
    const splitters = rows[0..i];
    std.debug.print("{d}\n", .{solve(Int, start, splitters)});
    std.debug.print("{d}\n", .{solve2(Vector, Int, start, splitters)});

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
    {
        const start_ns = std.time.nanoTimestamp();
        const start_cycles = rdtsc();
        var j: usize = 0;
        while (j < num_runs) : (j += 1) {
            std.mem.doNotOptimizeAway(@call(.never_inline, solve2, .{ Vector, Int, start, splitters }));
        }
        const end_cycles = rdtsc();
        const end_ns = std.time.nanoTimestamp();
        const elapsed_cycles = end_cycles - start_cycles;
        const avg_cycles = @divFloor(elapsed_cycles, num_runs);
        const elapsed_ns = end_ns - start_ns;
        const avg_ns = @divFloor(elapsed_ns, num_runs);
        std.debug.print("Average: {} cycles\n", .{avg_cycles});
        std.debug.print("Total: {} cycles\n", .{elapsed_cycles});
        std.debug.print("Average: {} ns\n", .{avg_ns});
        std.debug.print("Total: {} ns\n", .{elapsed_ns});
    }
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
