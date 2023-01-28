const std = @import("std");

const SharedLibKind = std.build.LibExeObjStep.SharedLibKind;

pub fn build(b: *std.build.Builder) void {
    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const lib = b.addSharedLibrary("zippy", "zig_src/main.zig", SharedLibKind.unversioned);
    lib.addIncludePath("/Users/jrcd/opt/erl25/lib/erlang/erts-13.1.4/include");
    lib.setBuildMode(mode);
    lib.force_pic = true;
    // needed for Mac OS X
    lib.linker_allow_shlib_undefined = true;
    // -------------------
    lib.install();

//    const main_tests = b.addTest("src/main.zig");
//    main_tests.setBuildMode(mode);
//
//    const test_step = b.step("test", "Run library tests");
//    test_step.dependOn(&main_tests.step);
}
