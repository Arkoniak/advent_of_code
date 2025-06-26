const std = @import("std");

const part1 = "src/08-1.zig";
const part2 = "src/08-2.zig";
const name1 = "aoc-8-1";
const name2 = "aoc-8-2";

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall. Here we do not
    // set a preferred release mode, allowing the user to decide how to optimize.
    const optimize = b.standardOptimizeOption(.{});

    // This creates a "module", which represents a collection of source files alongside
    // some compilation options, such as optimization mode and linked system libraries.
    // Every executable or library we compile will be based on one or more modules.
    // const lib_mod = b.createModule(.{
    //     // `root_source_file` is the Zig "entry point" of the module. If a module
    //     // only contains e.g. external object files, you can make this `null`.
    //     // In this case the main source file is merely a path, however, in more
    //     // complicated build scripts, this could be a generated file.
    //     .root_source_file = b.path("src/root.zig"),
    //     .target = target,
    //     .optimize = optimize,
    // });

    // We will also create a module for our other entry point, 'main.zig'.
    const exe_mod1 = b.createModule(.{
        // `root_source_file` is the Zig "entry point" of the module. If a module
        // only contains e.g. external object files, you can make this `null`.
        // In this case the main source file is merely a path, however, in more
        // complicated build scripts, this could be a generated file.
        .root_source_file = b.path(part1),
        .target = target,
        .optimize = optimize,
    });

    // Modules can depend on one another using the `std.Build.Module.addImport` function.
    // This is what allows Zig source code to use `@import("foo")` where 'foo' is not a
    // file path. In this case, we set up `exe_mod` to import `lib_mod`.
    // exe_mod.addImport("_01_lib", lib_mod);

    // Now, we will create a static library based on the module we created above.
    // This creates a `std.Build.Step.Compile`, which is the build step responsible
    // for actually invoking the compiler.
    // const lib = b.addLibrary(.{
    //     .linkage = .static,
    //     .name = "_01",
    //     .root_module = lib_mod,
    // });

    // This declares intent for the library to be installed into the standard
    // location when the user invokes the "install" step (the default step when
    // running `zig build`).
    // b.installArtifact(lib);

    // This creates another `std.Build.Step.Compile`, but this one builds an executable
    // rather than a static library.
    const exe1 = b.addExecutable(.{
        .name = name1,
        .root_module = exe_mod1,
    });

    // This declares intent for the executable to be installed into the
    // standard location when the user invokes the "install" step (the default
    // step when running `zig build`).
    b.installArtifact(exe1);

    // This *creates* a Run step in the build graph, to be executed when another
    // step is evaluated that depends on it. The next line below will establish
    // such a dependency.
    const run1_cmd = b.addRunArtifact(exe1);

    // By making the run step depend on the install step, it will be run from the
    // installation directory rather than directly from within the cache directory.
    // This is not necessary, however, if the application depends on other installed
    // files, this ensures they will be present and in the expected location.
    run1_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- arg1 arg2 etc`
    if (b.args) |args| {
        run1_cmd.addArgs(args);
    }

    // This creates a build step. It will be visible in the `zig build --help` menu,
    // and can be selected like this: `zig build run`
    // This will evaluate the `run` step rather than the default, which is "install".
    const run1_step = b.step("run1", "Run the -1.zig");
    run1_step.dependOn(&run1_cmd.step);

    // === ЦЕЛЬ 2: main2 ===
    const exe2 = b.addExecutable(.{
        .name = name2,
        .root_source_file = b.path(part2),
        .target = target,
        .optimize = optimize,
    });

    // Устанавливаем артефакт для цели 2
    b.installArtifact(exe2);

    // Создаём step для запуска второго приложения
    const run2_cmd = b.addRunArtifact(exe2);
    run2_cmd.step.dependOn(b.getInstallStep());

    // Передаём аргументы командной строки в приложение
    if (b.args) |args| {
        run2_cmd.addArgs(args);
    }

    // Создаём step с именем "run2"
    const run2_step = b.step("run2", "Run the -2.zig");
    run2_step.dependOn(&run2_cmd.step);

    // === ДЕФОЛТНЫЙ RUN (по умолчанию запускает main1) ===
    const run_step = b.step("run", "Run the default app");
    run_step.dependOn(&run1_cmd.step);

    // === ДОПОЛНИТЕЛЬНЫЕ ЦЕЛИ ===

    // Цель для сборки без запуска
    const build1_step = b.step("build1", "Build -1.zig only");
    build1_step.dependOn(&exe1.step);

    const build2_step = b.step("build2", "Build -2.zig only");
    build2_step.dependOn(&exe2.step);

    // Цель для сборки всех приложений
    const build_all_step = b.step("build-all", "Build both applications");
    build_all_step.dependOn(&exe1.step);
    build_all_step.dependOn(&exe2.step);

    // === ТЕСТЫ (если нужны) ===

    // Тесты для main1
    const unit_tests1 = b.addTest(.{
        .root_source_file = b.path(part1),
        .target = target,
        .optimize = optimize,
    });

    const run_unit_tests1 = b.addRunArtifact(unit_tests1);
    const test1_step = b.step("test1", "Run unit tests for -1.zig");
    test1_step.dependOn(&run_unit_tests1.step);

    // Тесты для main2
    const unit_tests2 = b.addTest(.{
        .root_source_file = b.path(part2),
        .target = target,
        .optimize = optimize,
    });

    const run_unit_tests2 = b.addRunArtifact(unit_tests2);
    const test2_step = b.step("test2", "Run unit tests for -2.zig");
    test2_step.dependOn(&run_unit_tests2.step);

    // Общий step для всех тестов
    const test_step = b.step("test", "Run all unit tests");
    test_step.dependOn(&run_unit_tests1.step);
    test_step.dependOn(&run_unit_tests2.step);

    //     // Creates a step for unit testing. This only builds the test executable
    //     // but does not run it.
    //     // const lib_unit_tests = b.addTest(.{
    //     //     .root_module = lib_mod,
    //     // });

    //     // const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    //     const exe_unit_tests = b.addTest(.{
    //         .root_module = exe_mod1,
    //     });

    //     const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    //     // Similar to creating the run step earlier, this exposes a `test` step to
    //     // the `zig build --help` menu, providing a way for the user to request
    //     // running the unit tests.
    //     const test_step = b.step("test", "Run unit tests");
    //     // test_step.dependOn(&run_lib_unit_tests.step);
    //     test_step.dependOn(&run_exe_unit_tests.step);
}
