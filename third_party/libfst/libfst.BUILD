# GTKWave's FST library (MIT), built as its own Meson build configures it on a
# POSIX host, without the writer's own compression thread: a dump compresses
# its blocks on the one thread that writes it, which is the thread an ending
# waits on to finish the file, so no thread of the library's can be the one
# that file waits for.

load("@rules_cc//cc:defs.bzl", "cc_library")

# The license, which a program carrying the library carries beside it.
exports_files(["LICENSE"])

cc_library(
    name = "libfst",
    srcs = [
        "src/fastlz.c",
        "src/fastlz.h",
        "src/fstapi.c",
        "src/lz4.c",
        "src/lz4.h",
    ],
    hdrs = ["src/fstapi.h"],
    includes = ["src"],
    local_defines = [
        "_GNU_SOURCE",
        "HAVE_FSEEKO",
        "HAVE_REALPATH",
    ],
    visibility = ["//visibility:public"],
    deps = ["@zlib"],
)
