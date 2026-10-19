# GTKWave's FST library (MIT), built as its own Meson build configures it on a
# POSIX host: the writer may compress a block on a thread of its own.

load("@rules_cc//cc:defs.bzl", "cc_library")

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
    copts = [
        "-D_GNU_SOURCE",
        "-DFST_WRITER_PARALLEL",
        "-DHAVE_LIBPTHREAD",
        "-DHAVE_FSEEKO",
        "-DHAVE_REALPATH",
    ],
    includes = ["src"],
    linkopts = ["-lpthread"],
    visibility = ["//visibility:public"],
    deps = ["@zlib"],
)
