# zlib (Zlib license), built as its own configure script configures it on a
# POSIX host: the file functions it calls are declared by <unistd.h>. Every
# includer reads the same configuration, since it decides a type in zlib's own
# header.

load("@rules_cc//cc:defs.bzl", "cc_library")

cc_library(
    name = "zlib",
    srcs = [
        "adler32.c",
        "compress.c",
        "crc32.c",
        "crc32.h",
        "deflate.c",
        "deflate.h",
        "gzclose.c",
        "gzguts.h",
        "gzlib.c",
        "gzread.c",
        "gzwrite.c",
        "infback.c",
        "inffast.c",
        "inffast.h",
        "inffixed.h",
        "inflate.c",
        "inflate.h",
        "inftrees.c",
        "inftrees.h",
        "trees.c",
        "trees.h",
        "uncompr.c",
        "zutil.c",
        "zutil.h",
    ],
    hdrs = [
        "zconf.h",
        "zlib.h",
    ],
    defines = ["HAVE_UNISTD_H"],
    includes = ["."],
    visibility = ["//visibility:public"],
)
