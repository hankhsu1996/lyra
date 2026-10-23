# CUDD (BSD-3-Clause), Fabio Somenzi's decision-diagram package: the diagram
# library and the parts of its support libraries it calls, configured by the
# header `cudd.patch` adds, as its own configure script configures it on a
# 64-bit POSIX host.

load("@rules_cc//cc:defs.bzl", "cc_library")

cc_library(
    name = "cudd",
    srcs = glob(
        [
            "cudd/*.c",
            "epd/*.c",
            "mtr/*.c",
            "st/*.c",
        ],
        # The package's own test programs.
        exclude = [
            "cudd/testcudd.c",
            "cudd/testextra.c",
            "mtr/testmtr.c",
            "st/testst.c",
        ],
    ) + [
        "cudd/cuddInt.h",
        "epd/epdInt.h",
        "mtr/mtrInt.h",
        "util/cpu_stats.c",
        "util/cpu_time.c",
        "util/cstringstream.c",
        "util/cstringstream.h",
        "util/datalimit.c",
        "util/prtime.c",
        "util/safe_mem.c",
        "util/ucbqsort.c",
    ],
    # The support library's header is among them for the allocator's
    # out-of-memory hook, which a program that must not exit on it replaces.
    hdrs = [
        "config.h",
        "cudd/cudd.h",
        "epd/epd.h",
        "mtr/mtr.h",
        "st/st.h",
        "util/util.h",
    ],
    includes = [
        ".",
        "cudd",
        "epd",
        "mtr",
        "st",
        "util",
    ],
    visibility = ["//visibility:public"],
)
