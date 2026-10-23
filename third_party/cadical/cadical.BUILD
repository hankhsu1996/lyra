# CaDiCaL (MIT), Armin Biere's satisfiability solver, built for embedding as
# its own `./configure -q` builds it: no generated build description, no
# tracing, assertions or messages.

load("@rules_cc//cc:defs.bzl", "cc_library")

cc_library(
    name = "cadical",
    srcs = glob(
        [
            "src/*.c",
            "src/*.cpp",
            "src/*.h",
            "src/*.hpp",
        ],
        exclude = [
            "src/cadical.cpp",
            "src/cadical.hpp",
            "src/ccadical.cpp",
            "src/ccadical.h",
            "src/ipasir.cpp",
            "src/ipasir.h",
            "src/mobical.cpp",
        ],
    ),
    hdrs = ["src/cadical.hpp"],
    includes = ["src"],
    local_defines = [
        "NBUILD",
        "NCLOSEFROM",
        "NDEBUG",
        "NTRACING",
        "QUIET",
    ],
    visibility = ["//visibility:public"],
)
