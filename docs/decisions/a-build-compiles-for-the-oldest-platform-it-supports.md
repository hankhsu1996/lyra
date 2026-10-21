# A Build Compiles For The Oldest Platform It Supports

## Date

2026-10-20

## Status

Accepted. Keeps the runtime's compiler of
[the-design-is-optimized-with-the-runtime-at-the-link](the-design-is-optimized-with-the-runtime-at-the-link.md)
and the sanitizer runtime of
[a-program-is-checked-by-the-sanitizer-of-whoever-instrumented-it](a-program-is-checked-by-the-sanitizer-of-whoever-instrumented-it.md);
changes where both come from.

## Why this decision matters

An artifact needs of a machine what it was compiled and linked against, and no more. The build used
to compile against whatever the machine running it had: the clang that compiled the runtime library
was an official release linked against the libstdc++ of the machine it was built on, so it would not
start where the system library was older, and both clangs read the machine's own C++ headers,
finding a newer GCC's under `/opt` on one host and an older one's on another. So what a build
needed, and what its output needed, followed the machine rather than anything the project stated,
and a machine whose system library was GCC 11's with a GCC 13 installed beside it could no longer
build Lyra at all.

## What the field does

- **rustc** ships its LLVM with libstdc++ linked in ("since a dynamic libstdcpp may not be
  available", `rust/src/ci/run.sh`) and builds its release on the oldest system it promises, CentOS
  7, to promise glibc 2.17 (`rust/src/ci/docker/host-x86_64/dist-x86_64-linux/Dockerfile`).
- **PyPA's manylinux** builds a binary wheel in an image of the oldest C library of its class,
  AlmaLinux 8 for glibc 2.28 and AlmaLinux 9 for 2.34, with a newer GCC, and the wheel runs on that
  C library or any later one.
- **Red Hat's GCC Toolset** links the parts of the C++ library newer than the system's into what it
  builds, so the result runs on the base system's library (RHEL 9 _Developing C and C++
  applications_, 4.3.3).
- **hermeticbuild/hermetic-llvm**, the module Lyra already reads LLVM from, ships its clang as a
  static executable and builds the C library's link stubs and the C++ library from source at the
  versions a target platform names.

What they share is that the result is built against the oldest version it promises, by tools that
need nothing of the machine they run on.

## Decisions

### D1. The floor is two versions, glibc 2.34 and libstdc++ 13

Building Lyra needs a Linux with glibc 2.34 or newer. Compiling a design needs a C++ compiler whose
libstdc++ is 13 or newer, the release that brought `<format>`, which the runtime's headers use.
Running a built program needs the system's own library. The floor is stated as versions, as
manylinux states its own, and not as any distribution: which systems it covers follows from the
number. The glibc number is a choice of how far back to reach, and under D2 moving it is one
constant of the build; the libstdc++ number is what the runtime's code uses.

### D2. Everything the build compiles is compiled for that platform, by a compiler that needs nothing of the machine

The build's target platform is glibc 2.34 and libstdc++ 13.1, the first release of the oldest
library promised, since headers of a later release may name symbols an earlier one lacks. The
compiler is the static clang of the LLVM release Lyra links, from the module that builds the two
libraries at those versions; the machine's own compilers and headers are read by nothing. So a build
on any machine compiles the same thing, and the runtime library it ships links into a program built
by any compiler at or above the floor.

### D3. Every program the build links carries its C++ library inside it

`lyra` links libstdc++ statically, so it runs on a machine whose system library is older than the
one it was compiled against, and every test does the same, so that a test runs the library that
ships. The runtime library is archived and never linked here. The user's compiler links it, against
the user's library.

### D4. GCC stays a gate, and the runtime is compiled by the same clang under it

`--config=gcc` builds Lyra with the remote image's GCC and its own library, because every warning is
fatal and each compiler warns of things the other does not. The runtime library is compiled by the
clang of D2 there too, as its bitcode has to be that release's.

## Rejected

| Alternative                                                                  | The requirement it fails                                                                                                               |
| ---------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------- |
| Naming a host's newer GCC install, or declaring `/opt` as a header directory | D2. It carries one machine's layout into the project, and the result still follows the machine.                                        |
| Building and publishing our own static clang                                 | It is the module's prebuilt, rebuilt by us.                                                                                            |
| `lyra` linked against libc++                                                 | It would run, but the runtime has to be libstdc++ for the user's compiler, and the tests would then run a different library.           |
| Linking the module's shared libstdc++ into tests                             | Its `std::filesystem::directory_iterator` faults on first use at 13.1, 13.4 and 14.3, while the same sources linked statically do not. |

## What was measured

The official clang 23.1.0 needs `GLIBCXX_3.4.30`, GCC 12's library (`objdump -p`). Built this way,
`lyra` needs no `libstdc++.so` and no glibc symbol newer than 2.34.
