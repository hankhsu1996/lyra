# A build compiles for the oldest platform it supports

Status: Accepted, 2026-10-20.

## Requirement

What building Lyra needs of a machine, and what compiling a design with it needs, is stated by the
project as versions and is the same on every machine. It never follows what the machine doing the
build happens to have installed. Before this, both clangs read the machine's own C++ headers and the
runtime's clang needed the libstdc++ of the machine it was released on, so a host whose system
library was older than its installed GCC could no longer build Lyra at all.

## Choice

- **The floor is two versions.** Building Lyra needs glibc 2.34 or newer. Compiling a design needs a
  C++ compiler whose libstdc++ is 13.2 or newer. Release 13 brought `<format>`, which the runtime's
  headers use, and 13.2 is the release whose headers the runtime is compiled against. The floor is
  stated as versions, as manylinux states its own, never as a distribution.
- **Everything the build compiles is compiled for that floor, by a compiler that needs nothing of
  the machine.** That compiler is the static clang of the LLVM release Lyra links, from the module
  that also builds the C library's link stubs and libstdc++ from source at the floor's versions. The
  machine's own compilers and headers are read by nothing, as rustc builds its release on the oldest
  system it promises.
- **Every program the build links carries its C++ library inside it.** `lyra` and every test link
  libstdc++ statically, so a test runs the library that ships. The runtime library is archived and
  never linked by the build; the user's compiler links it against the user's library.
- **GCC stays a gate.** The merge gate builds Lyra under GCC for its warnings, and the runtime is
  compiled by the clang above there too
  ([the-pinned-clang-builds-the-lyra-that-is-run](the-pinned-clang-builds-the-lyra-that-is-run.md)).

## Alternatives turned down

- **Naming a host's newer GCC install, or declaring a directory such as `/opt` for its headers.** It
  fails a build that is the same on every machine. The failure rests on that layout being one
  machine's.
- **`lyra` linked against libc++.** It fails tests running the library that ships. The failure rests
  on the runtime having to be libstdc++ for the user's compiler, which on Linux links libstdc++.
- **The module's shared libstdc++ linked into tests.** It fails tests that run at all. The failure
  rests on that build's `std::filesystem::directory_iterator` faulting on first use at 13.1, 13.4
  and 14.3 while the same sources linked statically do not; a release where it works reopens this.

## Consequences across layers

- Moving the glibc floor is one constant of the build. Moving the libstdc++ floor moves what the
  runtime's code may use and what every user's compiler must bring.
- The clang that builds Lyra and the clang that compiles the runtime are one release by default
  ([the-design-is-optimized-with-the-runtime-at-the-link](the-design-is-optimized-with-the-runtime-at-the-link.md)).
- `CONTRIBUTING.md` states the floor for a reader setting up a machine.
