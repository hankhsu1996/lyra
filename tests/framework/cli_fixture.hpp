#pragma once

#include <filesystem>
#include <nlohmann/json.hpp>
#include <string_view>

#include "conformance/framework/process.hpp"

namespace lyra::test {

// Where the compiler under test is, as this run was given it.
auto ResolveLyra() -> std::filesystem::path;

// Where the examples a reader is handed are, as this run was given them.
auto ResolveShippedExamples() -> std::filesystem::path;

// The symbol lister of the LLVM release Lyra links, as this run was given it.
// The shipped runtime carries that release's bitcode beside its machine code,
// and a host's `nm` may read it through whatever release's plugin the host has,
// listing nothing for an object it cannot read and still exiting 0.
auto ResolveLlvmNm() -> std::filesystem::path;

// The clang++ of the LLVM release Lyra links, as this run was given it. A test
// whose answer depends on what clang makes of a program names this one rather
// than whatever a machine has on PATH, so it gives the same answer wherever it
// runs, the remote executor included, which has no clang of its own.
auto ResolveClangxx() -> std::filesystem::path;

// The GCC of the machine running the test. The remote executor's image carries
// one, so a test that asks a question of GCC beside clang runs anywhere.
auto ResolveHostGcc() -> std::filesystem::path;

// The clang installed on the machine running the test, with everything an
// installation carries beside the compiler: a linker that reads bitcode, the
// sanitizer's headers. Only a machine with one can run a test that asks for it.
auto ResolveHostClang() -> std::filesystem::path;

// A host compiler a case runs under: what the case is called under it, and
// where the test finds it. A case run under each, side by side, is a question
// about what each compiler does with the same input.
struct HostCompiler {
  std::string_view name;
  std::filesystem::path (*find)();
};

// Runs lyra from `dir`, which is what a declaration search reads and what no
// argument can express.
auto RunLyraFrom(
    const std::filesystem::path& lyra, const std::filesystem::path& dir,
    std::string_view args) -> conformance::framework::ProcessOutcome;

// The smallest design that produces observable output. What the cases using it
// are about is what surrounds a design, so the design itself carries no weight
// beyond proving the program ran.
auto WriteTrivialSource(const std::filesystem::path& path) -> void;

// A design whose four DPI-C imports are defined in foreign code handed over
// compiled, each of which calls back the design's export: given 10, it prints
// "foreign 21 22 23 24" when every import reached the function written for it.
auto WriteForeignCodeDesign(const std::filesystem::path& path) -> void;

// Compiles that foreign code into `dir` with the host's C compiler, in each
// form a library is handed over in: `object.o`, `libarchive.a`, a shared
// library `shared/libshared.so.1` declaring that soname with
// `shared/libshared.so` a link to it, and `plain/libplain.so` declaring none,
// which the bootstrap file `plain/libraries` lists (LRM J.4.1). Beside
// `libshared.so.1` lies another `libplain.so`, defining its import otherwise,
// so a program that loads a library of that name from anywhere but where it
// was named prints another number.
auto CompileForeignCodeInto(const std::filesystem::path& dir)
    -> conformance::framework::ProcessOutcome;

// A file a run wrote about itself, parsed. A file that is absent or is not
// JSON answers with a discarded value, so the caller reports it as the run
// having written nothing rather than crashing on it.
auto ReadJson(const std::filesystem::path& path) -> nlohmann::json;

}  // namespace lyra::test
