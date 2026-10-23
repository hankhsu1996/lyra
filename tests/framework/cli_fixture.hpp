#pragma once

#include <expected>
#include <filesystem>
#include <nlohmann/json.hpp>
#include <optional>
#include <string>
#include <string_view>

#include "tests/framework/process.hpp"

namespace lyra::test {

// A directory of the caller's own to emit into. The name is drawn rather than
// derived from the test's, because a test that reruns must not inherit what a
// previous run left behind. Returns why it failed rather than throwing, so a
// setup failure is reported as the test failing rather than as a crash.
auto MakeScratchDir() -> std::expected<std::filesystem::path, std::string>;

// Where the compiler under test is, as this run was given it.
auto ResolveLyra() -> std::filesystem::path;

// Where the examples a reader is handed are, as this run was given them.
auto ResolveShippedExamples() -> std::filesystem::path;

// The symbol lister of the LLVM release Lyra links, as this run was given it.
// The shipped runtime carries that release's bitcode beside its machine code,
// and a host's `nm` may read it through whatever release's plugin the host has,
// listing nothing for an object it cannot read and still exiting 0.
auto ResolveLlvmNm() -> std::filesystem::path;

// Runs lyra from `dir`, which is what a declaration search reads and what no
// argument can express.
auto RunLyraFrom(
    const std::filesystem::path& lyra, const std::filesystem::path& dir,
    std::string_view args) -> ProcessOutcome;

// Clang, where this host has it. What a build costs is a fact about the
// compiler that ran it, so a test measuring one asks for that compiler by name
// rather than for whatever this machine happens to call a C++ compiler.
auto FindClang() -> std::optional<std::filesystem::path>;

// The smallest design that produces observable output. What the cases using it
// are about is what surrounds a design, so the design itself carries no weight
// beyond proving the program ran.
auto WriteTrivialSource(const std::filesystem::path& path) -> void;

// A file a run wrote about itself, parsed. A file that is absent or is not
// JSON answers with a discarded value, so the caller reports it as the run
// having written nothing rather than crashing on it.
auto ReadJson(const std::filesystem::path& path) -> nlohmann::json;

}  // namespace lyra::test
