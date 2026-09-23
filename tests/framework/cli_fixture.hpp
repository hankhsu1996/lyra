#pragma once

#include <expected>
#include <filesystem>
#include <optional>
#include <string>

namespace lyra::test {

// A directory of the caller's own to emit into. The name is drawn rather than
// derived from the test's, because a test that reruns must not inherit what a
// previous run left behind. Returns why it failed rather than throwing, so a
// setup failure is reported as the test failing rather than as a crash.
auto MakeScratchDir() -> std::expected<std::filesystem::path, std::string>;

// Where the compiler under test is, as this run was given it.
auto ResolveLyra() -> std::filesystem::path;

// The host compiler an emitted project's own recipe defaults to, where this
// host has one. What such a build costs is a fact about that compiler, so a
// test measuring one asks for it by name rather than for whatever this machine
// happens to call a C++ compiler.
auto FindDefaultCxx() -> std::optional<std::filesystem::path>;

// The smallest design that produces observable output. What the cases using it
// are about is what surrounds a design, so the design itself carries no weight
// beyond proving the program ran.
auto WriteTrivialSource(const std::filesystem::path& path) -> void;

}  // namespace lyra::test
