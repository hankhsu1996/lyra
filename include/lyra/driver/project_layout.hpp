#pragma once

#include <cstdint>
#include <string_view>

namespace lyra::driver {

// How hard the host compiler works on a design. Iterating pays the compile on
// every edit and the run once; a long run inverts that. The runtime library is
// prebuilt and always optimized, so it is not on this axis.
enum class Optimization : std::uint8_t { kIterate, kRelease };

// A precompiled header and the translation unit including it must be compiled
// alike, so both arms name a flag: a compiler's default is not something two
// command lines can be checked against.
[[nodiscard]] constexpr auto OptimizationFlag(Optimization optimization)
    -> std::string_view {
  return optimization == Optimization::kRelease ? "-O2" : "-O0";
}

// Relative layout and build recipe of a self-contained emitted C++ project.
// Shared by the `build.sh` the project ships and the in-process build that
// `compile`/`run` drive, so the two never drift.
inline constexpr std::string_view kRuntimeIncludeDir = "runtime/include";
inline constexpr std::string_view kRuntimeLibDir = "runtime/lib";
inline constexpr std::string_view kRuntimeLibFile = "libcpp_runtime.a";
inline constexpr std::string_view kRuntimeCacheDir = "runtime/cache";
// Where a compiled object lands. Every source is compiled on its own so that a
// build may work on several at once, which needs somewhere for a finished one
// to wait until the link reads them all. A foreign source's object sits under
// this directory too, in a subdirectory of its own, because the design's own
// names and the user's are two name spaces and nothing forbids a collision
// between them.
inline constexpr std::string_view kObjectDir = "obj";
inline constexpr std::string_view kProgramName = "program";
inline constexpr std::string_view kCxxStandardFlag = "-std=c++23";
// The DPI-C boundary surface a user's foreign sources compile against (LRM 35):
// the generated prototypes plus the standard header they are spelled in. Both
// sit at the project root so one include path reaches them.
inline constexpr std::string_view kDpiAbiHeader = "dpi.h";
inline constexpr std::string_view kSvdpiHeader = "svdpi.h";
// Where a portable project keeps its copies of the user's DPI-C sources, so the
// directory builds on another machine without reaching back to their originals.
inline constexpr std::string_view kDpiSourceDir = "dpi";

}  // namespace lyra::driver
