#pragma once

#include <cstdint>
#include <string_view>

namespace lyra::driver {

// How hard the host compiler works on a design. Iterating pays the compile on
// every edit and the run once; a long run inverts that. The runtime library is
// prebuilt and always optimized, so it is not on this axis.
enum class Optimization : std::uint8_t { kIterate, kRelease };

// Both arms are spelled rather than one left to the compiler's default: a
// precompiled header and the unit including it must be compiled alike, and two
// command lines cannot be checked against a default.
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
inline constexpr std::string_view kMainSource = "main.cpp";
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
