#pragma once

#include <string_view>

namespace lyra::driver {

// Relative layout and build recipe of a self-contained emitted C++ project.
// Shared by the `build.sh` the project ships and the in-process build that
// `compile`/`run` drive, so the two never drift.
inline constexpr std::string_view kRuntimeIncludeDir = "runtime/include";
inline constexpr std::string_view kRuntimeLibDir = "runtime/lib";
inline constexpr std::string_view kRuntimeLibFile = "libcpp_runtime.a";
inline constexpr std::string_view kRuntimeCacheDir = "runtime/cache";
inline constexpr std::string_view kPreludeHeader = "lyra/runtime/prelude.hpp";
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
