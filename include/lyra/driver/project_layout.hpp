#pragma once

#include <string_view>

namespace lyra::driver {

// Relative layout and build recipe of a self-contained emitted C++ project.
// Shared by the `build.sh` the project ships and the in-process build that
// `compile`/`run` drive, so the two never drift.
inline constexpr std::string_view kRuntimeIncludeDir = "runtime/include";
inline constexpr std::string_view kRuntimeLibDir = "runtime/lib";
inline constexpr std::string_view kRuntimeLibFile = "libcpp_runtime.a";
inline constexpr std::string_view kMainSource = "main.cpp";
inline constexpr std::string_view kProgramName = "program";
inline constexpr std::string_view kCxxStandardFlag = "-std=c++23";

}  // namespace lyra::driver
