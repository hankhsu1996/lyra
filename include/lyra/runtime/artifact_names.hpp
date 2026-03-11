#pragma once

#include <string_view>

namespace lyra::runtime {

// Runtime library artifact names (must match Bazel target outputs in
// BUILD.bazel). Used by the driver and test framework for runtime library
// discovery.
inline constexpr std::string_view kStaticLibName = "liblyra_runtime_static.a";
inline constexpr std::string_view kSharedLibName = "liblyra_runtime.so";

}  // namespace lyra::runtime
