#pragma once

#include <string_view>

namespace lyra::base {

// The name a file or a program Lyra writes says it was written by.
inline constexpr std::string_view kProductName = "Lyra";

// The version of Lyra this was built from, as its module states it.
[[nodiscard]] auto LyraVersion() -> std::string_view;

}  // namespace lyra::base
