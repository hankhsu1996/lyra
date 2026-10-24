#pragma once

#include <string_view>

namespace lyra::runtime {

// The notices of the third-party code the runtime carries, which every program
// linking it carries too: each library's name above its license.
[[nodiscard]] auto ThirdPartyLicenses() -> std::string_view;

}  // namespace lyra::runtime
