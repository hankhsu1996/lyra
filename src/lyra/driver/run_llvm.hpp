#pragma once

#include <string>
#include <vector>

namespace lyra::driver {

auto RunLlvm(const std::vector<std::string>& files) -> int;

}  // namespace lyra::driver
