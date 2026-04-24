#pragma once

#include <string_view>

namespace lyra {

void PrintError(std::string_view message);
void PrintWarning(std::string_view message);

}  // namespace lyra
