#pragma once

#include <string>

namespace lyra::driver {

auto DumpHir(const std::string& path) -> int;
auto DumpMir(const std::string& path) -> int;
auto DumpLlvm(const std::string& path) -> int;

}  // namespace lyra::driver
