#pragma once

namespace lyra::driver {

struct CompilationInput;

auto RunMir(const CompilationInput& input) -> int;

}  // namespace lyra::driver
