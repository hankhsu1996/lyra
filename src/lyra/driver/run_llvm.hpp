#pragma once

namespace lyra::driver {

struct CompilationInput;

auto RunLlvm(const CompilationInput& input) -> int;

}  // namespace lyra::driver
