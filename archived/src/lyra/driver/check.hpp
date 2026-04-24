#pragma once

namespace lyra::driver {

struct CompilationInput;

auto Check(const CompilationInput& input) -> int;

}  // namespace lyra::driver
