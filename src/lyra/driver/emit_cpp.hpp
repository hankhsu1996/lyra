#pragma once

#include <string>

namespace lyra::driver {

struct CompilationInput;

auto EmitCpp(const CompilationInput& input, const std::string& output_dir)
    -> int;

}  // namespace lyra::driver
