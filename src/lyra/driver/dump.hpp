#pragma once

namespace lyra::driver {

struct CompilationInput;

auto DumpHir(const CompilationInput& input) -> int;
auto DumpMir(const CompilationInput& input) -> int;
auto DumpLlvm(const CompilationInput& input) -> int;

}  // namespace lyra::driver
