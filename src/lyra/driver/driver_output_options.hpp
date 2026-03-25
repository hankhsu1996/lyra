#pragma once

#include "driver_output_types.hpp"

namespace lyra::driver {

struct CompilationInput;

auto BuildJitDriverOutputOptions(const CompilationInput& input, bool emit_stats)
    -> DriverOutputOptions;
auto BuildLliDriverOutputOptions(const CompilationInput& input, bool emit_stats)
    -> DriverOutputOptions;
auto BuildCompileDriverOutputOptions(const CompilationInput& input)
    -> DriverOutputOptions;
auto BuildDumpDriverOutputOptions(const CompilationInput& input)
    -> DriverOutputOptions;
auto BuildCheckDriverOutputOptions(const CompilationInput& input)
    -> DriverOutputOptions;

}  // namespace lyra::driver
