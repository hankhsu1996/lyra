#include "driver_output_options.hpp"

#include "frontend.hpp"

namespace lyra::driver {

namespace {

auto BuildNonLinkingDriverOutputOptions(const CompilationInput& input)
    -> DriverOutputOptions {
  return {
      .show_phase_progress = input.verbose >= 1,
      .show_stats = false,
      .show_analysis = input.verbose >= 2,
      .show_link_progress = false,
  };
}

}  // namespace

auto BuildJitDriverOutputOptions(const CompilationInput& input, bool emit_stats)
    -> DriverOutputOptions {
  return {
      .show_phase_progress = input.verbose >= 1,
      .show_stats = emit_stats,
      .show_analysis = input.verbose >= 2,
      .show_link_progress = input.verbose >= 1,
  };
}

auto BuildLliDriverOutputOptions(const CompilationInput& input, bool emit_stats)
    -> DriverOutputOptions {
  return {
      .show_phase_progress = input.verbose >= 1,
      .show_stats = emit_stats,
      .show_analysis = input.verbose >= 2,
      .show_link_progress = false,
  };
}

auto BuildCompileDriverOutputOptions(const CompilationInput& input)
    -> DriverOutputOptions {
  return BuildNonLinkingDriverOutputOptions(input);
}

auto BuildDumpDriverOutputOptions(const CompilationInput& input)
    -> DriverOutputOptions {
  return BuildNonLinkingDriverOutputOptions(input);
}

auto BuildCheckDriverOutputOptions(const CompilationInput& input)
    -> DriverOutputOptions {
  return {
      .show_phase_progress = input.verbose >= 1,
      .show_stats = false,
      .show_analysis = false,
      .show_link_progress = false,
  };
}

}  // namespace lyra::driver
