#include "lyra/mir_transform/transform.hpp"

#include <array>
#include <format>
#include <ranges>
#include <span>
#include <utility>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/phase.hpp"
#include "lyra/mir/verify.hpp"
#include "lyra/mir_transform/box_large_values.hpp"
#include "lyra/mir_transform/inst_simplify.hpp"
#include "lyra/mir_transform/pass.hpp"
#include "lyra/mir_transform/reaction.hpp"
#include "lyra/profiling/time_trace.hpp"

namespace lyra::mir_transform {

namespace {

// InstSimplify runs on the unit as built: a whole it reads is then still a read
// of the value, which boxing then holds wherever a frame could not.
constexpr std::array kPipeline{
    Pass{
        .name = "InstSimplify",
        .policy = PassPolicy::kOptional,
        .run = InstSimplify,
        .phase_after = std::nullopt},
    Pass{
        .name = "BoxLargeValues",
        .policy = PassPolicy::kRequired,
        .run = BoxLargeValues,
        .phase_after = mir::Phase::kForBackends},
    Pass{
        .name = "SplitReactions",
        .policy = PassPolicy::kOptional,
        .run = SplitReactions,
        .phase_after = std::nullopt},
};

auto Runs(const Pass& pass, const PassOptions& options) -> bool {
  if (!MayBeSkipped(pass.policy)) {
    return true;
  }
  for (const PassOverride& asked : options.overrides | std::views::reverse) {
    if (asked.name == pass.name) {
      return asked.runs;
    }
  }
  return true;
}

}  // namespace

auto Pipeline() -> std::span<const Pass> {
  return kPipeline;
}

auto Transform(mir::CompilationUnit unit, const PassOptions& options)
    -> mir::CompilationUnit {
  const profiling::TimeTraceScope span(
      "transform MIR", [&] { return unit.name; });
  mir::Phase phase = mir::Phase::kBuilt;
  if (options.verify_each) {
    mir::Verify(unit, phase, "as lowered");
  }
  for (const Pass& pass : kPipeline) {
    if (!Runs(pass, options)) {
      continue;
    }
    {
      const profiling::TimeTraceScope pass_span(
          pass.name, [&] { return unit.name; });
      unit = pass.run(std::move(unit));
    }
    phase = pass.phase_after.value_or(phase);
    if (options.verify_each) {
      mir::Verify(unit, phase, std::format("after {}", pass.name));
    }
  }
  if (!options.verify_each) {
    mir::Verify(unit, phase, "as it leaves its rewrites");
  }
  return unit;
}

}  // namespace lyra::mir_transform
