#include "lyra/mir_transform/transform.hpp"

#include <array>
#include <format>
#include <ranges>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/phase.hpp"
#include "lyra/mir/verify.hpp"
#include "lyra/mir_transform/box_large_values.hpp"
#include "lyra/mir_transform/inst_simplify.hpp"
#include "lyra/mir_transform/order_evaluation.hpp"
#include "lyra/mir_transform/pass.hpp"
#include "lyra/mir_transform/reaction.hpp"
#include "lyra/profiling/time_trace.hpp"

namespace lyra::mir_transform {

namespace {

// An evaluation order made explicit, then every value a frame would hold too
// large for one held through a box -- the temporaries first, so boxing holds
// them as it holds every local.
constexpr std::array kBuiltToLegalized{
    Pass{.name = "OrderEvaluation", .run = OrderEvaluation},
    Pass{.name = "BoxLargeValues", .run = BoxLargeValues}};

// A step of the pipeline: a conversion into the next phase, or an optional
// rewrite inside the phase the unit is in, which leaves it there.
using Step = std::variant<Conversion, Pass>;

// InstSimplify runs on the unit as built: a whole it reads is then still a read
// of the value, which boxing then holds wherever a frame could not.
constexpr std::array<Step, 3> kPipeline{
    Pass{.name = "InstSimplify", .run = InstSimplify},
    Conversion{.into = mir::Phase::kLegalized, .passes = kBuiltToLegalized},
    Pass{.name = "SplitReactions", .run = SplitReactions},
};

auto PhaseName(mir::Phase phase) -> std::string_view {
  switch (phase) {
    case mir::Phase::kBuilt:
      return "Built";
    case mir::Phase::kLegalized:
      return "Legalized";
  }
  throw InternalError("mir_transform: unknown phase");
}

auto Runs(const Pass& optional, const PassOptions& options) -> bool {
  for (const PassOverride& asked : options.overrides | std::views::reverse) {
    if (asked.name == optional.name) {
      return asked.runs;
    }
  }
  return true;
}

auto Timed(const Pass& pass, mir::CompilationUnit unit)
    -> mir::CompilationUnit {
  const profiling::TimeTraceScope span(pass.name, [&] { return unit.name; });
  return pass.run(std::move(unit));
}

}  // namespace

auto OptionalPasses() -> std::vector<std::string> {
  std::vector<std::string> names;
  for (const Step& step : kPipeline) {
    if (const auto* optional = std::get_if<Pass>(&step)) {
      names.emplace_back(optional->name);
    }
  }
  return names;
}

auto Transform(mir::CompilationUnit unit, const PassOptions& options)
    -> mir::CompilationUnit {
  const profiling::TimeTraceScope span(
      "transform MIR", [&] { return unit.name; });
  if (options.verify_each) {
    mir::Verify(unit, "as lowered");
  }
  for (const Step& step : kPipeline) {
    std::visit(
        Overloaded{
            [&](const Conversion& conversion) {
              for (const Pass& pass : conversion.passes) {
                unit = Timed(pass, std::move(unit));
              }
              unit.phase = conversion.into;
              if (options.verify_each) {
                mir::Verify(
                    unit,
                    std::format("on reaching {}", PhaseName(conversion.into)));
              }
            },
            [&](const Pass& optional) {
              if (!Runs(optional, options)) {
                return;
              }
              unit = Timed(optional, std::move(unit));
              if (options.verify_each) {
                mir::Verify(unit, std::format("after {}", optional.name));
              }
            }},
        step);
  }
  if (!options.verify_each) {
    mir::Verify(unit, "as it leaves its rewrites");
  }
  return unit;
}

}  // namespace lyra::mir_transform
