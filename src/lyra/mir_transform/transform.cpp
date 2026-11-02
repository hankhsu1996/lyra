#include "lyra/mir_transform/transform.hpp"

#include <array>
#include <format>
#include <optional>
#include <ranges>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/phase.hpp"
#include "lyra/mir/verify.hpp"
#include "lyra/mir_transform/bound_frames.hpp"
#include "lyra/mir_transform/evaluation_temporaries.hpp"
#include "lyra/mir_transform/inst_simplify.hpp"
#include "lyra/mir_transform/pass.hpp"
#include "lyra/mir_transform/reaction.hpp"
#include "lyra/profiling/time_trace.hpp"

namespace lyra::mir_transform {

namespace {

// An evaluation order made explicit, which asks no value's size; then where
// each value lies, in that order, so that no frame follows a size a design
// declares.
constexpr std::array kBuiltToLegalized{
    Pass{.name = "OrderEvaluation", .run_on_body = OrderEvaluationOnBody},
    Pass{
        .name = "BoundFrames",
        .run_on_body = BoundFramesOnBody,
        .run_on_unit = BoundFramesOfDeclarations}};

// A step of the pipeline: a conversion into the next phase, or an optional
// rewrite inside the phase the unit is in, which leaves it there.
using Step = std::variant<Conversion, Pass>;

// InstSimplify runs on the unit as built: a whole it reads is then still a read
// of the value, which boxing then holds wherever a frame could not.
constexpr std::array<Step, 3> kPipeline{
    Pass{.name = "InstSimplify", .run_on_body = InstSimplifyOnBody},
    Conversion{.into = mir::Phase::kLegalized, .passes = kBuiltToLegalized},
    Pass{.name = "SplitReactions", .run_on_unit = SplitReactions},
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
  if (pass.run_on_unit != nullptr) {
    unit = pass.run_on_unit(std::move(unit));
  }
  if (pass.run_on_body != nullptr) {
    unit = mir::RewriteEachCode(std::move(unit), pass.run_on_body);
  }
  return unit;
}

// Whether the unit is checked as lowered, after every optional rewrite and on
// reaching each phase.
auto VerifiesEachStep(VerifierKind verifier) -> bool {
  switch (verifier) {
    case VerifierKind::kEachStep:
      return true;
    case VerifierKind::kOutput:
    case VerifierKind::kNone:
      return false;
  }
  throw InternalError("mir_transform: unknown verifier kind");
}

// Whether the unit is checked once, as it leaves the last rewrite.
auto VerifiesOutput(VerifierKind verifier) -> bool {
  switch (verifier) {
    case VerifierKind::kOutput:
      return true;
    case VerifierKind::kEachStep:
    case VerifierKind::kNone:
      return false;
  }
  throw InternalError("mir_transform: unknown verifier kind");
}

// Takes what `run` rewrites through the pipeline. `run` answers whether a pass
// did anything to it, `reached` is told each phase it enters, and `check` when
// the verification `options` asks is due, with the phase it is in.
template <typename Run, typename Reached, typename Check>
void WalkPipeline(
    const PassOptions& options, const Run& run, const Reached& reached,
    const Check& check) {
  mir::Phase phase = mir::Phase::kBuilt;
  const auto step_checked = [&](std::string_view when) {
    if (VerifiesEachStep(options.verifier)) {
      check(phase, when);
    }
  };
  step_checked("as lowered");
  for (const Step& step : kPipeline) {
    std::visit(
        Overloaded{
            [&](const Conversion& conversion) {
              for (const Pass& pass : conversion.passes) {
                run(pass);
              }
              phase = conversion.into;
              reached(phase);
              step_checked(
                  std::format("on reaching {}", PhaseName(conversion.into)));
            },
            [&](const Pass& optional) {
              if (Runs(optional, options) && run(optional)) {
                step_checked(std::format("after {}", optional.name));
              }
            }},
        step);
  }
  if (VerifiesOutput(options.verifier)) {
    check(phase, "as it leaves its rewrites");
  }
}

}  // namespace

auto TransformBody(
    const mir::CompilationUnit& unit, const mir::CodeSource& at,
    mir::CallableCode code, const PassOptions& options) -> mir::CallableCode {
  WalkPipeline(
      options,
      [&](const Pass& pass) {
        if (pass.run_on_body == nullptr) {
          return false;
        }
        if (std::optional<mir::CallableCode> answer =
                pass.run_on_body(unit, at, code)) {
          code = *std::move(answer);
        }
        return true;
      },
      [](mir::Phase) {},
      [&](mir::Phase phase, std::string_view when) {
        mir::VerifyBody(unit, phase, at, code, when);
      });
  return code;
}

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
  WalkPipeline(
      options,
      [&](const Pass& pass) {
        unit = Timed(pass, std::move(unit));
        return true;
      },
      [&](mir::Phase phase) { unit.phase = phase; },
      [&](mir::Phase, std::string_view when) { mir::Verify(unit, when); });
  return unit;
}

}  // namespace lyra::mir_transform
