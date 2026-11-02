#include "lyra/mir_transform/state_caller_locations.hpp"

#include <algorithm>
#include <format>
#include <optional>
#include <string>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/caller_location.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir_transform/block_rebuild.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::mir_transform {

namespace {

// Whether any call of `block`, or of a block nested in it, hands over where it
// was written.
auto HandsALocation(const mir::CompilationUnit& unit, const mir::Block& block)
    -> bool {
  return std::ranges::any_of(
             block.exprs,
             [&](const mir::Expr& expr) {
               const auto* call = std::get_if<mir::CallExpr>(&expr.data);
               return call != nullptr &&
                      mir::TakesCallerLocation(unit, *call, expr.type);
             }) ||
         std::ranges::any_of(block.child_scopes, [&](const mir::Block& child) {
           return HandsALocation(unit, child);
         });
}

auto WithLocationsStated(
    const mir::CompilationUnit& unit, const diag::SourceManager& sources,
    const mir::CodeSource& at, const mir::CallableCode& code)
    -> mir::CallableCode {
  const mir::TypeId location_type = unit.builtins.source_location;
  const RebuildRules rules{
      .expr = [&](BlockRebuild& rebuild,
                  mir::ExprId id) -> std::optional<mir::Expr> {
        const mir::Expr& expr = rebuild.From().exprs.Get(id);
        const auto* call = std::get_if<mir::CallExpr>(&expr.data);
        if (call == nullptr ||
            !mir::TakesCallerLocation(unit, *call, expr.type)) {
          return std::nullopt;
        }
        if (!code.caller_location.has_value() &&
            expr.span == diag::SourceSpan{}) {
          const std::optional<support::BuiltinFn> entry =
              mir::DirectBuiltinFn(*call);
          throw InternalError(
              std::format(
                  "mir_transform: {} calls {}, which reports where it was "
                  "written, and names no place in the source for the call -- "
                  "please report this as a bug",
                  mir::Describe(unit, at),
                  entry.has_value() ? std::format(
                                          "the library's '{}'",
                                          support::RuntimeEntryOf(*entry).name)
                                    : std::string("a body of the program")));
        }
        mir::ExprData carried = rebuild.WithOperandsTaken(
            expr.data,
            [&](mir::ExprId operand) { return rebuild.Expr(operand); });
        // A body reporting at its caller hands on the location it was handed;
        // any other hands the unit's constant for the location of the call.
        const mir::ExprId location =
            code.caller_location.has_value()
                ? rebuild.Add(
                      mir::MakeCopyExpr(
                          rebuild.Add(
                              mir::MakeLocalRefExpr(
                                  *code.caller_location, location_type)),
                          location_type))
                : mir::BuildSourceLocation(
                      unit, mir::PositionOf(sources, expr.span), rebuild);
        std::get<mir::CallExpr>(carried).arguments.push_back(location);
        return mir::Expr{.data = std::move(carried), .type = expr.type};
      }};
  return RebuildCode(code, rules);
}

}  // namespace

auto StateCallerLocationsOnBody(
    const PassContext& context, const mir::CodeSource& at,
    const mir::CallableCode& code) -> std::optional<mir::CallableCode> {
  const mir::CompilationUnit& unit = *context.unit;
  if (!code.body.has_value() || !HandsALocation(unit, *code.body)) {
    return std::nullopt;
  }
  return WithLocationsStated(unit, *context.sources, at, code);
}

}  // namespace lyra::mir_transform
