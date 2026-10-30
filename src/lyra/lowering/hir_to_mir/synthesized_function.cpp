#include "lyra/lowering/hir_to_mir/synthesized_function.hpp"

#include <optional>
#include <utility>
#include <vector>

#include "lyra/hir/subroutine_kind.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto PayloadOf(mir::CompilationUnit& unit, std::optional<mir::TypeId> type)
    -> mir::TypeId {
  return CompletionPayloadType(
      unit, type.has_value() ? std::vector<mir::TypeId>{*type}
                             : std::vector<mir::TypeId>{});
}

}  // namespace

SynthesizedFunction::SynthesizedFunction(
    UnitLowerer& unit_lowerer, const WalkFrame& owner, const ClassShape& shape,
    CallableForm form)
    : unit_lowerer_(&unit_lowerer), bindings_(unit_lowerer.Unit(), code_) {
  BoundImplicitParameters bound = BindImplicitParameters(
      unit_lowerer,
      owner.WithBlock(&code_.Body())
          .WithBindings(&bindings_)
          .WithRunsAsFunctionCall(true),
      shape, form);
  frame_ = bound.frame;
  params_ = std::move(bound.params);
}

auto SynthesizedFunction::AddFormal(mir::TypeId type) -> mir::LocalId {
  params_.push_back(bindings_.DeclareAnonymous(type));
  return params_.back();
}

void SynthesizedFunction::AddFormalLocal(mir::LocalId local) {
  params_.push_back(local);
}

auto SynthesizedFunction::Finish(std::optional<mir::ExprId> result)
    -> mir::CallableCode {
  mir::CompilationUnit& unit = unit_lowerer_->Unit();
  params_.push_back(bindings_.DeclareAnonymous(unit.builtins.read_report_ptr));
  const mir::TypeId payload =
      PayloadOf(unit, result.transform([&](mir::ExprId e) {
        return code_.Body().exprs.Get(e).type;
      }));
  std::vector<mir::ExprId> parts;
  if (result.has_value()) parts.push_back(*result);
  code_.Body().AppendStmt(
      mir::ReturnStmt{
          .value = code_.Body().exprs.Add(
              mir::Expr{
                  .data = mir::CompositeExpr{.parts = std::move(parts)},
                  .type = payload})});
  code_.params = std::move(params_);
  code_.result_type =
      SubroutineCallType(unit, hir::SubroutineKind::kFunction, payload);
  return std::move(code_);
}

auto BuildFunctionCall(
    mir::CompilationUnit& unit, const WalkFrame& frame, mir::Callee callee,
    std::vector<mir::ExprId> arguments, std::optional<mir::TypeId> result)
    -> mir::ExprId {
  mir::Block& block = *frame.current_block;
  arguments.push_back(BuildReportHandedOn(unit, frame));
  const mir::ExprId call = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = std::move(callee),
                  .arguments = std::move(arguments)},
          .type = SubroutineCallType(
              unit, hir::SubroutineKind::kFunction, PayloadOf(unit, result))});
  if (!result.has_value()) return call;
  return block.AsValue(
      unit.types,
      block.exprs.Add(
          mir::MakeFieldAccessExpr(
              call, mir::ComponentFieldTarget{.position = kCompletionResult},
              *result)));
}

}  // namespace lyra::lowering::hir_to_mir
