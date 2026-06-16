#include "lyra/lowering/hir_to_mir/expression/references.hpp"

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/hir_to_mir/capture_sink.hpp"
#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/procedural_depth.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto LowerTimeScale(hir::TimeScale s) -> mir::TimeScale {
  switch (s) {
    case hir::TimeScale::kFs:
      return mir::TimeScale::kFs;
    case hir::TimeScale::kPs:
      return mir::TimeScale::kPs;
    case hir::TimeScale::kNs:
      return mir::TimeScale::kNs;
    case hir::TimeScale::kUs:
      return mir::TimeScale::kUs;
    case hir::TimeScale::kMs:
      return mir::TimeScale::kMs;
    case hir::TimeScale::kS:
      return mir::TimeScale::kS;
  }
  throw InternalError("LowerTimeScale: unknown HIR TimeScale");
}

auto LowerSignedness(hir::Signedness s) -> mir::Signedness {
  switch (s) {
    case hir::Signedness::kSigned:
      return mir::Signedness::kSigned;
    case hir::Signedness::kUnsigned:
      return mir::Signedness::kUnsigned;
  }
  throw InternalError("LowerSignedness: unknown HIR Signedness");
}

auto LowerStateKind(hir::IntegralStateKind k) -> mir::IntegralStateKind {
  switch (k) {
    case hir::IntegralStateKind::kTwoState:
      return mir::IntegralStateKind::kTwoState;
    case hir::IntegralStateKind::kFourState:
      return mir::IntegralStateKind::kFourState;
  }
  throw InternalError("LowerStateKind: unknown HIR IntegralStateKind");
}

auto LowerHirIntegralConstant(const hir::IntegralConstant& c)
    -> mir::IntegralConstant {
  return mir::IntegralConstant{
      .value_words = c.value_words,
      .state_words = c.state_words,
      .width = c.width,
      .signedness = LowerSignedness(c.signedness),
      .state_kind = LowerStateKind(c.state_kind),
  };
}

auto LowerHirIntegerLiteral(const hir::IntegerLiteral& i, mir::TypeId type)
    -> mir::Expr {
  return mir::Expr{
      .data = mir::IntegerLiteral{.value = LowerHirIntegralConstant(i.value)},
      .type = type};
}

auto LowerHirStringLiteral(const hir::StringLiteral& s, mir::TypeId type)
    -> mir::Expr {
  return mir::Expr{.data = mir::StringLiteral{.value = s.value}, .type = type};
}

auto LowerHirTimeLiteral(const ModuleLowerer& module, const hir::TimeLiteral& t)
    -> mir::Expr {
  return mir::Expr{
      .data =
          mir::TimeLiteral{.value = t.value, .scale = LowerTimeScale(t.scale)},
      .type = module.Unit().builtins.realtime};
}

auto LowerHirRealLiteral(const hir::RealLiteral& r, mir::TypeId type)
    -> mir::Expr {
  return mir::Expr{.data = mir::RealLiteral{.value = r.value}, .type = type};
}

auto LowerStructuralVarRefExpr(
    const StructuralScopeLowerer& scope, const hir::StructuralVarRef& m,
    mir::TypeId type) -> mir::Expr {
  return mir::Expr{
      .data = scope.TranslateStructuralVar(m.hops, m.var), .type = type};
}

auto LowerProceduralVarRefExpr(
    ProcessLowerer& process, const WalkFrame& frame,
    const hir::ProceduralVarRef& l, mir::TypeId type) -> mir::Expr {
  if (auto* sink = frame.active_closure) {
    const auto& binding = process.LookupProceduralVar(l.var);
    if (binding.declaration_procedural_depth < sink->BoundaryDepth()) {
      return mir::Expr{
          .data = sink->Capture(
              binding.var, binding.declaration_procedural_depth, type,
              frame.procedural_depth),
          .type = type};
    }
  }
  return mir::Expr{
      .data = process.TranslateProceduralVar(frame, l.var), .type = type};
}

auto LowerCrossUnitVarRefExpr(
    const StructuralScopeLowerer& scope, const WalkFrame& frame,
    const hir::CrossUnitVarRef& c, mir::TypeId type) -> mir::Expr {
  const auto& meta = scope.CrossUnitRefTarget(c.id);
  const mir::StructuralVarRef target = meta.target;
  const auto* ptr = std::get_if<mir::PointerType>(
      &scope.Module().Unit().GetType(meta.slot_type).data);
  if (ptr != nullptr && ptr->ownership == mir::PointerOwnership::kBorrowed) {
    const mir::ExprId pointer = frame.current_procedural_scope->AddExpr(
        mir::Expr{.data = target, .type = meta.slot_type});
    return mir::Expr{.data = mir::DerefExpr{.pointer = pointer}, .type = type};
  }
  return mir::Expr{.data = target, .type = type};
}

auto LowerLoopVarRefExprAsProcedural(
    const StructuralScopeLowerer& scope, const hir::LoopVarRef& lv,
    mir::TypeId type) -> mir::Expr {
  return mir::Expr{
      .data = scope.TranslateLoopVarAsProcedural(lv.loop_var), .type = type};
}

auto LowerLoopVarRefExprAsStructuralParam(
    const StructuralScopeLowerer& scope, const hir::LoopVarRef& lv,
    mir::TypeId type) -> mir::Expr {
  return mir::Expr{
      .data = scope.TranslateLoopVarAsStructuralParam(lv.hops, lv.loop_var),
      .type = type};
}

}  // namespace

auto LowerHirPrimaryExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::Primary& p,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& scope = process.Scope();
  return std::visit(
      Overloaded{
          [&](const hir::IntegerLiteral& i) -> mir::Expr {
            return LowerHirIntegerLiteral(i, result_type);
          },
          [&](const hir::StringLiteral& s) -> mir::Expr {
            return LowerHirStringLiteral(s, result_type);
          },
          [&](const hir::TimeLiteral& t) -> mir::Expr {
            return LowerHirTimeLiteral(process.Module(), t);
          },
          [&](const hir::RealLiteral& r) -> mir::Expr {
            return LowerHirRealLiteral(r, result_type);
          },
          [&](const hir::StructuralVarRef& m) -> mir::Expr {
            return LowerStructuralVarRefExpr(scope, m, result_type);
          },
          [&](const hir::ProceduralVarRef& l) -> mir::Expr {
            return LowerProceduralVarRefExpr(process, frame, l, result_type);
          },
          [&](const hir::LoopVarRef& lv) -> mir::Expr {
            return LowerLoopVarRefExprAsStructuralParam(scope, lv, result_type);
          },
          [&](const hir::CrossUnitVarRef& c) -> mir::Expr {
            return LowerCrossUnitVarRefExpr(scope, frame, c, result_type);
          },
      },
      p);
}

auto LowerHirPrimaryExprStructural(
    const StructuralScopeLowerer& scope, WalkFrame frame, const hir::Primary& p,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  return std::visit(
      Overloaded{
          [&](const hir::IntegerLiteral& i) -> mir::Expr {
            return LowerHirIntegerLiteral(i, result_type);
          },
          [&](const hir::StringLiteral& s) -> mir::Expr {
            return LowerHirStringLiteral(s, result_type);
          },
          [&](const hir::TimeLiteral& t) -> mir::Expr {
            return LowerHirTimeLiteral(scope.Module(), t);
          },
          [&](const hir::RealLiteral& r) -> mir::Expr {
            return LowerHirRealLiteral(r, result_type);
          },
          [&](const hir::StructuralVarRef& m) -> mir::Expr {
            return LowerStructuralVarRefExpr(scope, m, result_type);
          },
          [](const hir::ProceduralVarRef&) -> mir::Expr {
            throw InternalError(
                "LowerHirPrimaryExprStructural: HIR ProceduralVarRef does not "
                "appear in structural expressions");
          },
          [&](const hir::LoopVarRef& lv) -> mir::Expr {
            if (frame.loop_var_mode ==
                LoopVarLoweringMode::kProceduralInduction) {
              return LowerLoopVarRefExprAsProcedural(scope, lv, result_type);
            }
            return LowerLoopVarRefExprAsStructuralParam(scope, lv, result_type);
          },
          [&](const hir::CrossUnitVarRef& c) -> mir::Expr {
            return LowerCrossUnitVarRefExpr(scope, frame, c, result_type);
          },
      },
      p);
}

}  // namespace lyra::lowering::hir_to_mir
