#include "lyra/lowering/hir_to_mir/expression/references.hpp"

#include <cstdint>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/hir_to_mir/block_depth.hpp"
#include "lyra/lowering/hir_to_mir/capture_sink.hpp"
#include "lyra/lowering/hir_to_mir/class_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/type.hpp"

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

// LRM 5.9: pack a string literal's bytes into the integer constant they denote,
// the first byte most significant. A byte is 8-bit-aligned, so it never
// straddles a 64-bit word.
auto StringBytesToConstant(
    std::string_view text, const mir::PackedArrayType& pa)
    -> mir::IntegralConstant {
  const auto width = static_cast<std::uint32_t>(pa.BitWidth());
  std::vector<std::uint64_t> value_words((width + 63U) / 64U, 0U);
  for (std::size_t i = 0; i < text.size(); ++i) {
    const std::uint64_t bit_offset = width - (8U * (i + 1U));
    value_words[bit_offset / 64U] |=
        static_cast<std::uint64_t>(static_cast<unsigned char>(text[i]))
        << (bit_offset % 64U);
  }
  return mir::IntegralConstant{
      .value_words = std::move(value_words),
      .state_words = {},
      .width = width,
      .signedness = pa.signedness,
      .state_kind = mir::IntegralStateKind::kTwoState,
  };
}

// An SV string literal is a packed bit-vector constant (LRM 5.9), so MIR
// carries it as the integer constant of its bytes. A string-typed literal (a
// string parameter's value) instead builds a `value::String` via the
// constructor.
auto LowerHirStringLiteral(
    const ModuleLowerer& module, const WalkFrame& frame,
    const hir::StringLiteral& s, mir::TypeId type) -> mir::Expr {
  const auto& ty = module.Unit().GetType(type);
  if (ty.IsIntegralPacked()) {
    return mir::Expr{
        .data =
            mir::IntegerLiteral{
                .value = StringBytesToConstant(s.value, ty.AsIntegralPacked())},
        .type = type};
  }
  // A string-typed literal (e.g. a string parameter's value) builds a
  // `value::String` from the software literal via the constructor, on the
  // block that holds the surrounding expression.
  auto& block = *frame.current_block;
  const mir::ExprId lit = block.exprs.Add(
      mir::Expr{.data = mir::StringLiteral{.value = s.value}, .type = type});
  return mir::Expr{
      .data =
          mir::CallExpr{.callee = mir::ConstructorCallee{}, .arguments = {lit}},
      .type = type};
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

// Reach the storage cell of a member: `MemberAccess(self, member)`.
// `Expr.type` is the var's declared MIR type -- a wrapper
// (`ObservableType` / `ExternalRefType`) for observable storage, the plain
// value type otherwise. The dispatcher decides whether to wrap the result
// in an `ObservableMethod{kGet}` call to unwrap to a value.
auto LowerStructuralVarRefExpr(
    const ClassLowerer& lowerer, const WalkFrame& frame,
    const hir::StructuralVarRef& m) -> mir::Expr {
  const mir::MemberRef member = lowerer.TranslateStructuralVar(m.hops, m.var);
  return BuildStructuralMemberAccessExpr(frame, member);
}

auto LowerProceduralVarRefExpr(
    ProcessLowerer& process, const WalkFrame& frame,
    const hir::ProceduralVarRef& l, mir::TypeId type) -> mir::Expr {
  const auto& binding = process.LookupProceduralVar(l.var);
  // A static-lifetime local lives as a per-instance member on the
  // owner class (LRM 13.3.1); the read reaches it through `self` like any
  // member ref.
  if (const auto* sb = std::get_if<StaticVarBinding>(&binding)) {
    const mir::MemberRef member{
        .hops = mir::EnclosingHops{.value = 0}, .var = sb->var};
    return BuildStructuralMemberAccessExpr(frame, member);
  }
  const auto& ab = std::get<AutomaticVarBinding>(binding);
  if (auto* sink = frame.capture_sink) {
    if (ab.declaration_procedural_depth < sink->BoundaryDepth()) {
      return mir::Expr{
          .data = sink->Capture(
              ab.var, ab.declaration_procedural_depth, type, frame.block_depth),
          .type = type};
    }
  }
  if (ab.declaration_procedural_depth > frame.block_depth) {
    throw InternalError(
        "LowerProceduralVarRefExpr: declaration depth exceeds current depth "
        "(forward reference into a child scope)");
  }
  return mir::MakeLocalRefExpr(
      frame.block_depth - ab.declaration_procedural_depth, ab.var, type);
}

auto LowerCrossUnitVarRefExpr(
    const ClassLowerer& lowerer, const WalkFrame& frame,
    const hir::CrossUnitVarRef& c) -> mir::Expr {
  const auto& meta = lowerer.CrossUnitRefTarget(c.id);
  const mir::MemberRef target = meta.target;
  const mir::TypeId self_ptr_type = frame.current_class->self_pointer_type;
  const mir::ExprId self_ref =
      frame.current_block->exprs.Add(MakeSelfRefExpr(frame, self_ptr_type));
  const auto* ptr = std::get_if<mir::PointerType>(
      &lowerer.Module().Unit().GetType(meta.slot_type).data);
  if (ptr != nullptr && ptr->ownership == mir::PointerOwnership::kBorrowed) {
    const mir::ExprId pointer = frame.current_block->exprs.Add(
        mir::MakeMemberAccessExpr(self_ref, target, meta.slot_type));
    return mir::Expr{
        .data = mir::DerefExpr{.pointer = pointer}, .type = ptr->pointee};
  }
  return mir::MakeMemberAccessExpr(self_ref, target, meta.slot_type);
}

auto LowerLoopVarRefExprAsProcedural(
    const ClassLowerer& lowerer, const hir::LoopVarRef& lv, mir::TypeId type)
    -> mir::Expr {
  return mir::Expr{
      .data = lowerer.TranslateLoopVarAsProcedural(lv.loop_var), .type = type};
}

auto LowerLoopVarRefExprAsStructuralParam(
    const ClassLowerer& lowerer, const hir::LoopVarRef& lv, mir::TypeId type)
    -> mir::Expr {
  return mir::Expr{
      .data = lowerer.TranslateLoopVarAsStructuralParam(lv.hops, lv.loop_var),
      .type = type};
}

}  // namespace

auto LowerHirPrimaryExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::Primary& p,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& lowerer = process.Owner();
  return std::visit(
      Overloaded{
          [&](const hir::IntegerLiteral& i) -> mir::Expr {
            return LowerHirIntegerLiteral(i, result_type);
          },
          [&](const hir::StringLiteral& s) -> mir::Expr {
            return LowerHirStringLiteral(
                process.Module(), frame, s, result_type);
          },
          [&](const hir::TimeLiteral& t) -> mir::Expr {
            return LowerHirTimeLiteral(process.Module(), t);
          },
          [&](const hir::RealLiteral& r) -> mir::Expr {
            return LowerHirRealLiteral(r, result_type);
          },
          [&](const hir::StructuralVarRef& m) -> mir::Expr {
            return LowerStructuralVarRefExpr(lowerer, frame, m);
          },
          [&](const hir::ProceduralVarRef& l) -> mir::Expr {
            return LowerProceduralVarRefExpr(process, frame, l, result_type);
          },
          [&](const hir::LoopVarRef& lv) -> mir::Expr {
            return LowerLoopVarRefExprAsStructuralParam(
                lowerer, lv, result_type);
          },
          [&](const hir::CrossUnitVarRef& c) -> mir::Expr {
            return LowerCrossUnitVarRefExpr(lowerer, frame, c);
          },
      },
      p);
}

auto LowerHirPrimaryExprStructural(
    const ClassLowerer& lowerer, WalkFrame frame, const hir::Primary& p,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  return std::visit(
      Overloaded{
          [&](const hir::IntegerLiteral& i) -> mir::Expr {
            return LowerHirIntegerLiteral(i, result_type);
          },
          [&](const hir::StringLiteral& s) -> mir::Expr {
            return LowerHirStringLiteral(
                lowerer.Module(), frame, s, result_type);
          },
          [&](const hir::TimeLiteral& t) -> mir::Expr {
            return LowerHirTimeLiteral(lowerer.Module(), t);
          },
          [&](const hir::RealLiteral& r) -> mir::Expr {
            return LowerHirRealLiteral(r, result_type);
          },
          [&](const hir::StructuralVarRef& m) -> mir::Expr {
            return LowerStructuralVarRefExpr(lowerer, frame, m);
          },
          [](const hir::ProceduralVarRef&) -> mir::Expr {
            throw InternalError(
                "LowerHirPrimaryExprStructural: HIR ProceduralVarRef does not "
                "appear in structural expressions");
          },
          [&](const hir::LoopVarRef& lv) -> mir::Expr {
            if (frame.loop_var_mode ==
                LoopVarLoweringMode::kProceduralInduction) {
              return LowerLoopVarRefExprAsProcedural(lowerer, lv, result_type);
            }
            return LowerLoopVarRefExprAsStructuralParam(
                lowerer, lv, result_type);
          },
          [&](const hir::CrossUnitVarRef& c) -> mir::Expr {
            return LowerCrossUnitVarRefExpr(lowerer, frame, c);
          },
      },
      p);
}

}  // namespace lyra::lowering::hir_to_mir
