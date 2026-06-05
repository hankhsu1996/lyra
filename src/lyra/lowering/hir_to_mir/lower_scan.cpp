#include "lyra/lowering/hir_to_mir/lower_scan.hpp"

#include <expected>
#include <format>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/lower_expr.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/runtime_scan.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// LRM 21.3.4.3: $sscanf accepts string, integral, or unpacked-array-of-byte
// sources. The runtime entry takes `value::String`, so the non-string
// shapes wrap in an implicit `ConversionExpr` and the backend picks the
// matching constructor (string literal unwrap or `String::FromByteArray`).
// Any source type outside the three permitted shapes is an upstream
// invariant violation -- slang's type-check rejects it before HIR.
auto LiftStringSource(
    const UnitLoweringState& unit_state,
    ProceduralScopeLoweringState& proc_scope_state, mir::TypeId source_type,
    mir::ExprId source_id) -> mir::ExprId {
  const mir::TypeKind kind = unit_state.GetType(source_type).Kind();
  if (kind == mir::TypeKind::kString) return source_id;

  if (kind == mir::TypeKind::kUnpackedArray) {
    const auto& ua =
        std::get<mir::UnpackedArrayType>(unit_state.GetType(source_type).data);
    const auto& elem = unit_state.GetType(ua.element_type);
    if (!elem.IsIntegralPacked() || elem.AsIntegralPacked().BitWidth() != 8U) {
      throw InternalError(
          "LiftStringSource: $sscanf unpacked-array source must have an "
          "8-bit integral element (LRM 21.3.4.3); slang's type-check should "
          "have rejected other shapes");
    }
  } else if (kind != mir::TypeKind::kPackedArray) {
    throw InternalError(
        "LiftStringSource: $sscanf source is not string, integral, or "
        "unpacked array of byte (LRM 21.3.4.3); slang's type-check should "
        "have rejected upstream");
  }

  return proc_scope_state.AddExpr(
      mir::Expr{
          .data =
              mir::ConversionExpr{
                  .operand = source_id, .kind = mir::ConversionKind::kImplicit},
          .type = unit_state.Builtins().string});
}

}  // namespace

auto LowerScanSystemSubroutineCall(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::CallExpr& call,
    const support::ScanSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  // LRM 21.3.4.3 grammar requires source + format + at least one output,
  // none elidable; slang's ArgCountPolicy enforces this upstream.
  if (call.arguments.size() < 3) {
    throw InternalError(
        "LowerScanSystemSubroutineCall: fewer than 3 arguments reached "
        "lowering");
  }
  if (!call.arguments[0].has_value() || !call.arguments[1].has_value()) {
    throw InternalError(
        "LowerScanSystemSubroutineCall: source / format arg elided");
  }

  auto source_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
      hir_proc.exprs.at(call.arguments[0]->value));
  if (!source_or) return std::unexpected(std::move(source_or.error()));
  const mir::TypeId source_type = source_or->type;
  mir::ExprId source_id = proc_scope_state.AddExpr(*std::move(source_or));
  switch (info.source) {
    case support::ScanSourceKind::kString:
      source_id = LiftStringSource(
          unit_state, proc_scope_state, source_type, source_id);
      break;
    case support::ScanSourceKind::kFile:
      // LRM 21.3.1 binds fd to integer; slang's type-check rejects
      // non-integral arguments before HIR.
      if (unit_state.GetType(source_type).Kind() !=
          mir::TypeKind::kPackedArray) {
        throw InternalError(
            "LowerScanSystemSubroutineCall: $fscanf fd is not packed-integer");
      }
      break;
  }

  auto format_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
      hir_proc.exprs.at(call.arguments[1]->value));
  if (!format_or) return std::unexpected(std::move(format_or.error()));
  const mir::ExprId format_id = proc_scope_state.AddExpr(*std::move(format_or));

  // Output slots are restricted to bare var refs; the runtime ABI
  // (`ScanSlot::Make`) binds a whole-variable `Var<T>` / cell reference,
  // not a partial-write target.
  std::vector<mir::ExprId> slot_refs;
  slot_refs.reserve(call.arguments.size() - 2);
  for (std::size_t i = 2; i < call.arguments.size(); ++i) {
    if (!call.arguments[i].has_value()) {
      throw InternalError("LowerScanSystemSubroutineCall: output arg elided");
    }
    auto actual_or = LowerExpr(
        unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
        hir_proc.exprs.at(call.arguments[i]->value));
    if (!actual_or) return std::unexpected(std::move(actual_or.error()));
    if (!std::holds_alternative<mir::StructuralVarRef>(actual_or->data) &&
        !std::holds_alternative<mir::ProceduralVarRef>(actual_or->data)) {
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedSubroutineArgument,
          std::format(
              "{} output argument must be a bare variable reference; "
              "bit-selects, element indices, and struct fields are not yet "
              "supported (LRM 21.3.4.3)",
              info.source == support::ScanSourceKind::kFile ? "$fscanf"
                                                            : "$sscanf"),
          diag::UnsupportedCategory::kFeature);
    }
    slot_refs.push_back(proc_scope_state.AddExpr(*std::move(actual_or)));
  }

  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{
              .call =
                  mir::RuntimeScanCall{
                      .source_kind = info.source,
                      .source = source_id,
                      .format = format_id,
                      .slots = std::move(slot_refs)}},
      .type = unit_state.Builtins().integer};
}

}  // namespace lyra::lowering::hir_to_mir
