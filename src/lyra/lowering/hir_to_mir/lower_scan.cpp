#include "lyra/lowering/hir_to_mir/lower_scan.hpp"

#include <expected>
#include <format>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/copy_out_desugar.hpp"
#include "lyra/lowering/hir_to_mir/lower_expr.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/runtime_scan.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

auto RejectScanCallInExprPosition(
    const support::SystemSubroutineDesc& desc, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  return diag::Unsupported(
      span, diag::DiagCode::kUnsupportedSubroutineArgument,
      std::format(
          "{} writes through output arguments and is only supported as a "
          "bare call or as the right-hand side of a blocking assignment "
          "(LRM 13.5 copy-out semantics)",
          std::string{desc.name}),
      diag::UnsupportedCategory::kFeature);
}

auto LowerScanSystemSubroutineCallStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state, const hir::ProceduralBody& hir_proc,
    const hir::Stmt& stmt, const hir::CallExpr& call,
    const support::ScanSystemSubroutineInfo& info,
    std::optional<hir::ExprId> assign_target, mir::TypeId result_type)
    -> diag::Result<mir::Stmt> {
  if (call.arguments.size() < 3) {
    throw InternalError(
        "LowerScanSystemSubroutineCallStmt: scan family requires at least "
        "source, format, and one output argument (slang's ArgCountPolicy "
        "should have rejected fewer)");
  }

  ProceduralScopeLoweringState wrapper;
  ProceduralDepthGuard depth_guard{proc_state};

  // Scan family rejects positional elision -- all source/format/output
  // slots are required by LRM 21.3.4.3 syntax (slang enforces the count
  // policy upstream).
  if (!call.arguments[0].has_value() || !call.arguments[1].has_value()) {
    throw InternalError(
        "LowerScanSystemSubroutineCallStmt: scan source/format args "
        "unexpectedly elided");
  }
  auto source_or = LowerExpr(
      unit_state, scope_state, proc_state, wrapper, hir_proc,
      hir_proc.exprs.at(call.arguments[0]->value));
  if (!source_or) return std::unexpected(std::move(source_or.error()));
  const mir::TypeId source_type = source_or->type;
  mir::ExprId source_id = wrapper.AddExpr(*std::move(source_or));
  const mir::TypeKind source_kind = unit_state.GetType(source_type).Kind();
  switch (info.source) {
    case support::ScanSourceKind::kString: {
      // LRM 21.3.4.3 lists exactly three valid source types for $sscanf:
      // string, integral, and unpacked array of byte. The runtime entry
      // takes `value::String`; the two non-string shapes lift here via an
      // implicit ConversionExpr. Any other type is invalid SV; slang's
      // type-check is expected to reject upstream.
      if (source_kind == mir::TypeKind::kString) {
        break;
      }
      if (source_kind == mir::TypeKind::kPackedArray) {
        // LRM 5.9 + 6.16: string literals and integral expressions reach
        // lowering as packed bit-vectors whose bytes are the input chars.
        source_id = wrapper.AddExpr(
            mir::Expr{
                .data =
                    mir::ConversionExpr{
                        .operand = source_id,
                        .kind = mir::ConversionKind::kImplicit},
                .type = unit_state.Builtins().string});
        break;
      }
      if (source_kind == mir::TypeKind::kUnpackedArray) {
        const auto& ua = std::get<mir::UnpackedArrayType>(
            unit_state.GetType(source_type).data);
        const auto& elem = unit_state.GetType(ua.element_type);
        if (!elem.IsIntegralPacked() ||
            elem.AsIntegralPacked().BitWidth() != 8U) {
          throw InternalError(
              "LowerScanSystemSubroutineCallStmt: $sscanf source unpacked "
              "array must have an 8-bit integral element per LRM 21.3.4.3 "
              "'unpacked array of byte'; slang's type-check should have "
              "rejected other element shapes");
        }
        // LRM 21.3.4.3: byte-array source linearises element-order into the
        // scanner's input stream; the backend renders the conversion as a
        // `value::String::FromByteArray` call.
        source_id = wrapper.AddExpr(
            mir::Expr{
                .data =
                    mir::ConversionExpr{
                        .operand = source_id,
                        .kind = mir::ConversionKind::kImplicit},
                .type = unit_state.Builtins().string});
        break;
      }
      throw InternalError(
          "LowerScanSystemSubroutineCallStmt: $sscanf source must be string, "
          "integral, or unpacked array of byte (LRM 21.3.4.3); slang's "
          "type-check should have rejected other source types");
    }
    case support::ScanSourceKind::kFile:
      if (source_kind != mir::TypeKind::kPackedArray) {
        // LRM 21.3.1 binds fd as a 32-bit int; slang's type-check on the
        // $fscanf signature rejects non-integral arguments before HIR.
        // Any other MIR type reaching here is an invariant violation
        // upstream, not a missing feature.
        throw InternalError(
            "LowerScanSystemSubroutineCallStmt: $fscanf fd must be a "
            "packed-integer-typed expression (LRM 21.3.1); slang's "
            "type-check should have rejected non-integral arguments");
      }
      break;
  }

  auto format_or = LowerExpr(
      unit_state, scope_state, proc_state, wrapper, hir_proc,
      hir_proc.exprs.at(call.arguments[1]->value));
  if (!format_or) return std::unexpected(std::move(format_or.error()));
  const mir::ExprId format_id = wrapper.AddExpr(*std::move(format_or));

  // Output slots: args[2..]. Copy-in init so unmatched slots round-trip
  // through the unconditional writeback as no-ops (LRM 21.3.4.3 only writes
  // successfully matched outputs).
  std::vector<OutputArgSlot> slots;
  slots.reserve(call.arguments.size() - 2);
  std::vector<mir::ExprId> slot_refs;
  slot_refs.reserve(call.arguments.size() - 2);
  for (std::size_t i = 2; i < call.arguments.size(); ++i) {
    if (!call.arguments[i].has_value()) {
      throw InternalError(
          "LowerScanSystemSubroutineCallStmt: scan output arg unexpectedly "
          "elided");
    }
    const std::string temp_name = "_lyra_scan_dest_" + std::to_string(i - 2);
    auto slot_or = BuildOutputArgSlot(
        unit_state, scope_state, proc_state, wrapper, hir_proc,
        *call.arguments[i], temp_name);
    if (!slot_or) return std::unexpected(std::move(slot_or.error()));
    slots.push_back(*slot_or);
    slot_refs.push_back(wrapper.AddExpr(
        mir::Expr{.data = slots.back().temp, .type = slots.back().type}));
  }

  mir::Expr call_expr{
      .data =
          mir::RuntimeCallExpr{
              .call =
                  mir::RuntimeScanCall{
                      .source_kind = info.source,
                      .source = source_id,
                      .format = format_id,
                      .slots = std::move(slot_refs)}},
      .type = unit_state.Builtins().int32};

  std::optional<mir::ExprId> assign_target_id = std::nullopt;
  if (assign_target.has_value()) {
    auto lhs_or = LowerExpr(
        unit_state, scope_state, proc_state, wrapper, hir_proc,
        hir_proc.exprs.at(assign_target->value));
    if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
    assign_target_id = wrapper.AddExpr(*std::move(lhs_or));
  }

  return BuildCopyOutBlock(
      wrapper, stmt, result_type, std::move(call_expr), assign_target_id,
      slots);
}

}  // namespace lyra::lowering::hir_to_mir
