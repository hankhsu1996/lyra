#include "lyra/lowering/hir_to_mir/lower_scan.hpp"

#include <cstdint>
#include <expected>
#include <format>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/capture_sink.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/lower_expr.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/runtime_scan.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// LRM 21.3.4.3 valid `$sscanf` source types: string, integral (lifted via
// implicit conversion), or unpacked-array-of-byte (lifted via implicit
// conversion). Any other shape is an upstream-validation invariant
// violation -- slang's type-check rejects it before HIR.
auto LiftStringSource(
    const ModuleLowerer& module, WalkFrame frame, mir::TypeId source_type,
    mir::ExprId source_id) -> mir::ExprId {
  const mir::TypeKind kind = module.Unit().GetType(source_type).Kind();
  if (kind == mir::TypeKind::kString) return source_id;

  if (kind == mir::TypeKind::kUnpackedArray) {
    const auto& ua = std::get<mir::UnpackedArrayType>(
        module.Unit().GetType(source_type).data);
    const auto& elem = module.Unit().GetType(ua.element_type);
    if (!elem.IsIntegralPacked() || elem.AsIntegralPacked().BitWidth() != 8U) {
      throw InternalError(
          "LiftStringSource: $sscanf unpacked-array source must have an "
          "8-bit integral element (LRM 21.3.4.3)");
    }
  } else if (kind != mir::TypeKind::kPackedArray) {
    throw InternalError(
        "LiftStringSource: $sscanf source is not string, integral, or "
        "unpacked array of byte (LRM 21.3.4.3)");
  }

  return frame.current_procedural_scope->AddExpr(
      mir::Expr{
          .data =
              mir::ConversionExpr{
                  .operand = source_id, .kind = mir::ConversionKind::kImplicit},
          .type = module.Unit().builtins.string});
}

// LRM 21.3.4.3 valid scan format types: string or integral (lifted via
// implicit conversion). The byte-array form is source-only per spec.
auto LiftStringFormat(
    const ModuleLowerer& module, WalkFrame frame, mir::TypeId format_type,
    mir::ExprId format_id) -> mir::ExprId {
  const auto& t = module.Unit().GetType(format_type);
  if (t.Kind() == mir::TypeKind::kString) return format_id;
  if (!t.IsIntegralPacked()) {
    throw InternalError(
        "LiftStringFormat: scan format is not string or integral (LRM "
        "21.3.4.3)");
  }
  return frame.current_procedural_scope->AddExpr(
      mir::Expr{
          .data =
              mir::ConversionExpr{
                  .operand = format_id, .kind = mir::ConversionKind::kImplicit},
          .type = module.Unit().builtins.string});
}

// LRM 21.3.4.3: "If the format string or the str argument to $sscanf
// contains unknown bits (x or z), then the system function shall return
// EOF (-1)." Emitted unconditionally as a body-entry guard inside the
// closure-IIFE; whether the operand can actually carry x/z is the
// backend's render-time concern (the MIR primitive is universal across
// value types). Placement is BEFORE the PackedArray->String lift, since
// the lift silently drops x/z.
//
// The rule names $sscanf literally, but the str argument's role under
// $fscanf is a file descriptor (no string semantics), and the format
// argument's role is identical under both subroutines. The caller picks
// which arguments receive the guard: $sscanf str, $sscanf format,
// $fscanf format -- never the $fscanf descriptor.
auto EmitIsUnknownGuard(
    const ModuleLowerer& module, WalkFrame frame, mir::TypeId bit_t,
    mir::ExprId operand_id) -> void {
  auto& body = *frame.current_procedural_scope;
  const mir::ExprId guard_id = body.AddExpr(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::BuiltinMethodCallee{
                          .method =
                              mir::ValueMethodInfo{
                                  .kind = mir::ValueMethodKind::kIsUnknown}},
                  .arguments = {operand_id}},
          .type = bit_t});

  mir::ProceduralScope then_body;
  const mir::ExprId minus_one = then_body.AddExpr(
      mir::MakeIntegerLiteral(
          module.Unit().builtins.integer, static_cast<std::int64_t>(-1)));
  then_body.AppendStmt(mir::ReturnStmt{.value = minus_one});

  body.AppendIfThen(guard_id, std::move(then_body));
}

// The per-slot type metadata the runtime needs to materialize a fresh
// value of the output arg's declared shape. Derived from the HIR type
// alone -- no MIR Expr for the output arg is needed at this point.
struct SlotMeta {
  mir::TypeId mir_type;
  bool is_string;
  std::uint32_t bit_width;
  bool is_signed;
  bool is_four_state;
};

auto ComputeSlotMeta(
    const mir::CompilationUnit& unit, mir::TypeId mir_type,
    support::ScanSourceKind source_kind, diag::SourceSpan span)
    -> diag::Result<SlotMeta> {
  SlotMeta meta{
      .mir_type = mir_type,
      .is_string = false,
      .bit_width = 0,
      .is_signed = false,
      .is_four_state = false};
  const auto& target = unit.GetType(mir_type);
  if (target.Kind() == mir::TypeKind::kString) {
    meta.is_string = true;
    return meta;
  }
  if (target.IsIntegralPacked()) {
    const auto& pa = target.AsIntegralPacked();
    meta.bit_width = static_cast<std::uint32_t>(pa.BitWidth());
    meta.is_signed = pa.signedness == mir::Signedness::kSigned;
    meta.is_four_state = pa.IsFourState();
    return meta;
  }
  return diag::Unsupported(
      span, diag::DiagCode::kUnsupportedSubroutineArgument,
      std::format(
          "{} output argument must be an integral or string lvalue "
          "(LRM 21.3.4.3)",
          source_kind == support::ScanSourceKind::kFile ? "$fscanf"
                                                        : "$sscanf"),
      diag::UnsupportedCategory::kFeature);
}

// `body.AddExpr(ProceduralVarRef{hops, var}, type)` shorthand.
auto AppendVarRef(
    mir::ProceduralScope& scope, mir::ProceduralVarId var, std::uint32_t hops,
    mir::TypeId type) -> mir::ExprId {
  return scope.AddExpr(
      mir::Expr{
          .data =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = hops}, .var = var},
          .type = type});
}

}  // namespace

auto LowerScanSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    const support::ScanSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  if (call.arguments.size() < 3) {
    throw InternalError(
        "LowerScanSystemSubroutineCall: fewer than 3 arguments reached "
        "lowering");
  }
  if (!call.arguments[0].has_value() || !call.arguments[1].has_value()) {
    throw InternalError(
        "LowerScanSystemSubroutineCall: source / format arg elided");
  }

  const auto& module = process.Module();
  const auto& hir_proc = process.HirBody();
  auto& outer_scope = *frame.current_procedural_scope;

  // Compute slot metadata from HIR type alone -- output args are not
  // lowered until their conditional commit point inside the closure body.
  std::vector<SlotMeta> metas;
  metas.reserve(call.arguments.size() - 2);
  for (std::size_t i = 2; i < call.arguments.size(); ++i) {
    if (!call.arguments[i].has_value()) {
      throw InternalError("LowerScanSystemSubroutineCall: output arg elided");
    }
    const auto& hir_arg = hir_proc.exprs.at(call.arguments[i]->value);
    const mir::TypeId mir_type = module.TranslateType(hir_arg.type);
    auto meta_or = ComputeSlotMeta(module.Unit(), mir_type, info.source, span);
    if (!meta_or) return std::unexpected(std::move(meta_or.error()));
    metas.push_back(*std::move(meta_or));
  }

  const mir::TypeId integer_t = module.Unit().builtins.integer;
  const mir::TypeId bit_t = module.Unit().builtins.bit1;

  // Build the closure body. Entering the deeper procedural-depth frame
  // before installing the capture sink is what makes the sink's boundary
  // depth reflect the body's own depth; any leaf reference resolving above
  // it is captured by reference (sync IIFE -- aliasing is correct for both
  // reads and writes because the caller's storage is live throughout the
  // closure's evaluation).
  mir::ProceduralScope body;
  const WalkFrame body_frame = frame.WithProceduralScope(&body).Deeper();
  CaptureSink sink{body_frame.procedural_depth, body, outer_scope};
  const WalkFrame closure_frame = body_frame.WithClosure(&sink);

  // Source / format are rvalues inside the body. The leaf lowering routes
  // procedural-var leaves through the sink, producing body-side bindings.
  // For each operand the order is: raw-lower -> x/z guard (if 4-state
  // integral) -> conversion lift to string. The guard reads the raw
  // PackedArray; the lift is post-guard because conversion to string
  // silently drops x/z bits and would defeat the LRM 21.3.4.3 EOF rule.
  auto source_or = LowerExpr(
      process, closure_frame, hir_proc.exprs.at(call.arguments[0]->value));
  if (!source_or) return std::unexpected(std::move(source_or.error()));
  const mir::TypeId source_type = source_or->type;
  mir::ExprId source_id = body.AddExpr(*std::move(source_or));
  if (info.source == support::ScanSourceKind::kString) {
    EmitIsUnknownGuard(module, body_frame, bit_t, source_id);
    source_id = LiftStringSource(module, body_frame, source_type, source_id);
  } else if (
      module.Unit().GetType(source_type).Kind() !=
      mir::TypeKind::kPackedArray) {
    throw InternalError(
        "LowerScanSystemSubroutineCall: $fscanf fd is not packed-integer");
  }

  auto format_or = LowerExpr(
      process, closure_frame, hir_proc.exprs.at(call.arguments[1]->value));
  if (!format_or) return std::unexpected(std::move(format_or.error()));
  const mir::TypeId format_type = format_or->type;
  mir::ExprId format_id = body.AddExpr(*std::move(format_or));
  EmitIsUnknownGuard(module, body_frame, bit_t, format_id);
  format_id = LiftStringFormat(module, body_frame, format_type, format_id);

  // Allocate body-side temps for each slot. The parse call writes them;
  // the conditional commit later reads them back into the original
  // lvalue. Default-init is a syntactic requirement for procedural locals;
  // the value is never observed because parse runs before commit.
  std::vector<mir::ProceduralVarId> temp_ids;
  temp_ids.reserve(metas.size());
  for (std::size_t k = 0; k < metas.size(); ++k) {
    const mir::ExprId init_id =
        SynthesizeDefaultValueExpr(module, closure_frame, metas[k].mir_type);
    const mir::ProceduralVarRef temp_ref = body.AppendLocal(
        mir::ProceduralVarDecl{
            .name = std::format("_lyra_scan_temp_{}", k),
            .type = metas[k].mir_type},
        init_id);
    temp_ids.push_back(temp_ref.var);
  }

  // RuntimeScanCall: each slot points at its body-local temp through a
  // ProceduralVarRef, carrying the parser's per-slot metadata.
  std::vector<mir::ScanSlotDesc> mir_slots;
  mir_slots.reserve(metas.size());
  for (std::size_t k = 0; k < metas.size(); ++k) {
    const mir::ExprId temp_ref_id =
        AppendVarRef(body, temp_ids[k], 0, metas[k].mir_type);
    if (metas[k].is_string) {
      mir_slots.emplace_back(mir::StringScanSlot{.temp = temp_ref_id});
    } else {
      mir_slots.emplace_back(
          mir::IntegralScanSlot{
              .temp = temp_ref_id,
              .bit_width = metas[k].bit_width,
              .is_signed = metas[k].is_signed,
              .is_four_state = metas[k].is_four_state});
    }
  }

  const mir::ExprId parse_call_id = body.AddExpr(
      mir::Expr{
          .data =
              mir::RuntimeCallExpr{
                  .call =
                      mir::RuntimeScanCall{
                          .source_kind = info.source,
                          .source = source_id,
                          .format = format_id,
                          .slots = std::move(mir_slots)}},
          .type = integer_t});

  const mir::ProceduralVarRef count_ref = body.AppendLocal(
      mir::ProceduralVarDecl{.name = "_lyra_scan_count", .type = integer_t},
      parse_call_id);

  // Conditional commits: for each output arg k, lower the lvalue HIR
  // freshly inside the then-scope. The leaf lowering path routes any
  // procedural-var leaf above the sink's boundary through the sink, which
  // installs a by-reference binding in the closure body so writes to the
  // rebased lvalue propagate to the caller's storage.
  for (std::size_t k = 0; k < metas.size(); ++k) {
    const mir::ExprId count_read_id =
        body.AddExpr(mir::Expr{.data = count_ref, .type = integer_t});
    const mir::ExprId k_lit_id = body.AddExpr(
        mir::MakeIntegerLiteral(
            module.Unit().builtins.integer, static_cast<std::int64_t>(k + 1)));
    const mir::ExprId cond_id = body.AddExpr(
        mir::Expr{
            .data =
                mir::BinaryExpr{
                    .op = mir::BinaryOp::kGreaterEqual,
                    .lhs = count_read_id,
                    .rhs = k_lit_id},
            .type = bit_t});

    mir::ProceduralScope then_body;
    const WalkFrame then_frame =
        closure_frame.WithProceduralScope(&then_body).Deeper();
    auto lvalue_or = LowerExpr(
        process, then_frame, hir_proc.exprs.at(call.arguments[k + 2]->value));
    if (!lvalue_or) return std::unexpected(std::move(lvalue_or.error()));
    const mir::ExprId lvalue_id = then_body.AddExpr(*std::move(lvalue_or));
    const mir::ExprId temp_read_id =
        AppendVarRef(then_body, temp_ids[k], 1, metas[k].mir_type);
    const mir::ExprId assign_id = then_body.AddExpr(
        mir::Expr{
            .data = mir::AssignExpr{.target = lvalue_id, .value = temp_read_id},
            .type = metas[k].mir_type});
    then_body.AppendStmt(mir::ExprStmt{.expr = assign_id});

    body.AppendIfThen(cond_id, std::move(then_body));
  }

  // return count_local -- the closure's yield value.
  const mir::ExprId return_value_id =
      body.AddExpr(mir::Expr{.data = count_ref, .type = integer_t});
  body.AppendStmt(mir::ReturnStmt{.value = return_value_id});

  // The sync IIFE aliases the caller's storage, which is live throughout the
  // closure's evaluation -- so every captured identity is a by-reference
  // capture.
  std::vector<mir::Capture> captures;
  for (const CaptureRequest& request : sink.TakeRequests()) {
    captures.emplace_back(
        mir::ByReferenceCapture{
            .target = request.source, .binding = request.binding});
  }
  mir::ClosureExpr closure;
  closure.captures = std::move(captures);
  closure.body = std::make_unique<mir::ProceduralScope>(std::move(body));

  const mir::ExprId closure_id = outer_scope.AddExpr(
      mir::Expr{.data = std::move(closure), .type = integer_t});

  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::ClosureRef{.closure = closure_id},
              .arguments = {}},
      .type = integer_t};
}

}  // namespace lyra::lowering::hir_to_mir
