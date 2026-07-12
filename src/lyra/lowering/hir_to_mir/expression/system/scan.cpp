#include "lyra/lowering/hir_to_mir/expression/system/scan.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <string>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/lhs_observable.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// LRM 21.3.4.3 permits string, integral, or unpacked-array-of-byte as
// the `$sscanf` source; the latter two are lifted to string here so the
// parser only has to handle one shape.
auto LiftStringSource(
    const ModuleLowerer& module, WalkFrame frame, mir::TypeId source_type,
    mir::ExprId source_id) -> mir::ExprId {
  const mir::TypeKind kind = module.Unit().types.Get(source_type).Kind();
  if (kind == mir::TypeKind::kString) return source_id;

  if (kind == mir::TypeKind::kUnpackedArray) {
    const auto& ua = std::get<mir::UnpackedArrayType>(
        module.Unit().types.Get(source_type).data);
    const auto& elem = module.Unit().types.Get(ua.element_type);
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

  return frame.current_block->exprs.Add(BuildValueConversion(
      module.Unit(), *frame.current_block, source_id,
      module.Unit().builtins.string));
}

// LRM 21.3.4.3 permits string or integral as the format argument; the
// integral form is lifted to string.
auto LiftStringFormat(
    const ModuleLowerer& module, WalkFrame frame, mir::TypeId format_type,
    mir::ExprId format_id) -> mir::ExprId {
  const auto& t = module.Unit().types.Get(format_type);
  if (t.Kind() == mir::TypeKind::kString) return format_id;
  if (!t.IsIntegralPacked()) {
    throw InternalError(
        "LiftStringFormat: scan format is not string or integral (LRM "
        "21.3.4.3)");
  }
  return frame.current_block->exprs.Add(BuildValueConversion(
      module.Unit(), *frame.current_block, format_id,
      module.Unit().builtins.string));
}

// LRM 21.3.4.3: a source or format string that contains x or z makes
// the call return -1 before any conversion. Emit the guard before the
// lift to string, because the lift silently drops the unknown bits.
auto EmitIsUnknownGuard(
    const ModuleLowerer& module, WalkFrame frame, mir::TypeId bit_t,
    mir::ExprId operand_id) -> void {
  auto& body = *frame.current_block;
  const mir::ExprId guard_id = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kIsUnknown},
                  .arguments = {operand_id}},
          .type = bit_t});

  mir::Block then_body;
  const mir::ExprId minus_one = then_body.exprs.Add(
      mir::MakeIntegerLiteral(
          module.Unit().builtins.integer, static_cast<std::int64_t>(-1)));
  then_body.AppendStmt(mir::ReturnStmt{.value = minus_one});

  body.AppendIfThen(
      ReduceToCondition(body, guard_id, bit_t), std::move(then_body));
}

auto ValidateTargetType(
    const mir::CompilationUnit& unit, mir::TypeId mir_type,
    support::ScanSourceKind source_kind, diag::SourceSpan span)
    -> diag::Result<void> {
  const auto& target = unit.types.Get(mir_type);
  if (target.Kind() == mir::TypeKind::kString) return {};
  if (target.IsIntegralPacked()) return {};
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedSubroutineArgument,
      std::format(
          "{} output argument must be an integral or string lvalue "
          "(LRM 21.3.4.3)",
          source_kind == support::ScanSourceKind::kFile ? "$fscanf"
                                                        : "$sscanf"));
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

  const auto& hir_proc = process.HirBody();
  auto& module = process.Module();
  auto& unit = module.Unit();
  const mir::TypeId integer_t = unit.builtins.integer;
  const mir::TypeId int32_t_id = unit.builtins.int32;
  const mir::TypeId string_t = unit.builtins.string;
  const mir::TypeId bit_t = unit.builtins.bit1;
  const mir::TypeId void_t = unit.builtins.void_type;
  const bool is_file = info.source == support::ScanSourceKind::kFile;

  std::vector<mir::TypeId> target_types;
  target_types.reserve(call.arguments.size() - 2);
  for (std::size_t i = 2; i < call.arguments.size(); ++i) {
    if (!call.arguments[i].has_value()) {
      throw InternalError("LowerScanSystemSubroutineCall: output arg elided");
    }
    const auto& hir_arg = hir_proc.exprs.Get(*call.arguments[i]);
    const mir::TypeId mir_type = module.TranslateType(hir_arg.type);
    auto valid_or = ValidateTargetType(unit, mir_type, info.source, span);
    if (!valid_or) return std::unexpected(std::move(valid_or.error()));
    target_types.push_back(mir_type);
  }

  // LRM 21.3.4.3 returns a matched-conversion count and writes the
  // parsed values to the call's output lvalues; the lowering encloses
  // both effects in a synchronous IIFE so the call sits in expression
  // position.
  ClosureBuilder closure(unit, frame);
  mir::Block& body = closure.Body();
  const WalkFrame& closure_frame = closure.Frame();

  auto raw_source_or =
      process.LowerExpr(hir_proc.exprs.Get(*call.arguments[0]), closure_frame);
  if (!raw_source_or) {
    return std::unexpected(std::move(raw_source_or.error()));
  }
  const mir::TypeId raw_source_type = raw_source_or->type;
  const mir::ExprId raw_source_id = body.exprs.Add(*std::move(raw_source_or));

  mir::ExprId source_id{};
  mir::ExprId fd_id{};
  if (is_file) {
    if (unit.types.Get(raw_source_type).Kind() != mir::TypeKind::kPackedArray) {
      throw InternalError(
          "LowerScanSystemSubroutineCall: $fscanf fd is not packed-integer");
    }
    fd_id = raw_source_id;
    const mir::ExprId services_id =
        body.exprs.Add(BuildServicesCallExpr(process.Module(), closure_frame));
    const mir::ExprId files_id = body.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee = mir::Direct{.target = support::BuiltinFn::kFiles},
                    .arguments = {services_id}},
            .type = unit.builtins.files});
    source_id = body.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target = support::BuiltinFn::kPeekBuffered},
                    .arguments = {files_id, fd_id}},
            .type = string_t});
  } else {
    EmitIsUnknownGuard(module, closure_frame, bit_t, raw_source_id);
    source_id =
        LiftStringSource(module, closure_frame, raw_source_type, raw_source_id);
  }

  auto format_or =
      process.LowerExpr(hir_proc.exprs.Get(*call.arguments[1]), closure_frame);
  if (!format_or) return std::unexpected(std::move(format_or.error()));
  const mir::TypeId format_type = format_or->type;
  mir::ExprId format_id = body.exprs.Add(*std::move(format_or));
  EmitIsUnknownGuard(module, closure_frame, bit_t, format_id);
  format_id = LiftStringFormat(module, closure_frame, format_type, format_id);

  // LRM 21.3.4.3 "the offending input character is left unread in the
  // input stream": the parse returns the byte-count it consumed so the
  // file form can rewind the unconsumed tail before the next read.
  const mir::ExprId consumed_init =
      body.exprs.Add(mir::MakeInt32Literal(int32_t_id, 0));
  const mir::LocalId consumed_var = closure.Bindings().DeclareAnonymous(
      mir::LocalDecl{.name = "_lyra_scan_consumed", .type = int32_t_id});
  body.AppendStmt(
      mir::LocalDeclStmt{.target = consumed_var, .init = consumed_init});

  std::vector<mir::LocalId> temp_ids;
  temp_ids.reserve(target_types.size());
  for (std::size_t k = 0; k < target_types.size(); ++k) {
    const mir::ExprId init_id = body.exprs.Add(
        BuildDefaultValueExpr(module, closure_frame, target_types[k]));
    const mir::LocalId temp_var = closure.Bindings().DeclareAnonymous(
        mir::LocalDecl{
            .name = std::format("_lyra_scan_temp_{}", k),
            .type = target_types[k]});
    body.AppendStmt(mir::LocalDeclStmt{.target = temp_var, .init = init_id});
    temp_ids.push_back(temp_var);
  }

  std::vector<mir::ExprId> scan_args;
  scan_args.reserve(3 + target_types.size());
  scan_args.push_back(source_id);
  scan_args.push_back(format_id);
  scan_args.push_back(
      body.exprs.Add(mir::MakeLocalRefExpr(consumed_var, int32_t_id)));
  for (std::size_t k = 0; k < target_types.size(); ++k) {
    scan_args.push_back(
        body.exprs.Add(mir::MakeLocalRefExpr(temp_ids[k], target_types[k])));
  }
  const mir::ExprId parse_call_id = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = support::BuiltinFn::kScan},
                  .arguments = std::move(scan_args)},
          .type = integer_t});

  const mir::LocalId count_var = closure.Bindings().DeclareAnonymous(
      mir::LocalDecl{.name = "_lyra_scan_count", .type = integer_t});
  body.AppendStmt(
      mir::LocalDeclStmt{.target = count_var, .init = parse_call_id});

  if (is_file) {
    const mir::ExprId services_after =
        body.exprs.Add(BuildServicesCallExpr(process.Module(), closure_frame));
    const mir::ExprId files_after = body.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee = mir::Direct{.target = support::BuiltinFn::kFiles},
                    .arguments = {services_after}},
            .type = unit.builtins.files});
    const mir::ExprId consumed_read =
        body.exprs.Add(mir::MakeLocalRefExpr(consumed_var, int32_t_id));
    const mir::ExprId advance_call = body.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{.target = support::BuiltinFn::kAdvanceFd},
                    .arguments = {files_after, fd_id, consumed_read}},
            .type = void_t});
    body.AppendStmt(mir::ExprStmt{.expr = advance_call});
  }

  // LRM 21.3.4.3: the k-th output lvalue is only written when at least
  // k+1 matches were made, so the commit is gated on the matched count.
  for (std::size_t k = 0; k < target_types.size(); ++k) {
    const mir::ExprId count_read_id =
        body.exprs.Add(mir::MakeLocalRefExpr(count_var, integer_t));
    const mir::ExprId k_lit_id = body.exprs.Add(
        mir::MakeIntegerLiteral(integer_t, static_cast<std::int64_t>(k + 1)));
    const mir::ExprId cond_id = body.exprs.Add(
        mir::Expr{
            .data =
                mir::BinaryExpr{
                    .op = mir::BinaryOp::kGreaterEqual,
                    .lhs = count_read_id,
                    .rhs = k_lit_id},
            .type = bit_t});

    mir::Block then_body;
    const WalkFrame then_frame = closure_frame.WithBlock(&then_body);
    auto lvalue_or = process.LowerLhsExpr(
        hir_proc.exprs.Get(*call.arguments[k + 2]), then_frame);
    if (!lvalue_or) return std::unexpected(std::move(lvalue_or.error()));
    const mir::ExprId lvalue_id = then_body.exprs.Add(*std::move(lvalue_or));
    const mir::ExprId temp_read_id = then_body.exprs.Add(
        mir::MakeLocalRefExpr(temp_ids[k], target_types[k]));
    const mir::ExprId services_id_then = then_body.exprs.Add(
        BuildServicesCallExpr(process.Module(), then_frame));
    const mir::Expr assign_expr = BuildObservableAssignExpr(
        unit, then_body, services_id_then, lvalue_id, temp_read_id,
        std::nullopt, target_types[k], void_t);
    const mir::ExprId assign_id = then_body.exprs.Add(assign_expr);
    then_body.AppendStmt(mir::ExprStmt{.expr = assign_id});

    body.AppendIfThen(
        ReduceToCondition(body, cond_id, unit.builtins.bit1),
        std::move(then_body));
  }

  const mir::ExprId count_id =
      body.exprs.Add(mir::MakeLocalRefExpr(count_var, integer_t));
  return BuildClosureCallExpr(
      unit, *frame.current_block, closure.Build(count_id));
}

}  // namespace lyra::lowering::hir_to_mir
