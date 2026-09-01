#include "lyra/lowering/hir_to_mir/expression/system/scan.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/call_operands.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/lhs_store.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// A scan completes with the matched-conversion count, how far the parse
// advanced, then one value per conversion in the order the call named them
// (LRM 21.3.4.3). The positions and the type the call carries are stated
// together here, so a step reading a component and the type declaring it
// cannot come to disagree.
constexpr base::ComponentIndex kScanMatched{0};
constexpr base::ComponentIndex kScanConsumed{1};

auto ScanParsedValue(std::size_t conversion) -> base::ComponentIndex {
  return base::ComponentIndex{
      kScanConsumed.value + 1 + static_cast<std::uint32_t>(conversion)};
}

auto ScanCompletionComponents(
    mir::TypeId matched, mir::TypeId consumed,
    const std::vector<mir::TypeId>& parsed) -> std::vector<mir::TypeId> {
  std::vector<mir::TypeId> components;
  components.reserve(parsed.size() + 2);
  components.push_back(matched);
  components.push_back(consumed);
  components.insert(components.end(), parsed.begin(), parsed.end());
  return components;
}

// LRM 21.3.4.3 permits string, integral, or unpacked-array-of-byte as
// the `$sscanf` source; the latter two are lifted to string here so the
// parser only has to handle one shape.
auto LiftStringSource(
    const UnitLowerer& unit_lowerer, WalkFrame frame, mir::TypeId source_type,
    mir::ExprId source_id) -> mir::ExprId {
  const mir::TypeKind kind = unit_lowerer.Unit().types.Get(source_type).Kind();
  if (kind == mir::TypeKind::kString) return source_id;

  if (kind == mir::TypeKind::kUnpackedArray) {
    const auto& ua = std::get<mir::UnpackedArrayType>(
        unit_lowerer.Unit().types.Get(source_type).data);
    const auto& elem = unit_lowerer.Unit().types.Get(ua.element_type);
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
      unit_lowerer.Unit(), *frame.current_block, source_id,
      unit_lowerer.Unit().builtins.string));
}

// LRM 21.3.4.3 permits string or integral as the format argument; the
// integral form is lifted to string.
auto LiftStringFormat(
    const UnitLowerer& unit_lowerer, WalkFrame frame, mir::TypeId format_type,
    mir::ExprId format_id) -> mir::ExprId {
  const auto& t = unit_lowerer.Unit().types.Get(format_type);
  if (t.Kind() == mir::TypeKind::kString) return format_id;
  if (!t.IsIntegralPacked()) {
    throw InternalError(
        "LiftStringFormat: scan format is not string or integral (LRM "
        "21.3.4.3)");
  }
  return frame.current_block->exprs.Add(BuildValueConversion(
      unit_lowerer.Unit(), *frame.current_block, format_id,
      unit_lowerer.Unit().builtins.string));
}

// LRM 21.3.4.3: a source or format that contains x or z makes the call answer
// -1 and convert nothing. The test is on the operand as written, since the lift
// to string silently drops the unknown bits. A format is always an operand the
// rule covers; a source is one only where the call names it, which `$fscanf`
// does not -- its source is the file's buffered text.
auto EmitScanOperandsKnown(
    mir::Block& body, mir::TypeId bit_t, std::optional<mir::ExprId> source,
    mir::ExprId format) -> mir::ExprId {
  const auto known = [&](mir::ExprId operand) {
    const mir::ExprId unknown_id = body.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{.target = support::BuiltinFn::kIsUnknown},
                    .arguments = {operand}},
            .type = bit_t});
    return body.exprs.Add(
        mir::Expr{
            .data =
                mir::UnaryExpr{
                    .op = mir::UnaryOp::kLogicalNot, .operand = unknown_id},
            .type = bit_t});
  };
  if (!source.has_value()) {
    return known(format);
  }
  const mir::ExprId source_known = known(*source);
  return body.exprs.Add(
      mir::Expr{
          .data =
              mir::BinaryExpr{
                  .op = mir::BinaryOp::kLogicalAnd,
                  .lhs = source_known,
                  .rhs = known(format)},
          .type = bit_t});
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
  // $fscanf(fd, format, target...) / $sscanf(str, format, target...) --
  // LRM 21.3.4.2. Source and format, then one target per conversion.
  const std::vector<hir::ExprId> operands = RequiredOperands(call);
  if (operands.size() < 3) {
    throw InternalError(
        "LowerScanSystemSubroutineCall: fewer than 3 arguments reached "
        "lowering");
  }

  const auto& hir_proc = process.HirBody();
  auto& unit_lowerer = process.Owner();
  auto& unit = unit_lowerer.Unit();
  const mir::TypeId integer_t = unit.builtins.integer;
  const mir::TypeId int_type = unit.builtins.int_type;
  const mir::TypeId string_t = unit.builtins.string;
  const mir::TypeId bit_t = unit.builtins.bit1;
  const mir::TypeId void_t = unit.builtins.void_type;
  const bool is_file = info.source == support::ScanSourceKind::kFile;

  std::vector<mir::TypeId> target_types;
  target_types.reserve(operands.size() - 2);
  for (std::size_t i = 2; i < operands.size(); ++i) {
    const auto& hir_arg = hir_proc.exprs.Get(operands[i]);
    const mir::TypeId mir_type = unit_lowerer.TranslateType(hir_arg.type);
    auto valid_or = ValidateTargetType(unit, mir_type, info.source, span);
    if (!valid_or) return std::unexpected(std::move(valid_or.error()));
    target_types.push_back(mir_type);
  }

  // LRM 21.3.4.3 returns a matched-conversion count and writes the parsed
  // values to the call's output lvalues; both effects are steps of one block
  // expression, so the call sits in expression position.
  BlockBuilder steps(frame);
  mir::Block& body = steps.Body();
  const WalkFrame& step_frame = steps.Frame();

  // Each operand the source wrote is evaluated once, in the order it was
  // written, and bound -- so the unknown-operand rule and the conversion behind
  // it both read the value that was evaluated rather than evaluating again.
  auto raw_source_or =
      process.LowerExpr(hir_proc.exprs.Get(operands[0]), step_frame);
  if (!raw_source_or) {
    return std::unexpected(std::move(raw_source_or.error()));
  }
  const mir::TypeId raw_source_type = raw_source_or->type;
  const mir::LocalId source_var = steps.Bindings().DeclareAnonymous(
      mir::LocalDecl{.name = "_lyra_scan_source", .type = raw_source_type});
  body.AppendStmt(
      mir::LocalDeclStmt{
          .target = source_var,
          .init = body.exprs.Add(*std::move(raw_source_or))});

  auto format_or =
      process.LowerExpr(hir_proc.exprs.Get(operands[1]), step_frame);
  if (!format_or) return std::unexpected(std::move(format_or.error()));
  const mir::TypeId format_type = format_or->type;
  const mir::LocalId format_var = steps.Bindings().DeclareAnonymous(
      mir::LocalDecl{.name = "_lyra_scan_format", .type = format_type});
  body.AppendStmt(
      mir::LocalDeclStmt{
          .target = format_var, .init = body.exprs.Add(*std::move(format_or))});

  // The answer until a conversion settles it (LRM 21.3.4.3).
  const mir::LocalId count_var = steps.Bindings().DeclareAnonymous(
      mir::LocalDecl{.name = "_lyra_scan_count", .type = integer_t});
  body.AppendStmt(
      mir::LocalDeclStmt{
          .target = count_var,
          .init =
              BuildIntegerLiteral(unit, body, static_cast<std::int64_t>(-1))});

  const std::optional<mir::ExprId> rule_source =
      is_file ? std::nullopt
              : std::optional{body.exprs.Add(
                    mir::MakeLocalRefExpr(source_var, raw_source_type))};
  const mir::ExprId known_id = EmitScanOperandsKnown(
      body, bit_t, rule_source,
      body.exprs.Add(mir::MakeLocalRefExpr(format_var, format_type)));

  mir::Block scan_body;
  const WalkFrame scan_frame = step_frame.WithBlock(&scan_body);

  mir::ExprId source_id{};
  mir::ExprId fd_id{};
  if (is_file) {
    if (unit.types.Get(raw_source_type).Kind() != mir::TypeKind::kPackedArray) {
      throw InternalError(
          "LowerScanSystemSubroutineCall: $fscanf fd is not packed-integer");
    }
    fd_id =
        scan_body.exprs.Add(mir::MakeLocalRefExpr(source_var, raw_source_type));
    const mir::ExprId runtime_id =
        scan_body.exprs.Add(BuildCurrentRuntimeCallExpr(process.Owner()));
    const mir::ExprId files_id = scan_body.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee = mir::Direct{.target = support::BuiltinFn::kFiles},
                    .arguments = {runtime_id}},
            .type = unit.builtins.files});
    source_id = scan_body.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target = support::BuiltinFn::kPeekBuffered},
                    .arguments = {files_id, fd_id}},
            .type = string_t});
  } else {
    source_id = LiftStringSource(
        unit_lowerer, scan_frame, raw_source_type,
        scan_body.exprs.Add(
            mir::MakeLocalRefExpr(source_var, raw_source_type)));
  }

  const mir::ExprId format_id = LiftStringFormat(
      unit_lowerer, scan_frame, format_type,
      scan_body.exprs.Add(mir::MakeLocalRefExpr(format_var, format_type)));

  // One prototype per conversion. It states the shape that conversion parses
  // into, which nothing else on the call states, and it is what the completion
  // carries back where no conversion reached it.
  std::vector<mir::ExprId> prototypes;
  prototypes.reserve(target_types.size());
  for (const mir::TypeId target_type : target_types) {
    prototypes.push_back(scan_body.exprs.Add(
        BuildDefaultValueExpr(unit_lowerer, scan_frame, target_type)));
  }
  const mir::ExprId prototypes_id = scan_body.exprs.Add(
      mir::Expr{
          .data = mir::TupleExpr{.components = std::move(prototypes)},
          .type = unit.types.Intern(mir::TupleType{.elements = target_types})});

  // LRM 21.3.4.3(a) gives `$sscanf` alone the rule that a null character
  // counts as white space, so which of the two parses runs is fixed by the
  // system function the source names, not by the bytes it receives.
  const support::BuiltinFn parse_fn =
      is_file ? support::BuiltinFn::kScanFile : support::BuiltinFn::kScanString;
  const mir::TypeId payload_type = CompletionPayloadType(
      unit, ScanCompletionComponents(integer_t, int_type, target_types));
  const mir::ExprId parse_call_id = scan_body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = parse_fn},
                  .arguments = {source_id, format_id, prototypes_id}},
          .type = payload_type});
  const mir::LocalId completion = steps.Bindings().DeclareAnonymous(
      mir::LocalDecl{.name = "_lyra_scan", .type = payload_type});
  scan_body.AppendStmt(
      mir::LocalDeclStmt{.target = completion, .init = parse_call_id});

  const mir::ExprId count_target =
      scan_body.exprs.Add(mir::MakeLocalRefExpr(count_var, integer_t));
  scan_body.AppendStmt(
      mir::ExprStmt{
          .expr = scan_body.exprs.Add(
              mir::Expr{
                  .data =
                      mir::AssignExpr{
                          .target = count_target,
                          .value = ProjectCompletionComponent(
                              scan_body, completion, payload_type, kScanMatched,
                              integer_t)},
                  .type = integer_t})});

  if (is_file) {
    const mir::ExprId runtime_after =
        scan_body.exprs.Add(BuildCurrentRuntimeCallExpr(process.Owner()));
    const mir::ExprId files_after = scan_body.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee = mir::Direct{.target = support::BuiltinFn::kFiles},
                    .arguments = {runtime_after}},
            .type = unit.builtins.files});
    // LRM 21.3.4.3 "the offending input character is left unread in the input
    // stream": how far the parse advanced is what lets the file form rewind
    // the unconsumed tail before the next read.
    const mir::ExprId consumed_read = ProjectCompletionComponent(
        scan_body, completion, payload_type, kScanConsumed, int_type);
    const mir::ExprId advance_call = scan_body.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{.target = support::BuiltinFn::kAdvanceFd},
                    .arguments = {files_after, fd_id, consumed_read}},
            .type = void_t});
    scan_body.AppendStmt(mir::ExprStmt{.expr = advance_call});
  }

  // LRM 21.3.4.3: the k-th output lvalue is only written when at least
  // k+1 matches were made, so the commit is gated on the matched count.
  for (std::size_t k = 0; k < target_types.size(); ++k) {
    const mir::ExprId count_read_id =
        scan_body.exprs.Add(mir::MakeLocalRefExpr(count_var, integer_t));
    const mir::ExprId k_lit_id =
        BuildIntegerLiteral(unit, scan_body, static_cast<std::int64_t>(k + 1));
    const mir::ExprId cond_id = scan_body.exprs.Add(
        mir::Expr{
            .data =
                mir::BinaryExpr{
                    .op = mir::BinaryOp::kGreaterEqual,
                    .lhs = count_read_id,
                    .rhs = k_lit_id},
            .type = bit_t});

    mir::Block then_body;
    const WalkFrame then_frame = scan_frame.WithBlock(&then_body);
    auto lvalue_or =
        process.LowerLhsExpr(hir_proc.exprs.Get(operands[k + 2]), then_frame);
    if (!lvalue_or) return std::unexpected(std::move(lvalue_or.error()));
    const mir::ExprId lvalue_id = then_body.exprs.Add(*std::move(lvalue_or));
    const mir::ExprId parsed_id = ProjectCompletionComponent(
        then_body, completion, payload_type, ScanParsedValue(k),
        target_types[k]);
    const mir::Expr assign_expr = BuildStoreExpr(
        unit, then_body, lvalue_id, parsed_id, std::nullopt, target_types[k]);
    const mir::ExprId assign_id = then_body.exprs.Add(assign_expr);
    then_body.AppendStmt(mir::ExprStmt{.expr = assign_id});

    scan_body.AppendIfThen(
        ReduceToCondition(unit, scan_body, cond_id), std::move(then_body));
  }

  body.AppendIfThen(
      ReduceToCondition(unit, body, known_id), std::move(scan_body));

  const mir::ExprId count_id =
      body.exprs.Add(mir::MakeLocalRefExpr(count_var, integer_t));
  return steps.Build(count_id);
}

}  // namespace lyra::lowering::hir_to_mir
