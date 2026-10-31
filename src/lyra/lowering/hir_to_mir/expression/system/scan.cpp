#include "lyra/lowering/hir_to_mir/expression/system/scan.hpp"

#include <array>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <span>
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
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/lvalue.hpp"
#include "lyra/lowering/hir_to_mir/predicate.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/value_operations.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// A scan completes with the matched-conversion count, then one value per
// conversion in the order the call named them (LRM 21.3.4.3). The positions
// and the type the call carries are stated together here, so a step reading a
// component and the type declaring it cannot come to disagree.
constexpr base::ComponentIndex kScanMatched{0};

auto ScanParsedValue(std::size_t conversion) -> base::ComponentIndex {
  return base::ComponentIndex{
      kScanMatched.value + 1 + static_cast<std::uint32_t>(conversion)};
}

auto ScanCompletionComponents(
    mir::TypeId matched, const std::vector<mir::TypeId>& parsed)
    -> std::vector<mir::TypeId> {
  std::vector<mir::TypeId> components;
  components.reserve(parsed.size() + 1);
  components.push_back(matched);
  components.insert(components.end(), parsed.begin(), parsed.end());
  return components;
}

// LRM 21.3.4.3 permits string, integral, or unpacked-array-of-byte as
// the `$sscanf` source; the latter two are lifted to string here so the
// parser only has to handle one shape.
auto LiftStringSource(
    const UnitLowerer& unit_lowerer, WalkFrame frame, mir::TypeId source_type,
    mir::ExprId source_id) -> mir::ExprId {
  const mir::Type& source = unit_lowerer.Unit().types.Get(source_type);
  if (source.Is<mir::StringType>()) return source_id;

  if (source.Is<mir::UnpackedArrayType>()) {
    const auto& ua = unit_lowerer.Unit()
                         .types.Get(source_type)
                         .Get<mir::UnpackedArrayType>();
    const auto& elem = unit_lowerer.Unit().types.Get(ua.element_type);
    if (!elem.IsIntegral() || elem.Integral().bit_width != 8U) {
      throw InternalError(
          "LiftStringSource: an unpacked-array $sscanf source reached "
          "lowering whose element is not 8-bit integral");
    }
  } else if (!source.IsIntegral()) {
    throw InternalError(
        "LiftStringSource: a $sscanf source reached lowering that is not "
        "string, integral, or an unpacked array of byte");
  }

  return frame.Builder().Add(BuildValueConversion(
      unit_lowerer.Unit(), frame.Builder(), source_id,
      unit_lowerer.Unit().builtins.string));
}

// LRM 21.3.4.3 permits string or integral as the format argument; the
// integral form is lifted to string.
auto LiftStringFormat(
    const UnitLowerer& unit_lowerer, WalkFrame frame, mir::TypeId format_type,
    mir::ExprId format_id) -> mir::ExprId {
  const auto& t = unit_lowerer.Unit().types.Get(format_type);
  if (t.Is<mir::StringType>()) return format_id;
  if (!t.IsIntegral()) {
    throw InternalError(
        "LiftStringFormat: a scan format reached lowering that is not string "
        "or integral");
  }
  return frame.Builder().Add(BuildValueConversion(
      unit_lowerer.Unit(), frame.Builder(), format_id,
      unit_lowerer.Unit().builtins.string));
}

// LRM 21.3.4.3: a source or format that contains x or z makes the call answer
// -1 and convert nothing. The test is on the operand as written, since the lift
// to string silently drops the unknown bits. An operand whose type holds no
// unknown bit, a `string` among them, is known by that type and is asked
// nothing.
auto EmitScanOperandsKnown(
    const mir::CompilationUnit& unit, mir::Builder body, mir::TypeId bit_t,
    std::span<const mir::ExprId> operands) -> mir::ExprId {
  std::vector<mir::ExprId> known;
  for (const mir::ExprId operand : operands) {
    if (!CarriesUnknowns(unit, body.Get(operand).type)) continue;
    const mir::ExprId unknown_id =
        ConvertToType(unit, body, BuildUnknownTest(unit, body, operand), bit_t);
    known.push_back(BuildLogicalNot(unit, body, unknown_id));
  }
  return BuildMirLogicalAnd(unit, body, bit_t, known);
}

auto ValidateTargetType(
    const mir::CompilationUnit& unit, mir::TypeId mir_type,
    support::ScanSourceKind source_kind, diag::SourceSpan span)
    -> diag::Result<void> {
  const auto& target = unit.types.Get(mir_type);
  if (target.Is<mir::StringType>()) return {};
  if (target.IsIntegral()) return {};
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedSubroutineArgument,
      "a '{}' output argument other than an integral or string variable is "
      "not yet supported",
      source_kind == support::ScanSourceKind::kFile ? "$fscanf" : "$sscanf");
}

}  // namespace

auto LowerScanSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    const support::ScanSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  // $fscanf(fd, format, target...) / $sscanf(str, format, target...) --
  // LRM 21.3.4.3. Source and format, then one target per conversion.
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
  const mir::TypeId bit_t = unit.builtins.bit1;
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
  mir::Builder body = steps.Body();
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
  const mir::LocalId source_var =
      steps.Bindings().DeclareAnonymous(raw_source_type);
  body.AppendStmt(
      mir::LocalDeclStmt{
          .target = source_var, .init = body.Add(*std::move(raw_source_or))});

  auto format_or =
      process.LowerExpr(hir_proc.exprs.Get(operands[1]), step_frame);
  if (!format_or) return std::unexpected(std::move(format_or.error()));
  const mir::TypeId format_type = format_or->type;
  const mir::LocalId format_var =
      steps.Bindings().DeclareAnonymous(format_type);
  body.AppendStmt(
      mir::LocalDeclStmt{
          .target = format_var, .init = body.Add(*std::move(format_or))});

  // The answer until a conversion settles it (LRM 21.3.4.3).
  const mir::LocalId count_var = steps.Bindings().DeclareAnonymous(integer_t);
  body.AppendStmt(
      mir::LocalDeclStmt{
          .target = count_var,
          .init = mir::BuildIntegerLiteral(
              unit, body, static_cast<std::int64_t>(-1))});

  // The format is always an operand the rule covers. The source is one only
  // where the call names it, which `$fscanf` does not: its source is the file.
  std::vector<mir::ExprId> rule_operands;
  if (!is_file) {
    rule_operands.push_back(body.ReadLocal(source_var, raw_source_type));
  }
  rule_operands.push_back(body.ReadLocal(format_var, format_type));
  const mir::ExprId known_id =
      EmitScanOperandsKnown(unit, body, bit_t, rule_operands);

  mir::Block scan_body;
  mir::Builder scan_body_builder = body.Into(scan_body);
  const WalkFrame scan_frame = step_frame.WithBlock(&scan_body);

  // `$fscanf` reads the stream the descriptor names, so the call is made on
  // the run's file table and names the descriptor as the number it works in;
  // `$sscanf` reads the string its source names. LRM 21.3.4.3(a) gives
  // `$sscanf` alone the rule that a null character counts as white space, so
  // which of the two parses runs is fixed by the system function the source
  // names, not by the bytes it receives.
  mir::Direct parse_fn{};
  mir::ExprId source_id{};
  if (is_file) {
    if (!unit.types.Get(raw_source_type).IsIntegral()) {
      throw InternalError(
          "LowerScanSystemSubroutineCall: $fscanf fd is not packed-integer");
    }
    const mir::ExprId runtime_id =
        scan_body_builder.Add(BuildCurrentRuntimeCallExpr(process.Owner()));
    const mir::ExprId files_id = scan_body_builder.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target = support::BuiltinFn::kFiles,
                            .receiver = runtime_id},
                    .arguments = {}},
            .type = unit.builtins.files});
    parse_fn = mir::Direct{
        .target = support::BuiltinFn::kFileScan, .receiver = files_id};
    source_id = BuildToInt64Call(
        unit, scan_body_builder,
        scan_body_builder.ReadLocal(source_var, raw_source_type));
  } else {
    parse_fn = mir::Direct{.target = support::BuiltinFn::kScanString};
    source_id = LiftStringSource(
        unit_lowerer, scan_frame, raw_source_type,
        scan_body_builder.ReadLocal(source_var, raw_source_type));
  }

  const mir::ExprId format_id = LiftStringFormat(
      unit_lowerer, scan_frame, format_type,
      scan_body_builder.ReadLocal(format_var, format_type));

  // One prototype per conversion. It states the shape that conversion parses
  // into, which nothing else on the call states, and it is what the completion
  // carries back where no conversion reached it.
  std::vector<mir::ExprId> prototypes;
  prototypes.reserve(target_types.size());
  for (const mir::TypeId target_type : target_types) {
    prototypes.push_back(scan_body_builder.Add(BuildDefaultValueExpr(
        unit_lowerer.Unit(), scan_body_builder, target_type)));
  }
  const mir::ExprId prototypes_id = scan_body_builder.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(prototypes)},
          .type = unit.types.Intern(
              mir::Type{mir::TupleType{.elements = target_types}})});

  const mir::TypeId payload_type = CompletionPayloadType(
      unit, ScanCompletionComponents(integer_t, target_types));
  const mir::ExprId parse_call_id = scan_body_builder.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = parse_fn,
                  .arguments = {source_id, format_id, prototypes_id}},
          .type = payload_type});
  const mir::LocalId completion =
      steps.Bindings().DeclareAnonymous(payload_type);
  scan_body_builder.AppendStmt(
      mir::LocalDeclStmt{.target = completion, .init = parse_call_id});

  const mir::ExprId count_target =
      scan_body_builder.LocalPlace(count_var, integer_t);
  scan_body_builder.AppendStmt(
      mir::ExprStmt{
          .expr = scan_body_builder.Add(
              mir::MakeAssignExpr(
                  unit.builtins, count_target,
                  ProjectCompletionComponent(
                      scan_body_builder, completion, payload_type, kScanMatched,
                      integer_t)))});

  // LRM 21.3.4.3: the k-th output lvalue is only written when at least
  // k+1 matches were made, so the commit is gated on the matched count.
  for (std::size_t k = 0; k < target_types.size(); ++k) {
    const mir::ExprId count_read_id =
        scan_body_builder.ReadLocal(count_var, integer_t);
    const mir::ExprId k_lit_id = mir::BuildIntegerLiteral(
        unit, scan_body_builder, static_cast<std::int64_t>(k + 1));
    // The count is an `integer`, so the comparison answers a bit that can be
    // unknown (LRM 11.4.4), and the condition reads its truth.
    const mir::ExprId cond_id = scan_body_builder.Add(
        mir::MakeBinary(
            unit, scan_body_builder.Body(), mir::BinaryOp::kGreaterEqual,
            count_read_id, k_lit_id,
            OneBitAnswerType(unit, std::array{integer_t, integer_t})));

    mir::Block then_body;
    const WalkFrame then_frame = scan_frame.WithBlock(&then_body);
    auto lvalue_or =
        LowerLvalue(process, hir_proc.exprs.Get(operands[k + 2]), then_frame);
    if (!lvalue_or) return std::unexpected(std::move(lvalue_or.error()));
    const mir::ExprId parsed_id = ProjectCompletionComponent(
        then_frame.Builder(), completion, payload_type, ScanParsedValue(k),
        target_types[k]);
    auto stored =
        AppendStores(process.Owner(), then_frame, *lvalue_or, parsed_id);
    if (!stored) return std::unexpected(std::move(stored.error()));

    const mir::ExprId matched =
        ReduceToCondition(unit, scan_body_builder, cond_id);
    scan_body_builder.AppendIfThen(matched, std::move(then_body));
  }

  const mir::ExprId all_known = ReduceToCondition(unit, body, known_id);
  body.AppendIfThen(all_known, std::move(scan_body));

  const mir::ExprId count_id = body.LocalPlace(count_var, integer_t);
  return steps.Build(count_id);
}

}  // namespace lyra::lowering::hir_to_mir
