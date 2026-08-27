#include "lyra/lowering/hir_to_mir/expression/system/bit_vector.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/lowering/hir_to_mir/call_operands.hpp"
#include "lyra/lowering/hir_to_mir/flat_packed_type.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"  // IWYU pragma: keep
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"  // IWYU pragma: keep
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The four-state values a count admits, one flag per value at the index its
// bit-plane pair encodes: the value plane in the low bit, the state plane above
// it. LRM 20.9 admits a value by naming it as a control bit, and naming it
// twice admits it once, which is what a set of flags records.
using AdmittedBitValues = std::uint8_t;

constexpr unsigned kBitValueCount = 4;

auto FlagOfPlanes(bool value, bool state) -> AdmittedBitValues {
  return static_cast<AdmittedBitValues>(
      1U
      << ((static_cast<unsigned>(state) << 1U) | static_cast<unsigned>(value)));
}

// The four-state value at `index` of a constant, read off its two planes.
auto PlanesAt(const hir::IntegralConstant& constant, std::uint32_t index)
    -> AdmittedBitValues {
  const std::size_t word = index / 64U;
  const std::uint64_t bit = std::uint64_t{1} << (index % 64U);
  const bool state =
      constant.state_kind == hir::IntegralStateKind::kFourState &&
      word < constant.state_words.size() &&
      (constant.state_words[word] & bit) != 0U;
  return FlagOfPlanes((constant.value_words[word] & bit) != 0U, state);
}

// The values a `$countbits` call names. Each control argument contributes its
// least significant bit (LRM 20.9), which is where an argument wider than one
// bit carries the control bit it stands for.
template <ExprLowerer Lowerer>
auto AdmittedByControlArguments(
    const Lowerer& lowerer, std::span<const hir::ExprId> controls,
    diag::SourceSpan span) -> diag::Result<AdmittedBitValues> {
  const auto& hir_exprs = lowerer.HirExprs();
  AdmittedBitValues admitted = 0;
  for (const hir::ExprId control : controls) {
    const hir::Expr& arg = hir_exprs.Get(control);
    const auto* primary = std::get_if<hir::PrimaryExpr>(&arg.data);
    const auto* literal =
        primary == nullptr ? nullptr
                           : std::get_if<hir::IntegerLiteral>(&primary->data);
    if (literal == nullptr) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedSubroutineArgument,
          "$countbits with a control bit the running simulation computes is "
          "not yet supported; LRM 20.9 spells a control bit as a literal");
    }
    admitted =
        static_cast<AdmittedBitValues>(admitted | PlanesAt(literal->value, 0));
  }
  return admitted;
}

// The control bits the runtime counts under, as a value of their own: the
// admitted four-state values laid out one per bit position, in the order the
// bit-plane encoding gives them. No source declaration names this value -- LRM
// 20.9 spells the same set as a variable-length argument list, which no single
// runtime entry can take -- so the lowering builds it.
auto MakeControlBitsExpr(mir::CompilationUnit& unit, AdmittedBitValues admitted)
    -> mir::Expr {
  std::uint64_t value_word = 0;
  std::uint64_t state_word = 0;
  std::uint32_t width = 0;
  for (unsigned i = 0; i < kBitValueCount; ++i) {
    if ((admitted & (1U << i)) == 0U) {
      continue;
    }
    if ((i & 1U) != 0U) {
      value_word |= std::uint64_t{1} << width;
    }
    if ((i & 2U) != 0U) {
      state_word |= std::uint64_t{1} << width;
    }
    ++width;
  }
  return mir::Expr{
      .data =
          mir::IntegerLiteral{
              .value =
                  mir::IntegralConstant{
                      .value_words = {value_word},
                      .state_words = {state_word},
                      .width = width,
                      .signedness = mir::Signedness::kUnsigned,
                      .state_kind = mir::IntegralStateKind::kFourState}},
      .type = InternFlatPacked(unit, width, mir::BitAtom::kLogic)};
}

auto ReadingComparison(support::BitCountReading reading)
    -> std::optional<mir::BinaryOp> {
  switch (reading) {
    case support::BitCountReading::kExactlyOne:
      return mir::BinaryOp::kEquality;
    case support::BitCountReading::kAtMostOne:
      return mir::BinaryOp::kLessEqual;
    default:
      return std::nullopt;
  }
}

// LRM 20.9 defines `$isunknown` as `$countbits(expr, 'x, 'z) != 0`, but every
// value type answers the unknown question on its own, so that reading takes the
// direct entry instead of counting and then comparing.
template <ExprLowerer Lowerer>
auto LowerUnknownTest(Lowerer& lowerer, WalkFrame frame, hir::ExprId value)
    -> diag::Result<mir::Expr> {
  auto& body = *frame.current_block;
  auto operand_or = lowerer.LowerExpr(lowerer.HirExprs().Get(value), frame);
  if (!operand_or) {
    return std::unexpected(std::move(operand_or.error()));
  }
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = support::BuiltinFn::kIsUnknown},
              .arguments = {body.exprs.Add(*std::move(operand_or))}},
      .type = lowerer.Owner().Unit().builtins.bit1};
}

}  // namespace

template <ExprLowerer Lowerer>
auto LowerBitVectorSystemSubroutineCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& call,
    const support::BitVectorSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  // Every one of these reads its value first; $countbits (LRM 20.9) follows it
  // with control bits and the rest take nothing more. None of them elides.
  const std::vector<hir::ExprId> operands = RequiredOperands(call);

  AdmittedBitValues admitted = 0;
  switch (info.values) {
    // The unknown set is only ever asked whether any bit is in it
    // (`$isunknown`), which the runtime answers directly rather than by
    // counting.
    case support::BitValueSet::kUnknowns:
      if (info.reading != support::BitCountReading::kAny) {
        throw InternalError(
            "LowerBitVectorSystemSubroutineCall: counting the unknown bits is "
            "not a reading any subroutine declares");
      }
      return LowerUnknownTest(lowerer, frame, operands[0]);
    case support::BitValueSet::kOnes:
      admitted = FlagOfPlanes(true, false);
      break;
    case support::BitValueSet::kControlArguments: {
      auto named = AdmittedByControlArguments(
          lowerer, std::span{operands}.subspan(1), span);
      if (!named) {
        return std::unexpected(std::move(named.error()));
      }
      admitted = *named;
      break;
    }
  }

  auto& unit = lowerer.Owner().Unit();
  auto& body = *frame.current_block;
  auto operand_or =
      lowerer.LowerExpr(lowerer.HirExprs().Get(operands[0]), frame);
  if (!operand_or) {
    return std::unexpected(std::move(operand_or.error()));
  }
  const mir::ExprId operand_id = body.exprs.Add(*std::move(operand_or));
  const mir::ExprId control_id =
      body.exprs.Add(MakeControlBitsExpr(unit, admitted));
  mir::Expr count{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = support::BuiltinFn::kCountBits},
              .arguments = {operand_id, control_id}},
      .type = unit.builtins.int_type};

  const std::optional<mir::BinaryOp> op = ReadingComparison(info.reading);
  if (!op) {
    return count;
  }
  return mir::Expr{
      .data =
          mir::BinaryExpr{
              .op = *op,
              .lhs = body.exprs.Add(std::move(count)),
              .rhs = body.exprs.Add(
                  mir::MakeIntLiteral(unit.builtins.int_type, 1))},
      .type = unit.builtins.bit1};
}

template auto LowerBitVectorSystemSubroutineCall(
    ProcessLowerer&, WalkFrame, const hir::CallExpr&,
    const support::BitVectorSystemSubroutineInfo&, diag::SourceSpan)
    -> diag::Result<mir::Expr>;
template auto LowerBitVectorSystemSubroutineCall(
    const StructuralScopeLowerer&, WalkFrame, const hir::CallExpr&,
    const support::BitVectorSystemSubroutineInfo&, diag::SourceSpan)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
