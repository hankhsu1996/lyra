#include "lyra/mir/expr_builders.hpp"

#include <algorithm>
#include <array>
#include <cstdint>
#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/frame_size.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/integral_constant_folding.hpp"
#include "lyra/mir/integral_constant_id.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/unary_op.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/integral_operation.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::mir {

auto CanonicalIntegralConstant(
    const IntegralType& integral, const IntegralConstant& value)
    -> IntegralConstant {
  // A caller may have written too few words, or left the sign bits above the
  // width set.
  const auto held_as = [&](std::span<const std::uint64_t> written,
                           std::span<std::uint64_t> plane) {
    std::ranges::copy(
        written.first(std::min(written.size(), plane.size())), plane.begin());
    lyra::value::ClearAboveWidth(plane, integral.bit_width);
  };
  IntegralConstant canonical = BlankIntegralConstant(integral);
  const bool has_unknown = std::ranges::any_of(
      value.state_words, [](std::uint64_t word) { return word != 0U; });
  if (has_unknown && canonical.state_words.empty()) {
    throw InternalError(
        "CanonicalIntegralConstant: a 2-state type cannot carry an X or Z bit");
  }
  held_as(value.value_words, canonical.value_words);
  held_as(value.state_words, canonical.state_words);
  return canonical;
}

auto BuildMachineIntLiteral(
    const CompilationUnit& unit, Block& block, std::int64_t value) -> ExprId {
  return block.exprs.Add(
      Expr{
          .data = MachineIntLiteral{.value = value},
          .type = unit.builtins.machine_int64});
}

auto MakeIntegralLiteral(
    const CompilationUnit& unit, TypeId type, const IntegralConstant& value)
    -> Expr {
  const IntegralConstantId constant = unit.integral_constants.Intern(
      IntegralConstantDecl{
          .type = type,
          .value = CanonicalIntegralConstant(
              unit.types.Get(type).Integral(), value)});
  return Expr{
      .data =
          ReferenceExpr{.target = IntegralConstantRef{.constant = constant}},
      .type = type};
}

namespace {

// The narrowest integral type a value of `integral` extends from: the bits up
// to the highest one that differs from the value's top bit, read unsigned where
// every position above them holds 0 and signed, with one more bit to carry the
// sign, where they hold 1, x or z (LRM 11.6.1). `canonical` is the value's
// planes at `integral`.
auto MinimalExtendingType(
    const IntegralType& integral, const IntegralConstant& canonical)
    -> IntegralType {
  const std::span<const std::uint64_t> value(canonical.value_words);
  const std::span<const std::uint64_t> unknown(canonical.state_words);
  const std::uint64_t top = integral.bit_width - 1U;
  const bool value_above = lyra::value::BitAt(value, top);
  const bool unknown_above = lyra::value::BitAt(unknown, top);
  std::uint64_t differing = 0;
  for (std::uint64_t position = integral.bit_width; position-- > 0U;) {
    if (lyra::value::BitAt(value, position) != value_above ||
        lyra::value::BitAt(unknown, position) != unknown_above) {
      differing = position + 1U;
      break;
    }
  }
  const bool extends_zero = !value_above && !unknown_above;
  return IntegralType{
      .bit_width = std::min(
          integral.bit_width, extends_zero
                                  ? std::max<std::uint64_t>(differing, 1U)
                                  : differing + 1U),
      .signedness = extends_zero ? Signedness::kUnsigned : Signedness::kSigned,
      .state_kind = integral.state_kind};
}

}  // namespace

auto BuildIntegralLiteral(
    const CompilationUnit& unit, Block& block, TypeId type,
    const IntegralConstant& value) -> ExprId {
  // A constant of a type too large for a frame is as large as the type,
  // however little it says: a fill, a default and a small number widened are
  // one scalar above a few bits. So it is stated as the extension of those
  // bits, and what the unit holds as data is as long as what differs.
  if (const auto* integral = unit.types.Get(type).As<IntegralType>();
      integral != nullptr && IsTooLargeForAFrame(unit, type)) {
    const IntegralConstant canonical =
        CanonicalIntegralConstant(*integral, value);
    const IntegralType narrowest = MinimalExtendingType(*integral, canonical);
    if (lyra::value::WordCountForBits(narrowest.bit_width) <
        lyra::value::WordCountForBits(integral->bit_width)) {
      const ExprId extended_from = block.exprs.Add(MakeIntegralLiteral(
          unit, unit.types.Intern(Type{narrowest}), canonical));
      return block.exprs.Add(MakeBuiltinCall(
          unit, block, support::BuiltinFn::kIntegralConvert, std::nullopt,
          {extended_from}, type));
    }
  }
  return block.exprs.Add(MakeIntegralLiteral(unit, type, value));
}

namespace {

auto IsIntegral(const CompilationUnit& unit, const Block& block, ExprId id)
    -> bool {
  return unit.types.Get(block.exprs.Get(id).type).IsIntegral();
}

// `unfolded`, which states `op` over `operands`, or the constant that
// operation evaluates to where every operand is fixed before the program runs.
auto FoldedOr(
    const CompilationUnit& unit, const Block& block, support::IntegralOp op,
    std::span<const ExprId> operands, Expr unfolded) -> Expr {
  const TypeId type = unfolded.type;
  std::optional<FoldedIntegral> folded =
      FoldIntegral(unit, block, op, operands, type);
  if (!folded) {
    return unfolded;
  }
  return std::visit(
      Overloaded{
          [&](const IntegralConstant& constant) {
            return MakeIntegralLiteral(unit, type, constant);
          },
          [&](bool predicate) {
            return Expr{
                .data = MachineBoolLiteral{.value = predicate}, .type = type};
          },
          [&](std::int64_t number) {
            return Expr{
                .data = MachineIntLiteral{.value = number}, .type = type};
          }},
      *folded);
}

}  // namespace

auto MakeBinary(
    const CompilationUnit& unit, const Block& block, BinaryOp op, ExprId lhs,
    ExprId rhs, TypeId result_type) -> Expr {
  Expr binary{
      .data = BinaryExpr{.op = op, .lhs = lhs, .rhs = rhs},
      .type = result_type};
  if (!IsIntegral(unit, block, lhs)) {
    return binary;
  }
  return FoldedOr(
      unit, block, IntegralOpOf(op), std::array{lhs, rhs}, std::move(binary));
}

auto BuildMachineIntBinary(
    const CompilationUnit& unit, Block& block, BinaryOp op, ExprId lhs,
    ExprId rhs) -> ExprId {
  return block.exprs.Add(
      MakeBinary(unit, block, op, lhs, rhs, unit.builtins.machine_int64));
}

auto MakeUnary(
    const CompilationUnit& unit, const Block& block, UnaryOp op, ExprId operand,
    TypeId result_type) -> Expr {
  Expr unary{
      .data = UnaryExpr{.op = op, .operand = operand}, .type = result_type};
  if (!IsIntegral(unit, block, operand)) {
    return unary;
  }
  return FoldedOr(
      unit, block, IntegralOpOf(op), std::array{operand}, std::move(unary));
}

auto EntryOver(
    const CompilationUnit& unit, support::BuiltinFn entry, TypeId over)
    -> support::BuiltinFn {
  const std::optional<support::BuiltinFn> over_integral_values =
      support::RuntimeEntryOf(entry).over_integral_values;
  if (over_integral_values.has_value() && unit.types.Get(over).IsIntegral()) {
    return *over_integral_values;
  }
  return entry;
}

namespace {

// The entry a call on `stated` names, given the operands it is called with.
auto EntryCalled(
    const CompilationUnit& unit, const Block& block, support::BuiltinFn stated,
    std::span<const ExprId> operands) -> support::BuiltinFn {
  if (operands.empty()) {
    return stated;
  }
  return EntryOver(unit, stated, block.exprs.Get(operands.front()).type);
}

}  // namespace

auto MakeBuiltinCall(
    const CompilationUnit& unit, const Block& block, support::BuiltinFn stated,
    std::optional<ExprId> receiver, std::vector<ExprId> arguments,
    TypeId result_type) -> Expr {
  const support::BuiltinFn called = EntryCalled(
      unit, block, stated,
      receiver.has_value() ? std::span<const ExprId>(&*receiver, 1)
                           : std::span<const ExprId>(arguments));
  return MakeBuiltinCall(
      unit, block, called, receiver, std::nullopt,
      BuiltinCalleeAnswering(called, receiver, result_type).type_argument,
      std::move(arguments), result_type);
}

auto MakeBuiltinCall(
    const CompilationUnit& unit, const Block& block, support::BuiltinFn stated,
    std::optional<ExprId> receiver, std::optional<CallPart> part,
    std::optional<TypeId> type_argument, std::vector<ExprId> arguments,
    TypeId result_type) -> Expr {
  std::vector<ExprId> operands;
  if (receiver) {
    operands.push_back(*receiver);
  }
  operands.insert(operands.end(), arguments.begin(), arguments.end());
  const support::BuiltinFn entry = EntryCalled(unit, block, stated, operands);
  const support::RuntimeEntry declared = support::RuntimeEntryOf(entry);
  Expr call{
      .data =
          CallExpr{
              .callee =
                  Direct{
                      .target = entry,
                      .receiver = receiver,
                      .part = part,
                      .type_argument = type_argument},
              .arguments = std::move(arguments)},
      .type = result_type};
  // An entry that is not an operation over integral values has no value this
  // evaluates.
  if (!declared.integral.has_value()) {
    return call;
  }
  return FoldedOr(unit, block, *declared.integral, operands, std::move(call));
}

auto MakeSameRepresentationCast(
    const CompilationUnit& unit, const Block& block, ExprId operand,
    TypeId type) -> Expr {
  Expr cast{.data = CastExpr{.operand = operand}, .type = type};
  // Only an integral value is one a constant of the unit spells.
  if (!unit.types.Get(type).IsIntegral()) {
    return cast;
  }
  return FoldedOr(
      unit, block, support::IntegralOp::kConvert, std::array{operand},
      std::move(cast));
}

auto MakeIntegralConversion(
    const CompilationUnit& unit, const Block& block, ExprId operand,
    TypeId type) -> Expr {
  if (unit.types.Get(block.exprs.Get(operand).type).Integral() !=
      unit.types.Get(type).Integral()) {
    return MakeBuiltinCall(
        unit, block, support::BuiltinFn::kIntegralConvert, std::nullopt,
        {operand}, type);
  }
  return MakeSameRepresentationCast(unit, block, operand, type);
}

auto BuildIntLiteral(
    const CompilationUnit& unit, Block& block, std::int64_t value) -> ExprId {
  return BuildIntegralLiteral(
      unit, block, unit.builtins.int_type,
      IntegralConstant{
          .value_words = {static_cast<std::uint64_t>(value)},
          .state_words = {}});
}

auto BuildIntegerLiteral(
    const CompilationUnit& unit, Block& block, std::int64_t value) -> ExprId {
  return BuildIntegralLiteral(
      unit, block, unit.builtins.integer,
      IntegralConstant{
          .value_words = {static_cast<std::uint64_t>(value)},
          .state_words = {}});
}

auto BuildBit1Literal(const CompilationUnit& unit, Block& block, bool value)
    -> ExprId {
  return BuildIntegralLiteral(
      unit, block, unit.builtins.bit1,
      IntegralConstant{
          .value_words = {value ? 1ULL : 0ULL}, .state_words = {}});
}

}  // namespace lyra::mir
