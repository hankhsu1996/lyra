#include "lyra/lowering/hir_to_mir/integral_literal.hpp"

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
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/integral_constant_folding.hpp"
#include "lyra/mir/integral_constant_id.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/unary_op.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/integral_operation.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::lowering::hir_to_mir {

auto CanonicalIntegralConstant(
    const mir::IntegralType& integral, const mir::IntegralConstant& value)
    -> mir::IntegralConstant {
  // A caller may have written too few words, or left the sign bits above the
  // width set.
  const auto held_as = [&](std::span<const std::uint64_t> written,
                           std::span<std::uint64_t> plane) {
    std::ranges::copy(
        written.first(std::min(written.size(), plane.size())), plane.begin());
    lyra::value::ClearAboveWidth(plane, integral.bit_width);
  };
  mir::IntegralConstant canonical = mir::BlankIntegralConstant(integral);
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
    const mir::CompilationUnit& unit, mir::Block& block, std::int64_t value)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data = mir::MachineIntLiteral{.value = value},
          .type = unit.builtins.machine_int64});
}

auto MakeIntegralLiteral(
    const mir::CompilationUnit& unit, mir::TypeId type,
    const mir::IntegralConstant& value) -> mir::Expr {
  const mir::IntegralConstantId constant = unit.integral_constants.Intern(
      mir::IntegralConstantDecl{
          .type = type,
          .value = CanonicalIntegralConstant(
              unit.types.Get(type).Integral(), value)});
  return mir::Expr{
      .data =
          mir::ReferenceExpr{
              .target = mir::IntegralConstantRef{.constant = constant}},
      .type = type};
}

namespace {

// The narrowest integral type a value of `integral` extends from: the bits up
// to the highest one that differs from the value's top bit, read unsigned where
// every position above them holds 0 and signed, with one more bit to carry the
// sign, where they hold 1, x or z (LRM 11.6.1). `canonical` is the value's
// planes at `integral`.
auto MinimalExtendingType(
    const mir::IntegralType& integral, const mir::IntegralConstant& canonical)
    -> mir::IntegralType {
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
  return mir::IntegralType{
      .bit_width = std::min(
          integral.bit_width, extends_zero
                                  ? std::max<std::uint64_t>(differing, 1U)
                                  : differing + 1U),
      .signedness =
          extends_zero ? mir::Signedness::kUnsigned : mir::Signedness::kSigned,
      .state_kind = integral.state_kind};
}

}  // namespace

auto BuildIntegralLiteral(
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId type,
    const mir::IntegralConstant& value) -> mir::ExprId {
  // A value of a type that owns its words is as large as the type, however
  // little it says: a fill, a default and a small number widened are one scalar
  // above a few bits. So it is stated as the extension of those bits, and what
  // the unit holds as data is as long as what differs.
  if (const auto* integral = unit.types.Get(type).As<mir::IntegralType>();
      integral != nullptr && mir::UsesHeapWords(*integral)) {
    const mir::IntegralConstant canonical =
        CanonicalIntegralConstant(*integral, value);
    const mir::IntegralType narrowest =
        MinimalExtendingType(*integral, canonical);
    if (lyra::value::WordCountForBits(narrowest.bit_width) <
        lyra::value::WordCountForBits(integral->bit_width)) {
      const mir::ExprId extended_from = block.exprs.Add(MakeIntegralLiteral(
          unit, unit.types.Intern(mir::Type{narrowest}), canonical));
      return block.exprs.Add(MakeBuiltinCall(
          unit, block, support::BuiltinFn::kIntegralConvert, std::nullopt,
          {extended_from}, type));
    }
  }
  return block.exprs.Add(MakeIntegralLiteral(unit, type, value));
}

namespace {

auto IsIntegral(
    const mir::CompilationUnit& unit, const mir::Block& block, mir::ExprId id)
    -> bool {
  return unit.types.Get(block.exprs.Get(id).type).IsIntegral();
}

// `unfolded`, which states `op` over `operands`, or the constant that
// operation evaluates to where every operand is fixed before the program runs.
auto FoldedOr(
    const mir::CompilationUnit& unit, const mir::Block& block,
    support::IntegralOp op, std::span<const mir::ExprId> operands,
    mir::Expr unfolded) -> mir::Expr {
  const mir::TypeId type = unfolded.type;
  std::optional<mir::FoldedIntegral> folded =
      mir::FoldIntegral(unit, block, op, operands, type);
  if (!folded) {
    return unfolded;
  }
  return std::visit(
      Overloaded{
          [&](const mir::IntegralConstant& constant) {
            return MakeIntegralLiteral(unit, type, constant);
          },
          [&](bool predicate) {
            return mir::Expr{
                .data = mir::MachineBoolLiteral{.value = predicate},
                .type = type};
          },
          [&](std::int64_t number) {
            return mir::Expr{
                .data = mir::MachineIntLiteral{.value = number}, .type = type};
          }},
      *folded);
}

}  // namespace

auto MakeBinary(
    const mir::CompilationUnit& unit, const mir::Block& block, mir::BinaryOp op,
    mir::ExprId lhs, mir::ExprId rhs, mir::TypeId result_type) -> mir::Expr {
  mir::Expr binary{
      .data = mir::BinaryExpr{.op = op, .lhs = lhs, .rhs = rhs},
      .type = result_type};
  if (!IsIntegral(unit, block, lhs)) {
    return binary;
  }
  return FoldedOr(
      unit, block, mir::IntegralOpOf(op), std::array{lhs, rhs},
      std::move(binary));
}

auto MakeUnary(
    const mir::CompilationUnit& unit, const mir::Block& block, mir::UnaryOp op,
    mir::ExprId operand, mir::TypeId result_type) -> mir::Expr {
  mir::Expr unary{
      .data = mir::UnaryExpr{.op = op, .operand = operand},
      .type = result_type};
  if (!IsIntegral(unit, block, operand)) {
    return unary;
  }
  return FoldedOr(
      unit, block, mir::IntegralOpOf(op), std::array{operand},
      std::move(unary));
}

auto EntryOver(
    const mir::CompilationUnit& unit, support::BuiltinFn entry,
    mir::TypeId over) -> support::BuiltinFn {
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
    const mir::CompilationUnit& unit, const mir::Block& block,
    support::BuiltinFn stated, std::span<const mir::ExprId> operands)
    -> support::BuiltinFn {
  if (operands.empty()) {
    return stated;
  }
  return EntryOver(unit, stated, block.exprs.Get(operands.front()).type);
}

}  // namespace

auto MakeBuiltinCall(
    const mir::CompilationUnit& unit, const mir::Block& block,
    support::BuiltinFn stated, std::optional<mir::ExprId> receiver,
    std::vector<mir::ExprId> arguments, mir::TypeId result_type) -> mir::Expr {
  const support::BuiltinFn called = EntryCalled(
      unit, block, stated,
      receiver.has_value() ? std::span<const mir::ExprId>(&*receiver, 1)
                           : std::span<const mir::ExprId>(arguments));
  return MakeBuiltinCall(
      unit, block, called, receiver, std::nullopt,
      mir::BuiltinCalleeAnswering(called, receiver, result_type).type_argument,
      std::move(arguments), result_type);
}

auto MakeBuiltinCall(
    const mir::CompilationUnit& unit, const mir::Block& block,
    support::BuiltinFn stated, std::optional<mir::ExprId> receiver,
    std::optional<mir::CallPart> part, std::optional<mir::TypeId> type_argument,
    std::vector<mir::ExprId> arguments, mir::TypeId result_type) -> mir::Expr {
  std::vector<mir::ExprId> operands;
  if (receiver) {
    operands.push_back(*receiver);
  }
  operands.insert(operands.end(), arguments.begin(), arguments.end());
  const support::BuiltinFn entry = EntryCalled(unit, block, stated, operands);
  const support::RuntimeEntry declared = support::RuntimeEntryOf(entry);
  mir::Expr call{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
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
    const mir::CompilationUnit& unit, const mir::Block& block,
    mir::ExprId operand, mir::TypeId type) -> mir::Expr {
  return FoldedOr(
      unit, block, support::IntegralOp::kConvert, std::array{operand},
      mir::Expr{.data = mir::CastExpr{.operand = operand}, .type = type});
}

auto BuildIntLiteral(
    const mir::CompilationUnit& unit, mir::Block& block, std::int64_t value)
    -> mir::ExprId {
  return BuildIntegralLiteral(
      unit, block, unit.builtins.int_type,
      mir::IntegralConstant{
          .value_words = {static_cast<std::uint64_t>(value)},
          .state_words = {}});
}

auto BuildIntegerLiteral(
    const mir::CompilationUnit& unit, mir::Block& block, std::int64_t value)
    -> mir::ExprId {
  return BuildIntegralLiteral(
      unit, block, unit.builtins.integer,
      mir::IntegralConstant{
          .value_words = {static_cast<std::uint64_t>(value)},
          .state_words = {}});
}

auto BuildBit1Literal(
    const mir::CompilationUnit& unit, mir::Block& block, bool value)
    -> mir::ExprId {
  return BuildIntegralLiteral(
      unit, block, unit.builtins.bit1,
      mir::IntegralConstant{
          .value_words = {value ? 1ULL : 0ULL}, .state_words = {}});
}

}  // namespace lyra::lowering::hir_to_mir
