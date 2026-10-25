#pragma once

#include <concepts>
#include <cstdint>
#include <optional>
#include <ranges>
#include <string>
#include <utility>
#include <vector>

#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/unary_op.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::mir {

// A machine integer, written as MIR's own literal. This is the domain a
// runtime entry's count, size, and index operands are in: they are not
// SystemVerilog values, so they carry no declared type.
[[nodiscard]] auto BuildMachineIntLiteral(
    const CompilationUnit& unit, Block& block, std::int64_t value) -> ExprId;

// A count the lowering holds -- a number of bits, of elements -- written as
// such a literal.
[[nodiscard]] auto BuildMachineCount(
    const CompilationUnit& unit, Block& block, std::uint64_t count) -> ExprId;

// A machine integer `op` makes of two machine integers.
[[nodiscard]] auto BuildMachineIntBinary(
    const CompilationUnit& unit, Block& block, BinaryOp op, ExprId lhs,
    ExprId rhs) -> ExprId;

// A statement raising `message` as a run-time error of the simulated program
// unless the machine boolean `holds` does, after what `block` holds.
void AppendRunTimeCheck(
    const CompilationUnit& unit, Block& block, ExprId holds,
    std::string message);

// Whether the comparison `op` holds of two machine integers, as a machine
// boolean.
[[nodiscard]] auto BuildMachineIntComparison(
    const CompilationUnit& unit, Block& block, BinaryOp op, ExprId lhs,
    ExprId rhs) -> ExprId;

// `then_value` where the machine boolean `condition` holds and `else_value`
// otherwise, both machine integers.
[[nodiscard]] auto BuildMachineIntConditional(
    const CompilationUnit& unit, Block& block, ExprId condition,
    ExprId then_value, ExprId else_value) -> ExprId;

// A declared range as whichever layer states one: its left bound and its
// right.
template <typename Range>
concept DeclaredRange = requires(const Range& range) {
  { range.left } -> std::convertible_to<std::int64_t>;
  { range.right } -> std::convertible_to<std::int64_t>;
};

// The declared ranges of an array's unpacked dimensions, outermost first, as
// the operand an entry addressing the array by its declared indices takes:
// machine integers, each dimension's left bound then its right. A range is
// fixed by the declaration, so it is stated as numbers.
template <std::ranges::sized_range Ranges>
  requires DeclaredRange<std::ranges::range_value_t<Ranges>>
[[nodiscard]] auto BuildDeclaredRanges(
    const CompilationUnit& unit, Block& block, const Ranges& ranges) -> ExprId {
  std::vector<ExprId> bounds;
  bounds.reserve(2 * std::ranges::size(ranges));
  for (const auto& range : ranges) {
    bounds.push_back(BuildMachineIntLiteral(unit, block, range.left));
    bounds.push_back(BuildMachineIntLiteral(unit, block, range.right));
  }
  const TypeId type =
      MachineArrayOf(unit.types, unit.builtins.machine_int64, bounds.size());
  return block.exprs.Add(
      Expr{.data = CompositeExpr{.parts = std::move(bounds)}, .type = type});
}

// The bits of `value` in the form a value of `integral` holds them: one word
// per 64 bits of its width with the bits above it cleared, and no unknown plane
// unless the type has one. Two spellings of one value come out as the same
// words, and a consumer never reads bits the type does not have.
[[nodiscard]] auto CanonicalIntegralConstant(
    const IntegralType& integral, const IntegralConstant& value)
    -> IntegralConstant;

// An integral constant at `type`: the unit holds it once, among the constants
// it was written with, and an occurrence names that entry. The bits are brought
// to the canonical form the pool keys on first, so two spellings of one value
// are one entry.
[[nodiscard]] auto MakeIntegralLiteral(
    const CompilationUnit& unit, TypeId type, const IntegralConstant& value)
    -> Expr;

// A value of `type` whose bits are `value`, added to `block`: that constant,
// or, where the type is too large for a frame and the value is one scalar
// above a few bits, the extension of the constant those bits are.
[[nodiscard]] auto BuildIntegralLiteral(
    const CompilationUnit& unit, Block& block, TypeId type,
    const IntegralConstant& value) -> ExprId;

// The entry that carries `entry` out over a value of type `over`. An entry the
// source states over values of several kinds names the one over integral
// values alone where `over` is integral, and every other entry is itself.
[[nodiscard]] auto EntryOver(
    const CompilationUnit& unit, support::BuiltinFn entry, TypeId over)
    -> support::BuiltinFn;

// The three ways an operation over values is stated: an operator a target
// applies to two values of one type or to one, and a call to the runtime entry
// that performs it. Each answers with the constant the operation evaluates to
// where it is an operation over integral values and every operand is fixed
// before the program runs (LRM 11.2.1), and with the operation itself
// otherwise. An entry generic over the type it answers with is called at
// `result_type`, and a call of `stated` names the entry over the kind of value
// its first operand is.
[[nodiscard]] auto MakeBinary(
    const CompilationUnit& unit, const Block& block, BinaryOp op, ExprId lhs,
    ExprId rhs, TypeId result_type) -> Expr;
[[nodiscard]] auto MakeUnary(
    const CompilationUnit& unit, const Block& block, UnaryOp op, ExprId operand,
    TypeId result_type) -> Expr;
[[nodiscard]] auto MakeBuiltinCall(
    const CompilationUnit& unit, const Block& block, support::BuiltinFn stated,
    std::optional<ExprId> receiver, std::vector<ExprId> arguments,
    TypeId result_type) -> Expr;
// The same where the callee states more than its receiver: the part it names,
// and the type it is called at.
[[nodiscard]] auto MakeBuiltinCall(
    const CompilationUnit& unit, const Block& block, support::BuiltinFn stated,
    std::optional<ExprId> receiver, std::optional<CallPart> part,
    std::vector<TypeId> type_arguments, std::vector<ExprId> arguments,
    TypeId result_type) -> Expr;
// `operand` held to `type`, which lays a value out as the operand's own type
// does -- an enumeration and its base (LRM 6.19.3, 6.24.1), a reference and
// one to the same referent read-only (LRM 13.5.2) -- so the value already
// fits, and only what the program holds it to be changes.
[[nodiscard]] auto MakeSameRepresentationCast(
    const CompilationUnit& unit, const Block& block, ExprId operand,
    TypeId type) -> Expr;

// `operand`, an integral value, as a value of the integral type `type`. A
// representation is all an integral type says -- width, signedness and state
// domain -- and an enumeration (LRM 6.19.3) shares its base's while being a
// type of its own. Two types that differ in it are a reshape of the bits; two
// that agree hold the same bits, and only the type the program holds the value
// to be changes.
[[nodiscard]] auto MakeIntegralConversion(
    const CompilationUnit& unit, const Block& block, ExprId operand,
    TypeId type) -> Expr;

// 2-state signed 32-bit constant, typed `int` (LRM 6.11.1).
[[nodiscard]] auto BuildIntLiteral(
    const CompilationUnit& unit, Block& block, std::int64_t value) -> ExprId;

// 4-state signed 32-bit constant, typed `integer` (LRM 6.11.1), for an operand
// that meets an `integer` value and has to share its state domain.
[[nodiscard]] auto BuildIntegerLiteral(
    const CompilationUnit& unit, Block& block, std::int64_t value) -> ExprId;

// 1-bit unsigned 2-state constant. The value a boolean fold yields when it has
// nothing to fold, and the constant a synthesized flag is seeded with.
[[nodiscard]] auto BuildBit1Literal(
    const CompilationUnit& unit, Block& block, bool value) -> ExprId;

}  // namespace lyra::mir
