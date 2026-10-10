#pragma once

#include <concepts>
#include <cstdint>
#include <optional>
#include <ranges>
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

namespace lyra::lowering::hir_to_mir {

// A machine integer, written as MIR's own literal. This is the domain a
// runtime entry's count, size, and index operands are in: they are not
// SystemVerilog values, so they carry no declared type.
[[nodiscard]] auto BuildMachineIntLiteral(
    const mir::CompilationUnit& unit, mir::Block& block, std::int64_t value)
    -> mir::ExprId;

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
    const mir::CompilationUnit& unit, mir::Block& block, const Ranges& ranges)
    -> mir::ExprId {
  std::vector<mir::ExprId> bounds;
  bounds.reserve(2 * std::ranges::size(ranges));
  for (const auto& range : ranges) {
    bounds.push_back(BuildMachineIntLiteral(unit, block, range.left));
    bounds.push_back(BuildMachineIntLiteral(unit, block, range.right));
  }
  const mir::TypeId type = mir::MachineArrayOf(
      unit.types, unit.builtins.machine_int64, bounds.size());
  return block.exprs.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(bounds)},
          .type = type});
}

// The bits of `value` in the form a value of `integral` holds them: one word
// per 64 bits of its width with the bits above it cleared, and no unknown plane
// unless the type has one. Two spellings of one value come out as the same
// words, and a consumer never reads bits the type does not have.
[[nodiscard]] auto CanonicalIntegralConstant(
    const mir::IntegralType& integral, const mir::IntegralConstant& value)
    -> mir::IntegralConstant;

// An integral constant at `type`: the unit holds it once, among the constants
// it was written with, and an occurrence names that entry. The bits are brought
// to the canonical form the pool keys on first, so two spellings of one value
// are one entry.
[[nodiscard]] auto MakeIntegralLiteral(
    const mir::CompilationUnit& unit, mir::TypeId type,
    const mir::IntegralConstant& value) -> mir::Expr;

// The same, added to `block`.
[[nodiscard]] auto BuildIntegralLiteral(
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId type,
    const mir::IntegralConstant& value) -> mir::ExprId;

// The entry that carries `entry` out over a value of type `over`. An entry the
// source states over values of several kinds names the one over integral
// values alone where `over` is integral, and every other entry is itself.
[[nodiscard]] auto EntryOver(
    const mir::CompilationUnit& unit, support::BuiltinFn entry,
    mir::TypeId over) -> support::BuiltinFn;

// The three ways an operation over values is stated: an operator a target
// applies to two values of one type or to one, and a call to the runtime entry
// that performs it. Each answers with the constant the operation evaluates to
// where it is an operation over integral values and every operand is fixed
// before the program runs (LRM 11.2.1), and with the operation itself
// otherwise. An entry generic over the type it answers with is called at
// `result_type`, and a call of `stated` names the entry over the kind of value
// its first operand is.
[[nodiscard]] auto MakeBinary(
    const mir::CompilationUnit& unit, const mir::Block& block, mir::BinaryOp op,
    mir::ExprId lhs, mir::ExprId rhs, mir::TypeId result_type) -> mir::Expr;
[[nodiscard]] auto MakeUnary(
    const mir::CompilationUnit& unit, const mir::Block& block, mir::UnaryOp op,
    mir::ExprId operand, mir::TypeId result_type) -> mir::Expr;
[[nodiscard]] auto MakeBuiltinCall(
    const mir::CompilationUnit& unit, const mir::Block& block,
    support::BuiltinFn stated, std::optional<mir::ExprId> receiver,
    std::vector<mir::ExprId> arguments, mir::TypeId result_type) -> mir::Expr;
// The same where the callee states more than its receiver: the part it names,
// and the type it is called at.
[[nodiscard]] auto MakeBuiltinCall(
    const mir::CompilationUnit& unit, const mir::Block& block,
    support::BuiltinFn stated, std::optional<mir::ExprId> receiver,
    std::optional<mir::CallPart> part, std::optional<mir::TypeId> type_argument,
    std::vector<mir::ExprId> arguments, mir::TypeId result_type) -> mir::Expr;
// `operand` held to `type`, which lays a value out as the operand's own type
// does (LRM 6.19.3, 6.24.1): the bits already fit, so only what the program
// holds the value to be changes.
[[nodiscard]] auto MakeSameRepresentationCast(
    const mir::CompilationUnit& unit, const mir::Block& block,
    mir::ExprId operand, mir::TypeId type) -> mir::Expr;

// 2-state signed 32-bit constant, typed `int` (LRM 6.11.1).
[[nodiscard]] auto BuildIntLiteral(
    const mir::CompilationUnit& unit, mir::Block& block, std::int64_t value)
    -> mir::ExprId;

// 4-state signed 32-bit constant, typed `integer` (LRM 6.11.1). Used by sites
// that compare against the matched-count return of `$sscanf` / `$fscanf` --
// those return `integer`, so the operand on the other side must match
// state-kind.
[[nodiscard]] auto BuildIntegerLiteral(
    const mir::CompilationUnit& unit, mir::Block& block, std::int64_t value)
    -> mir::ExprId;

// 1-bit unsigned 2-state constant. The value a boolean fold yields when it has
// nothing to fold, and the constant a synthesized flag is seeded with.
[[nodiscard]] auto BuildBit1Literal(
    const mir::CompilationUnit& unit, mir::Block& block, bool value)
    -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
