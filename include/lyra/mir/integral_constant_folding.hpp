#pragma once

#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <variant>

#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/unary_op.hpp"
#include "lyra/support/integral_operation.hpp"
#include "lyra/value/format.hpp"

namespace lyra::mir {

// The operation an operator is where its operands are integral values (LRM
// 11.4).
[[nodiscard]] auto IntegralOpOf(BinaryOp op) -> support::IntegralOp;
[[nodiscard]] auto IntegralOpOf(UnaryOp op) -> support::IntegralOp;

// What an operation over constants evaluates to: an integral value, or the
// machine predicate or number the operation answers with.
using FoldedIntegral = std::variant<IntegralConstant, bool, std::int64_t>;

// The constant an integral operation evaluates to when every operand is a
// constant the unit holds or a literal, and nothing otherwise. Such an
// operation's value is fixed before the program runs, so it is stated by the
// unit like any other constant rather than computed each time control reaches
// it.
//
// It is asked by whatever builds the operation, with the operation it is about
// to state, so no consumer reads a built node back to learn what it holds. The
// answer is computed by the same word arithmetic the program's own operations
// are, so a folded operation and an unfolded one cannot disagree.
//
// The operands and `result` are held to what the operation declares whether or
// not they are constants: an operand that is no integral value where the
// operation takes one, two sides of different types where it takes one type,
// or an answer stated at another type than the operation answers at is a
// defect in whatever built the operation, and is raised as one.
[[nodiscard]] auto FoldIntegral(
    const CompilationUnit& unit, const Block& block, support::IntegralOp op,
    std::span<const ExprId> operands, TypeId result)
    -> std::optional<FoldedIntegral>;

// The string an integral constant's bytes are (LRM 6.16), and nothing where
// `operand` is no constant. A string literal is such a constant (LRM 5.9), so
// one used as a string is that string before the program runs.
[[nodiscard]] auto FoldStringFromBits(
    const CompilationUnit& unit, const Expr& operand)
    -> std::optional<std::string>;

// The text an integral constant prints as under `spec` (LRM 21.2.1), and
// nothing where `operand` is no constant. The conversion reads nothing but the
// value, so one of a constant is the same text however often it is reached.
// `spec` is a conversion whose text no setting of the running program enters,
// which a time conversion is not (LRM 20.4.3).
[[nodiscard]] auto FoldFormattedText(
    const CompilationUnit& unit, const Expr& operand,
    const value::FormatSpec& spec) -> std::optional<std::string>;

}  // namespace lyra::mir
