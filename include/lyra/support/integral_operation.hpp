#pragma once

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <span>
#include <string_view>
#include <utility>

namespace lyra::support {

// The operations over integral values (LRM 11.4, 11.5.1, 6.24.1, 20.8, 20.9):
// the language's operators on a value of an integral type, and the builtins
// that read one or build one. Each is one operation whatever width its operands
// are, so the compiler evaluating it over constants, generated code performing
// it, and the library carrying it out for a type it was compiled without all
// name it by this.
enum class IntegralOp : std::uint8_t {
  // LRM 11.4.3.
  kAdd,
  kSubtract,
  kMultiply,
  kDivide,
  kModulo,
  kNegate,
  kPower,
  // LRM 11.4.8.
  kBitwiseAnd,
  kBitwiseOr,
  kBitwiseXor,
  kBitwiseXnor,
  kBitwiseNot,
  // LRM 11.4.4, 11.4.5, 11.4.6, and the two matches a `casez` and a `casex`
  // make (LRM 12.5.1).
  kEqual,
  kNotEqual,
  kLess,
  kLessEqual,
  kGreater,
  kGreaterEqual,
  kCaseEqual,
  kWildcardEqual,
  kCasezMatch,
  kCasexMatch,
  // LRM 11.4.7, each operand read as its truth value, and LRM 12.4 whether a
  // value holds as a condition.
  kLogicalAnd,
  kLogicalOr,
  kLogicalNot,
  kLogicalEquivalence,
  kIsTrue,
  // LRM 11.4.9.
  kReductionAnd,
  kReductionOr,
  kReductionXor,
  kReductionNand,
  kReductionNor,
  kReductionXnor,
  // LRM 11.4.10.
  kShiftLeft,
  kLogicalShiftRight,
  kArithmeticShiftRight,
  // LRM 11.4.12, 11.4.12.1.
  kConcat,
  kReplicate,
  // LRM 11.5.1: the bits a position names, and a value with those bits
  // replaced.
  kSlice,
  kWithSlice,
  // LRM 6.24.1 one integral type's value as another's; a machine number or
  // predicate carried into an integral type (LRM 11.6.1); a value read out as
  // a machine number (LRM 6.12.1); and the position an index names (LRM
  // 11.5.1, 7.4.5).
  kConvert,
  kFromInt,
  kFromBool,
  kToInt64,
  kToPosition,
  // LRM 20.9 `$isunknown` and `$countbits`, LRM 20.8.1 `$clog2`, and the two
  // questions a write asks of a value: whether it holds an x or z, and whether
  // it is the same bits as another (LRM 9.4.2).
  kIsUnknown,
  kHasUnknown,
  kBitIdentical,
  kCountBits,
  kCeilLog2,
  // LRM 11.4.11 the arms of a conditional whose condition is ambiguous; LRM
  // 6.6.1, 6.6.3 two contributions to a net folded under each table; LRM
  // 28.12.1 a stronger contribution over a weaker one.
  kMergeConditional,
  kResolveTriState,
  kResolveWiredAnd,
  kResolveWiredOr,
  kDominate,
  // LRM 11.4.14.2 a value's fixed-size blocks in reversed order.
  kReverseBlocks,
  // LRM 5.9 text taken as a value, and what a foreign call left behind in the
  // forms LRM Annex H.10 fixes.
  kFromText,
  kReadCanonicalBits,
  kReadCanonicalLogic,
  kFromSvLogic,
};

// What an operation is handed as one operand, which is also what code compiled
// once for every integral type has to be told about it.
enum class IntegralOperandKind : std::uint8_t {
  // An integral value the operation reads the bits of: how wide it is and
  // whether it can hold x or z.
  kBits,
  // An integral value the operation reads as a number, so its signedness is
  // part of what is read (LRM 11.4.3, 11.4.4).
  kNumber,
  // An integral value of the first operand's type, which the language requires
  // of it (LRM 11.6.1), so nothing more is told of it.
  kSameType,
  // A machine number, predicate, or `svLogic` scalar.
  kMachineInt,
  kMachineBool,
  kSvLogic,
  // Text, and the two buffers a foreign call hands a vector back in (LRM
  // Annex H.10.1).
  kText,
  kCanonicalBits,
  kCanonicalLogic,
};

// Whether an operand of this kind is an integral value.
[[nodiscard]] constexpr auto IsIntegralOperand(IntegralOperandKind kind)
    -> bool {
  switch (kind) {
    case IntegralOperandKind::kBits:
    case IntegralOperandKind::kNumber:
    case IntegralOperandKind::kSameType:
      return true;
    case IntegralOperandKind::kMachineInt:
    case IntegralOperandKind::kMachineBool:
    case IntegralOperandKind::kSvLogic:
    case IntegralOperandKind::kText:
    case IntegralOperandKind::kCanonicalBits:
    case IntegralOperandKind::kCanonicalLogic:
      return false;
  }
  std::unreachable();
}

// Whether an operand of this kind is read as a number, at the signedness its
// type states.
[[nodiscard]] constexpr auto IsNumberOperand(IntegralOperandKind kind) -> bool {
  switch (kind) {
    case IntegralOperandKind::kNumber:
      return true;
    case IntegralOperandKind::kBits:
    case IntegralOperandKind::kSameType:
    case IntegralOperandKind::kMachineInt:
    case IntegralOperandKind::kMachineBool:
    case IntegralOperandKind::kSvLogic:
    case IntegralOperandKind::kText:
    case IntegralOperandKind::kCanonicalBits:
    case IntegralOperandKind::kCanonicalLogic:
      return false;
  }
  std::unreachable();
}

// The type an operation answers at.
enum class IntegralAnswer : std::uint8_t {
  // The first operand's own type.
  kOfFirstOperand,
  // One bit, able to hold x where any operand can (LRM 11.4.4, 11.4.5,
  // 11.4.7).
  kOneBit,
  // One bit that is never x (LRM 11.4.5 `===`, 20.9).
  kTwoStateBit,
  // As wide as both operands together, able to hold x where either can (LRM
  // 11.4.12).
  kJoined,
  // The `int`, `integer` and position types the language or the library fixes
  // for the answer.
  kInt,
  kInteger,
  kPosition,
  // The type the call itself states, which no operand settles: as many copies
  // as fill it, the bits of a part, a conversion's destination.
  kOfTheCall,
  // A machine predicate or number rather than an integral value.
  kMachineBool,
  kMachineInt,
};

// The operands an operation takes, in order.
class IntegralOperands {
 public:
  constexpr IntegralOperands(std::initializer_list<IntegralOperandKind> kinds)
      : count_(kinds.size()) {
    std::ranges::copy(kinds, kinds_.begin());
  }

  [[nodiscard]] constexpr auto Kinds() const
      -> std::span<const IntegralOperandKind> {
    return std::span<const IntegralOperandKind>(kinds_).first(count_);
  }

 private:
  static constexpr std::size_t kMost = 3;
  std::array<IntegralOperandKind, kMost> kinds_{};
  std::size_t count_ = 0;
};

// One operation: its stable spelling, what it takes and what it answers.
//
// The spelling names the operation in a dump and in a diagnostic. Everything
// else about an operation that code handling one needs -- what it is told of
// each operand's type, which operand it reads as a number, where its answer's
// type comes from -- is read off these fields, so an operation gains a reader
// without that reader keeping a list of its own.
struct IntegralOperation {
  IntegralOp op;
  std::string_view name;
  IntegralOperands operands;
  IntegralAnswer answer;
};

namespace detail {

using enum IntegralOperandKind;

inline constexpr std::array kIntegralOperations = {
    IntegralOperation{
        .op = IntegralOp::kAdd,
        .name = "add",
        .operands = {kBits, kSameType},
        .answer = IntegralAnswer::kOfFirstOperand},
    IntegralOperation{
        .op = IntegralOp::kSubtract,
        .name = "sub",
        .operands = {kBits, kSameType},
        .answer = IntegralAnswer::kOfFirstOperand},
    IntegralOperation{
        .op = IntegralOp::kMultiply,
        .name = "mul",
        .operands = {kBits, kSameType},
        .answer = IntegralAnswer::kOfFirstOperand},
    IntegralOperation{
        .op = IntegralOp::kDivide,
        .name = "div",
        .operands = {kNumber, kSameType},
        .answer = IntegralAnswer::kOfFirstOperand},
    IntegralOperation{
        .op = IntegralOp::kModulo,
        .name = "mod",
        .operands = {kNumber, kSameType},
        .answer = IntegralAnswer::kOfFirstOperand},
    IntegralOperation{
        .op = IntegralOp::kNegate,
        .name = "neg",
        .operands = {kBits},
        .answer = IntegralAnswer::kOfFirstOperand},
    IntegralOperation{
        .op = IntegralOp::kPower,
        .name = "pow",
        .operands = {kNumber, kNumber},
        .answer = IntegralAnswer::kOfFirstOperand},
    IntegralOperation{
        .op = IntegralOp::kBitwiseAnd,
        .name = "and",
        .operands = {kBits, kSameType},
        .answer = IntegralAnswer::kOfFirstOperand},
    IntegralOperation{
        .op = IntegralOp::kBitwiseOr,
        .name = "or",
        .operands = {kBits, kSameType},
        .answer = IntegralAnswer::kOfFirstOperand},
    IntegralOperation{
        .op = IntegralOp::kBitwiseXor,
        .name = "xor",
        .operands = {kBits, kSameType},
        .answer = IntegralAnswer::kOfFirstOperand},
    IntegralOperation{
        .op = IntegralOp::kBitwiseXnor,
        .name = "xnor",
        .operands = {kBits, kSameType},
        .answer = IntegralAnswer::kOfFirstOperand},
    IntegralOperation{
        .op = IntegralOp::kBitwiseNot,
        .name = "not",
        .operands = {kBits},
        .answer = IntegralAnswer::kOfFirstOperand},
    IntegralOperation{
        .op = IntegralOp::kEqual,
        .name = "eq",
        .operands = {kBits, kSameType},
        .answer = IntegralAnswer::kOneBit},
    IntegralOperation{
        .op = IntegralOp::kNotEqual,
        .name = "ne",
        .operands = {kBits, kSameType},
        .answer = IntegralAnswer::kOneBit},
    IntegralOperation{
        .op = IntegralOp::kLess,
        .name = "lt",
        .operands = {kNumber, kSameType},
        .answer = IntegralAnswer::kOneBit},
    IntegralOperation{
        .op = IntegralOp::kLessEqual,
        .name = "le",
        .operands = {kNumber, kSameType},
        .answer = IntegralAnswer::kOneBit},
    IntegralOperation{
        .op = IntegralOp::kGreater,
        .name = "gt",
        .operands = {kNumber, kSameType},
        .answer = IntegralAnswer::kOneBit},
    IntegralOperation{
        .op = IntegralOp::kGreaterEqual,
        .name = "ge",
        .operands = {kNumber, kSameType},
        .answer = IntegralAnswer::kOneBit},
    IntegralOperation{
        .op = IntegralOp::kCaseEqual,
        .name = "case_equal",
        .operands = {kBits, kSameType},
        .answer = IntegralAnswer::kTwoStateBit},
    IntegralOperation{
        .op = IntegralOp::kWildcardEqual,
        .name = "wildcard_equal",
        .operands = {kBits, kSameType},
        .answer = IntegralAnswer::kOneBit},
    IntegralOperation{
        .op = IntegralOp::kCasezMatch,
        .name = "casez_match",
        .operands = {kBits, kSameType},
        .answer = IntegralAnswer::kTwoStateBit},
    IntegralOperation{
        .op = IntegralOp::kCasexMatch,
        .name = "casex_match",
        .operands = {kBits, kSameType},
        .answer = IntegralAnswer::kTwoStateBit},
    IntegralOperation{
        .op = IntegralOp::kLogicalAnd,
        .name = "logical_and",
        .operands = {kBits, kBits},
        .answer = IntegralAnswer::kOneBit},
    IntegralOperation{
        .op = IntegralOp::kLogicalOr,
        .name = "logical_or",
        .operands = {kBits, kBits},
        .answer = IntegralAnswer::kOneBit},
    IntegralOperation{
        .op = IntegralOp::kLogicalNot,
        .name = "logical_not",
        .operands = {kBits},
        .answer = IntegralAnswer::kOneBit},
    IntegralOperation{
        .op = IntegralOp::kLogicalEquivalence,
        .name = "logical_equivalence",
        .operands = {kBits, kBits},
        .answer = IntegralAnswer::kOneBit},
    IntegralOperation{
        .op = IntegralOp::kIsTrue,
        .name = "is_true",
        .operands = {kBits},
        .answer = IntegralAnswer::kMachineBool},
    IntegralOperation{
        .op = IntegralOp::kReductionAnd,
        .name = "reduction_and",
        .operands = {kBits},
        .answer = IntegralAnswer::kOneBit},
    IntegralOperation{
        .op = IntegralOp::kReductionOr,
        .name = "reduction_or",
        .operands = {kBits},
        .answer = IntegralAnswer::kOneBit},
    IntegralOperation{
        .op = IntegralOp::kReductionXor,
        .name = "reduction_xor",
        .operands = {kBits},
        .answer = IntegralAnswer::kOneBit},
    IntegralOperation{
        .op = IntegralOp::kReductionNand,
        .name = "reduction_nand",
        .operands = {kBits},
        .answer = IntegralAnswer::kOneBit},
    IntegralOperation{
        .op = IntegralOp::kReductionNor,
        .name = "reduction_nor",
        .operands = {kBits},
        .answer = IntegralAnswer::kOneBit},
    IntegralOperation{
        .op = IntegralOp::kReductionXnor,
        .name = "reduction_xnor",
        .operands = {kBits},
        .answer = IntegralAnswer::kOneBit},
    IntegralOperation{
        .op = IntegralOp::kShiftLeft,
        .name = "shift_left",
        .operands = {kBits, kBits},
        .answer = IntegralAnswer::kOfFirstOperand},
    IntegralOperation{
        .op = IntegralOp::kLogicalShiftRight,
        .name = "logical_shift_right",
        .operands = {kBits, kBits},
        .answer = IntegralAnswer::kOfFirstOperand},
    IntegralOperation{
        .op = IntegralOp::kArithmeticShiftRight,
        .name = "arithmetic_shift_right",
        .operands = {kNumber, kBits},
        .answer = IntegralAnswer::kOfFirstOperand},
    IntegralOperation{
        .op = IntegralOp::kConcat,
        .name = "concat",
        .operands = {kBits, kBits},
        .answer = IntegralAnswer::kJoined},
    IntegralOperation{
        .op = IntegralOp::kReplicate,
        .name = "replicate",
        .operands = {kBits},
        .answer = IntegralAnswer::kOfTheCall},
    IntegralOperation{
        .op = IntegralOp::kSlice,
        .name = "slice",
        .operands = {kBits, kNumber},
        .answer = IntegralAnswer::kOfTheCall},
    IntegralOperation{
        .op = IntegralOp::kWithSlice,
        .name = "with_slice",
        .operands = {kBits, kNumber, kBits},
        .answer = IntegralAnswer::kOfFirstOperand},
    IntegralOperation{
        .op = IntegralOp::kConvert,
        .name = "convert",
        .operands = {kNumber},
        .answer = IntegralAnswer::kOfTheCall},
    IntegralOperation{
        .op = IntegralOp::kFromInt,
        .name = "from_int",
        .operands = {kMachineInt},
        .answer = IntegralAnswer::kOfTheCall},
    IntegralOperation{
        .op = IntegralOp::kFromBool,
        .name = "from_bool",
        .operands = {kMachineBool},
        .answer = IntegralAnswer::kOfTheCall},
    IntegralOperation{
        .op = IntegralOp::kToInt64,
        .name = "to_int64",
        .operands = {kNumber},
        .answer = IntegralAnswer::kMachineInt},
    IntegralOperation{
        .op = IntegralOp::kToPosition,
        .name = "to_position",
        .operands = {kNumber},
        .answer = IntegralAnswer::kPosition},
    IntegralOperation{
        .op = IntegralOp::kIsUnknown,
        .name = "is_unknown",
        .operands = {kBits},
        .answer = IntegralAnswer::kTwoStateBit},
    IntegralOperation{
        .op = IntegralOp::kHasUnknown,
        .name = "has_unknown",
        .operands = {kBits},
        .answer = IntegralAnswer::kMachineBool},
    IntegralOperation{
        .op = IntegralOp::kBitIdentical,
        .name = "bit_identical",
        .operands = {kBits, kSameType},
        .answer = IntegralAnswer::kMachineBool},
    IntegralOperation{
        .op = IntegralOp::kCountBits,
        .name = "count_bits",
        .operands = {kBits, kBits},
        .answer = IntegralAnswer::kInt},
    IntegralOperation{
        .op = IntegralOp::kCeilLog2,
        .name = "clog2",
        .operands = {kBits},
        .answer = IntegralAnswer::kInteger},
    IntegralOperation{
        .op = IntegralOp::kMergeConditional,
        .name = "merge_conditional",
        .operands = {kBits, kSameType},
        .answer = IntegralAnswer::kOfFirstOperand},
    IntegralOperation{
        .op = IntegralOp::kResolveTriState,
        .name = "resolve_tri_state",
        .operands = {kBits, kSameType},
        .answer = IntegralAnswer::kOfFirstOperand},
    IntegralOperation{
        .op = IntegralOp::kResolveWiredAnd,
        .name = "resolve_wired_and",
        .operands = {kBits, kSameType},
        .answer = IntegralAnswer::kOfFirstOperand},
    IntegralOperation{
        .op = IntegralOp::kResolveWiredOr,
        .name = "resolve_wired_or",
        .operands = {kBits, kSameType},
        .answer = IntegralAnswer::kOfFirstOperand},
    IntegralOperation{
        .op = IntegralOp::kDominate,
        .name = "dominate",
        .operands = {kBits, kSameType},
        .answer = IntegralAnswer::kOfFirstOperand},
    IntegralOperation{
        .op = IntegralOp::kReverseBlocks,
        .name = "reverse_blocks",
        .operands = {kBits, kMachineInt},
        .answer = IntegralAnswer::kOfFirstOperand},
    IntegralOperation{
        .op = IntegralOp::kFromText,
        .name = "from_text",
        .operands = {kText},
        .answer = IntegralAnswer::kOfTheCall},
    IntegralOperation{
        .op = IntegralOp::kReadCanonicalBits,
        .name = "read_canonical_bits",
        .operands = {kCanonicalBits},
        .answer = IntegralAnswer::kOfTheCall},
    IntegralOperation{
        .op = IntegralOp::kReadCanonicalLogic,
        .name = "read_canonical_logic",
        .operands = {kCanonicalLogic},
        .answer = IntegralAnswer::kOfTheCall},
    IntegralOperation{
        .op = IntegralOp::kFromSvLogic,
        .name = "from_sv_logic",
        .operands = {kSvLogic},
        .answer = IntegralAnswer::kOfTheCall},
};

// An operation is found by its position, so the rows stand in the order the
// operations are enumerated in.
[[nodiscard]] constexpr auto RowsFollowTheEnumeration() -> bool {
  for (std::size_t row = 0; row < kIntegralOperations.size(); ++row) {
    if (std::to_underlying(kIntegralOperations.at(row).op) != row) {
      return false;
    }
  }
  return true;
}
static_assert(RowsFollowTheEnumeration());

}  // namespace detail

// Every operation, in the order they are enumerated in.
[[nodiscard]] constexpr auto IntegralOperations()
    -> std::span<const IntegralOperation> {
  return detail::kIntegralOperations;
}

// The one declaration of `op`.
[[nodiscard]] constexpr auto IntegralOperationOf(IntegralOp op)
    -> const IntegralOperation& {
  return detail::kIntegralOperations.at(std::to_underlying(op));
}

}  // namespace lyra::support
