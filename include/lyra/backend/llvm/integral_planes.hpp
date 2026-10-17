#pragma once

#include <cstddef>
#include <optional>
#include <variant>
#include <vector>

#include <llvm/ADT/STLFunctionalExtras.h>
#include <llvm/IR/IRBuilder.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/support/integral_operation.hpp"
#include "lyra/value/frame_bound.hpp"
#include "lyra/value/integral.hpp"

namespace llvm {
class Value;
}  // namespace llvm

namespace lyra::backend::llvm_backend {

// The planes of one integral value an operation is stated on, each an integer
// of exactly the value's width, so nothing above the width exists to be kept
// clear. A value that cannot hold x or z has a constant clear unknown
// plane, which is what makes one statement of an operation serve both: every
// term reading that plane folds away as it is built.
struct Planes {
  llvm::Value* value;
  llvm::Value* unknown;
};

// The integral types one application of an operation is at: each operand's
// where the operand is an integral value, and the answer's where the answer
// is one.
struct IntegralShapes {
  std::vector<std::optional<value::IntegralShape>> operands;
  std::optional<value::IntegralShape> answer;
};

// Where an operation on planes takes its operands from, each by its
// position among them: the planes of one that is an integral value, and the
// machine value of one that is not. Planes are asked for where the operation
// reads them, so whatever reading them emits lands there.
struct PlaneOperands {
  llvm::function_ref<Planes(std::size_t)> planes;
  llvm::function_ref<llvm::Value*(std::size_t)> machine;
};

// What an operation on planes answers with: the planes of an integral
// answer, at exactly the answer's width, or the machine value of an operation
// answering one.
using PlaneAnswer = std::variant<Planes, llvm::Value*>;

// Whether an operation over a value of `shape` is stated on the value's planes
// where it is compiled: up to the size a frame keeps a value of, so the code of
// one site follows a declared width only as far as a frame does.
[[nodiscard]] constexpr auto IsStatedOnPlanes(const value::IntegralShape& shape)
    -> bool {
  return shape.Bytes() <= value::kMaxInlineBytes;
}

// Whether a value of `shape` is one machine word per plane.
[[nodiscard]] constexpr auto IsSingleWord(const value::IntegralShape& shape)
    -> bool {
  return shape.width <= 64;
}

// Whether `op` is instructions only where every value it reads and answers is
// one machine word, and a call past that: a product, a quotient and a
// remainder, whose work grows with the square of the width, and reaching a
// part, which on planes shifts the whole value to where the part starts and so
// grows with the whole width however narrow the part, where the library
// touches only the words the part spans.
[[nodiscard]] constexpr auto IsOnOneWordOnly(support::IntegralOp op) -> bool {
  switch (op) {
    case support::IntegralOp::kMultiply:
    case support::IntegralOp::kDivide:
    case support::IntegralOp::kModulo:
    case support::IntegralOp::kSlice:
    case support::IntegralOp::kWithSlice:
      return true;
    case support::IntegralOp::kAdd:
    case support::IntegralOp::kSubtract:
    case support::IntegralOp::kNegate:
    case support::IntegralOp::kPower:
    case support::IntegralOp::kBitwiseAnd:
    case support::IntegralOp::kBitwiseOr:
    case support::IntegralOp::kBitwiseXor:
    case support::IntegralOp::kBitwiseXnor:
    case support::IntegralOp::kBitwiseNot:
    case support::IntegralOp::kEqual:
    case support::IntegralOp::kNotEqual:
    case support::IntegralOp::kLess:
    case support::IntegralOp::kLessEqual:
    case support::IntegralOp::kGreater:
    case support::IntegralOp::kGreaterEqual:
    case support::IntegralOp::kCaseEqual:
    case support::IntegralOp::kWildcardEqual:
    case support::IntegralOp::kCasezMatch:
    case support::IntegralOp::kCasexMatch:
    case support::IntegralOp::kLogicalAnd:
    case support::IntegralOp::kLogicalOr:
    case support::IntegralOp::kLogicalNot:
    case support::IntegralOp::kLogicalEquivalence:
    case support::IntegralOp::kIsTrue:
    case support::IntegralOp::kReductionAnd:
    case support::IntegralOp::kReductionOr:
    case support::IntegralOp::kReductionXor:
    case support::IntegralOp::kReductionNand:
    case support::IntegralOp::kReductionNor:
    case support::IntegralOp::kReductionXnor:
    case support::IntegralOp::kShiftLeft:
    case support::IntegralOp::kLogicalShiftRight:
    case support::IntegralOp::kArithmeticShiftRight:
    case support::IntegralOp::kConcat:
    case support::IntegralOp::kReplicate:
    case support::IntegralOp::kConvert:
    case support::IntegralOp::kFromInt:
    case support::IntegralOp::kFromBool:
    case support::IntegralOp::kToInt64:
    case support::IntegralOp::kToPosition:
    case support::IntegralOp::kIsUnknown:
    case support::IntegralOp::kHasUnknown:
    case support::IntegralOp::kBitIdentical:
    case support::IntegralOp::kCountBits:
    case support::IntegralOp::kCeilLog2:
    case support::IntegralOp::kMergeConditional:
    case support::IntegralOp::kResolveTriState:
    case support::IntegralOp::kResolveWiredAnd:
    case support::IntegralOp::kResolveWiredOr:
    case support::IntegralOp::kDominate:
    case support::IntegralOp::kReverseBlocks:
    case support::IntegralOp::kFromText:
    case support::IntegralOp::kReadCanonicalBits:
    case support::IntegralOp::kReadCanonicalLogic:
    case support::IntegralOp::kFromSvLogic:
      return false;
  }
  throw InternalError("llvm codegen: unknown integral operation");
}

// `op` as the instructions it is on planes (LRM 11.4), where every integral
// value it is handed and answers with is stated on its planes, so the
// operation costs the words it touches. Nothing for an operation whose work is
// long at every width, for one on one word only past one machine word, or over
// a larger value. It builds only arithmetic on the planes it is handed, so over
// constant planes a folding builder answers with constants.
auto LowerOnPlanes(
    llvm::IRBuilderBase& builder, support::IntegralOp op,
    const IntegralShapes& shapes, const PlaneOperands& operands)
    -> std::optional<PlaneAnswer>;

// The planes of the value of type `shape` lying at `at`, each loaded as the
// integer its bytes are and cut to the value's width.
auto LoadPlanes(
    llvm::IRBuilderBase& builder, llvm::Value* at,
    const value::IntegralShape& shape) -> Planes;
// Planes laid out at `at` as a value of type `shape`, each widened to its
// storage unit, which is what keeps every position above the width clear.
void StorePlanes(
    llvm::IRBuilderBase& builder, Planes planes, llvm::Value* at,
    const value::IntegralShape& shape);
// The value of type `shape` lying at `at` laid back out there as its type
// holds one, every position above its width clear.
void ClearAboveWidth(
    llvm::IRBuilderBase& builder, llvm::Value* at,
    const value::IntegralShape& shape);

// The 0, 1 or x a comparison answers (LRM 11.4.4, 11.4.5) as the byte a runtime
// entry answers it in -- the value bit, and above it whether it is unknown --
// read out of a value of the one-bit type `answer` laid out at `at`, and laid
// out there as one. A type that holds no x holds an unknown answer as 0 (LRM
// 6.11.2).
auto LoadComparisonAnswer(
    llvm::IRBuilderBase& builder, llvm::Value* at,
    const value::IntegralShape& answer) -> llvm::Value*;
void StoreComparisonAnswer(
    llvm::IRBuilderBase& builder, llvm::Value* scalar, llvm::Value* at,
    const value::IntegralShape& answer);

}  // namespace lyra::backend::llvm_backend
