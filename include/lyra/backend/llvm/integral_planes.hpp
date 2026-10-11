#pragma once

#include <cstddef>
#include <optional>
#include <variant>
#include <vector>

#include <llvm/ADT/STLFunctionalExtras.h>
#include <llvm/IR/IRBuilder.h>

#include "lyra/support/integral_operation.hpp"
#include "lyra/value/integral.hpp"

namespace llvm {
class Value;
}  // namespace llvm

namespace lyra::backend::llvm_backend {

// The planes of one integral value that lies in place, each an integer of
// exactly the value's width, so nothing above the width exists to be kept
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

// Whether a value of `shape` lies in whatever holds it. Up to that size an
// operation is stated on the value's planes where it is compiled, so the code
// of one site follows a declared width only as far as a frame does.
[[nodiscard]] constexpr auto LiesInPlace(const value::IntegralShape& shape)
    -> bool {
  return !value::UsesHeapWords(shape.width, shape.domain);
}

// Whether a value of `shape` is one machine word per plane.
[[nodiscard]] constexpr auto IsSingleWord(const value::IntegralShape& shape)
    -> bool {
  return shape.width <= 64;
}

// `op` as the instructions it is on planes (LRM 11.4), where every integral
// value it is handed and answers with lies in place, so the operation costs
// the words it touches. Nothing for an operation whose work is long at every
// width, for a product, a quotient or a remainder past one machine word, whose
// work grows with the square of the width, or over a larger value. It builds
// only arithmetic on the planes it is handed, so over constant planes a folding
// builder answers with constants.
auto LowerOnPlanes(
    llvm::IRBuilderBase& builder, support::IntegralOp op,
    const IntegralShapes& shapes, const PlaneOperands& operands)
    -> std::optional<PlaneAnswer>;

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
