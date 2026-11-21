#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
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

// One plane of a value held as words.
enum class Plane : std::uint8_t { kValue, kUnknown };

// The words of a value held as words, as reading a part of it reads them: a
// word of a plane named by its index, which is 0 where the value has no word
// there or no such plane. Where the words come from is the implementer's:
// storage where the value lies, or words already known.
class ValueWords {
 public:
  ValueWords() = default;
  ValueWords(const ValueWords&) = delete;
  ValueWords(ValueWords&&) = delete;
  auto operator=(const ValueWords&) -> ValueWords& = delete;
  auto operator=(ValueWords&&) -> ValueWords& = delete;
  virtual ~ValueWords() = default;

  [[nodiscard]] virtual auto Read(Plane plane, llvm::Value* index) const
      -> llvm::Value* = 0;
};

// The words of a value a part is written into: also the bits a mask selects of
// a word replaced, which changes nothing where the value has no word there or
// no such plane, and the same at an index the value is known to have a word
// at, which then asks nothing of the index.
class MutableValueWords : public ValueWords {
 public:
  virtual void Replace(
      Plane plane, llvm::Value* index, llvm::Value* bits,
      llvm::Value* mask) const = 0;
  virtual void ReplaceHeld(
      Plane plane, llvm::Value* index, llvm::Value* bits,
      llvm::Value* mask) const = 0;
};

// The words of the value of type `shape` lying at `at`, each plane a run of
// 64-bit words, the unknown plane after the value plane. A word is loaded and
// stored only at an index the value has, so an index past either end reads 0
// and writes nothing.
class WordsLyingAt final : public MutableValueWords {
 public:
  WordsLyingAt(
      llvm::IRBuilderBase& builder, llvm::Value* at,
      const value::IntegralShape& shape);

  [[nodiscard]] auto Read(Plane plane, llvm::Value* index) const
      -> llvm::Value* override;
  void Replace(
      Plane plane, llvm::Value* index, llvm::Value* bits,
      llvm::Value* mask) const override;
  void ReplaceHeld(
      Plane plane, llvm::Value* index, llvm::Value* bits,
      llvm::Value* mask) const override;

 private:
  [[nodiscard]] auto Holds(llvm::Value* index) const -> llvm::Value*;
  // The word at `at` with the bits `mask` selects replaced by those of `bits`.
  void ReplaceAt(llvm::Value* at, llvm::Value* bits, llvm::Value* mask) const;
  [[nodiscard]] auto PlaneAt(Plane plane) const -> llvm::Value*;
  [[nodiscard]] auto WordAt(Plane plane, llvm::Value* index) const
      -> llvm::Value*;

  llvm::IRBuilderBase* builder_;
  llvm::Value* at_;
  value::IntegralShape shape_;
};

// Where an operation on planes takes its operands from, each by its
// position among them: the planes of one that is an integral value, the
// machine value of one that is not, and the words of one an operation reaches
// part of by words. Each is asked for where the operation reads it, so
// whatever reading it emits lands there.
struct PlaneOperands {
  llvm::function_ref<Planes(std::size_t)> planes;
  llvm::function_ref<llvm::Value*(std::size_t)> machine;
  llvm::function_ref<const ValueWords&(std::size_t)> words;
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

// Whether a value of `shape` is at most two machine words per plane, the
// widest integer whose every operation the machine target carries out in a few
// instructions or a call of its own runtime.
[[nodiscard]] constexpr auto IsAtMostTwoWords(const value::IntegralShape& shape)
    -> bool {
  return shape.width <= value::kWidestPrimitiveBits;
}

// Whether a part of type `part` of a value of `whole` is read and written by
// the words it spans, where the value lies: a value held as words, and a part
// a frame keeps. The code of one site then follows the part and never the
// value, as the work does, so it holds at every width of the value; a part
// past what a frame keeps is the library's, which touches only its words.
[[nodiscard]] constexpr auto IsReachedByWords(
    const value::IntegralShape& whole, const value::IntegralShape& part)
    -> bool {
  return !IsSingleWord(whole) && IsStatedOnPlanes(part);
}

// `op` as the instructions it is (LRM 11.4), where every integral value it is
// handed and answers with is at most two machine words, as a machine's own
// two-word integer is: an operator over a wider value is its type's own body,
// which MIR states, and nothing is built for one here, nor for an operation
// whose work is long at every width. It builds only arithmetic on what it is
// handed, so over constant planes and words a folding builder answers with
// constants.
auto LowerOnPlanes(
    llvm::IRBuilderBase& builder, support::IntegralOp op,
    const IntegralShapes& shapes, const PlaneOperands& operands)
    -> std::optional<PlaneAnswer>;

// One part a write reaches through, as it is written: where it starts within
// what it is a part of, the type that start is a value of, and how many bits
// the part has.
struct PartOfValue {
  Planes start{};
  value::IntegralShape start_shape;
  std::uint64_t width = 0;
};

// LRM 11.5.1: the bits `parts` name, outermost first, the last as wide as
// `answer`, of the value of type `whole` whose words are `words`, where
// `IsReachedByWords` holds of the value and the answer. A position outside the
// value, or outside a part the bits are reached through, reads x, or 0 in a
// two-state answer, and so does every bit where a part names no position. Only
// the words the bits span are read.
auto ReadPartOfWords(
    llvm::IRBuilderBase& builder, const ValueWords& words,
    const value::IntegralShape& whole, std::span<const PartOfValue> parts,
    const value::IntegralShape& answer) -> Planes;

// LRM 11.5.1: `bits` written over the bits `parts` name, outermost first, in
// the value of type `whole` whose words are `words`, where `IsReachedByWords`
// holds of the value and the bits. A part reaches nothing outside the part it
// is taken in nor outside the value, and a start naming no position writes
// nothing; bits with no x settle the positions they land on, and an x or z
// lands in a two-state value as 0. Only the words the bits span are read and
// written.
void WritePartOfWords(
    llvm::IRBuilderBase& builder, const MutableValueWords& words,
    const value::IntegralShape& whole, std::span<const PartOfValue> parts,
    Planes bits, const value::IntegralShape& bits_shape);

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
