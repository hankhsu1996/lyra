#pragma once

#include <array>
#include <cstddef>
#include <span>

#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/language_trait.hpp"

namespace lyra::mir {

// Each kind of array's impls: its own methods, as a Rust type's inherent impl
// names its items, and the language traits it implements. A method is what the
// source reaches through the array itself -- by its name, by a select or a
// stream's window of it, by an assignment into it, or as a step of one of
// those -- and a language trait what a construct asks of an array handed to it
// as an operand. A method the library's traits require of a type is an item of
// that trait, never one of these.
//
// A fixed-size array's methods (LRM 7.4): a run of its elements read and
// written, a window of them as bits and the array with it replaced, its
// elements reversed and put in an order (LRM 7.4.6, 7.12, 11.4.14.4), and the
// array taking another's elements (LRM 7.6).
inline constexpr std::array kFixedArrayMethods{
    support::BuiltinFn::kElementSlice,
    support::BuiltinFn::kAssignElementSlice,
    support::BuiltinFn::kWindowToBitVec,
    support::BuiltinFn::kWithWindow,
    support::BuiltinFn::kReverse,
    support::BuiltinFn::kApplyOrder,
    support::BuiltinFn::kUnpackedArrayFromArray,
};

// A dynamic array's (LRM 7.5).
inline constexpr std::array kDynamicArrayMethods{
    support::BuiltinFn::kDynamicArrayNew,
    support::BuiltinFn::kDynamicArrayNewCopy,
    support::BuiltinFn::kElement,
    support::BuiltinFn::kElementMut,
    support::BuiltinFn::kDelete,
    support::BuiltinFn::kSize,
    support::BuiltinFn::kDynamicArrayFromArray,
    support::BuiltinFn::kElementSlice,
    support::BuiltinFn::kAssignElementSlice,
    support::BuiltinFn::kWindowToBitVec,
    support::BuiltinFn::kWithWindow,
    support::BuiltinFn::kReverse,
    support::BuiltinFn::kApplyOrder,
    support::BuiltinFn::kSortOrder,
};

// A queue's (LRM 7.10).
inline constexpr std::array kQueueMethods{
    support::BuiltinFn::kSize,
    support::BuiltinFn::kElement,
    support::BuiltinFn::kQueueElementMut,
    support::BuiltinFn::kDelete,
    support::BuiltinFn::kDeleteIndex,
    support::BuiltinFn::kInsert,
    support::BuiltinFn::kPushBack,
    support::BuiltinFn::kPushFront,
    support::BuiltinFn::kPopBack,
    support::BuiltinFn::kPopFront,
    support::BuiltinFn::kQueueSlice,
    support::BuiltinFn::kAssignQueueSlice,
    support::BuiltinFn::kQueueAllocate,
    support::BuiltinFn::kQueueHoldToBound,
    support::BuiltinFn::kQueueFromArray,
    support::BuiltinFn::kWindowToBitVec,
    support::BuiltinFn::kWithWindow,
    support::BuiltinFn::kReverse,
    support::BuiltinFn::kApplyOrder,
};

// An associative array's (LRM 7.8, 7.9): an element an index names, to read
// it and to write it, how many entries it holds, whether an index names one,
// removing one or every entry, where the first, last, next and previous index
// lies (LRM 7.9.4 -- 7.9.7), and the value an absent index reads, as a
// `default:` clause gives it (LRM 7.9.11).
inline constexpr std::array kAssociativeArrayMethods{
    support::BuiltinFn::kSize,
    support::BuiltinFn::kExists,
    support::BuiltinFn::kAssocElement,
    support::BuiltinFn::kAssocElementMut,
    support::BuiltinFn::kDelete,
    support::BuiltinFn::kAssocDeleteIndex,
    support::BuiltinFn::kAssocFirst,
    support::BuiltinFn::kAssocLast,
    support::BuiltinFn::kAssocNext,
    support::BuiltinFn::kAssocPrev,
    support::BuiltinFn::kAssocStatedDefault,
    support::BuiltinFn::kAssocSetStatedDefault,
};

// The language traits an array whose elements a position names implements:
// it is a memory a memory task loads and dumps (LRM 21.4, 21.5), an actual a
// DPI-C open array formal takes (LRM 35.5.6.1), and a random variable (LRM
// 18.4).
inline constexpr auto kPositionedArrayTraits =
    std::to_array<const support::LanguageTrait*>(
        {&support::kMemoryWords, &support::kOpenArrayImage,
         &support::kRandomWalk});

// An associative array's: it is a memory, addressed by its keys (LRM 21.4.1),
// and no open array formal takes one (LRM 35.5.6.1). It is a random variable
// too (LRM 18.4), whose walk is not yet carried out.
inline constexpr auto kAssociativeArrayTraits =
    std::to_array<const support::LanguageTrait*>({&support::kMemoryWords});

// How many operations `methods` and the requirements of `traits` are together.
consteval auto OperationCount(
    std::span<const support::BuiltinFn> methods,
    std::span<const support::LanguageTrait* const> traits) -> std::size_t {
  std::size_t count = methods.size();
  for (const support::LanguageTrait* trait : traits) {
    count += trait->requirements.size();
  }
  return count;
}

// Every operation a kind is called by: its `kMethods`, then the requirements of
// each of its `kTraits` in turn.
template <const auto& kMethods, const auto& kTraits>
consteval auto OperationsOf()
    -> std::array<support::BuiltinFn, OperationCount(kMethods, kTraits)> {
  std::array<support::BuiltinFn, OperationCount(kMethods, kTraits)>
      operations{};
  std::size_t at = 0;
  for (const support::BuiltinFn method : kMethods) {
    operations.at(at++) = method;
  }
  for (const support::LanguageTrait* trait : kTraits) {
    for (const support::BuiltinFn requirement : trait->requirements) {
      operations.at(at++) = requirement;
    }
  }
  return operations;
}

inline constexpr auto kFixedArrayOperations =
    OperationsOf<kFixedArrayMethods, kPositionedArrayTraits>();
inline constexpr auto kDynamicArrayOperations =
    OperationsOf<kDynamicArrayMethods, kPositionedArrayTraits>();
inline constexpr auto kQueueOperations =
    OperationsOf<kQueueMethods, kPositionedArrayTraits>();
inline constexpr auto kAssociativeArrayOperations =
    OperationsOf<kAssociativeArrayMethods, kAssociativeArrayTraits>();

// Whether `operations` and `rows` name the same operations in the same order,
// which a table building each operation of a kind is held to at compile time.
template <typename Row, std::size_t N>
consteval auto NamesTheSameOperations(
    std::span<const support::BuiltinFn> operations,
    const std::array<Row, N>& rows) -> bool {
  if (operations.size() != rows.size()) {
    return false;
  }
  for (std::size_t i = 0; i < rows.size(); ++i) {
    if (operations[i] != rows[i].name) {
      return false;
    }
  }
  return true;
}

}  // namespace lyra::mir
