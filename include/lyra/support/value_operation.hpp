#pragma once

#include <cstdint>
#include <string_view>
#include <variant>

#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/value_step.hpp"

namespace lyra::support {

// The items of the equality trait: the equality operators the language
// defines on a value of any type and its case equality (LRM 11.4.5, 11.4.6),
// as Rust's `PartialEq` holds `eq` and `ne`. A type the compiler builds the
// operations of implements them with bodies of its own; any other type's are
// the library's.
enum class EqualityItem : std::uint8_t {
  kEqual,
  kNotEqual,
  kCaseEqual,
};

// The item of the change-test trait: whether a write left a value as it was,
// which the holder of a variable asks (LRM 9.4.2). The library's entry of the
// same name answers it for a type the compiler builds no operations of.
enum class ChangeTestItem : std::uint8_t {
  kBitIdentical,
};

// The items of the unknown-bit trait: whether a value holds an x or a z (LRM
// 20.9), and the value a conditional whose condition is unknown answers (LRM
// 11.4.11). The library's entries of the same names answer them for a type the
// compiler builds no operations of.
enum class UnknownItem : std::uint8_t {
  kHasUnknown,
  kMergeConditional,
};

// The items of the stream trait (LRM 6.24.3): how many bits a value streams
// and how many of them hold a value, the value as the bits of a fixed width
// and built back from them, and the same where the width is the running
// program's. The library's entries of the same names answer them for a type
// the compiler builds no operations of.
enum class StreamItem : std::uint8_t {
  kBitstreamWidth,
  kCountBits,
  kToBitstream,
  kFromBitstream,
  kToBitVec,
  kFromBitVec,
};

// The items of the positions trait, over the positions a value of a type
// numbers, the first least significant: a value taking a run of them from
// another of its type, which a write of some of a value's parts at once is
// built of (LRM 7.6); how many there are; every one read out as one integral
// value, and a value built back from such; and the ones a view reaches written
// in where the value lies, answering whether any changed, which a net resolving
// each position from its own drivers asks (LRM 6.6.7). The library's entries of
// the same names answer them for a type the compiler builds no operations of.
enum class PositionsItem : std::uint8_t {
  kTakePositions,
  kPositionCount,
  kToPositions,
  kWritePositions,
  kFromPositions,
};

// The items of the default trait: what a value of a type is before anything
// is written to it (LRM Table 6-7, 7.2.2), a run-time-sized container's the
// empty container. The library's entry of the same name answers it for a type
// the compiler builds no operations of.
enum class DefaultItem : std::uint8_t {
  kDeclaredDefault,
};

// The items of the element-access trait of an array whose elements a machine
// index reaches: the element at an index, to read and to write, which a body
// walking the elements asks with no check (LRM 7.4.5 leaves a position the
// program computed to the element access), and where a write through a
// position lands (LRM 7.10.1).
enum class ElementAccessItem : std::uint8_t {
  kGetUnchecked,
  kGetUncheckedMut,
  kElementPlace,
};

// The item of the resize trait: the array made a count long, which
// randomization asks (LRM 18.4).
enum class ResizeItem : std::uint8_t {
  kResized,
};

// An item of a trait named, and called, as the library's entry of the same
// name is; for a type the compiler builds no operations of, that entry
// answers it.
using LibraryAnsweredItem = std::variant<
    ChangeTestItem, UnknownItem, StreamItem, PositionsItem, DefaultItem,
    ElementAccessItem, ResizeItem>;

// The library entry answering an item for a type the compiler builds no
// operations of, which names the item and says how it is called.
[[nodiscard]] auto LibraryEntryOf(ChangeTestItem item) -> BuiltinFn;
[[nodiscard]] auto LibraryEntryOf(UnknownItem item) -> BuiltinFn;
[[nodiscard]] auto LibraryEntryOf(StreamItem item) -> BuiltinFn;
[[nodiscard]] auto LibraryEntryOf(PositionsItem item) -> BuiltinFn;
[[nodiscard]] auto LibraryEntryOf(DefaultItem item) -> BuiltinFn;
[[nodiscard]] auto LibraryEntryOf(ElementAccessItem item) -> BuiltinFn;
[[nodiscard]] auto LibraryEntryOf(ResizeItem item) -> BuiltinFn;
[[nodiscard]] auto LibraryEntryOf(LibraryAnsweredItem item) -> BuiltinFn;

// The conversions of an array holding a bounded queue below its outermost
// dimension into the form of its type with no queue bounded, and back, which
// every such type has and the compiler builds the bodies of (LRM 7.6, 7.10.5).
// The library's entries of the same names state how each is called and answer
// none.
enum class BoundFreeItem : std::uint8_t {
  kToBoundFree,
  kFromBoundFree,
};

// The library entry naming a bound-free conversion and saying how it is
// called.
[[nodiscard]] auto LibraryEntryOf(BoundFreeItem item) -> BuiltinFn;

// An operation a type may answer with a body of its own: an item of the
// equality trait, an item of a trait the library also answers, a bound-free
// conversion, one of an array kind's own methods by the name the source asks
// it by, or a step of the value's life. A value the library holds is asked an
// item by the library entry's name. Only a type whose steps are not built from
// its parts -- a run-time-sized container, whose elements are no parts a type
// lays out -- answers a step with a body.
using ValueOperation = std::variant<
    EqualityItem, LibraryAnsweredItem, BoundFreeItem, BuiltinFn, ValueStep>;

// Whether the operation is asked of a value, which the body answering it takes
// as its receiver, rather than of a type, which builds one.
[[nodiscard]] auto ReachesAReceiver(ValueOperation operation) -> bool;

// Whether the body answering the operation is handed where its caller wrote it,
// as the entry it answers is: what a type's own body does can fail wherever
// what it holds can.
[[nodiscard]] auto TakesCallerLocation(ValueOperation operation) -> bool;

// The operation's stable spelling: the operator's token, or the entry's name.
[[nodiscard]] auto ValueOperationName(ValueOperation operation)
    -> std::string_view;

}  // namespace lyra::support
