#pragma once

#include <array>
#include <cstdint>
#include <span>

#include "lyra/support/value_operation.hpp"
#include "lyra/support/value_step.hpp"

namespace lyra::support {

// What the runtime library asks, by name, of every value of a type
// implementing it, wherever the library's own code holds such a value: the
// operations it requires, as a Rust trait lists its required methods. A type
// implementing one answers each requirement with its own operation of the
// same name, and every target reaches that one operation, however its library
// is told of it -- a table of the operations' addresses, or a class its
// templates are instantiated at.
struct LibraryTrait {
  std::span<const ValueOperation> requirements;
};

// A value's life: its default and a copy of a value, each answering the value
// it builds, writing a copy over a value, and ending one. A move is none of
// them: it relocates a value whatever the type.
inline constexpr auto kValueLifeRequirements = std::to_array<ValueOperation>(
    {DefaultItem::kDeclaredDefault, ValueStep::kCopy, ValueStep::kAssign,
     ValueStep::kEnd});
inline constexpr LibraryTrait kValueLife{
    .requirements = kValueLifeRequirements};

// Whether a write changed a value, which the holder of any variable asks (LRM
// 9.4.2).
inline constexpr auto kChangeTestRequirements =
    std::to_array<ValueOperation>({ChangeTestItem::kBitIdentical});
inline constexpr LibraryTrait kChangeTest{
    .requirements = kChangeTestRequirements};

// An element of an array numbered by position: the one at a machine index,
// to read and to write.
inline constexpr auto kElementAccessRequirements =
    std::to_array<ValueOperation>(
        {ElementAccessItem::kGetUnchecked,
         ElementAccessItem::kGetUncheckedMut});
inline constexpr LibraryTrait kElementAccess{
    .requirements = kElementAccessRequirements};

// Where a write through an index of an array lands -- a position, or an
// associative array's index -- which the library's own steps within a write
// ask (LRM 7.4.5, 7.8.7, 7.10.1).
inline constexpr auto kIndexedWriteRequirements =
    std::to_array<ValueOperation>({ElementAccessItem::kElementPlace});
inline constexpr LibraryTrait kIndexedWrite{
    .requirements = kIndexedWriteRequirements};

// The value made a count long, which randomization asks (LRM 18.4).
inline constexpr auto kResizeRequirements =
    std::to_array<ValueOperation>({ResizeItem::kResized});
inline constexpr LibraryTrait kResize{.requirements = kResizeRequirements};

// A takeover: some positions of a value taken from another, which a procedural
// continuous assignment holding part or all of a variable asks (LRM 10.6.2). A
// value held whole is one position, so taking any of it takes all of it.
inline constexpr auto kTakeoverRequirements =
    std::to_array<ValueOperation>({PositionsItem::kTakePositions});
inline constexpr LibraryTrait kTakeover{.requirements = kTakeoverRequirements};

// What a net of a type asks of a value of it, which it resolves position by
// position from its drivers (LRM 6.7.1): its positions read out whole, how
// many it has, and some of them written in.
inline constexpr auto kNetPositionsRequirements = std::to_array<ValueOperation>(
    {PositionsItem::kToPositions, PositionsItem::kPositionCount,
     PositionsItem::kWritePositions});
inline constexpr LibraryTrait kNetPositions{
    .requirements = kNetPositionsRequirements};

// The questions about a whole value a library holding values of any type asks
// of each one: its equality (LRM 11.4.5), and its case equality and whether
// it holds an unknown bit (LRM 11.4.6).
inline constexpr auto kWholeValueQuestionsRequirements =
    std::to_array<ValueOperation>(
        {EqualityItem::kEqual, EqualityItem::kNotEqual,
         EqualityItem::kCaseEqual, UnknownItem::kHasUnknown});
inline constexpr LibraryTrait kWholeValueQuestions{
    .requirements = kWholeValueQuestionsRequirements};

// A value as a stream of bits (LRM 6.24.3): how wide it streams, how many of
// its bits hold a value, and the value as bits.
inline constexpr auto kStreamRequirements = std::to_array<ValueOperation>(
    {StreamItem::kBitstreamWidth, StreamItem::kCountBits,
     StreamItem::kToBitVec});
inline constexpr LibraryTrait kStream{.requirements = kStreamRequirements};

// A value read back out of a stream of bits, which a type that can be a
// stream's destination has (LRM 11.4.14.3).
inline constexpr auto kStreamReadRequirements =
    std::to_array<ValueOperation>({StreamItem::kFromBitVec});
inline constexpr LibraryTrait kStreamRead{
    .requirements = kStreamReadRequirements};

// The same for a value whose stream has a width its type fixes: the value as
// exactly that many bits and back.
inline constexpr auto kFixedStreamRequirements = std::to_array<ValueOperation>(
    {StreamItem::kBitstreamWidth, StreamItem::kCountBits,
     StreamItem::kToBitstream, StreamItem::kFromBitstream});
inline constexpr LibraryTrait kFixedStream{
    .requirements = kFixedStreamRequirements};

// When a type of a library declaration implements a trait: always, or
// where a value of it has a stream of bits (LRM 6.24.3), as a Rust impl may
// hold only where its type arguments meet a bound.
enum class ImplementedWhere : std::uint8_t { kAlways, kItStreams };

// A trait a library declaration's types implement, and when.
struct TraitImplementation {
  const LibraryTrait* trait;
  ImplementedWhere where;
};

}  // namespace lyra::support
