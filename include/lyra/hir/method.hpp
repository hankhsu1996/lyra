#pragma once

#include <cstdint>
#include <variant>

namespace lyra::hir {

// LRM 6.19.5 -- enum intrinsic methods.
enum class EnumMethodKind : std::uint8_t {
  kFirst,
  kLast,
  kNum,
  kNext,
  kPrev,
  kName,
};

// LRM 6.16.1 through 6.16.15 -- string intrinsic methods.
enum class StringMethodKind : std::uint8_t {
  kLen,
  kGetc,
  kPutc,
  kToupper,
  kTolower,
  kCompare,
  kIcompare,
  kSubstr,
  kAtoi,
  kAtohex,
  kAtooct,
  kAtobin,
  kAtoreal,
  kItoa,
  kHextoa,
  kOcttoa,
  kBintoa,
  kRealtoa,
};

// LRM 15.5 named event operations. Trigger / Await arise from collapsing
// `-> e;` and `@e;` at HIR -> MIR; Triggered surfaces from slang's
// `e.triggered` method call.
enum class EventMethodKind : std::uint8_t {
  kTrigger,
  kAwait,
  kTriggered,
};

// LRM 7.5.2 / 7.5.3 dynamic-array methods plus the LRM 7.12 array-method
// family (ordering, reduction, and the 7.12.1 locator methods). The arms
// are named for "array" rather than "dynamic array" because the method
// semantics are LRM-uniform across container kinds (fixed unpacked, queue);
// only the AST -> HIR receiver-detection block differs per container, and
// other containers will extend the routing without renaming this enum.
//
// The 7.12.1 locator arms (`kFind` onward) return a queue: an element queue
// for the value locators (`find`, `find_first`, `find_last`, `min`, `max`,
// `unique`) and an `int` queue for the index locators (`find_index`,
// `find_first_index`, `find_last_index`, `unique_index`).
enum class ArrayMethodKind : std::uint8_t {
  kSize,
  kDelete,
  kReverse,
  kSort,
  kRsort,
  kSum,
  kProduct,
  kAnd,
  kOr,
  kXor,
  kFind,
  kFindIndex,
  kFindFirst,
  kFindFirstIndex,
  kFindLast,
  kFindLastIndex,
  kMin,
  kMax,
  kUnique,
  kUniqueIndex,
};

// LRM 7.10.2 queue-native methods. These are exclusive to the queue container
// (no dynamic-array / fixed-unpacked analogue): they mutate the receiver in
// place, and `pop_front` / `pop_back` also return the removed element. The
// LRM 7.12 ordering / reduction / locator family that queues share with the
// other unpacked arrays stays in `ArrayMethodKind`.
enum class QueueMethodKind : std::uint8_t {
  kSize,
  kInsert,
  kDelete,
  kPopFront,
  kPopBack,
  kPushFront,
  kPushBack,
};

// LRM 7.9 associative-array methods. Exclusive to the associative container:
// `num` / `size` query the entry count, `exists` tests a key, and `delete`
// removes one entry (with index) or clears the array (without). Like the
// queue-native family they mutate or query the receiver and have no analogue
// in the other containers; the shared LRM 7.12 family stays in
// `ArrayMethodKind`.
enum class AssociativeMethodKind : std::uint8_t {
  kNum,
  kSize,
  kExists,
  kDelete,
};

// LRM 7.12.4 iterator intrinsic methods (only `index` is in scope today;
// extends naturally if SV adds more iterator methods).
enum class IteratorMethodKind : std::uint8_t {
  kIndex,
};

struct BuiltinMethodRef {
  std::variant<
      EnumMethodKind, StringMethodKind, EventMethodKind, ArrayMethodKind,
      QueueMethodKind, AssociativeMethodKind, IteratorMethodKind>
      method;
};

}  // namespace lyra::hir
