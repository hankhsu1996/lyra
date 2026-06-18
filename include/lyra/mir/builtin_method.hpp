#pragma once

#include <cstdint>
#include <variant>

#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

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

// LRM 15.5 named event operations. Trigger is non-suspending; Await suspends
// the caller until the event fires; Triggered is a pure read of the
// slot-persistent triggered state.
enum class EventMethodKind : std::uint8_t {
  kTrigger,
  kAwait,
  kTriggered,
};

// LRM 7.5.2 / 7.5.3 dynamic-array methods plus the LRM 7.12 array-method
// family (ordering, reduction, and the 7.12.1 locator methods). The locator
// arms (`kFind` onward) return a queue -- an element queue for the value
// locators and an `int` queue for the index locators.
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

// LRM 7.10.2 queue-native methods. Exclusive to the queue container: they
// mutate the receiver, and `pop_front` / `pop_back` also return the removed
// element. The shared LRM 7.12 family stays in `ArrayMethodKind`.
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
// `num` / `size` query the entry count, `exists` tests a key, `delete` removes
// one entry or clears the array. The traversal family (`first` / `last` /
// `next` / `prev`, LRM 7.9.4 -- 7.9.7) assigns the visited key through a `ref`
// index argument and returns 0 / 1 / -1. The shared LRM 7.12 family stays in
// `ArrayMethodKind`.
enum class AssociativeMethodKind : std::uint8_t {
  kNum,
  kSize,
  kExists,
  kDelete,
  kFirst,
  kLast,
  kNext,
  kPrev,
};

// Side-effect-free queries that are defined for every runtime value type
// (string, packed integral, unpacked array, ...). The receiver's MIR type
// alone determines the emit shape; lowering passes the operand uniformly.
// The LRM 21.3.4.3 "unknown bits in $sscanf str / format imply EOF" guard
// lowers to kIsUnknown; future LRM 20.9 `$isunknown` lowers to the same
// leaf.
enum class ValueMethodKind : std::uint8_t {
  kIsUnknown,
};

// LRM 7.12.4 iterator intrinsic methods (only `index` is in scope today).
enum class IteratorMethodKind : std::uint8_t {
  kIndex,
};

// Methods on the runtime scope handle (the `self` receiver). `kServices`
// reaches the engine facade `RuntimeServices` -- the engine handle every
// runtime-effect call threads as a plain argument
// (docs/decisions/runtime-effects-as-generic-calls.md).
enum class ScopeMethodKind : std::uint8_t {
  kServices,
};

struct EnumMethodInfo {
  TypeId enum_type;
  EnumMethodKind kind;
};

struct StringMethodInfo {
  StringMethodKind kind;
};

struct EventMethodInfo {
  EventMethodKind kind;
};

struct ArrayMethodInfo {
  ArrayMethodKind kind;
};

struct QueueMethodInfo {
  QueueMethodKind kind;
};

struct AssociativeMethodInfo {
  AssociativeMethodKind kind;
};

struct ValueMethodInfo {
  ValueMethodKind kind;
};

struct IteratorMethodInfo {
  IteratorMethodKind kind;
};

struct ScopeMethodInfo {
  ScopeMethodKind kind;
};

struct BuiltinMethodCallee {
  std::variant<
      EnumMethodInfo, StringMethodInfo, EventMethodInfo, ArrayMethodInfo,
      QueueMethodInfo, AssociativeMethodInfo, ValueMethodInfo,
      IteratorMethodInfo, ScopeMethodInfo>
      method;
};

}  // namespace lyra::mir
