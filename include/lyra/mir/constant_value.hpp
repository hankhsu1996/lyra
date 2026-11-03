#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/base/interner.hpp"
#include "lyra/mir/constant_id.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

struct ConstantValue;

// The bits of a value of an integral type, canonical for that type: one word
// per 64 bits of its width, the top word's bits above the width cleared, and
// the state plane empty unless the type carries one. Two spellings of one
// value would otherwise be two entries, and a consumer reading a plane would
// see bits the type does not have.
struct IntegralValue {
  IntegralConstant planes;

  auto operator==(const IntegralValue&) const -> bool = default;
};

// A machine integer, or a machine boolean as 0 or 1.
struct MachineIntValue {
  std::int64_t value = 0;

  auto operator==(const MachineIntValue&) const -> bool = default;
};

// A pointer naming nothing.
struct NullValue {
  auto operator==(const NullValue&) const -> bool = default;
};

// A NUL-terminated text, as the machine C string a library record holds.
struct CStringValue {
  std::string text;

  auto operator==(const CStringValue&) const -> bool = default;
};

// The address of what `to` names, which is a value of `type`: data the
// program holds before it runs -- another constant of the unit, a class's
// definition, a variable of a namespace -- or code, which is its own address.
// The pointer may be held at another type than a pointer to `type`: an entry
// held where a table holds every entry at one prototype, an address held where
// the library reads data of no type.
struct PointerValue {
  ReferenceTarget to;
  TypeId type;

  auto operator==(const PointerValue&) const -> bool = default;
};

// A structure of the runtime library, a part per member it declares, in its
// order.
struct RecordValue {
  std::vector<ConstantValue> parts;

  auto operator==(const RecordValue&) const -> bool;
};

// Values of the element type, one after another, as many as the array's type
// says.
struct ArrayValue {
  std::vector<ConstantValue> elements;

  auto operator==(const ArrayValue&) const -> bool;
};

// A view of every entry of the array `array`, another constant of the unit,
// which holds as many as that array's type says.
struct SpanValue {
  ConstantId array;

  auto operator==(const SpanValue&) const -> bool = default;
};

// A value settled before the program runs, as what it is: its bits, its
// parts, the addresses it holds. Nothing in it runs, so a target writes it as
// data from it alone, reading only the type it is a value of for how each
// part is laid out. It carries no type of its own, as clang's `APValue` and
// rustc's `ConstValue` carry none; what holds one states the type.
struct ConstantValue {
  std::variant<
      IntegralValue, MachineIntValue, NullValue, CStringValue, PointerValue,
      RecordValue, ArrayValue, SpanValue>
      value;

  auto operator==(const ConstantValue&) const -> bool = default;
};

// Folds `value` into `seed`, for a key that holds a value among its parts.
void HashConstantValue(std::size_t& seed, const ConstantValue& value);

// One value a unit holds and the type it is a value of. Both belong to its
// identity: the same bits at two types are two values, because the type
// decides how they are read and laid out.
struct ConstantDecl {
  TypeId type;
  ConstantValue value;

  auto operator==(const ConstantDecl&) const -> bool = default;

  struct Hash {
    auto operator()(const ConstantDecl& decl) const -> std::size_t;
  };
};

// The values one compilation unit settled before the program runs. Two
// occurrences of one value at one type reach one entry, which is what lets the
// unit state it once and every use name which one, as clang's `ASTContext`
// keeps one unnamed global constant per type and value.
using ConstantPool =
    base::Interner<ConstantDecl, ConstantId, ConstantDecl::Hash>;

// Whether a pointer to what `to` names is that name's address taken -- data the
// program holds before it runs: a constant of the unit, a class's definition,
// a variable of a namespace or a class -- rather than the name read as a
// value, which for code and a table a target lays out is already an address.
// A local has no address a constant could hold.
[[nodiscard]] auto PointsAtData(const ReferenceTarget& to) -> bool;

// The type of the pointer `value` is, before it is held at any other: a
// read-only pointer to what it names where that is data, and what naming it
// answers otherwise.
[[nodiscard]] auto OwnPointerType(
    const TypePool& types, const PointerValue& value) -> TypeId;

// Calls `visit` with what every pointer `value` holds names, and with what
// every constant of the unit it reaches holds names, each reached constant
// once: everything an artifact defining `value` refers to.
void ForEachNameReached(
    const ConstantPool& constants, const ConstantValue& value,
    const std::function<void(const ReferenceTarget&)>& visit);

// The bits of the integral constant `data` names, or nothing where it names
// none, for whatever folds over or tests the value of an operand.
[[nodiscard]] auto IntegralPlanesNamedBy(
    const ConstantPool& constants, const ExprData& data)
    -> const IntegralConstant*;

}  // namespace lyra::mir
