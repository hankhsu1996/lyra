#pragma once

// The variables VPI reaches and what each answers (LRM 37.15 to 37.26): a
// variable a scope or a namespace declares, the parts of one -- a member, an
// element, a bit, an element of a packed array -- the name a `ref` port gives
// the variable connected to it, a typespec and the typespecs it is built from,
// a range, and the constants a relation answers. The value of a variable is
// read and written through the bodies the class declaring it states, which
// are compiled where its type is known, so nothing here reads storage itself;
// which bits of a packed value a part is, is decided here.

#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

#include "lyra/abi/records.hpp"
#include "lyra/runtime/hierarchical_name.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/vpi_object.hpp"
#include "lyra/value/bit_vec.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/string.hpp"
#include "vpi_user.h"

namespace lyra::runtime {

class RuntimeEffects;

// Whether `object` is one of the objects this file answers for.
[[nodiscard]] auto IsVariablePart(const VpiObject& object) -> bool;

// The data object `part` names directly in `enclosing` of what `holder`
// declares -- none for one written in no block -- with the indices the name
// writes stepped into it; none where none is so named or an index names
// nothing (LRM 38.21, 37.17 detail 28).
[[nodiscard]] auto VariableNamed(
    const ScopeHolder& holder, const abi::LexicalScopeRecord* enclosing,
    const NamePart& part) -> VpiAnswer<std::optional<VpiObject>>;

// The member `part` names of `at`, with the indices the name writes stepped
// into it, none where it has no member so named.
[[nodiscard]] auto MemberNamed(const VariableObject& at, const NamePart& part)
    -> VpiAnswer<std::optional<VpiObject>>;

// Every variable of the run by the storage a wait on it watches, read to find
// the variable a `ref` port's reference is bound to (LRM 37.15 detail 3). A
// reference port is bound to a whole variable while the design is built, and
// nothing binds one again, so the table is built once, the first time it is
// asked, and each question after costs one look.
class VariablesByStorage {
 public:
  [[nodiscard]] auto Find(RuntimeEffects& run, const void* storage)
      -> std::optional<VariableObject>;

 private:
  std::optional<std::unordered_map<const void*, VariableObject>> table_;
};

// The variables, or the references, of type `relation` declared directly in
// `enclosing` of what `holder` declares (LRM 37.15, 37.17, 37.20, 36.12.1),
// none where `relation` relates a scope to nothing this file answers for.
[[nodiscard]] auto VariablesDeclaredIn(
    const ScopeHolder& holder, const abi::LexicalScopeRecord* enclosing,
    PLI_INT32 relation, VpiMode mode) -> std::optional<std::vector<VpiObject>>;

// The part of `of` at `indices`, one index per dimension stepped into in turn
// (LRM 38.19, 38.20), none where an index names nothing.
[[nodiscard]] auto VariableElementAt(
    const VariableObject& of, std::span<const std::int64_t> indices)
    -> VpiAnswer<std::optional<VpiObject>>;

// Whether `a` and `b` are one object (LRM 38.3): a part of a packed value is
// the bits it is of that value, however the steps to it were written, so
// `ps[0]` and `ps.b[7]` of a structure whose last member `b` is eight bits are
// one bit.
[[nodiscard]] auto SameVariablePart(const VpiObject& a, const VpiObject& b)
    -> bool;

// The type of `object` (LRM 37.3.1) in `mode`.
[[nodiscard]] auto TypeOfVariablePart(const VpiObject& object, VpiMode mode)
    -> PLI_INT32;

// The type of the object `object` stands for in `mode`: of a reference object,
// the one it is bound to (LRM 37.15 detail 3), and of any other, its own --
// what decides what a value of it is and what a change of it calls back.
[[nodiscard]] auto TypeOfActual(const VpiObject& object, VpiMode mode)
    -> PLI_INT32;

// The objects of type `relation` related to `from`, and the one where only
// one can be.
[[nodiscard]] auto RelatedToVariablePart(
    PLI_INT32 relation, const VpiObject& from, VpiMode mode)
    -> VpiAnswer<std::vector<VpiObject>>;
[[nodiscard]] auto OneRelatedToVariablePart(
    RuntimeEffects& run, PLI_INT32 relation, const VpiObject& from)
    -> VpiAnswer<std::optional<VpiObject>>;

// The number a step of a selection selects by, as `vpiIndex` answers it (LRM
// 37.18 detail 6, 37.19), which a callback on an element is also handed (LRM
// 38.36.1): a number, or an integral key of 64 bits or fewer by the number it
// is at its type's signedness. A key of any other type is no number.
[[nodiscard]] auto IndexOfStep(const Selection::Step& step)
    -> std::optional<std::int64_t>;

// An integer property and a string property of `of`.
[[nodiscard]] auto VariablePartIntegerProperty(
    PLI_INT32 property, const VpiObject& of, VpiMode mode)
    -> VpiAnswer<PLI_INT64>;
[[nodiscard]] auto VariablePartStringProperty(
    PLI_INT32 property, const VpiObject& of, VpiMode mode)
    -> VpiAnswer<std::optional<std::string>>;

// A value a variable holds or is written, as a reader by name hands it on:
// the bits of an integral value, a real, or a string. Whether the bits are
// signed is the object's, which its shape states.
using HeldValue = std::variant<value::BitVec, double, value::String>;

// What a value of an object is -- an integral value of a width, signed or
// not, a real, or a string -- and the format LRM 38.15 hands it in where the
// reader asks for the object's own.
struct IntegralShape {
  std::uint64_t width = 0;
  bool is_signed = false;
};
struct RealShape {};
struct TextShape {};
struct ValueShape {
  std::variant<IntegralShape, RealShape, TextShape> held;
  PLI_INT32 natural_format = vpiObjTypeVal;
};

// The shape of `object`'s value; an answer of nothing for an object with no
// value (LRM 37.17 detail 11, 37.26 detail 1).
[[nodiscard]] auto ValueShapeOf(const VpiObject& object)
    -> VpiAnswer<ValueShape>;

// The trigger a wait for a change of what `object` names would enrol (LRM
// 9.4.2), so whatever is told of its changes is told as the design's own wait
// on that select would be: at the part its selection names, through what a
// net shows and a port is bound to, reading the bits the object is where it is
// bits of a packed value. Where the path a wait watches stops at a part
// holding the object -- an associative array, an untagged union -- it watches
// all of that part.
[[nodiscard]] auto WatchOfVariablePart(const VariableObject& object) -> Trigger;

// The value `object` holds now, read as the design's own read of it would be
// (LRM 38.15); and a write of `written` to it as the design's own write
// would land (LRM 38.34), what waits on it seeing the write. What the design
// cannot do of either is that request's error, and the run goes on.
[[nodiscard]] auto ReadVariablePart(const VpiObject& object)
    -> VpiAnswer<HeldValue>;
[[nodiscard]] auto WriteVariablePart(
    const VpiObject& object, const HeldValue& written) -> VpiAnswer<void>;

// The object the class variable `object` names now, read as the design's own
// read of it would be. No foreign code is handed it (LRM 38.36.1).
[[nodiscard]] auto ObjectNamedByVariablePart(const VpiObject& object)
    -> VpiAnswer<value::ObjectRef>;

// A procedural force of `object` to `forced`, and its release, answering the
// value it holds after (LRM 38.34, 10.6.2).
[[nodiscard]] auto ForceVariablePart(
    const VpiObject& object, const HeldValue& forced) -> VpiAnswer<void>;
[[nodiscard]] auto ReleaseVariablePart(const VpiObject& object)
    -> VpiAnswer<HeldValue>;

}  // namespace lyra::runtime
