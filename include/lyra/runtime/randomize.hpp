#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <optional>
#include <type_traits>

#include "lyra/runtime/object_change.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/randomization.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/enumeration.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/value_witnesses_of.hpp"

namespace lyra::runtime {

class RuntimeEffects;

// The type a randomization reads a value of `T` through: an integral one's own
// record, which states its width, and otherwise the type code holding a value
// of `T` names, whose parts state theirs.
template <value::LyraValue T>
[[nodiscard]] auto ConstraintTypeOf() -> const value::ValueType& {
  if constexpr (value::IntegralValue<T>) {
    return value::IntegralValueType::Of<T>().Type();
  } else {
    return value::kValueTypeOf<T>;
  }
}

// A call of `randomize()` (LRM 18.6, 18.7, 18.11, 18.12) is the steps below,
// which the code calling it runs in order: begin, name the variables the call
// lists, let the object state its constraints, add those written with the
// call, and finish. Beginning runs `pre_randomize()` on the object and every
// random object it reaches (LRM 18.6.2) and answers the problem the call
// fills; finishing solves it, lands the answer, runs `post_randomize()` where
// there was one, and answers 1 or 0 (LRM 18.6.1, 18.6.3).
//
// A class's `randomize()` draws from the object's own generator (LRM 18.14.3);
// `std::randomize()` from the running thread's (LRM 18.14.2) and always names
// the variables it chooses.
[[nodiscard]] auto RandomizeObject(GcObject* object, RuntimeEffects& runtime)
    -> value::ObjectRef;
[[nodiscard]] auto RandomizeObjectListed(
    GcObject* object, RuntimeEffects& runtime) -> value::ObjectRef;
[[nodiscard]] auto RandomizeScope(RuntimeEffects& runtime) -> value::ObjectRef;

[[nodiscard]] inline auto RandomizeObject(
    const value::ObjectRef& handle, RuntimeEffects& runtime)
    -> value::ObjectRef {
  return RandomizeObject(ObjectRootOf(handle), runtime);
}
[[nodiscard]] inline auto RandomizeObjectListed(
    const value::ObjectRef& handle, RuntimeEffects& runtime)
    -> value::ObjectRef {
  return RandomizeObjectListed(ObjectRootOf(handle), runtime);
}

// Each variable a call lists crosses as a reference to it, and as the value it
// holds now, which carries its type. A variable that is an array is each of
// its elements, at any depth, and its size where the program chooses it, as
// though its mode were turned on (LRM 18.11); `members` are those of the
// enumeration each element is, where it is one, and `within` the array whose
// size the program chooses that the storage lies in.
void RandomizeListErased(
    const value::ObjectRef& problem, const ErasedReference& variable,
    const void* value, const value::ValueType& type,
    const value::Enumeration* members, std::optional<std::int64_t> within);
[[nodiscard]] auto RandomizeListArrayErased(
    const value::ObjectRef& problem, const ErasedReference& array,
    ArrayResizer resize) -> std::int64_t;

void RandomizeConstrain(const value::ObjectRef& problem);
[[nodiscard]] auto RandomizeFinish(const value::ObjectRef& problem)
    -> value::Int;

// What a part of a random variable takes from the variable it lies in, as the
// declaration walks it: the path its owner keeps its mode and its cycle under,
// whether every array it lies in is chosen, the members of the enumeration it
// is where it is one, and the innermost array around it whose size the
// program chooses.
struct DeclaredPart {
  RandomVariablePath path;
  bool enabled;
  const value::Enumeration* members;
  std::optional<std::int64_t> within;

  // The part at `position` of this one, where whether this one is chosen is
  // `chosen` and `array` is the array around that part whose size the program
  // chooses.
  [[nodiscard]] auto Into(
      std::size_t position, bool chosen,
      std::optional<std::int64_t> array) const -> DeclaredPart {
    return DeclaredPart{
        .path = path.Into(position),
        .enabled = chosen,
        .members = members,
        .within = array};
  }
};

// The whole of the variable `variable` names, as a declaration starts walking
// it.
[[nodiscard]] auto DeclaredWhole(
    const value::ObjectRef& problem, const ErasedReference& variable)
    -> DeclaredPart;

// What a class's description states, each through the problem it is handed:
// a random variable, by how the source declared it (LRM 18.4), which every
// object of the call declares before any states a constraint; and a variable a
// constraint reads, answering its term, which is the one declared there or,
// where none was, one not declared random (LRM 18.3). Whether one declared
// random is chosen is its owner's mode (LRM 18.8) unless the call lists what
// it chooses. A variable that is an array is each of its elements, at any
// depth, each chosen where its own mode and those of every array it lies in
// are on, `part` saying what it takes from the variable it lies in.
void ConstraintDeclareErased(
    const value::ObjectRef& problem, const ErasedReference& variable,
    const void* value, const value::ValueType& type, RandomQualifier declared,
    const DeclaredPart& part);

// The variable `variable` names is of an enumerated type, or an array of one,
// declaring `members` (LRM 18.5), stated before the variable is declared or
// listed; and the members stated for it, where any were.
void ConstraintMembersOf(
    const value::ObjectRef& problem, const ErasedReference& variable,
    const value::Enumeration& members);
template <
    value::LyraValue T, std::uint64_t kWidth, bool kFourState,
    std::size_t kMembers>
void ConstraintMembers(
    const value::ObjectRef& problem, const VariableRef<T>& variable,
    const value::EnumerationMembers<kWidth, kFourState, kMembers>& members) {
  ConstraintMembersOf(problem, variable.Erased(), members.Read());
}
[[nodiscard]] auto NamedMembers(
    const value::ObjectRef& problem, const ErasedReference& variable)
    -> const value::Enumeration*;

// A random array whose size the program chooses, declared ahead of its
// elements, which are declared `within` what this answers; `resize` makes it
// as big as a constraint chose (LRM 18.4).
[[nodiscard]] auto ConstraintDeclareArrayErased(
    const value::ObjectRef& problem, const ErasedReference& array,
    ArrayResizer resize, bool enabled) -> std::int64_t;

// Whether the mode of the random variable `path` names, which `variable` names
// the storage of now, is on (LRM 18.8).
[[nodiscard]] auto RandEnabledErased(
    const ErasedReference& variable, const RandomVariablePath& path) -> bool;

// Whether `T` is an array whose size its declaration fixes.
template <class T>
concept FixedSize =
    requires { std::integral_constant<std::size_t, T::RawSize()>{}; };

// How a random array of `T` the storage `whole` names is resized: kept as far
// as it reaches and grown by elements at their default (LRM 7.5.1), written as
// an assignment to it is, and put back by writing what it held.
template <value::LyraValue T>
[[nodiscard]] auto ResizeOf(const ErasedReference& whole) -> ArrayResizer {
  return [whole](std::size_t count) -> std::function<void()> {
    const T& current = *static_cast<const T*>(whole.storage);
    auto before = std::make_shared<T>(current);
    T resized = current;
    if constexpr (requires { T::NewCopy(std::int64_t{}, current); }) {
      resized = T::NewCopy(static_cast<std::int64_t>(count), current);
    } else {
      while (resized.RawSize() < count) {
        resized.PushBack(typename T::ElementType{});
      }
      while (resized.RawSize() > count) {
        resized.DeleteIndex(
            value::Position::FromInt(
                static_cast<std::int64_t>(resized.RawSize() - 1)));
      }
    }
    WriteLaidOutThrough(whole, value::kValueTypeOf<T>, &resized);
    return [whole, before] {
      WriteLaidOutThrough(whole, value::kValueTypeOf<T>, before.get());
    };
  };
}

// The library's own walk of an array of a type generated code names, element
// by element, which code naming the type only through a value type walks by
// the parts that type answers instead.
template <value::LyraValue T>
void ConstraintDeclare(
    const value::ObjectRef& problem, const ErasedReference& variable,
    const T& value, RandomQualifier declared, const DeclaredPart& part) {
  if constexpr (value::OrdinalElements<T>) {
    const bool on = part.enabled && RandEnabledErased(variable, part.path);
    std::optional<std::int64_t> array = part.within;
    if constexpr (!FixedSize<T>) {
      array = ConstraintDeclareArrayErased(
          problem, variable, ResizeOf<T>(variable), on);
    }
    T& held = *static_cast<T*>(variable.storage);
    for (std::size_t i = 0; i < value.RawSize(); ++i) {
      ConstraintDeclare(
          problem,
          ErasedReference{.holder = variable.holder, .storage = &held.RawAt(i)},
          value.RawAt(i), declared, part.Into(i, on, array));
    }
  } else {
    ConstraintDeclareErased(
        problem, variable, &value, ConstraintTypeOf<T>(), declared, part);
  }
}

template <value::LyraValue T>
void ConstraintRand(
    const value::ObjectRef& problem, const VariableRef<T>& variable,
    const T& value) {
  ConstraintDeclare(
      problem, variable.Erased(), value, RandomQualifier::kRand,
      DeclaredWhole(problem, variable.Erased()));
}

template <value::LyraValue T>
void RandomizeListParts(
    const value::ObjectRef& problem, const ErasedReference& variable,
    const T& value, const value::Enumeration* members,
    std::optional<std::int64_t> within) {
  if constexpr (value::OrdinalElements<T>) {
    std::optional<std::int64_t> array = within;
    if constexpr (!FixedSize<T>) {
      array =
          RandomizeListArrayErased(problem, variable, ResizeOf<T>(variable));
    }
    T& held = *static_cast<T*>(variable.storage);
    for (std::size_t i = 0; i < value.RawSize(); ++i) {
      RandomizeListParts(
          problem,
          ErasedReference{.holder = variable.holder, .storage = &held.RawAt(i)},
          value.RawAt(i), members, array);
    }
  } else {
    RandomizeListErased(
        problem, variable, &value, ConstraintTypeOf<T>(), members, within);
  }
}

template <value::LyraValue T>
void RandomizeList(
    const value::ObjectRef& problem, const VariableRef<T>& variable,
    const T& value) {
  RandomizeListParts(
      problem, variable.Erased(), value,
      NamedMembers(problem, variable.Erased()), std::nullopt);
}

template <value::LyraValue T>
void ConstraintRandc(
    const value::ObjectRef& problem, const VariableRef<T>& variable,
    const T& value) {
  ConstraintDeclare(
      problem, variable.Erased(), value, RandomQualifier::kRandc,
      DeclaredWhole(problem, variable.Erased()));
}

[[nodiscard]] auto ConstraintVariableErased(
    const value::ObjectRef& problem, const ErasedReference& variable,
    const void* value, const value::ValueType& type) -> std::int64_t;

template <value::LyraValue T>
[[nodiscard]] auto ConstraintVariable(
    const value::ObjectRef& problem, const VariableRef<T>& variable,
    const T& value) -> std::int64_t {
  return ConstraintVariableErased(
      problem, variable.Erased(), &value, ConstraintTypeOf<T>());
}

// The term for the size of an array a constraint reads (LRM 18.5.7.1), the
// storage `array` names, holding `count` elements now.
[[nodiscard]] auto ConstraintArraySizeErased(
    const value::ObjectRef& problem, const ErasedReference& array,
    std::size_t count) -> std::int64_t;

template <value::LyraValue T>
[[nodiscard]] auto ConstraintArraySize(
    const value::ObjectRef& problem, const VariableRef<T>& array,
    const T& value) -> std::int64_t {
  return ConstraintArraySizeErased(problem, array.Erased(), value.RawSize());
}

// Settles what is chosen first -- the size of every random array a constraint
// sized (LRM 18.5.7.1), and every variable a function a constraint calls is
// handed (LRM 18.5.11) -- answering whether the call has to state its
// description again, now with those held.
[[nodiscard]] auto RandomizeSettleFirst(const value::ObjectRef& problem)
    -> bool;
// The variable `variable` is handed to a function a constraint calls.
void ConstraintWant(const value::ObjectRef& p, std::int64_t variable);

[[nodiscard]] auto ConstraintConstantErased(
    const value::ObjectRef& problem, const void* value,
    const value::ValueType& type) -> std::int64_t;
template <value::LyraValue T>
[[nodiscard]] auto ConstraintConstant(
    const value::ObjectRef& problem, const T& value) -> std::int64_t {
  return ConstraintConstantErased(problem, &value, ConstraintTypeOf<T>());
}

// The random objects the handles of a rand property reach (LRM 18.4), where
// its mode is on: its variables and constraints join the call's, and its hooks
// run around it. The property is a handle or an array of them, walked as a
// variable is, `path` naming it as its owner keeps its mode.
void RandomizeNestedErased(
    const value::ObjectRef& problem, const ErasedReference& handle,
    const void* value, const value::ValueType& type,
    const RandomVariablePath& path, bool enabled);

template <value::LyraValue T>
void RandomizeNestedOf(
    const value::ObjectRef& problem, const ErasedReference& handle,
    const T& value, const RandomVariablePath& path, bool enabled) {
  if constexpr (value::OrdinalElements<T>) {
    const bool on = enabled && RandEnabledErased(handle, path);
    T& held = *static_cast<T*>(handle.storage);
    for (std::size_t i = 0; i < value.RawSize(); ++i) {
      RandomizeNestedOf(
          problem,
          ErasedReference{.holder = handle.holder, .storage = &held.RawAt(i)},
          value.RawAt(i), path.Into(i), on);
    }
  } else {
    RandomizeNestedErased(
        problem, handle, &value, ConstraintTypeOf<T>(), path, enabled);
  }
}

template <value::LyraValue T>
void RandomizeNested(
    const value::ObjectRef& problem, const VariableRef<T>& handle,
    const T& value) {
  RandomizeNestedOf(
      problem, handle.Erased(), value,
      RandomVariablePath{.declared = handle.Erased().storage, .positions = {}},
      true);
}

// Whether a constraint block of the object is on (LRM 18.9); a static block's
// mode is the class's, named by a key that names the block and the class that
// introduced it.
[[nodiscard]] auto ConstraintBlockEnabled(
    GcObject* self, const value::String& name) -> bool;
[[nodiscard]] inline auto ConstraintBlockEnabled(
    const value::ObjectRef& handle, const value::String& name) -> bool {
  return ConstraintBlockEnabled(ObjectRootOf(handle), name);
}
[[nodiscard]] auto StaticConstraintBlockEnabled(const value::String& key)
    -> bool;

// The operations over terms, each a function of the problem.
[[nodiscard]] auto ConstraintNegate(const value::ObjectRef& p, std::int64_t a)
    -> std::int64_t;
[[nodiscard]] auto ConstraintBitwiseNot(
    const value::ObjectRef& p, std::int64_t a) -> std::int64_t;
[[nodiscard]] auto ConstraintReduceAnd(
    const value::ObjectRef& p, std::int64_t a) -> std::int64_t;
[[nodiscard]] auto ConstraintReduceOr(const value::ObjectRef& p, std::int64_t a)
    -> std::int64_t;
[[nodiscard]] auto ConstraintReduceXor(
    const value::ObjectRef& p, std::int64_t a) -> std::int64_t;
[[nodiscard]] auto ConstraintLogicalNot(
    const value::ObjectRef& p, std::int64_t a) -> std::int64_t;
[[nodiscard]] auto ConstraintAdd(
    const value::ObjectRef& p, std::int64_t a, std::int64_t b) -> std::int64_t;
[[nodiscard]] auto ConstraintSubtract(
    const value::ObjectRef& p, std::int64_t a, std::int64_t b) -> std::int64_t;
[[nodiscard]] auto ConstraintMultiply(
    const value::ObjectRef& p, std::int64_t a, std::int64_t b) -> std::int64_t;
[[nodiscard]] auto ConstraintDivide(
    const value::ObjectRef& p, std::int64_t a, std::int64_t b, bool is_signed)
    -> std::int64_t;
[[nodiscard]] auto ConstraintRemainder(
    const value::ObjectRef& p, std::int64_t a, std::int64_t b, bool is_signed)
    -> std::int64_t;
[[nodiscard]] auto ConstraintBitwiseAnd(
    const value::ObjectRef& p, std::int64_t a, std::int64_t b) -> std::int64_t;
[[nodiscard]] auto ConstraintBitwiseOr(
    const value::ObjectRef& p, std::int64_t a, std::int64_t b) -> std::int64_t;
[[nodiscard]] auto ConstraintBitwiseXor(
    const value::ObjectRef& p, std::int64_t a, std::int64_t b) -> std::int64_t;
[[nodiscard]] auto ConstraintShiftLeft(
    const value::ObjectRef& p, std::int64_t a, std::int64_t amount)
    -> std::int64_t;
[[nodiscard]] auto ConstraintShiftRight(
    const value::ObjectRef& p, std::int64_t a, std::int64_t amount,
    bool arithmetic) -> std::int64_t;
[[nodiscard]] auto ConstraintEqual(
    const value::ObjectRef& p, std::int64_t a, std::int64_t b) -> std::int64_t;
[[nodiscard]] auto ConstraintLess(
    const value::ObjectRef& p, std::int64_t a, std::int64_t b, bool is_signed)
    -> std::int64_t;
[[nodiscard]] auto ConstraintLogicalAnd(
    const value::ObjectRef& p, std::int64_t a, std::int64_t b) -> std::int64_t;
[[nodiscard]] auto ConstraintLogicalOr(
    const value::ObjectRef& p, std::int64_t a, std::int64_t b) -> std::int64_t;
[[nodiscard]] auto ConstraintConcatenate(
    const value::ObjectRef& p, std::int64_t high, std::int64_t low)
    -> std::int64_t;
[[nodiscard]] auto ConstraintSelect(
    const value::ObjectRef& p, std::int64_t condition, std::int64_t then_term,
    std::int64_t else_term) -> std::int64_t;
[[nodiscard]] auto ConstraintResize(
    const value::ObjectRef& p, std::int64_t a, std::int64_t width,
    bool sign_extend) -> std::int64_t;
[[nodiscard]] auto ConstraintSlice(
    const value::ObjectRef& p, std::int64_t a, std::int64_t offset,
    std::int64_t width) -> std::int64_t;

void ConstraintRequire(const value::ObjectRef& p, std::int64_t condition);
void ConstraintPrefer(const value::ObjectRef& p, std::int64_t condition);
void ConstraintDisableSoft(const value::ObjectRef& p, std::int64_t variable);
void ConstraintSolveBefore(
    const value::ObjectRef& p, std::int64_t before, std::int64_t after);
[[nodiscard]] auto ConstraintDistribution(
    const value::ObjectRef& p, std::int64_t subject, std::int64_t guard,
    bool is_signed, bool soft) -> std::int64_t;
void ConstraintDistributionItem(
    const value::ObjectRef& p, std::int64_t distribution, std::int64_t low,
    std::int64_t high, std::int64_t weight, bool per_value);
void ConstraintDistributionDefault(
    const value::ObjectRef& p, std::int64_t distribution, std::int64_t weight);
[[nodiscard]] auto ConstraintUniqueSet(const value::ObjectRef& p)
    -> std::int64_t;
void ConstraintUniqueMember(
    const value::ObjectRef& p, std::int64_t set, std::int64_t term);
void ConstraintUniqueRequire(
    const value::ObjectRef& p, std::int64_t set, std::int64_t guard);

// Each element of the array the storage `array` names, at any depth, a member
// of the set `set`, as the variable it is.
void ConstraintUniqueArrayErased(
    const value::ObjectRef& p, std::int64_t set, const ErasedReference& array,
    const void* value, const value::ValueType& type);

template <value::LyraValue T>
void ConstraintUniqueArrayOf(
    const value::ObjectRef& p, std::int64_t set, const ErasedReference& array,
    const T& value) {
  if constexpr (value::OrdinalElements<T>) {
    T& held = *static_cast<T*>(array.storage);
    for (std::size_t i = 0; i < value.RawSize(); ++i) {
      ConstraintUniqueArrayOf(
          p, set,
          ErasedReference{.holder = array.holder, .storage = &held.RawAt(i)},
          value.RawAt(i));
    }
  } else {
    ConstraintUniqueArrayErased(p, set, array, &value, ConstraintTypeOf<T>());
  }
}

template <value::LyraValue T>
void ConstraintUniqueArray(
    const value::ObjectRef& p, std::int64_t set, const VariableRef<T>& array,
    const T& value) {
  ConstraintUniqueArrayOf(p, set, array.Erased(), value);
}

// LRM 18.8: a variable's mode, and every random variable of an object at
// once. A variable is named by the random variable the class declares, of the
// type its value `value` has, and a reference to the part of it the call
// names, which is the whole or an element of it at any depth. Whether to
// switch it on is the truth of the bit the source passed.
void RandModeSetErased(
    const ErasedReference& declared, const void* value,
    const value::ValueType& type, const ErasedReference& part, bool on);
[[nodiscard]] auto RandModeGetErased(
    const ErasedReference& declared, const void* value,
    const value::ValueType& type, const ErasedReference& part) -> value::Int;
// The positions of the part `part` names within the variable `declared` names,
// which holds `value` now: none for the whole, and an element's at any depth
// for an array.
template <value::LyraValue T>
auto PositionsOf(
    const T& value, void* declared, const void* part,
    std::vector<std::size_t>& out) -> bool {
  if (declared == part) {
    return true;
  }
  if constexpr (value::OrdinalElements<T>) {
    T& held = *static_cast<T*>(declared);
    for (std::size_t i = 0; i < value.RawSize(); ++i) {
      out.push_back(i);
      if (PositionsOf(value.RawAt(i), &held.RawAt(i), part, out)) {
        return true;
      }
      out.pop_back();
    }
  }
  return false;
}
template <value::LyraValue T>
[[nodiscard]] auto PathOf(
    const ErasedReference& declared, const T& value,
    const ErasedReference& part) -> RandomVariablePath {
  RandomVariablePath path{.declared = declared.storage, .positions = {}};
  if (!PositionsOf(value, declared.storage, part.storage, path.positions)) {
    throw InternalError(
        "rand_mode: the part named lies outside the variable declared");
  }
  return path;
}
// The mode of the random variable `path` names, which the variable `declared`
// names the storage of is the root of.
void RandModeSetAt(
    const ErasedReference& declared, const RandomVariablePath& path, bool on);
[[nodiscard]] auto RandModeGetAt(
    const ErasedReference& declared, const RandomVariablePath& path)
    -> value::Int;
template <value::LyraValue T, value::LyraValue Part>
void RandModeSet(
    const VariableRef<T>& declared, const T& value,
    const VariableRef<Part>& part, bool on) {
  RandModeSetAt(
      declared.Erased(), PathOf(declared.Erased(), value, part.Erased()), on);
}
template <value::LyraValue T, value::LyraValue Part>
[[nodiscard]] auto RandModeGet(
    const VariableRef<T>& declared, const T& value,
    const VariableRef<Part>& part) -> value::Int {
  return RandModeGetAt(
      declared.Erased(), PathOf(declared.Erased(), value, part.Erased()));
}
void ObjectRandModeSet(GcObject* object, bool on);
inline void ObjectRandModeSet(const value::ObjectRef& handle, bool on) {
  ObjectRandModeSet(ObjectRootOf(handle), on);
}

// LRM 18.9: a block's mode, by its name on the object, and every block of an
// object at once; a static block's by its key.
void ConstraintModeSet(GcObject* object, const value::String& name, bool on);
[[nodiscard]] auto ConstraintModeGet(
    GcObject* object, const value::String& name) -> value::Int;
void ObjectConstraintModeSet(GcObject* object, bool on);
inline void ConstraintModeSet(
    const value::ObjectRef& handle, const value::String& name, bool on) {
  ConstraintModeSet(ObjectRootOf(handle), name, on);
}
[[nodiscard]] inline auto ConstraintModeGet(
    const value::ObjectRef& handle, const value::String& name) -> value::Int {
  return ConstraintModeGet(ObjectRootOf(handle), name);
}
inline void ObjectConstraintModeSet(const value::ObjectRef& handle, bool on) {
  ObjectConstraintModeSet(ObjectRootOf(handle), on);
}
void StaticConstraintModeSet(const value::String& key, bool on);
[[nodiscard]] auto StaticConstraintModeGet(const value::String& key)
    -> value::Int;

}  // namespace lyra::runtime
