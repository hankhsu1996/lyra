#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <span>

#include "lyra/base/source_location.hpp"
#include "lyra/runtime/class_object.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/randomization.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/support/random_qualifier.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/enumeration.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/library_value_types.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

class RuntimeEffects;

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
//
// `at` is where the design wrote the call. The problem keeps it, because a
// description is stated and solved over several entries of the library, and
// what fails under one of them binds that.
[[nodiscard]] auto RandomizeObject(
    ClassObject* object, RuntimeEffects& runtime, base::SourceLocation at)
    -> value::ObjectRef;
[[nodiscard]] auto RandomizeObjectListed(
    ClassObject* object, RuntimeEffects& runtime, base::SourceLocation at)
    -> value::ObjectRef;
[[nodiscard]] auto RandomizeScope(
    RuntimeEffects& runtime, base::SourceLocation at) -> value::ObjectRef;

void RandomizeConstrain(const value::ObjectRef& problem);
[[nodiscard]] auto RandomizeFinish(const value::ObjectRef& problem)
    -> value::Int;

// A random variable walked by its own type's code (LRM 18.4): the call begins
// the walk on the whole variable, for what the walk is for -- declaring it
// random or randc, listing it, reaching the objects its handles name, or
// entering its elements into a uniqueness set -- and the type's code then
// names, part by part, each array and each value it holds, by a reference
// formed from the whole's as a write through it would form one, of the type a
// record names, entering each element's position before it and leaving it
// after. The library keeps what each part takes from those around it; the
// walk itself is the type's.
void RandomWalkDeclare(
    const value::ObjectRef& problem, const ErasedReference& whole,
    support::RandomQualifier declared);
void RandomWalkList(
    const value::ObjectRef& problem, const ErasedReference& whole);
void RandomWalkNested(
    const value::ObjectRef& problem, const ErasedReference& whole);
void RandomWalkUnique(
    const value::ObjectRef& problem, std::int64_t set,
    const ErasedReference& whole);
void RandomWalkFixedArray(const value::ObjectRef& problem);
void RandomWalkResizableArray(
    const value::ObjectRef& problem, const ErasedReference& array,
    const ArrayResizer& resize);
void RandomWalkEnter(const value::ObjectRef& problem, std::int64_t position);
void RandomWalkLeave(const value::ObjectRef& problem);
void RandomWalkLeaf(
    const value::ObjectRef& problem, const ErasedReference& lent,
    const value::ValueType* type);
void RandomWalkEnd(const value::ObjectRef& problem);

// How the array the storage `whole` names, of type `type`, is resized: kept
// as far as it reaches and grown by elements at their default (LRM 7.5.1),
// written as an assignment to it is, and put back by writing what it held.
// The array is a dynamic array or a queue, made a count long by its type's
// own body: the one the record states, or, where the type is named, the one
// the type itself has.
[[nodiscard]] auto ResizeOf(
    const ErasedReference& whole, const value::ValueType& type) -> ArrayResizer;
template <value::LyraValue T>
[[nodiscard]] auto ResizeOf(
    const ErasedReference& whole, const value::ValueType& type)
    -> ArrayResizer {
  return [whole, &type](std::size_t count) -> std::function<void()> {
    const T& current = *static_cast<const T*>(whole.storage);
    auto before = std::make_shared<T>(current);
    T resized = current.Resized(
        static_cast<std::int64_t>(count), base::SourceLocation{});
    WriteLaidOutThrough(whole, type, &resized);
    return [whole, &type, before] {
      WriteLaidOutThrough(whole, type, before.get());
    };
  };
}

template <value::LyraValue T>
void RandomWalkResizableArray(
    const value::ObjectRef& problem, const VariableRef<T>& array,
    const value::ValueType* type) {
  RandomWalkResizableArray(
      problem, array.Erased(), ResizeOf<T>(array.Erased(), *type));
}
template <value::LyraValue T>
void RandomWalkLeaf(
    const value::ObjectRef& problem, const VariableRef<T>& part,
    const value::ValueType* type) {
  RandomWalkLeaf(problem, part.Erased(), type);
}

// What a class's description states, each through the problem it is handed:
// a random variable, by how the source declared it (LRM 18.4), which every
// object of the call declares before any states a constraint, a variable the
// call lists (LRM 18.11), and the objects a random handle reaches -- each a
// walk begun on the variable's reference, which the variable's own type's code
// then makes; and a variable a constraint reads, answering its term, which is
// the one declared there or, where none was, one not declared random (LRM
// 18.3). Whether one declared random is chosen is its owner's mode (LRM 18.8)
// unless the call lists what it chooses.
template <value::LyraValue T>
void ConstraintRand(
    const value::ObjectRef& problem, const VariableRef<T>& variable) {
  RandomWalkDeclare(
      problem, variable.Erased(), support::RandomQualifier::kRand);
}
template <value::LyraValue T>
void ConstraintRandc(
    const value::ObjectRef& problem, const VariableRef<T>& variable) {
  RandomWalkDeclare(
      problem, variable.Erased(), support::RandomQualifier::kRandc);
}
template <value::LyraValue T>
void RandomizeList(
    const value::ObjectRef& problem, const VariableRef<T>& variable) {
  RandomWalkList(problem, variable.Erased());
}
template <value::LyraValue T>
void RandomizeNested(
    const value::ObjectRef& problem, const VariableRef<T>& handle) {
  RandomWalkNested(problem, handle.Erased());
}

// The variable `variable` names is of an enumerated type, or an array of one,
// declaring `members` (LRM 18.5), stated before the variable is declared or
// listed; and the members stated for it, where any were.
void ConstraintMembersOf(
    const value::ObjectRef& problem, const ErasedReference& variable,
    const value::Enumeration& members);
template <value::LyraValue T>
void ConstraintMembers(
    const value::ObjectRef& problem, const VariableRef<T>& variable,
    const value::Enumeration& members) {
  ConstraintMembersOf(problem, variable.Erased(), members);
}
[[nodiscard]] auto NamedMembers(
    const value::ObjectRef& problem, const ErasedReference& variable)
    -> const value::Enumeration*;

[[nodiscard]] auto ConstraintVariableErased(
    const value::ObjectRef& problem, const ErasedReference& variable,
    const void* value, const value::ValueType& type) -> std::int64_t;

template <value::LyraValue T>
[[nodiscard]] auto ConstraintVariable(
    const value::ObjectRef& problem, const VariableRef<T>& variable,
    const T& value) -> std::int64_t {
  return ConstraintVariableErased(
      problem, variable.Erased(), &value, value::LibraryTypeOf<T>());
}

// The term for the size of an array a constraint reads (LRM 18.5.7.1), the
// storage `array` names, holding `count` elements now.
[[nodiscard]] auto ConstraintArraySize(
    const value::ObjectRef& problem, const ErasedReference& array,
    std::int64_t count) -> std::int64_t;

template <value::LyraValue T>
[[nodiscard]] auto ConstraintArraySize(
    const value::ObjectRef& problem, const VariableRef<T>& array,
    std::int64_t count) -> std::int64_t {
  return ConstraintArraySize(problem, array.Erased(), count);
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
  return ConstraintConstantErased(problem, &value, value::LibraryTypeOf<T>());
}

// Whether a constraint block of the object is on (LRM 18.9); a static block's
// mode is the class's, named by a key that names the block and the class that
// introduced it.
[[nodiscard]] auto ConstraintBlockEnabled(
    GcObject* self, const value::String& name) -> bool;
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

// Each element of the array `array` names, at any depth, a member of the set
// `set`, as the variable it is: a walk the array's own code makes.
template <value::LyraValue T>
void ConstraintUniqueArray(
    const value::ObjectRef& p, std::int64_t set, const VariableRef<T>& array) {
  RandomWalkUnique(p, set, array.Erased());
}

// LRM 18.8: a variable's mode, and every random variable of an object at
// once. A variable is named by the random variable the class declares and the
// positions of the part of it the call names: none for the whole, and an
// element's at any depth. Whether to switch it on is the truth of the bit the
// source passed.
void RandModeSet(
    const ErasedReference& declared, std::span<const std::int64_t> positions,
    bool on);
[[nodiscard]] auto RandModeGet(
    const ErasedReference& declared, std::span<const std::int64_t> positions)
    -> value::Int;
template <value::LyraValue T>
void RandModeSet(
    const VariableRef<T>& declared, std::span<const std::int64_t> positions,
    bool on) {
  RandModeSet(declared.Erased(), positions, on);
}
template <value::LyraValue T>
[[nodiscard]] auto RandModeGet(
    const VariableRef<T>& declared, std::span<const std::int64_t> positions)
    -> value::Int {
  return RandModeGet(declared.Erased(), positions);
}
void ObjectRandModeSet(GcObject* object, bool on);

// LRM 18.9: a block's mode, by its name on the object, and every block of an
// object at once; a static block's by its key.
void ConstraintModeSet(GcObject* object, const value::String& name, bool on);
[[nodiscard]] auto ConstraintModeGet(
    GcObject* object, const value::String& name) -> value::Int;
void ObjectConstraintModeSet(GcObject* object, bool on);
void StaticConstraintModeSet(const value::String& key, bool on);
[[nodiscard]] auto StaticConstraintModeGet(const value::String& key)
    -> value::Int;

}  // namespace lyra::runtime
