#pragma once

// The objects of the design hierarchy VPI reaches (LRM 37.10 to 37.12, 37.15
// to 37.26, 37.85), the handles foreign code holds to them (LRM 38.3, 38.38),
// and what each answers of itself and of the objects related to it.
//
// An object is a scope the run constructed, a namespace, or a declaration as it
// stands in one of those: an array of instances or of blocks one declaration
// builds, a scope the run makes no object for -- a block, a fork, a loop, a
// task, a function, a class, a clocking block -- or a variable or a part of
// one. One declaration in each of several constructed scopes is so a
// different object in each (LRM 36.10). A typespec, and what a relation from
// one answers, is a constant of the unit and of no scope.

#include <cstddef>
#include <cstdint>
#include <deque>
#include <expected>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_set>
#include <variant>
#include <vector>

#include "lyra/abi/records.hpp"
#include "lyra/runtime/reflection.hpp"
#include "lyra/support/data_object.hpp"
#include "vpi_user.h"

namespace lyra::runtime {

class RuntimeEffects;
class Scope;

// A package or the compilation unit's scope (LRM 37.10): one object of the
// run, stated by what its unit hands the library.
struct NamespaceObject {
  const abi::ScopeDeclarations* scope_declarations = nullptr;

  auto operator==(const NamespaceObject&) const -> bool = default;
};

// What holds a declaration the run makes no object of its own for: the
// nearest constructed scope or namespace around it.
using ScopeHolder = std::variant<Scope*, NamespaceObject>;

// The instances an instance array declares, or the blocks a loop generate
// builds (LRM 37.11, 37.85), in the scope that declares them.
struct ScopeArray {
  Scope* holder = nullptr;
  const abi::ElaboratedScopeRecord* declaration = nullptr;

  auto operator==(const ScopeArray&) const -> bool = default;
};

// A scope of the source that the run makes no object for, as it stands in the
// object holding it (LRM 37.12).
struct LexicalScope {
  ScopeHolder holder;
  const abi::LexicalScopeRecord* declaration = nullptr;

  auto operator==(const LexicalScope&) const -> bool = default;
};

// The version of the standard a VPI application was written against (LRM
// 36.12), which decides what the objects of Table 36-10's first seven entries
// are; every version from 1800-2012 on behaves as the latest.
enum class VpiMode : std::uint8_t {
  k1364v1995,
  k1364v2001,
  k1364v2005,
  k1800v2005,
  k1800v2009,
  k1800v2012,
};

// One step into the packed value a variable's selection reaches (LRM 37.17,
// 37.18, 37.26): into a member of a packed structure or union, by the
// member's index among the members, or to the index the source would write --
// an element of a packed array, or a bit.
struct PackedMemberStep {
  std::uint32_t member = 0;

  auto operator==(const PackedMemberStep&) const -> bool = default;
};
struct PackedIndexStep {
  std::int64_t index = 0;

  auto operator==(const PackedIndexStep&) const -> bool = default;
};
using PackedStep = std::variant<PackedMemberStep, PackedIndexStep>;

// A variable a scope or a namespace declares, or a part of one (LRM 37.15 to
// 37.19, 37.26): what holds it, the position of its row among the data objects
// the holder describes, the selection that reaches a part of it that holds a
// packed value or is no packed value -- empty for the variable itself -- and
// the steps on from that packed value to the bits of it the part is, none for
// the whole value. What it names is evaluated whenever it is used, so an
// element of a dynamic array is whatever the array holds there then (LRM
// 37.17 detail 27). A row the name a `ref` port gives the variable connected
// to it is a reference object of the same shape (LRM 37.15).
struct VariableObject {
  ScopeHolder holder;
  std::uint32_t row = 0;
  Selection selection;
  std::vector<PackedStep> packed;

  auto operator==(const VariableObject&) const -> bool = default;
};

// A typespec a variable or another typespec relates to (LRM 37.25), and a
// member a structure's or a union's typespec lists (LRM 37.26).
struct TypespecObject {
  const abi::Typespec* typespec = nullptr;

  auto operator==(const TypespecObject&) const -> bool = default;
};
struct TypespecMemberObject {
  const abi::TypespecMember* member = nullptr;

  auto operator==(const TypespecMemberObject&) const -> bool = default;
};

// One dimension of an array or a vector (LRM 37.22): its bounds, none for an
// empty range -- an associative array's dimension, a dynamic array's or a
// queue's as a typespec states it or while it holds no element.
struct RangeObject {
  std::optional<support::Range> range;

  auto operator==(const RangeObject&) const -> bool = default;
};

// A constant a relation answers: a range's bound, an index of a select (LRM
// 37.17 details 5 and 6, 37.22).
struct ConstantObject {
  std::int64_t value = 0;

  auto operator==(const ConstantObject&) const -> bool = default;
};

// A member an enumeration's typespec declares (LRM 37.25), by its position.
struct EnumConstantObject {
  const abi::Typespec* enumeration = nullptr;
  std::uint32_t position = 0;

  auto operator==(const EnumConstantObject&) const -> bool = default;
};

using VpiObject = std::variant<
    Scope*, NamespaceObject, ScopeArray, LexicalScope, VariableObject,
    TypespecObject, TypespecMemberObject, RangeObject, ConstantObject,
    EnumConstantObject>;

// An iteration foreign code steps through (LRM 38.23, 38.40): the objects one
// relation answered when it was made, the object it was made on, if any, and
// how far the stepping has got.
struct VpiIteration {
  PLI_INT32 relation = 0;
  std::optional<VpiObject> use;
  std::vector<VpiObject> objects;
  std::size_t next = 0;
};

// The simulation, which a routine handed no object asks of (LRM 38.6, 38.13).
struct Simulation {
  auto operator==(const Simulation&) const -> bool = default;
};

// What a handle foreign code passes a routine refers to: the simulation where
// it is null, an object, or an iteration.
using Referent = std::variant<Simulation, VpiObject, const VpiIteration*>;

// Every handle this run has handed foreign code and not taken back. A handle
// is the address of a word holding its own number: one foreign code passes
// back is looked for among the addresses this table issued before anything
// is read through it, so a pointer that is no handle is found to be none
// without being followed (LRM 38.2).
class VpiObjectHandles {
 public:
  using Target = std::variant<VpiObject, VpiIteration>;

  auto Issue(Target target) -> vpiHandle;

  // What `handle` refers to, none where it is no handle this table holds.
  [[nodiscard]] auto Find(const PLI_UINT32* handle) -> Target*;

  // Takes `handle` back, answering whether it was one this table held.
  auto Release(const PLI_UINT32* handle) -> bool;

 private:
  struct Entry {
    PLI_UINT32 number = 0;
    std::optional<Target> target;
  };

  std::deque<Entry> entries_;
  std::unordered_set<const PLI_UINT32*> issued_;
  std::vector<PLI_UINT32> released_;
};

// What a question about an object answers, or why it answers nothing, which
// the routine asking reports as an error (LRM 38.2).
template <typename T>
using VpiAnswer = std::expected<T, std::string>;

// Why a question answers nothing: what it asks of is not yet supported, or the
// object has no such property.
[[nodiscard]] auto VpiNotYet(std::string_view what)
    -> std::unexpected<std::string>;
[[nodiscard]] auto VpiNotAProperty() -> std::unexpected<std::string>;

// What `holder` states of itself, and the text every full name of what it
// declares in `enclosing` -- none for what is written in no block -- begins
// with (LRM 37.10): its own full name and each named block around the
// declaration.
[[nodiscard]] auto ScopeDeclarationsOf(const ScopeHolder& holder)
    -> const abi::ScopeDeclarations&;
[[nodiscard]] auto DeclarationPrefix(
    const ScopeHolder& holder, const abi::LexicalScopeRecord* enclosing)
    -> std::string;

// The type of `object` (LRM 37.3.1) in `mode`, and the name of a type as the
// standard's header spells it (LRM 37.3.2), the earlier name of one a later
// version renamed in a mode of the earlier version.
[[nodiscard]] auto TypeOf(const VpiObject& object, VpiMode mode) -> PLI_INT32;
[[nodiscard]] auto TypeName(PLI_INT32 type, VpiMode mode)
    -> std::optional<std::string_view>;

// The objects of type `relation` related to `from` (LRM 37.10 to 37.12,
// 37.15 to 37.26, 37.85): from the design where it is the simulation.
[[nodiscard]] auto Related(
    RuntimeEffects& run, PLI_INT32 relation, const Referent& from, VpiMode mode)
    -> VpiAnswer<std::vector<VpiObject>>;

// The one object of type `relation` related to `from`, none where nothing is
// so related; an iteration is related only to its use (LRM 37.84).
[[nodiscard]] auto RelatedOne(
    RuntimeEffects& run, PLI_INT32 relation, const Referent& from)
    -> VpiAnswer<std::optional<VpiObject>>;

// The object `name` names, from the design's tops and its namespaces where
// `within` is the simulation, and from `within` otherwise (LRM 38.21).
[[nodiscard]] auto ObjectNamed(
    RuntimeEffects& run, std::string_view name, const Referent& within)
    -> VpiAnswer<std::optional<VpiObject>>;

// The element of `of` at `indices` (LRM 38.19, 38.20), none where it holds
// none there: an element of an array of scopes, or a part of a variable.
[[nodiscard]] auto ElementAt(
    const VpiObject& of, std::span<const std::int64_t> indices)
    -> VpiAnswer<std::optional<VpiObject>>;

// An integer property of `of` (LRM 38.6, 38.7) in `mode`.
[[nodiscard]] auto IntegerProperty(
    RuntimeEffects& run, PLI_INT32 property, const Referent& of, VpiMode mode)
    -> VpiAnswer<PLI_INT64>;

// A string property of `of` (LRM 38.11) in `mode`, none where it has the
// property and no text for it.
[[nodiscard]] auto StringProperty(
    PLI_INT32 property, const Referent& of, VpiMode mode)
    -> VpiAnswer<std::optional<std::string>>;

// The time unit `of` reads time in, as a power of ten (LRM 38.13).
[[nodiscard]] auto TimeUnitOf(RuntimeEffects& run, const Referent& of)
    -> VpiAnswer<std::int8_t>;

}  // namespace lyra::runtime
