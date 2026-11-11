#pragma once

// The objects of the design hierarchy VPI reaches (LRM 37.10 to 37.12, 37.85),
// the handles foreign code holds to them (LRM 38.3, 38.38), and what each
// answers of itself and of the objects related to it.
//
// An object is a scope the run constructed, a namespace, or a declaration as it
// stands in one of those: an array of instances or of blocks one declaration
// builds, or a scope the run makes no object for -- a block, a fork, a loop, a
// task, a function, a class, a clocking block. One declaration in each of
// several constructed scopes is so a different object in each (LRM 36.10).

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

using VpiObject =
    std::variant<Scope*, NamespaceObject, ScopeArray, LexicalScope>;

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

// The type of `object` (LRM 37.3.1), and the name of a type as the standard's
// header spells it (LRM 37.3.2).
[[nodiscard]] auto TypeOf(const VpiObject& object) -> PLI_INT32;
[[nodiscard]] auto TypeName(PLI_INT32 type) -> std::optional<std::string_view>;

// The objects of type `relation` related to `from` (LRM 37.10 to 37.12,
// 37.85): from the design where it is the simulation.
[[nodiscard]] auto Related(
    RuntimeEffects& run, PLI_INT32 relation, const Referent& from)
    -> VpiAnswer<std::vector<VpiObject>>;

// The one object of type `relation` related to `from`, none where nothing is
// so related; an iteration is related only to its use (LRM 37.84).
[[nodiscard]] auto RelatedOne(PLI_INT32 relation, const Referent& from)
    -> VpiAnswer<std::optional<VpiObject>>;

// The object `name` names, from the design's tops and its namespaces where
// `within` is the simulation, and from `within` otherwise (LRM 38.21).
[[nodiscard]] auto ObjectNamed(
    RuntimeEffects& run, std::string_view name, const Referent& within)
    -> VpiAnswer<std::optional<VpiObject>>;

// The element of the array `of` at `indices` (LRM 38.19, 38.20), none where it
// holds none there.
[[nodiscard]] auto ElementAt(
    const VpiObject& of, std::span<const std::int64_t> indices)
    -> VpiAnswer<std::optional<VpiObject>>;

// An integer property of `of` (LRM 38.6, 38.7).
[[nodiscard]] auto IntegerProperty(
    RuntimeEffects& run, PLI_INT32 property, const Referent& of)
    -> VpiAnswer<PLI_INT64>;

// A string property of `of` (LRM 38.11), none where it has the property and
// no text for it.
[[nodiscard]] auto StringProperty(PLI_INT32 property, const Referent& of)
    -> VpiAnswer<std::optional<std::string>>;

// The time unit `of` reads time in, as a power of ten (LRM 38.13).
[[nodiscard]] auto TimeUnitOf(RuntimeEffects& run, const Referent& of)
    -> VpiAnswer<std::int8_t>;

}  // namespace lyra::runtime
