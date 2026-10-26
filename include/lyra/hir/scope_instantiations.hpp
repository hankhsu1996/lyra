#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <span>
#include <vector>

#include "lyra/base/pool_id.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/owned_child_step.hpp"
#include "lyra/hir/scope_construction.hpp"
#include "lyra/hir/specialization_ref.hpp"
#include "lyra/hir/structural_scope.hpp"

namespace lyra::hir {

// Which entry of a scope's construction table an instantiation the scope
// declares is.
struct InstantiationIndex {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const InstantiationIndex&) const
      -> std::strong_ordering = default;
};

// Where the first object of the instance member `member` stands among the
// instantiations `scope` declares: every object of every member, the members
// in the order the scope declares them and each member's objects in row-major
// order (LRM 23.3.2). It is a fact of the scope alone, so the code of the
// scope names it wherever the scope stands, and counts out the member's other
// objects from there.
[[nodiscard]] auto InstantiationIndexOf(
    const StructuralScope& scope, InstanceMemberId member)
    -> InstantiationIndex;

struct PlacedBlock;

// The places of one generate, in the order its table lists them, each with
// the block an object builds there, absent where it builds none -- an
// alternative it did not select -- and whether each block is handed the index
// its loop stood at (LRM 27.4), the one value a block is ever handed, since a
// block has no parameter ports (Syntax 27-1).
struct GeneratePlaces {
  bool handed_an_index = false;
  std::vector<std::optional<PlacedBlock>> places;
};

// What one object of a scope makes at each instantiation the scope declares
// (LRM 23.3.2), which specialization of which unit, and at each place of each
// of its generates, which block body and the same of that block in turn. Which
// body is made where is the object's, stated where the program is assembled;
// the scope's code names an instantiation by where the scope declares it and a
// block by its generate and its place, so that code is the same wherever the
// scope stands. It is read off the specialization while its implementation is
// still held, which is why it carries the one fact of the implementation the
// assembly needs: whether each generate hands its blocks an index.
class ScopeInstantiations {
 public:
  ScopeInstantiations(
      const CompilationUnit& unit, const StructuralScope& scope,
      const ScopeConstruction& construction);

  // What is made at each of this scope's instantiations, in index order.
  [[nodiscard]] auto Instantiated() const
      -> std::span<const SpecializationRef> {
    return instantiated_;
  }
  // The places of each generate, in the order the scope declares them.
  [[nodiscard]] auto Generates() const -> std::span<const GeneratePlaces> {
    return generates_;
  }

 private:
  std::vector<SpecializationRef> instantiated_;
  std::vector<GeneratePlaces> generates_;
};

// The block one place builds: its body among the unit's, and what that block
// makes in turn.
struct PlacedBlock {
  BlockBodyId body;
  ScopeInstantiations made;
};

}  // namespace lyra::hir
