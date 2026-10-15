#pragma once

#include <compare>
#include <cstdint>
#include <string>
#include <vector>

#include "lyra/base/pool_id.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/owned_child_ref.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/unit_body_ref.hpp"

namespace lyra::hir {

// Which entry of a construction table an instantiation is.
struct InstantiationIndex {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const InstantiationIndex&) const
      -> std::strong_ordering = default;
};

// Where each instantiation of one scope stands in the construction table an
// object of its unit is built with, and where those of each block of each of
// its generates do.
class ScopeInstantiations {
 public:
  [[nodiscard]] auto Of(InstanceMemberId member, std::uint32_t alternative)
      const -> InstantiationIndex;
  [[nodiscard]] auto OfBlock(GenerateId generate, StructuralScopeId block) const
      -> const ScopeInstantiations&;

 private:
  friend class ConstructionTableLayout;

  std::vector<std::uint32_t> first_of_member_;
  std::vector<std::vector<ScopeInstantiations>> blocks_;
};

// The order a unit's construction table lists its instantiations in (LRM
// 23.3.2), computed from the unit by one rule, so whatever writes a table and
// whatever reads one agree on it, as a C++ ABI's VTT layout is fixed by one
// rule for the constructor that reads it and the definition that states it
// (Itanium C++ ABI 2.6). A scope's own instantiations come first -- each
// instance member's ways of being built, in the order the scope declares its
// members -- then those of each block of each of its generates, depth first.
// Every scope of an object is constructed with the same table, so the indices
// run across the whole unit.
class ConstructionTableLayout {
 public:
  explicit ConstructionTableLayout(const CompilationUnit& unit);

  [[nodiscard]] auto Root() const -> const ScopeInstantiations& {
    return root_;
  }

  // What is elaborated at each instantiation, in index order.
  [[nodiscard]] auto Instantiated() const -> const std::vector<UnitBodyRef>& {
    return instantiated_;
  }

 private:
  auto LayOut(const CompilationUnit& unit, const StructuralScope& scope)
      -> ScopeInstantiations;

  std::vector<UnitBodyRef> instantiated_;
  ScopeInstantiations root_;
};

}  // namespace lyra::hir
