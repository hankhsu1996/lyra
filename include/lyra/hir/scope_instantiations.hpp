#pragma once

#include <compare>
#include <cstdint>
#include <span>
#include <vector>

#include "lyra/base/pool_id.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/owned_child_ref.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/unit_body_ref.hpp"

namespace lyra::hir {

// Which entry of a scope's construction table an instantiation the scope
// declares is.
struct InstantiationIndex {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const InstantiationIndex&) const
      -> std::strong_ordering = default;
};

// Which of a scope's block tables the table one of its generate blocks is
// built with is.
struct BlockTableIndex {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const BlockTableIndex&) const
      -> std::strong_ordering = default;
};

// What one scope of an object makes at each instantiation it declares (LRM
// 23.3.2), and the same of each block of each of its generates. A scope's code
// names an instantiation by where the scope declares it and a block's table by
// the block, never by where either stands among the object's other scopes, so
// a scope's code says the same thing wherever the scope stands and whichever
// bodies its neighbours are; which body is made at an instantiation is the
// table's, stated where the program is assembled.
class ScopeInstantiations {
 public:
  explicit ScopeInstantiations(
      const CompilationUnit& unit, const StructuralScope& scope);

  [[nodiscard]] auto Of(InstanceMemberId member, std::uint32_t alternative)
      const -> InstantiationIndex;
  [[nodiscard]] auto OfBlock(GenerateId generate, StructuralScopeId block) const
      -> const ScopeInstantiations&;
  [[nodiscard]] auto BlockTableOf(
      GenerateId generate, StructuralScopeId block) const -> BlockTableIndex;

  // What is made at each of this scope's instantiations, in index order.
  [[nodiscard]] auto Instantiated() const -> std::span<const UnitBodyRef> {
    return instantiated_;
  }
  // Each block's layout, in block table order.
  [[nodiscard]] auto Blocks() const -> std::span<const ScopeInstantiations> {
    return blocks_;
  }

 private:
  std::vector<std::uint32_t> first_of_member_;
  std::vector<UnitBodyRef> instantiated_;
  // The block table index of the first block of each generate.
  std::vector<std::uint32_t> first_block_of_generate_;
  std::vector<ScopeInstantiations> blocks_;
};

}  // namespace lyra::hir
