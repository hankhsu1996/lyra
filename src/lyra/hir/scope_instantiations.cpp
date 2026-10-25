#include "lyra/hir/scope_instantiations.hpp"

#include <cstdint>
#include <vector>

#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/owned_child_ref.hpp"
#include "lyra/hir/structural_scope.hpp"

namespace lyra::hir {

ScopeInstantiations::ScopeInstantiations(
    const CompilationUnit& unit, const StructuralScope& scope) {
  first_of_member_.reserve(scope.instance_members.size());
  for (const InstanceMemberDecl& member : scope.instance_members) {
    first_of_member_.push_back(
        static_cast<std::uint32_t>(instantiated_.size()));
    for (const InstanceAlternative& alternative : member.alternatives) {
      instantiated_.push_back(
          UnitBodyRef{
              .unit =
                  unit.scope_classes.KeyOf(alternative.scope_class).unit_name,
              .body = alternative.body});
    }
  }
  first_block_of_generate_.reserve(scope.generates.size());
  for (const Generate& generate : scope.generates) {
    first_block_of_generate_.push_back(
        static_cast<std::uint32_t>(blocks_.size()));
    for (const GenerateBlock& block : generate.blocks) {
      blocks_.emplace_back(unit, block.scope);
    }
  }
}

auto ScopeInstantiations::Of(InstanceMemberId member, std::uint32_t alternative)
    const -> InstantiationIndex {
  return InstantiationIndex{first_of_member_.at(member.value) + alternative};
}

auto ScopeInstantiations::BlockTableOf(
    GenerateId generate, StructuralScopeId block) const -> BlockTableIndex {
  return BlockTableIndex{
      first_block_of_generate_.at(generate.value) + block.value};
}

auto ScopeInstantiations::OfBlock(GenerateId generate, StructuralScopeId block)
    const -> const ScopeInstantiations& {
  return blocks_.at(BlockTableOf(generate, block).value);
}

}  // namespace lyra::hir
