#include "lyra/hir/construction_table_layout.hpp"

#include <cstdint>
#include <vector>

#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/owned_child_ref.hpp"
#include "lyra/hir/structural_scope.hpp"

namespace lyra::hir {

auto ScopeLayout::Of(InstanceMemberId member, std::uint32_t alternative) const
    -> InstantiationIndex {
  return InstantiationIndex{first_of_member_.at(member.value) + alternative};
}

auto ScopeLayout::OfBlock(GenerateId generate, StructuralScopeId block) const
    -> const ScopeLayout& {
  return blocks_.at(generate.value).at(block.value);
}

ConstructionTableLayout::ConstructionTableLayout(const CompilationUnit& unit)
    : root_(LayOut(unit, unit.root_scope)) {
}

auto ConstructionTableLayout::LayOut(
    const CompilationUnit& unit, const StructuralScope& scope) -> ScopeLayout {
  ScopeLayout layout;
  layout.first_of_member_.reserve(scope.instance_members.size());
  for (const InstanceMemberDecl& member : scope.instance_members) {
    layout.first_of_member_.push_back(
        static_cast<std::uint32_t>(instantiated_.size()));
    for (const InstanceAlternative& alternative : member.alternatives) {
      instantiated_.push_back(
          BodyOfUnit{
              .unit = unit.external_scope_classes.Get(alternative.scope_class)
                          .unit_name,
              .body = alternative.body});
    }
  }
  layout.blocks_.reserve(scope.generates.size());
  for (const Generate& generate : scope.generates) {
    std::vector<ScopeLayout>& blocks = layout.blocks_.emplace_back();
    blocks.reserve(generate.blocks.size());
    for (const GenerateBlock& block : generate.blocks) {
      blocks.push_back(LayOut(unit, block.scope));
    }
  }
  return layout;
}

}  // namespace lyra::hir
