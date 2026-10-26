#include "lyra/hir/scope_instantiations.hpp"

#include <cstddef>
#include <cstdint>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/owned_child_step.hpp"
#include "lyra/hir/scope_construction.hpp"
#include "lyra/hir/specialization_ref.hpp"
#include "lyra/hir/structural_scope.hpp"

namespace lyra::hir {

auto InstantiationIndexOf(const StructuralScope& scope, InstanceMemberId member)
    -> InstantiationIndex {
  std::uint32_t before = 0;
  for (std::uint32_t m = 0; m < member.value; ++m) {
    before += static_cast<std::uint32_t>(
        scope.instance_members.Get(InstanceMemberId{m}).taken.size());
  }
  return InstantiationIndex{before};
}

ScopeInstantiations::ScopeInstantiations(
    const CompilationUnit& unit, const StructuralScope& scope,
    const ScopeConstruction& construction) {
  for (const std::vector<SpecializationRef>& member : construction.instances) {
    instantiated_.insert(instantiated_.end(), member.begin(), member.end());
  }
  generates_.reserve(construction.generates.size());
  for (const GenerateId g : scope.generates.Ids()) {
    const Generate& generate = scope.generates.Get(g);
    GeneratePlaces& placed = generates_.emplace_back();
    placed.handed_an_index = std::visit(
        Overloaded{
            [](const SingleBlock&) { return false; },
            [](const BlocksRepeat&) { return true; },
            [](const BlocksChoose&) { return false; }},
        generate.counting);
    placed.places.reserve(construction.generates[g.value].size());
    for (const BlockConstruction& block : construction.generates[g.value]) {
      if (!block.body.has_value()) {
        placed.places.emplace_back();
        continue;
      }
      placed.places.emplace_back(
          PlacedBlock{
              .body = *block.body,
              .made = ScopeInstantiations(
                  unit, unit.block_bodies.Get(*block.body).scope,
                  block.built)});
    }
  }
}

}  // namespace lyra::hir
