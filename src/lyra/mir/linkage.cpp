#include "lyra/mir/linkage.hpp"

#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::mir {

auto LinkageOf(const CompilationUnit& unit, ClassId id) -> support::Linkage {
  if (unit.GetClass(id).defining_unit.has_value()) {
    return support::Linkage::kExternal;
  }
  if (IsStatedByEveryBody(unit, id)) return support::Linkage::kShared;
  if (IsPublished(unit, id)) return support::Linkage::kExternal;
  return support::Linkage::kInternal;
}

auto LinkageOf(const CompilationUnit& unit, RecordId id) -> support::Linkage {
  return unit.GetRecord(id).defining_unit.has_value()
             ? support::Linkage::kExternal
             : support::Linkage::kShared;
}

auto ForeignScopeEntryLinkage() -> support::Linkage {
  return support::Linkage::kShared;
}

auto LinkageOf(const CompilationUnit& unit, const CodeSource& at)
    -> support::Linkage {
  const auto of_namespace = [](const NamespaceReach& reach) {
    return std::visit(
        Overloaded{
            [](const ReachedByLinkageName&) {
              return support::Linkage::kExternal;
            },
            [](const ReachedByName&) { return support::Linkage::kExternal; },
            [](const ReachedByBringUpEntry&) {
              return support::Linkage::kExternal;
            },
            [](const ReachedAsObjectEntry&) {
              return support::Linkage::kExternal;
            },
            [](const ReachedInAnotherUnit&) {
              return support::Linkage::kExternal;
            },
            [](const ReachedByPosition&) {
              return support::Linkage::kInternal;
            }},
        reach);
  };
  return std::visit(
      Overloaded{
          [&](const ConstructorSource& s) { return LinkageOf(unit, s.owner); },
          [&](const CallableTarget& s) { return LinkageOf(unit, s.owner); },
          [&](const UnitCallableTarget& s) {
            return of_namespace(NamespaceReachOf(unit, s.slot));
          },
          [&](const RecordMethodSource& s) {
            return LinkageOf(unit, s.record);
          },
          [](const TypeOperationSource&) {
            return support::Linkage::kInternal;
          },
          [](const ClosureSource&) { return support::Linkage::kInternal; },
          [](const ForeignScopeEntrySource&) {
            return ForeignScopeEntryLinkage();
          }},
      at);
}

}  // namespace lyra::mir
