#include "lyra/mir/linkage.hpp"

#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_id.hpp"
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
            [](const ReachedAsDataObjectAccess&) {
              return support::Linkage::kInternal;
            },
            [](const ReachedByPosition&) {
              return support::Linkage::kInternal;
            }},
        reach);
  };
  const auto of_callable = [&](CallableId callable) {
    return std::visit(
        Overloaded{
            [&](const DeclaredByClass& owner) {
              return LinkageOf(unit, owner.cls);
            },
            [&](const ConstructsClass& owner) {
              return LinkageOf(unit, owner.cls);
            },
            [&](const DeclaredByNamespace&) {
              return of_namespace(NamespaceReachOf(unit, callable));
            },
            [&](const DeclaredByRecord& owner) {
              return LinkageOf(unit, owner.record);
            },
            [](const DeclaredByClosure&) {
              return support::Linkage::kInternal;
            }},
        unit.OwnerOf(callable));
  };
  return std::visit(
      Overloaded{
          [&](const CallableTarget& s) { return of_callable(s.callable); },
          [](const TypeOperationSource&) {
            return support::Linkage::kInternal;
          },
          [](const ForeignScopeEntrySource&) {
            return ForeignScopeEntryLinkage();
          }},
      at);
}

}  // namespace lyra::mir
