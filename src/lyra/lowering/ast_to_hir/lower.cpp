#include "lyra/lowering/ast_to_hir/lower.hpp"

#include <expected>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>

#include <slang/ast/ASTVisitor.h>
#include <slang/ast/Compilation.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/module_unit.hpp"
#include "lyra/lowering/ast_to_hir/facts.hpp"
#include "lyra/lowering/ast_to_hir/module.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Collects the distinct unit bodies reachable from the tops. slang owns the
// structural descent: visiting an instance recurses into its body, and every
// container (generate blocks, instance arrays) is a Scope the visitor walks
// through, so an instance nested in a generate block is reached without this
// code knowing the container taxonomy. The canonical body keys the dedup so a
// module instantiated many times is lowered once.
struct UnitCollector
    : slang::ast::ASTVisitor<UnitCollector, slang::ast::VisitFlags::Canonical> {
  std::unordered_set<const slang::ast::InstanceBodySymbol*> seen;
  std::vector<const slang::ast::InstanceBodySymbol*> order;

  void handle(const slang::ast::InstanceSymbol& inst) {
    const auto* canonical = inst.getCanonicalBody();
    const auto& body = canonical != nullptr ? *canonical : inst.body;
    if (seen.insert(&body).second) {
      order.push_back(&body);
      visitDefault(inst);
    }
  }
};

}  // namespace

auto LowerCompilation(const LowerCompilationFacts& facts)
    -> diag::Result<std::vector<hir::ModuleUnit>> {
  const UnitLoweringFacts unit_facts(facts.SourceMapper(), facts.Sensitivity());
  const auto& root = facts.Compilation().getRoot();

  UnitCollector collector;
  for (const auto* top : root.topInstances) {
    top->visit(collector);
  }

  std::vector<hir::ModuleUnit> units;
  units.reserve(collector.order.size());
  for (const auto* body : collector.order) {
    auto u = LowerModule(unit_facts, *body);
    if (!u) return std::unexpected(std::move(u.error()));
    units.push_back(*std::move(u));
  }

  return units;
}

auto TopLevelUnitNames(slang::ast::Compilation& compilation)
    -> std::vector<std::string> {
  const auto& root = compilation.getRoot();
  std::vector<std::string> names;
  names.reserve(root.topInstances.size());
  for (const auto* inst : root.topInstances) {
    names.emplace_back(inst->body.name);
  }
  return names;
}

}  // namespace lyra::lowering::ast_to_hir
