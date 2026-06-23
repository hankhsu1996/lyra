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
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/specialization_name.hpp"

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

// Per-compilation lowerer: walks the top instances, dedups to one canonical
// body per distinct unit, and constructs one ModuleLowerer per unit. Runs
// once via Run(); file-local because no driver consumer needs to see it.
class CompilationLowerer {
 public:
  explicit CompilationLowerer(const LowerCompilationFacts& facts)
      : facts_(facts), unit_facts_(facts.SourceMapper(), facts.Sensitivity()) {
  }

  auto Run() -> diag::Result<std::vector<hir::ModuleUnit>> {
    const auto unit_bodies = CollectUnits();
    std::vector<hir::ModuleUnit> units;
    units.reserve(unit_bodies.size());
    for (const auto* body : unit_bodies) {
      ModuleLowerer module(unit_facts_, *body);
      auto unit_or = module.Run();
      if (!unit_or) return std::unexpected(std::move(unit_or.error()));
      units.push_back(*std::move(unit_or));
    }
    return units;
  }

 private:
  auto CollectUnits() -> std::vector<const slang::ast::InstanceBodySymbol*> {
    const auto& root = facts_.Compilation().getRoot();
    UnitCollector collector;
    for (const auto* top : root.topInstances) {
      top->visit(collector);
    }
    return std::move(collector.order);
  }

  LowerCompilationFacts facts_;
  LoweringFacts unit_facts_;
};

}  // namespace

auto LowerCompilation(const LowerCompilationFacts& facts)
    -> diag::Result<std::vector<hir::ModuleUnit>> {
  CompilationLowerer lowerer(facts);
  return lowerer.Run();
}

auto TopLevelUnitNames(slang::ast::Compilation& compilation)
    -> std::vector<std::string> {
  const auto& root = compilation.getRoot();
  std::vector<std::string> names;
  names.reserve(root.topInstances.size());
  for (const auto* inst : root.topInstances) {
    names.emplace_back(SpecializationName(*inst));
  }
  return names;
}

}  // namespace lyra::lowering::ast_to_hir
