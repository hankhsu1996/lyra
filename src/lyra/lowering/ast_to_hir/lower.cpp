#include "lyra/lowering/ast_to_hir/lower.hpp"

#include <expected>
#include <unordered_set>
#include <utility>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>

#include "facts.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/module_unit.hpp"
#include "module.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto CollectTopBodies(slang::ast::Compilation& compilation)
    -> std::vector<const slang::ast::InstanceBodySymbol*> {
  const auto& root = compilation.getRoot();
  std::vector<const slang::ast::InstanceBodySymbol*> bodies;
  std::unordered_set<const slang::ast::InstanceBodySymbol*> seen;
  for (const auto* inst : root.topInstances) {
    const auto* body = &inst->body;
    if (seen.insert(body).second) {
      bodies.push_back(body);
    }
  }
  return bodies;
}

}  // namespace

auto LowerCompilation(const LowerCompilationFacts& facts)
    -> diag::Result<std::vector<hir::ModuleUnit>> {
  const UnitLoweringFacts unit_facts(facts.SourceMapper());
  std::vector<hir::ModuleUnit> units;
  for (const auto* body : CollectTopBodies(facts.Compilation())) {
    auto u = LowerModule(unit_facts, *body);
    if (!u) return std::unexpected(std::move(u.error()));
    units.push_back(*std::move(u));
  }
  return units;
}

}  // namespace lyra::lowering::ast_to_hir
