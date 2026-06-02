#include "lyra/lowering/ast_to_hir/lower.hpp"

#include <cstddef>
#include <expected>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/module_unit.hpp"
#include "lyra/lowering/ast_to_hir/facts.hpp"
#include "lyra/lowering/ast_to_hir/module.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerCompilation(const LowerCompilationFacts& facts)
    -> diag::Result<std::vector<hir::ModuleUnit>> {
  const UnitLoweringFacts unit_facts(facts.SourceMapper(), facts.Sensitivity());
  const auto& root = facts.Compilation().getRoot();

  // Worklist of unit bodies, seeded with the top-level blocks and grown by
  // walking each lowered body's child instances, deduped so each definition is
  // lowered once. A child is reached only through its instance's definition
  // name (its header); the parent never reads the child's body.
  std::vector<const slang::ast::InstanceBodySymbol*> worklist;
  std::unordered_set<const slang::ast::DefinitionSymbol*> seen_defs;
  std::vector<hir::ModuleUnit> units;

  for (const auto* inst : root.topInstances) {
    const auto* body = &inst->body;
    if (seen_defs.insert(&body->getDefinition()).second) {
      worklist.push_back(body);
    }
  }

  for (std::size_t i = 0; i < worklist.size(); ++i) {
    const auto* body = worklist[i];
    auto u = LowerModule(unit_facts, *body);
    if (!u) return std::unexpected(std::move(u.error()));
    units.push_back(*std::move(u));

    for (const auto& member : body->members()) {
      if (member.kind != slang::ast::SymbolKind::Instance) {
        continue;
      }
      const auto* child_body = &member.as<slang::ast::InstanceSymbol>().body;
      if (seen_defs.insert(&child_body->getDefinition()).second) {
        worklist.push_back(child_body);
      }
    }
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
