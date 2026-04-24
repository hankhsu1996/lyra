#include "lyra/lowering/ast_to_hir/lower.hpp"

#include <unordered_set>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>

#include "lyra/hir/module_unit.hpp"
#include "module.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Frontend discovery: walk elaborated top instances and canonicalize to the
// set of unique specialization bodies. Dedup by body pointer is sufficient
// here because topInstances is already a unique set of named roots.
auto CollectTopCompilationUnitBodies(slang::ast::Compilation& compilation)
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

auto LowerCompilation(slang::ast::Compilation& compilation)
    -> std::vector<hir::ModuleUnit> {
  std::vector<hir::ModuleUnit> units;
  for (const auto* body : CollectTopCompilationUnitBodies(compilation)) {
    units.push_back(LowerModule(*body));
  }
  return units;
}

}  // namespace lyra::lowering::ast_to_hir
