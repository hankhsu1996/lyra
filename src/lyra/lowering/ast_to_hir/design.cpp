#include "lyra/lowering/ast_to_hir/design.hpp"

#include <utility>
#include <vector>

#include "lyra/hir/design.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/module.hpp"
#include "lyra/lowering/ast_to_hir/package.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerDesign(
    slang::ast::Compilation& compilation, SymbolRegistrar& registrar,
    Context* ctx) -> hir::Design {
  const slang::ast::RootSymbol& root = compilation.getRoot();

  std::vector<hir::DesignElement> elements;

  // Lower user-defined packages first (modules may reference them)
  for (const slang::ast::PackageSymbol* pkg : compilation.getPackages()) {
    if (pkg->name == "std") {
      continue;
    }
    elements.emplace_back(LowerPackage(*pkg, registrar, ctx));
  }

  // Lower top-level module instances
  for (const slang::ast::InstanceSymbol* instance : root.topInstances) {
    elements.emplace_back(LowerModule(*instance, registrar, ctx));
  }

  return hir::Design{
      .elements = std::move(elements),
  };
}

}  // namespace lyra::lowering::ast_to_hir
