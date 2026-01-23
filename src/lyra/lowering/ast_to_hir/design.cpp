#include "lyra/lowering/ast_to_hir/design.hpp"

#include <algorithm>
#include <cstddef>
#include <utility>
#include <vector>

#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/hir/design.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/module.hpp"
#include "lyra/lowering/ast_to_hir/package.hpp"
#include "lyra/lowering/ast_to_hir/source_utils.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {
// Phase 0: Register module-level symbols for one instance.
// Establishes SymbolIds before any lowering runs. Scope is currently
// unused downstream but assigned for future use.
void RegisterModuleDeclarations(
    const slang::ast::InstanceSymbol& instance, SymbolRegistrar& registrar,
    Context* ctx) {
  const slang::ast::InstanceBodySymbol& body = instance.body;
  SourceSpan span = ctx->SpanOf(GetSourceRange(instance));

  registrar.Register(instance, SymbolKind::kInstance, kInvalidTypeId);

  ScopeGuard scope_guard(registrar, ScopeKind::kModule);

  for (const slang::ast::VariableSymbol& var :
       body.membersOfType<slang::ast::VariableSymbol>()) {
    TypeId type = LowerType(var.getType(), span, ctx);
    if (type) {
      registrar.Register(var, SymbolKind::kVariable, type);
    }
  }

  for (const slang::ast::SubroutineSymbol& sub :
       body.membersOfType<slang::ast::SubroutineSymbol>()) {
    if (sub.subroutineKind == slang::ast::SubroutineKind::Function) {
      const auto& ret_type = sub.getReturnType();
      if (!ret_type.isIntegral() && !ret_type.isVoid()) {
        continue;
      }
      TypeId return_type = LowerType(ret_type, span, ctx);
      if (return_type) {
        registrar.Register(sub, SymbolKind::kFunction, return_type);
      }
    }
  }
}
}  // namespace

auto LowerDesign(
    slang::ast::Compilation& compilation, SymbolRegistrar& registrar,
    Context* ctx) -> hir::Design {
  const slang::ast::RootSymbol& root = compilation.getRoot();

  std::vector<hir::DesignElement> elements;

  // Enumerate all instances (BFS — no ordering semantics)
  std::vector<const slang::ast::InstanceSymbol*> all_instances;
  for (const auto* inst : root.topInstances) {
    all_instances.push_back(inst);
  }
  for (size_t i = 0; i < all_instances.size(); ++i) {
    for (const auto& child :
         all_instances[i]->body.membersOfType<slang::ast::InstanceSymbol>()) {
      all_instances.push_back(&child);
    }
  }

  // Stable sort for deterministic slot ordering.
  // getHierarchicalPath() returns a canonical string (no address components).
  std::ranges::sort(all_instances, [](const auto* a, const auto* b) {
    return a->getHierarchicalPath() < b->getHierarchicalPath();
  });

  // Phase 0: Register all module declarations (creation allowed)
  for (const auto* instance : all_instances) {
    RegisterModuleDeclarations(*instance, registrar, ctx);
  }

  // Lower packages (independent of instances)
  for (const slang::ast::PackageSymbol* pkg : compilation.getPackages()) {
    if (pkg->name == "std") {
      continue;
    }
    elements.emplace_back(LowerPackage(*pkg, registrar, ctx));
  }

  // Phase 1: Lower module bodies.
  // Design-level symbols were pre-registered in Phase 0.
  // Hierarchical refs use Lookup (fail-fast if Phase 0 missed a target).
  // Local symbols (function params, loop vars) are still created here.
  for (const auto* instance : all_instances) {
    elements.emplace_back(LowerModule(*instance, registrar, ctx));
  }

  return hir::Design{
      .elements = std::move(elements),
  };
}

}  // namespace lyra::lowering::ast_to_hir
