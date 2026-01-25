#include "lyra/lowering/ast_to_hir/design.hpp"

#include <algorithm>
#include <cstddef>
#include <format>
#include <span>
#include <utility>
#include <vector>

#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/hir/design.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/generate.hpp"
#include "lyra/lowering/ast_to_hir/module.hpp"
#include "lyra/lowering/ast_to_hir/package.hpp"
#include "lyra/lowering/ast_to_hir/port_binding.hpp"
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

  CollectedMembers members;
  CollectScopeMembers(body, registrar, members);

  for (const auto* var : members.variables) {
    TypeId type = LowerType(var->getType(), span, ctx);
    if (type) {
      registrar.Register(*var, SymbolKind::kVariable, type);
    }
  }

  for (const auto* sub : members.functions) {
    const auto& ret_type = sub->getReturnType();
    if (!ret_type.isIntegral() && !ret_type.isVoid()) {
      continue;
    }
    TypeId return_type = LowerType(ret_type, span, ctx);
    if (return_type) {
      registrar.Register(*sub, SymbolKind::kFunction, return_type);
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
  // Collect input port bindings during traversal.
  std::vector<const slang::ast::InstanceSymbol*> all_instances;
  TransientPortBindingPlan port_bindings;

  for (const auto* inst : root.topInstances) {
    all_instances.push_back(inst);
  }
  for (size_t i = 0; i < all_instances.size(); ++i) {
    const auto* parent = all_instances[i];

    for (const auto& child :
         parent->body.membersOfType<slang::ast::InstanceSymbol>()) {
      // Collect input port bindings for this parent->child edge
      for (const auto* port_sym : child.body.getPortList()) {
        // Skip non-Port symbols (e.g., InterfacePortSymbol)
        if (port_sym->kind != slang::ast::SymbolKind::Port) {
          continue;
        }

        const auto& port = port_sym->as<slang::ast::PortSymbol>();

        // Phase 1: Only handle input ports
        if (port.direction != slang::ast::ArgumentDirection::In) {
          continue;
        }

        // Fail loudly for null ports (no backing variable)
        slang::SourceRange port_range{port.location, port.location};
        if (port.internalSymbol == nullptr) {
          ctx->sink->Error(
              ctx->SpanOf(port_range),
              std::format(
                  "input port '{}' has no backing variable (null port)",
                  port.name));
          continue;
        }

        // Fail loudly for non-Variable backing (net, interface, etc.)
        if (port.internalSymbol->kind != slang::ast::SymbolKind::Variable) {
          ctx->sink->Error(
              ctx->SpanOf(port_range),
              std::format(
                  "input port '{}' backed by {} not supported (only variable "
                  "ports)",
                  port.name, toString(port.internalSymbol->kind)));
          continue;
        }

        // Get connection for this port
        const auto* conn = child.getPortConnection(port);
        if (conn == nullptr) {
          ctx->sink->Error(
              ctx->SpanOf(port_range),
              std::format(
                  "unconnected input port '{}' without default value",
                  port.name));
          continue;
        }

        const auto* expr = conn->getExpression();
        if (expr == nullptr) {
          // May be implicit named connection or default value
          ctx->sink->Error(
              ctx->SpanOf(port_range), std::format(
                                           "input port '{}': implicit named "
                                           "connections not yet supported",
                                           port.name));
          continue;
        }

        port_bindings[parent].push_back(
            InputPortBinding{
                .target_var =
                    &port.internalSymbol->as<slang::ast::VariableSymbol>(),
                .value_expr = expr,
                .connection_range = expr->sourceRange,
            });
      }

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
    auto it = port_bindings.find(instance);
    std::span<const InputPortBinding> bindings =
        (it != port_bindings.end())
            ? std::span<const InputPortBinding>(it->second)
            : std::span<const InputPortBinding>{};

    elements.emplace_back(LowerModule(*instance, registrar, ctx, bindings));
  }

  return hir::Design{
      .elements = std::move(elements),
  };
}

}  // namespace lyra::lowering::ast_to_hir
