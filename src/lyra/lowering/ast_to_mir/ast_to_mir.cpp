#include "lyra/lowering/ast_to_mir/ast_to_mir.hpp"

#include <format>
#include <memory>
#include <string>
#include <unordered_set>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>

#include "lyra/common/diagnostic.hpp"
#include "lyra/lowering/ast_to_mir/module.hpp"
#include "lyra/lowering/ast_to_mir/package.hpp"
#include "lyra/mir/module.hpp"

namespace lyra::lowering::ast_to_mir {

namespace {

// Recursively collect all modules in the hierarchy.
// Uses depth-first traversal: children are collected before parents,
// ensuring dependency order (child modules defined before parent needs them).
// Each instance gets its own MIR module (no signature deduplication).
// Deduplication happens at codegen emit time instead.
void CollectModulesRecursive(
    const slang::ast::InstanceSymbol& instance,
    std::vector<std::unique_ptr<mir::Module>>& modules,
    std::unordered_set<const slang::ast::InstanceSymbol*>& visited) {
  // Guard against revisiting same instance (defensive, shouldn't happen)
  if (visited.contains(&instance)) {
    return;
  }
  visited.insert(&instance);

  // First, recurse into child instances (depth-first)
  for (const auto& member : instance.body.members()) {
    if (member.kind == slang::ast::SymbolKind::Instance) {
      const auto& child = member.as<slang::ast::InstanceSymbol>();
      CollectModulesRecursive(child, modules, visited);
    }
  }

  // Then add this module (after all children are processed)
  modules.push_back(LowerModule(instance));
}

}  // namespace

auto AstToMir(slang::ast::Compilation& compilation, const std::string& top)
    -> LoweringResult {
  LoweringResult result;

  // Phase 1: Lower all packages (except built-in std package)
  for (const auto* pkg : compilation.getPackages()) {
    if (pkg->name == "std") {
      continue;  // Skip built-in std package
    }
    result.packages.push_back(LowerPackage(*pkg));
  }

  // Phase 2: Lower modules
  const auto& root = compilation.getRoot();

  // Collect all top-level instances
  std::vector<const slang::ast::InstanceSymbol*> top_instances;
  for (const auto& member : root.members()) {
    if (member.kind == slang::ast::SymbolKind::Instance) {
      const auto& instance_symbol = member.as<slang::ast::InstanceSymbol>();

      // Verify the instance has a valid name
      if (instance_symbol.name.empty()) {
        slang::SourceRange source_range(
            instance_symbol.location, instance_symbol.location);
        throw DiagnosticException(
            Diagnostic::Error(source_range, "instance symbol has empty name"));
      }

      // If top is specified, find and collect it with hierarchy
      if (!top.empty()) {
        if (instance_symbol.body.name == top) {
          std::unordered_set<const slang::ast::InstanceSymbol*> visited;
          CollectModulesRecursive(instance_symbol, result.modules, visited);
          return result;  // Returns all modules in dependency order
        }
        continue;  // Skip non-matching modules
      }

      top_instances.push_back(&instance_symbol);
    }
  }

  if (!top.empty() && result.modules.empty()) {
    throw DiagnosticException(
        Diagnostic::Error(
            slang::SourceRange{},
            std::format("top module '{}' not found", top)));
  }

  if (top_instances.empty()) {
    throw DiagnosticException(
        Diagnostic::Error(slang::SourceRange{}, "no top-level instance found"));
  }

  // No top specified: if single top-level instance, collect hierarchy
  // If multiple, collect all without hierarchy (for dump command)
  if (top_instances.size() == 1) {
    std::unordered_set<const slang::ast::InstanceSymbol*> visited;
    CollectModulesRecursive(*top_instances[0], result.modules, visited);
  } else {
    for (const auto* instance : top_instances) {
      result.modules.push_back(LowerModule(*instance));
    }
  }

  return result;
}

}  // namespace lyra::lowering::ast_to_mir
