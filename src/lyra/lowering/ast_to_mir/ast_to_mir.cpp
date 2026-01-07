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
#include "lyra/mir/module.hpp"

namespace lyra::lowering::ast_to_mir {

namespace {

// Recursively collect all modules in the hierarchy.
// Uses depth-first traversal: children are collected before parents,
// ensuring dependency order (child modules defined before parent needs them).
void CollectModulesRecursive(
    const slang::ast::InstanceSymbol& instance,
    std::vector<std::unique_ptr<mir::Module>>& modules,
    std::unordered_set<std::string>& processed) {
  std::string name(instance.body.name);

  // Skip already processed modules (avoids duplicates for multiple instances)
  if (processed.contains(name)) {
    return;
  }
  processed.insert(name);

  // First, recurse into child instances (depth-first)
  for (const auto& member : instance.body.members()) {
    if (member.kind == slang::ast::SymbolKind::Instance) {
      const auto& child = member.as<slang::ast::InstanceSymbol>();
      CollectModulesRecursive(child, modules, processed);
    }
  }

  // Then add this module (after all children are processed)
  modules.push_back(LowerModule(instance));
}

}  // namespace

auto AstToMir(const slang::ast::RootSymbol& root, const std::string& top)
    -> std::vector<std::unique_ptr<mir::Module>> {
  std::vector<std::unique_ptr<mir::Module>> modules;

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
          std::unordered_set<std::string> processed;
          CollectModulesRecursive(instance_symbol, modules, processed);
          return modules;  // Returns all modules in dependency order
        }
        continue;  // Skip non-matching modules
      }

      top_instances.push_back(&instance_symbol);
    }
  }

  if (!top.empty() && modules.empty()) {
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
    std::unordered_set<std::string> processed;
    CollectModulesRecursive(*top_instances[0], modules, processed);
  } else {
    for (const auto* instance : top_instances) {
      modules.push_back(LowerModule(*instance));
    }
  }

  return modules;
}

}  // namespace lyra::lowering::ast_to_mir
