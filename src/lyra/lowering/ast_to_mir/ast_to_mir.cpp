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

      // If top is specified, recursively collect all modules in hierarchy
      if (!top.empty()) {
        if (instance_symbol.body.name == top) {
          std::unordered_set<std::string> processed;
          CollectModulesRecursive(instance_symbol, modules, processed);
          return modules;  // Returns all modules in dependency order
        }
        continue;  // Skip non-matching modules
      }

      // No top specified: collect all modules (for dump command)
      modules.push_back(LowerModule(instance_symbol));
    }
  }

  if (!top.empty() && modules.empty()) {
    throw DiagnosticException(
        Diagnostic::Error(
            slang::SourceRange{},
            std::format("top module '{}' not found", top)));
  }

  if (modules.empty()) {
    throw DiagnosticException(
        Diagnostic::Error(slang::SourceRange{}, "no top-level instance found"));
  }

  return modules;
}

}  // namespace lyra::lowering::ast_to_mir
