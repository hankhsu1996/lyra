#include "lowering/ast_to_mir/ast_to_mir.hpp"

#include <stdexcept>

#include <fmt/format.h>
#include <slang/ast/Compilation.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>

#include "lowering/ast_to_mir/module.hpp"
#include "mir/module.hpp"

namespace lyra::lowering {

auto AstToMir(slang::ast::Compilation& compilation)
    -> std::unique_ptr<mir::Module> {
  const auto& root = compilation.getRoot();

  for (const auto& member : root.members()) {
    if (member.kind == slang::ast::SymbolKind::Instance) {
      const auto& instance_symbol = member.as<slang::ast::InstanceSymbol>();

      // Verify the instance has a valid name
      if (instance_symbol.name.empty()) {
        throw std::runtime_error("Instance symbol has empty name in AstToMir");
      }

      return LowerModule(instance_symbol);
    }
  }

  throw std::runtime_error("no top-level instance found in AstToMir");
}

}  // namespace lyra::lowering
