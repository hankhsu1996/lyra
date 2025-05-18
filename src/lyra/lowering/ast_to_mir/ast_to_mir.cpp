#include "lyra/lowering/ast_to_mir/ast_to_mir.hpp"

#include <stdexcept>

#include <fmt/format.h>
#include <slang/ast/Compilation.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>

#include "lyra/lowering/ast_to_mir/module.hpp"
#include "lyra/mir/module.hpp"

namespace lyra::lowering::ast_to_mir {

auto AstToMir(const slang::ast::RootSymbol& root)
    -> std::unique_ptr<mir::Module> {
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

}  // namespace lyra::lowering::ast_to_mir
