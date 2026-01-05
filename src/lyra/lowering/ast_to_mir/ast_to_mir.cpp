#include "lyra/lowering/ast_to_mir/ast_to_mir.hpp"

#include <memory>

#include <fmt/format.h>
#include <slang/ast/Compilation.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>

#include "lyra/common/diagnostic.hpp"
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
        slang::SourceRange source_range(
            instance_symbol.location, instance_symbol.location);
        throw DiagnosticException(
            Diagnostic::Error(source_range, "instance symbol has empty name"));
      }

      return LowerModule(instance_symbol);
    }
  }

  throw DiagnosticException(
      Diagnostic::Error(slang::SourceRange{}, "no top-level instance found"));
}

}  // namespace lyra::lowering::ast_to_mir
