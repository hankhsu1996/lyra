#include "lyra/lowering/ast_to_hir/library_cell.hpp"

#include <format>
#include <string>

#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/text/SourceLocation.h>

namespace lyra::lowering::ast_to_hir {

auto IsLibraryCell(const slang::ast::DefinitionSymbol& definition) -> bool {
  return definition.getParentScope()->asSymbol().kind ==
         slang::ast::SymbolKind::CompilationUnit;
}

auto CellName(const slang::ast::DefinitionSymbol& cell) -> std::string {
  if (cell.sourceLibrary.isDefault) return std::string{cell.name};
  return std::format("{}.{}", cell.sourceLibrary.name, cell.name);
}

}  // namespace lyra::lowering::ast_to_hir
