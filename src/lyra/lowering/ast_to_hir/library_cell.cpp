#include "lyra/lowering/ast_to_hir/library_cell.hpp"

#include <format>
#include <string>
#include <string_view>

#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/text/SourceLocation.h>

#include "lyra/base/internal_error.hpp"

namespace lyra::lowering::ast_to_hir {

auto IsLibraryCell(const slang::ast::DefinitionSymbol& definition) -> bool {
  return definition.getParentScope()->asSymbol().kind ==
         slang::ast::SymbolKind::CompilationUnit;
}

auto BodyDeclaring(const slang::ast::DefinitionSymbol& nested)
    -> const slang::ast::InstanceBodySymbol& {
  const auto* body = nested.getParentScope()
                         ->asSymbol()
                         .as_if<slang::ast::InstanceBodySymbol>();
  if (body == nullptr) {
    throw InternalError(
        "BodyDeclaring: a design element is declared outside every other or "
        "in the body of one");
  }
  return *body;
}

auto CellName(const slang::ast::DefinitionSymbol& cell) -> std::string {
  if (cell.sourceLibrary.isDefault) return std::string{cell.name};
  return std::format("{}.{}", cell.sourceLibrary.name, cell.name);
}

namespace {

// The cell a unit's text lies in. A design element declared inside another
// (LRM 23.4) is no cell, and neither is the one declaring it where that one is
// nested too, so the answer is the first one outward that is.
auto CellHolding(const slang::ast::Symbol& unit) -> std::string_view {
  if (unit.kind == slang::ast::SymbolKind::Package) return unit.name;
  if (unit.kind == slang::ast::SymbolKind::CompilationUnit) return "$unit";

  const auto* body = unit.as_if<slang::ast::InstanceBodySymbol>();
  if (body == nullptr) {
    throw InternalError(
        "CellHolding: a unit is a package, a `$unit` scope, or the body of a "
        "design element");
  }
  const slang::ast::DefinitionSymbol* definition = &body->getDefinition();
  while (!IsLibraryCell(*definition)) {
    definition = &BodyDeclaring(*definition).getDefinition();
  }
  return definition->name;
}

}  // namespace

auto LibraryBindingOf(const slang::ast::Symbol& unit) -> std::string {
  const slang::SourceLibrary* library = unit.getSourceLibrary();
  if (library == nullptr) {
    throw InternalError(
        "LibraryBindingOf: a unit lies in no compilation unit, so no library "
        "holds it");
  }
  return std::format("{}.{}", library->name, CellHolding(unit));
}

}  // namespace lyra::lowering::ast_to_hir
