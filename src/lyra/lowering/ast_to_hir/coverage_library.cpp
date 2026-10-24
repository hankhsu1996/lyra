#include "lyra/lowering/ast_to_hir/coverage_library.hpp"

#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>

#include <slang/ast/ASTContext.h>
#include <slang/ast/Compilation.h>
#include <slang/ast/Scope.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/CoverSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/syntax/AllSyntax.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/frontend/library_texts.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto LibraryUnit(slang::ast::Compilation& compilation)
    -> const slang::ast::CompilationUnitSymbol* {
  for (const auto* unit : compilation.getRoot().compilationUnits) {
    if (frontend::LibraryTextOf(*unit) == &frontend::kCoverageLibrary) {
      return unit;
    }
  }
  return nullptr;
}

auto DefinedClass(
    const slang::ast::CompilationUnitSymbol& library, std::string_view name)
    -> const slang::ast::ClassType& {
  const slang::ast::Symbol* found = library.find(name);
  if (found == nullptr || found->kind != slang::ast::SymbolKind::ClassType) {
    throw InternalError(
        std::format("Lyra's coverage library defines no class '{}'", name));
  }
  return found->as<slang::ast::ClassType>();
}

// The type a coverpoint's values have (LRM 19.5), as the source can name it
// from inside the coverpoint: the type it declares, or its expression's own.
// A coverpoint a cross declares for a variable (LRM 19.6) is the variable's.
auto CoverpointTypeText(const slang::ast::CoverpointSymbol& point)
    -> std::string {
  const slang::syntax::SyntaxNode* syntax = point.getSyntax();
  if (syntax == nullptr ||
      syntax->kind != slang::syntax::SyntaxKind::Coverpoint) {
    return std::format("type({})", point.name);
  }
  const auto& written = syntax->as<slang::syntax::CoverpointSyntax>();
  if (written.type->kind != slang::syntax::SyntaxKind::ImplicitType) {
    return written.type->toString();
  }
  return std::format("type({})", written.expr->toString());
}

// The scope a coverpoint's expression and type are read in: the covergroup's,
// which holds its arguments and those of an overriding `sample` (LRM 19.8.1)
// and reaches what encloses the covergroup. The body below it is not, since
// a coverpoint written over a variable takes the variable's name (LRM 19.5),
// and there the name would find the coverpoint.
auto CovergroupScopeOf(const slang::ast::CoverpointSymbol& point)
    -> const slang::ast::Scope& {
  return point.getParentScope()
      ->asSymbol()
      .getParentScope()
      ->asSymbol()
      .as<slang::ast::CovergroupType>();
}

// The integral coverpoint at the type a coverpoint's values have (LRM 19.5):
// the type it declares, or its expression's own. The definition is told it
// from the covergroup's scope, where its expression and its type mean what
// the source wrote, the arguments of an overriding `sample` among what they
// may name, as a coverpoint's own may (LRM 19.8.1).
auto SpecializeCoverpoint(
    const slang::ast::GenericClassDefSymbol& definition,
    const slang::ast::CoverpointSymbol& point) -> const slang::ast::ClassType& {
  const std::string type_text = CoverpointTypeText(point);
  const std::string text =
      std::format("{} #(.T({}))", definition.name, type_text);
  const auto& name = point.getCompilation()
                         .parseName(text)
                         .as<slang::syntax::ClassNameSyntax>();
  const auto* specialized =
      definition
          .getSpecialization(
              slang::ast::ASTContext(
                  CovergroupScopeOf(point), slang::ast::LookupLocation::max,
                  slang::ast::ASTFlags::AllowCoverageSampleFormal),
              *name.parameters)
          .as_if<slang::ast::ClassType>();
  if (specialized == nullptr) {
    throw InternalError(
        std::format(
            "DefineCoverageClasses: Lyra's coverage library does not define "
            "'{}'",
            text));
  }
  return *specialized;
}

}  // namespace

auto DefineCoverageClasses(
    slang::ast::Compilation& compilation,
    std::span<const slang::ast::CovergroupType* const> covergroups)
    -> std::optional<CoverageLibrary> {
  const slang::ast::CompilationUnitSymbol* unit = LibraryUnit(compilation);
  if (unit == nullptr) {
    return std::nullopt;
  }
  CoverageLibrary library;
  library.registry = &DefinedClass(*unit, "coverage_registry");
  library.group_type = &DefinedClass(*unit, "covergroup_type");
  library.group_instance = &DefinedClass(*unit, "covergroup_instance");
  library.cross = &DefinedClass(*unit, "coverage_cross");
  library.selection = &DefinedClass(*unit, "coverage_selection");
  library.real_coverpoint = &DefinedClass(*unit, "real_coverpoint");
  const auto& integral = unit->find("integral_coverpoint")
                             ->as<slang::ast::GenericClassDefSymbol>();
  for (const slang::ast::CovergroupType* covergroup : covergroups) {
    for (const auto& point :
         covergroup->getBody().membersOfType<slang::ast::CoverpointSymbol>()) {
      if (point.getType().isFloating()) continue;
      library.integral_coverpoints.emplace(
          &point, &SpecializeCoverpoint(integral, point));
    }
  }
  return library;
}

auto CoverpointClassOf(
    const CoverageLibrary& library, const slang::ast::CoverpointSymbol& point)
    -> const slang::ast::ClassType& {
  if (point.getType().isFloating()) return *library.real_coverpoint;
  const auto it = library.integral_coverpoints.find(&point);
  if (it == library.integral_coverpoints.end()) {
    throw InternalError(
        std::format(
            "CoverpointClassOf: coverpoint '{}' was given no class before the "
            "design's units were collected",
            point.name));
  }
  return *it->second;
}

}  // namespace lyra::lowering::ast_to_hir
