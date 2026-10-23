#include "lyra/lowering/ast_to_hir/std_classes.hpp"

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
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/syntax/AllSyntax.h>
#include <slang/text/SourceManager.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/frontend/std_definitions.hpp"
#include "lyra/support/imported_runtime_class.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Whether `cls` is a class the front end declares in `std` itself, rather than
// a user's class that happens to share a name with one.
auto IsDeclaredInStd(const slang::ast::ClassType& cls) -> bool {
  const slang::ast::Scope* scope = cls.getParentScope();
  return scope != nullptr &&
         scope == static_cast<const slang::ast::Scope*>(
                      &scope->getCompilation().getStdPackage());
}

// The definition of `declared` that a specialization of it reaches, specialized
// on the same parameters: the definition is told them from inside the
// declaration's specialization, where each parameter's name stands for what
// the design gave it.
auto SpecializeAlike(
    const slang::ast::GenericClassDefSymbol& definition,
    const slang::ast::ClassType& declared) -> const slang::ast::ClassType& {
  std::string assignments;
  for (const slang::ast::Symbol* param : declared.genericParameters) {
    if (!assignments.empty()) assignments += ", ";
    assignments += std::format(".{0}({0})", param->name);
  }
  const auto& name =
      declared.getCompilation()
          .parseName(std::format("{} #({})", declared.name, assignments))
          .as<slang::syntax::ClassNameSyntax>();
  const auto* specialized =
      definition
          .getSpecialization(
              slang::ast::ASTContext(declared, slang::ast::LookupLocation::max),
              *name.parameters)
          .as_if<slang::ast::ClassType>();
  if (specialized == nullptr) {
    throw InternalError(
        std::format(
            "DefineStdClasses: Lyra's definitions of std do not define '{}' "
            "for the parameters '{}'",
            declared.name, assignments));
  }
  return *specialized;
}

}  // namespace

auto ImportedRuntimeClassOf(const slang::ast::ClassType& cls)
    -> std::optional<support::ImportedRuntimeClass> {
  if (!IsDeclaredInStd(cls)) {
    return std::nullopt;
  }
  return support::ImportedRuntimeClassNamed(cls.name);
}

auto IsStdDefinitions(const slang::ast::CompilationUnitSymbol& unit) -> bool {
  const slang::SourceManager* sources =
      unit.getCompilation().getSourceManager();
  if (sources == nullptr) {
    return false;
  }
  for (const auto& member : unit.members()) {
    if (member.location.valid()) {
      return sources->getRawFileName(member.location.buffer()) ==
             frontend::kStdDefinitionsBufferName;
    }
  }
  return false;
}

namespace {

auto DefinitionsUnit(slang::ast::Compilation& compilation)
    -> const slang::ast::CompilationUnitSymbol& {
  for (const auto* unit : compilation.getRoot().compilationUnits) {
    if (IsStdDefinitions(*unit)) {
      return *unit;
    }
  }
  throw InternalError("every compilation reads Lyra's definitions of std");
}

auto DefinedClass(
    const slang::ast::CompilationUnitSymbol& definitions, std::string_view name)
    -> const slang::ast::ClassType& {
  const slang::ast::Symbol* found = definitions.find(name);
  if (found == nullptr || found->kind != slang::ast::SymbolKind::ClassType) {
    throw InternalError(
        std::format("Lyra's definitions of std define no class '{}'", name));
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
    -> CoverageLibrary {
  const slang::ast::CompilationUnitSymbol& definitions =
      DefinitionsUnit(compilation);
  CoverageLibrary library;
  library.registry = &DefinedClass(definitions, "coverage_registry");
  library.group_type = &DefinedClass(definitions, "covergroup_type");
  library.group_instance = &DefinedClass(definitions, "covergroup_instance");
  library.cross = &DefinedClass(definitions, "coverage_cross");
  library.selection = &DefinedClass(definitions, "coverage_selection");
  library.real_coverpoint = &DefinedClass(definitions, "real_coverpoint");
  const auto& integral = definitions.find("integral_coverpoint")
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

auto DefineStdClasses(slang::ast::Compilation& compilation) -> StdDefinitions {
  const slang::ast::CompilationUnitSymbol* definitions =
      &DefinitionsUnit(compilation);
  StdDefinitions defined;
  for (const slang::ast::Symbol& member :
       compilation.getStdPackage().members()) {
    const slang::ast::Symbol* definition = definitions->find(member.name);
    if (definition == nullptr) continue;
    if (const auto* cls = member.as_if<slang::ast::ClassType>()) {
      defined.emplace(cls, &definition->as<slang::ast::ClassType>());
      continue;
    }
    const auto& generic = member.as<slang::ast::GenericClassDefSymbol>();
    for (const slang::ast::Type& specialized : generic.specializations()) {
      const auto& declared = specialized.as<slang::ast::ClassType>();
      defined.emplace(
          &declared,
          &SpecializeAlike(
              definition->as<slang::ast::GenericClassDefSymbol>(), declared));
    }
  }
  return defined;
}

auto DefiningMethod(
    const slang::ast::ClassType& definition,
    const slang::ast::SubroutineSymbol& declared)
    -> const slang::ast::SubroutineSymbol& {
  const slang::ast::Symbol* member = definition.find(declared.name);
  const auto* defined = member == nullptr
                            ? nullptr
                            : member->as_if<slang::ast::SubroutineSymbol>();
  if (defined == nullptr) {
    throw InternalError(
        std::format(
            "DefiningMethod: Lyra's definition of '{}' defines no method '{}'",
            definition.name, declared.name));
  }
  return *defined;
}

}  // namespace lyra::lowering::ast_to_hir
