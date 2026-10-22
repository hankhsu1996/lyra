#include "lyra/lowering/ast_to_hir/std_classes.hpp"

#include <format>
#include <optional>
#include <string>

#include <slang/ast/ASTContext.h>
#include <slang/ast/Compilation.h>
#include <slang/ast/Scope.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
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

auto DefineStdClasses(slang::ast::Compilation& compilation) -> StdDefinitions {
  const slang::ast::CompilationUnitSymbol* definitions = nullptr;
  for (const auto* unit : compilation.getRoot().compilationUnits) {
    if (IsStdDefinitions(*unit)) {
      definitions = unit;
    }
  }
  if (definitions == nullptr) {
    throw InternalError(
        "DefineStdClasses: every compilation reads Lyra's definitions of std");
  }
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
