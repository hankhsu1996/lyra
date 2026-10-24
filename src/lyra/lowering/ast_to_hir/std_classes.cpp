#include "lyra/lowering/ast_to_hir/std_classes.hpp"

#include <algorithm>
#include <format>
#include <optional>
#include <string>

#include <slang/ast/ASTContext.h>
#include <slang/ast/Compilation.h>
#include <slang/ast/Scope.h>
#include <slang/ast/symbols/AttributeSymbol.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/syntax/AllSyntax.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/frontend/library_texts.hpp"
#include "lyra/lowering/ast_to_hir/expression/slang_atoms.hpp"
#include "lyra/lowering/ast_to_hir/subroutine_decl.hpp"
#include "lyra/support/builtin_fn.hpp"
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

auto DefinitionsUnit(slang::ast::Compilation& compilation)
    -> const slang::ast::CompilationUnitSymbol& {
  for (const auto* unit : compilation.getRoot().compilationUnits) {
    if (IsStdDefinitions(*unit)) {
      return *unit;
    }
  }
  throw InternalError("every compilation reads Lyra's definitions of std");
}

}  // namespace

auto ImportedRuntimeClassOf(const slang::ast::ClassType& cls)
    -> std::optional<support::ImportedRuntimeClass> {
  if (!IsDeclaredInStd(cls)) {
    return std::nullopt;
  }
  return support::ImportedRuntimeClassNamed(cls.name);
}

auto RuntimeCarriedMethodOf(const slang::ast::SubroutineSymbol& method)
    -> std::optional<support::BuiltinFn> {
  const slang::ast::ClassType* declaring = DeclaringClassOf(method);
  if (declaring == nullptr) return std::nullopt;
  const slang::ast::ClassType& owner = *declaring;
  if (const auto klass = ImportedRuntimeClassOf(owner)) {
    switch (*klass) {
      case support::ImportedRuntimeClass::kProcess:
        return LowerProcessMethodName(method.name);
    }
    throw InternalError("RuntimeCarriedMethodOf: unknown runtime class");
  }
  if (IsDeclaredInStd(owner) && owner.name == "weak_reference" &&
      method.name == "get_id") {
    return support::BuiltinFn::kObjectIdentity;
  }
  return std::nullopt;
}

auto IsHeldWeakly(const slang::ast::ClassPropertySymbol& prop) -> bool {
  const slang::ast::Scope* scope = prop.getParentScope();
  const slang::ast::Scope* unit =
      scope == nullptr ? nullptr : scope->asSymbol().getParentScope();
  if (unit == nullptr ||
      unit->asSymbol().kind != slang::ast::SymbolKind::CompilationUnit ||
      !IsStdDefinitions(
          unit->asSymbol().as<slang::ast::CompilationUnitSymbol>())) {
    return false;
  }
  return std::ranges::any_of(
      scope->getCompilation().getAttributes(prop),
      [](const slang::ast::AttributeSymbol* attribute) {
        return attribute->name == "lyra_weak";
      });
}

auto IsStdDefinitions(const slang::ast::CompilationUnitSymbol& unit) -> bool {
  return frontend::LibraryTextOf(unit) == &frontend::kStdDefinitions;
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
