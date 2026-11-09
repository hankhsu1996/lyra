#include "lyra/lowering/ast_to_hir/coverage_library.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

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
  library.unit = unit;
  library.registry = &DefinedClass(*unit, "coverage_registry");
  library.item = &DefinedClass(*unit, "coverage_item");
  library.coverpoint = &DefinedClass(*unit, "coverage_coverpoint");
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

auto FindLibraryMember(const slang::ast::ClassType& cls, std::string_view name)
    -> std::optional<LibraryMember> {
  for (const slang::ast::ClassType* at = &cls; at != nullptr;) {
    // A class's scope finds what it inherits too, so the class owning the
    // member is the one whose scope declares it.
    if (const slang::ast::Symbol* found = at->find(name);
        found != nullptr &&
        (found->kind == slang::ast::SymbolKind::Subroutine ||
         found->kind == slang::ast::SymbolKind::ClassProperty)) {
      return LibraryMember{
          .owner =
              &found->getParentScope()->asSymbol().as<slang::ast::ClassType>(),
          .member = found};
    }
    const slang::ast::Type* base = at->getBaseClass();
    at = base == nullptr
             ? nullptr
             : base->getCanonicalType().as_if<slang::ast::ClassType>();
  }
  return std::nullopt;
}

namespace {

// The type the library declares under `name`, where it is a `T`.
template <typename T>
auto LibraryTypeNamed(const CoverageLibrary& library, std::string_view name)
    -> const T* {
  const slang::ast::Symbol* declared = library.unit->find(name);
  const auto* alias = declared == nullptr
                          ? nullptr
                          : declared->as_if<slang::ast::TypeAliasType>();
  return alias == nullptr ? nullptr : alias->getCanonicalType().as_if<T>();
}

}  // namespace

auto LibraryEnumeratorOf(
    const CoverageLibrary& library, std::string_view enumeration,
    std::string_view value) -> const slang::ast::EnumValueSymbol& {
  if (const auto* type =
          LibraryTypeNamed<slang::ast::EnumType>(library, enumeration)) {
    for (const auto& member : type->values()) {
      if (member.name == value) return member;
    }
  }
  throw InternalError(
      std::format(
          "Lyra's coverage library declares no enumerator '{}' of '{}'", value,
          enumeration));
}

auto OptionMemberOf(
    const CoverageLibrary& library, OptionsHolder holder, bool type_option,
    std::string_view name) -> base::ComponentIndex {
  const std::string_view structure = [&] {
    switch (holder) {
      case OptionsHolder::kCovergroup:
        return type_option ? "covergroup_type_option" : "covergroup_option";
      case OptionsHolder::kCoverpoint:
        return type_option ? "coverpoint_type_option" : "coverpoint_option";
      case OptionsHolder::kCross:
        return type_option ? "cross_type_option" : "cross_option";
    }
    throw InternalError("OptionMemberOf: options of no holder");
  }();
  if (const auto* fields = LibraryTypeNamed<slang::ast::UnpackedStructType>(
          library, structure)) {
    for (std::size_t i = 0; i < fields->fields.size(); ++i) {
      if (fields->fields[i]->name == name) {
        return base::ComponentIndex{static_cast<std::uint32_t>(i)};
      }
    }
  }
  throw InternalError(
      std::format(
          "Lyra's coverage library's '{}' has no member '{}', which the "
          "standard's options have (LRM 19.10)",
          structure, name));
}

}  // namespace lyra::lowering::ast_to_hir
