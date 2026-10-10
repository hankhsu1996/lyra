#include "lyra/lir/symbol_name.hpp"

#include <cstdint>
#include <format>
#include <initializer_list>
#include <optional>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/support/def_path.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::lir {

namespace {

// The letter standing for each category. A letter is enough because a category
// is the one thing a symbol always opens with, and the parts that follow open
// with a marker of their own.
auto CategoryTag(SymbolCategory category) -> char {
  switch (category) {
    case SymbolCategory::kClassDefinition:
      return 'd';
    case SymbolCategory::kClosureDefinition:
      return 'e';
    case SymbolCategory::kConstructor:
      return 'n';
    case SymbolCategory::kClassCallable:
      return 'm';
    case SymbolCategory::kStructMethod:
      return 'r';
    case SymbolCategory::kNamespaceCallable:
      return 'f';
    case SymbolCategory::kNamespaceStorageInstall:
      return 'a';
    case SymbolCategory::kNamespaceStorageInitialize:
      return 'b';
    case SymbolCategory::kObjectEntry:
      return 'o';
    case SymbolCategory::kNamespaceVariable:
      return 'v';
    case SymbolCategory::kStaticProperty:
      return 'p';
    case SymbolCategory::kClosureInvoke:
      return 'i';
    case SymbolCategory::kConstructorPrologue:
      return 's';
    case SymbolCategory::kBaseObjectDestructor:
      return 'x';
    case SymbolCategory::kCompleteObjectDestructor:
      return 'z';
    case SymbolCategory::kDeletingDestructor:
      return 'k';
    case SymbolCategory::kClassConstant:
      return 'c';
    case SymbolCategory::kDispatchTable:
      return 'w';
    case SymbolCategory::kTypeInfo:
      return 'y';
  }
  throw InternalError("SymbolName: unknown symbol category");
}

}  // namespace

auto SymbolPart::Name(std::string_view name) -> SymbolPart {
  return SymbolPart{std::format("$n{}:{}", name.size(), name)};
}

auto SymbolPart::Ordinal(std::uint32_t value) -> SymbolPart {
  return SymbolPart{std::format("$i{};", value)};
}

auto SymbolPartOf(std::optional<std::string_view> name, std::uint32_t ordinal)
    -> SymbolPart {
  return name.has_value() ? SymbolPart::Name(*name)
                          : SymbolPart::Ordinal(ordinal);
}

auto SymbolPartOf(const support::DefPath& path) -> SymbolPart {
  // A step opens with the marker of its kind, then states its own extent: the
  // length of its name before the name, or a terminator after its position.
  // What else tells a step from its siblings follows the name, each behind a
  // mark of its own and ended by a terminator.
  const auto named = [](char kind, std::string_view name) {
    return std::format("${}{}:{}", kind, name.size(), name);
  };
  const auto placed = [](char kind, std::uint32_t position) {
    return std::format("${}{};", kind, position);
  };
  const auto bound = [](const std::optional<std::uint64_t>& arguments) {
    return arguments.has_value() ? std::format("#{:016x};", *arguments)
                                 : std::string{};
  };
  SymbolPart steps;
  for (const support::DefPathData& step : path.data) {
    steps.encoded += std::visit(
        Overloaded{
            [&](const support::GenerateBlockStep& block) {
              std::string part = named('g', block.label);
              if (block.disambiguator != 0) {
                part += std::format("~{};", block.disambiguator);
              }
              return part + bound(block.arguments);
            },
            [&](const support::ClassStep& cls) {
              return named('c', cls.name) + bound(cls.arguments);
            },
            [&](const support::SubroutineStep& subroutine) {
              return named('s', subroutine.name);
            },
            [&](const support::NamedBlockStep& block) {
              return named('b', block.name);
            },
            [&](const support::UnnamedBlockStep& block) {
              return placed('u', block.position);
            },
            [&](const support::TypeStep& type) {
              return named('t', type.name);
            },
            [&](const support::UnnamedTypeStep& type) {
              return placed('a', type.position);
            }},
        step);
  }
  return steps;
}

auto SymbolPartOf(
    const std::optional<support::DefPath>& path, std::uint32_t ordinal)
    -> SymbolPart {
  return path.has_value() ? SymbolPartOf(*path) : SymbolPart::Ordinal(ordinal);
}

auto SymbolName(
    SymbolCategory category, std::initializer_list<SymbolPart> parts)
    -> std::string {
  std::string out = std::format("${}", CategoryTag(category));
  for (const SymbolPart& part : parts) {
    out += part.encoded;
  }
  return out;
}

auto ConstructorSymbol(std::string_view unit_name, SymbolPart cls)
    -> std::string {
  return SymbolName(
      SymbolCategory::kConstructor,
      {SymbolPart::Name(unit_name), std::move(cls)});
}

auto ClassCallableSymbol(
    std::string_view unit_name, SymbolPart cls, SymbolPart callable)
    -> std::string {
  return SymbolName(
      SymbolCategory::kClassCallable,
      {SymbolPart::Name(unit_name), std::move(cls), std::move(callable)});
}

auto StructMethodSymbol(
    std::string_view unit_name, const support::DefPath& structure,
    support::ValueOperation operation) -> std::string {
  return SymbolName(
      SymbolCategory::kStructMethod,
      {SymbolPart::Name(unit_name), SymbolPartOf(structure),
       SymbolPart::Name(support::ValueOperationName(operation))});
}

auto StaticPropertySymbol(
    std::string_view unit_name, SymbolPart cls, SymbolPart property)
    -> std::string {
  return SymbolName(
      SymbolCategory::kStaticProperty,
      {SymbolPart::Name(unit_name), std::move(cls), std::move(property)});
}

auto NamespaceCallableSymbol(std::string_view unit_name, SymbolPart callable)
    -> std::string {
  return SymbolName(
      SymbolCategory::kNamespaceCallable,
      {SymbolPart::Name(unit_name), callable});
}

auto NamespaceStorageInstallSymbol(std::string_view unit_name) -> std::string {
  return SymbolName(
      SymbolCategory::kNamespaceStorageInstall, {SymbolPart::Name(unit_name)});
}

auto NamespaceStorageInitializeSymbol(std::string_view unit_name)
    -> std::string {
  return SymbolName(
      SymbolCategory::kNamespaceStorageInitialize,
      {SymbolPart::Name(unit_name)});
}

auto ObjectEntrySymbol(std::string_view unit_name) -> std::string {
  return SymbolName(
      SymbolCategory::kObjectEntry, {SymbolPart::Name(unit_name)});
}

auto NamespaceVariableSymbol(std::string_view unit_name, SymbolPart variable)
    -> std::string {
  return SymbolName(
      SymbolCategory::kNamespaceVariable,
      {SymbolPart::Name(unit_name), std::move(variable)});
}

auto ClosureInvokeSymbol(std::string_view unit_name, std::uint32_t ordinal)
    -> std::string {
  return SymbolName(
      SymbolCategory::kClosureInvoke,
      {SymbolPart::Name(unit_name), SymbolPart::Ordinal(ordinal)});
}

auto ClassDefinitionSymbol(std::string_view unit_name, SymbolPart cls)
    -> std::string {
  return SymbolName(
      SymbolCategory::kClassDefinition,
      {SymbolPart::Name(unit_name), std::move(cls)});
}

// A declaration this unit compiles carries the names it was emitted under; one
// another unit declares carries the names its signature gave, so both sides
// reach the same parts.
auto DefinitionSymbol(const CompilationUnit& unit, TypeId type)
    -> std::optional<std::string> {
  const std::optional<TypeDeclaration> declaration =
      unit.types.Get(type).Declaration();
  if (!declaration) {
    return std::nullopt;
  }
  return std::visit(
      Overloaded{
          [&](const ObjectType& o) {
            return DefinitionSymbol(unit, o.class_id);
          },
          [](const CrossUnitClassType& c) {
            return ClassDefinitionSymbol(
                c.unit_name, SymbolPartOf(c.class_path));
          },
          [&](const ClosureType& c) {
            return DefinitionSymbol(unit, c.closure_id);
          }},
      *declaration);
}

auto DefinitionSymbol(const CompilationUnit& unit, ClassId id) -> std::string {
  return ClassDefinitionSymbol(
      unit.name, SymbolPartOf(unit.classes.Get(id).path, id.value));
}

// A closure has no declaration of the source to take a name from, so what
// identifies it is the position its unit counted it at.
auto DefinitionSymbol(const CompilationUnit& unit, ClosureId id)
    -> std::string {
  return SymbolName(
      SymbolCategory::kClosureDefinition,
      {SymbolPart::Name(unit.name), SymbolPart::Ordinal(id.value)});
}

auto ConstructorPrologueSymbol(std::string_view definition) -> std::string {
  return SymbolName(
      SymbolCategory::kConstructorPrologue, {SymbolPart::Name(definition)});
}

auto DestructorSymbol(std::string_view definition, Destructor which)
    -> std::string {
  const SymbolCategory category = [&] {
    switch (which) {
      case Destructor::kBaseObject:
        return SymbolCategory::kBaseObjectDestructor;
      case Destructor::kCompleteObject:
        return SymbolCategory::kCompleteObjectDestructor;
      case Destructor::kDeleting:
        return SymbolCategory::kDeletingDestructor;
    }
    throw InternalError("DestructorSymbol: unknown destructor");
  }();
  return SymbolName(category, {SymbolPart::Name(definition)});
}

auto ClassConstantSymbol(std::string_view definition, std::uint32_t ordinal)
    -> std::string {
  return SymbolName(
      SymbolCategory::kClassConstant,
      {SymbolPart::Name(definition), SymbolPart::Ordinal(ordinal)});
}

auto DispatchTableSymbol(std::string_view definition) -> std::string {
  return SymbolName(
      SymbolCategory::kDispatchTable, {SymbolPart::Name(definition)});
}

auto TypeInfoSymbol(std::string_view definition) -> std::string {
  return SymbolName(SymbolCategory::kTypeInfo, {SymbolPart::Name(definition)});
}

}  // namespace lyra::lir
