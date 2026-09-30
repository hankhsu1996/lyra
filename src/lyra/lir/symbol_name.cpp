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
    case SymbolCategory::kScopeEntry:
      return 'x';
    case SymbolCategory::kTypeDescription:
      return 't';
    case SymbolCategory::kIntegralConstant:
      return 'l';
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

auto SymbolPartOf(const std::optional<std::string>& name, std::uint32_t ordinal)
    -> SymbolPart {
  return name.has_value() ? SymbolPart::Name(*name)
                          : SymbolPart::Ordinal(ordinal);
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

auto ClassDefinitionSymbol(std::string_view unit_name, SymbolPart cls)
    -> std::string {
  return SymbolName(
      SymbolCategory::kClassDefinition,
      {SymbolPart::Name(unit_name), std::move(cls)});
}

auto ClosureDefinitionSymbol(std::string_view unit_name, SymbolPart closure)
    -> std::string {
  return SymbolName(
      SymbolCategory::kClosureDefinition,
      {SymbolPart::Name(unit_name), std::move(closure)});
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
    std::string_view unit_name, std::string_view structure,
    support::ValueOperation operation) -> std::string {
  return SymbolName(
      SymbolCategory::kStructMethod,
      {SymbolPart::Name(unit_name), SymbolPart::Name(structure),
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

auto TypeDescriptionSymbol(std::string_view unit_name, std::uint32_t ordinal)
    -> std::string {
  return SymbolName(
      SymbolCategory::kTypeDescription,
      {SymbolPart::Name(unit_name), SymbolPart::Ordinal(ordinal)});
}

auto IntegralConstantSymbol(std::string_view unit_name, std::uint32_t ordinal)
    -> std::string {
  return SymbolName(
      SymbolCategory::kIntegralConstant,
      {SymbolPart::Name(unit_name), SymbolPart::Ordinal(ordinal)});
}

auto ClosureInvokeSymbol(std::string_view unit_name, std::uint32_t ordinal)
    -> std::string {
  return SymbolName(
      SymbolCategory::kClosureInvoke,
      {SymbolPart::Name(unit_name), SymbolPart::Ordinal(ordinal)});
}

auto ScopeEntrySymbol(
    std::string_view unit_name, SymbolPart cls, std::uint32_t ordinal)
    -> std::string {
  return SymbolName(
      SymbolCategory::kScopeEntry, {SymbolPart::Name(unit_name), std::move(cls),
                                    SymbolPart::Ordinal(ordinal)});
}

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
            return ClassDefinitionSymbol(
                unit.name,
                SymbolPartOf(
                    unit.classes.Get(o.class_id).name, o.class_id.value));
          },
          [&](const ExternalUnitObjectType& e) {
            const ExternalUnitObject& object =
                unit.external_unit_objects.Get(e.object);
            return ClassDefinitionSymbol(
                object.unit_name, SymbolPart::Name(object.class_name));
          },
          [](const CrossUnitClassType& c) {
            return ClassDefinitionSymbol(
                c.unit_name, SymbolPart::Name(c.class_name));
          },
          // A closure has no declaration of the source to take a name from, so
          // what identifies it is the position its unit counted it at.
          [&](const ClosureType& c) {
            return ClosureDefinitionSymbol(
                unit.name, SymbolPart::Ordinal(c.closure_id.value));
          }},
      *declaration);
}

}  // namespace lyra::lir
