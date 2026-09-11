#include "lyra/lir/symbol_name.hpp"

#include <cstdint>
#include <format>
#include <initializer_list>
#include <optional>
#include <string>
#include <string_view>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/type.hpp"

namespace lyra::lir {

namespace {

// The letter standing for each category. A letter is enough because a category
// is the one thing a symbol always opens with, and the parts that follow open
// with a marker of their own.
auto CategoryTag(SymbolCategory category) -> char {
  switch (category) {
    case SymbolCategory::kClass:
      return 'c';
    case SymbolCategory::kClassDefinition:
      return 'd';
    case SymbolCategory::kClosureDefinition:
      return 'e';
    case SymbolCategory::kStructDefinition:
      return 'g';
    case SymbolCategory::kConstructor:
      return 'n';
    case SymbolCategory::kMethod:
      return 'm';
    case SymbolCategory::kSynthesizedBody:
      return 'y';
    case SymbolCategory::kSynthesizedNamespaceBody:
      return 'w';
    case SymbolCategory::kNamespaceCallable:
      return 'f';
    case SymbolCategory::kNamespaceStorageInstall:
      return 'a';
    case SymbolCategory::kNamespaceStorageInitialize:
      return 'b';
    case SymbolCategory::kNamespaceVariable:
      return 'v';
    case SymbolCategory::kStaticProperty:
      return 'p';
    case SymbolCategory::kClosure:
      return 'k';
    case SymbolCategory::kClosureInvoke:
      return 'i';
    case SymbolCategory::kStruct:
      return 's';
    case SymbolCategory::kTypeDescription:
      return 't';
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

auto SymbolName(
    SymbolCategory category, std::initializer_list<SymbolPart> parts)
    -> std::string {
  std::string out = std::format("${}", CategoryTag(category));
  for (const SymbolPart& part : parts) {
    out += part.encoded;
  }
  return out;
}

auto ClassSymbol(std::string_view unit_name, std::string_view class_name)
    -> std::string {
  return SymbolName(
      SymbolCategory::kClass,
      {SymbolPart::Name(unit_name), SymbolPart::Name(class_name)});
}

auto ClassDefinitionSymbol(
    std::string_view unit_name, std::string_view class_name) -> std::string {
  return SymbolName(
      SymbolCategory::kClassDefinition,
      {SymbolPart::Name(unit_name), SymbolPart::Name(class_name)});
}

auto StructDefinitionSymbol(
    std::string_view unit_name, std::string_view struct_name) -> std::string {
  return SymbolName(
      SymbolCategory::kStructDefinition,
      {SymbolPart::Name(unit_name), SymbolPart::Name(struct_name)});
}

auto ClosureDefinitionSymbol(std::string_view unit_name, std::uint32_t ordinal)
    -> std::string {
  return SymbolName(
      SymbolCategory::kClosureDefinition,
      {SymbolPart::Name(unit_name), SymbolPart::Ordinal(ordinal)});
}

auto ConstructorSymbol(std::string_view unit_name, std::string_view class_name)
    -> std::string {
  return SymbolName(
      SymbolCategory::kConstructor,
      {SymbolPart::Name(unit_name), SymbolPart::Name(class_name)});
}

auto MethodSymbol(
    std::string_view unit_name, std::string_view class_name,
    std::string_view method_name) -> std::string {
  return SymbolName(
      SymbolCategory::kMethod,
      {SymbolPart::Name(unit_name), SymbolPart::Name(class_name),
       SymbolPart::Name(method_name)});
}

auto StaticPropertySymbol(
    std::string_view unit_name, std::string_view class_name,
    std::string_view property_name) -> std::string {
  return SymbolName(
      SymbolCategory::kStaticProperty,
      {SymbolPart::Name(unit_name), SymbolPart::Name(class_name),
       SymbolPart::Name(property_name)});
}

auto SynthesizedBodySymbol(
    std::string_view unit_name, std::string_view class_name,
    std::uint32_t ordinal) -> std::string {
  return SymbolName(
      SymbolCategory::kSynthesizedBody,
      {SymbolPart::Name(unit_name), SymbolPart::Name(class_name),
       SymbolPart::Ordinal(ordinal)});
}

auto SynthesizedNamespaceBodySymbol(
    std::string_view unit_name, std::uint32_t ordinal) -> std::string {
  return SymbolName(
      SymbolCategory::kSynthesizedNamespaceBody,
      {SymbolPart::Name(unit_name), SymbolPart::Ordinal(ordinal)});
}

auto NamespaceCallableSymbol(
    std::string_view unit_name, std::string_view callable_name) -> std::string {
  return SymbolName(
      SymbolCategory::kNamespaceCallable,
      {SymbolPart::Name(unit_name), SymbolPart::Name(callable_name)});
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

auto NamespaceVariableSymbol(
    std::string_view unit_name, std::string_view variable_name) -> std::string {
  return SymbolName(
      SymbolCategory::kNamespaceVariable,
      {SymbolPart::Name(unit_name), SymbolPart::Name(variable_name)});
}

auto StructSymbol(std::string_view unit_name, std::string_view struct_name)
    -> std::string {
  return SymbolName(
      SymbolCategory::kStruct,
      {SymbolPart::Name(unit_name), SymbolPart::Name(struct_name)});
}

auto TypeDescriptionSymbol(
    std::string_view unit_name, std::string_view description_name)
    -> std::string {
  return SymbolName(
      SymbolCategory::kTypeDescription,
      {SymbolPart::Name(unit_name), SymbolPart::Name(description_name)});
}

auto ClosureSymbol(std::string_view unit_name, std::uint32_t ordinal)
    -> std::string {
  return SymbolName(
      SymbolCategory::kClosure,
      {SymbolPart::Name(unit_name), SymbolPart::Ordinal(ordinal)});
}

auto ClosureInvokeSymbol(std::string_view unit_name, std::uint32_t ordinal)
    -> std::string {
  return SymbolName(
      SymbolCategory::kClosureInvoke,
      {SymbolPart::Name(unit_name), SymbolPart::Ordinal(ordinal)});
}

namespace {

// Which of the declarations a value can be built from a type names. Each is
// identified differently -- two of them by a name their unit gave, the third by
// the position its unit counted it at -- so the three are told apart before
// either symbol over them is composed.
enum class DeclarationKind : std::uint8_t { kClass, kClosure, kStruct };

// The parts of the declaration `type` names, or nothing where the type names no
// declaration. A declaration this unit compiles carries the names it was
// emitted under; one another unit declares carries the names its signature
// gave, so both sides reach the same parts.
struct DeclarationParts {
  DeclarationKind kind;
  std::string unit_name;
  std::string name;
  std::uint32_t ordinal;
};

auto PartsOf(const CompilationUnit& unit, TypeId type)
    -> std::optional<DeclarationParts> {
  return unit.types.Get(type).Visit(
      Overloaded{
          [&](const ObjectType& o) -> std::optional<DeclarationParts> {
            return DeclarationParts{
                .kind = DeclarationKind::kClass,
                .unit_name = unit.name,
                .name = unit.classes.Get(o.class_id).name,
                .ordinal = 0};
          },
          [&](const ExternalUnitObjectType& e)
              -> std::optional<DeclarationParts> {
            const ExternalUnitObject& object =
                unit.external_unit_objects.Get(e.object);
            return DeclarationParts{
                .kind = DeclarationKind::kClass,
                .unit_name = object.unit_name,
                .name = object.class_name,
                .ordinal = 0};
          },
          [](const CrossUnitClassType& c) -> std::optional<DeclarationParts> {
            return DeclarationParts{
                .kind = DeclarationKind::kClass,
                .unit_name = c.unit_name,
                .name = c.class_name,
                .ordinal = 0};
          },
          [&](const ClosureType& c) -> std::optional<DeclarationParts> {
            // A closure has no declaration of the source to take a name from,
            // so what identifies it is the position its unit counted it at.
            return DeclarationParts{
                .kind = DeclarationKind::kClosure,
                .unit_name = unit.name,
                .name = {},
                .ordinal = c.closure_id.value};
          },
          [&](const StructType& s) -> std::optional<DeclarationParts> {
            return DeclarationParts{
                .kind = DeclarationKind::kStruct,
                .unit_name = unit.name,
                .name = unit.structs.Get(s.struct_id).name,
                .ordinal = 0};
          },
          [](const auto&) -> std::optional<DeclarationParts> {
            return std::nullopt;
          }});
}

}  // namespace

auto DeclarationSymbol(const CompilationUnit& unit, TypeId type)
    -> std::optional<std::string> {
  const std::optional<DeclarationParts> parts = PartsOf(unit, type);
  if (!parts.has_value()) {
    return std::nullopt;
  }
  switch (parts->kind) {
    case DeclarationKind::kClass:
      return ClassSymbol(parts->unit_name, parts->name);
    case DeclarationKind::kStruct:
      return StructSymbol(parts->unit_name, parts->name);
    case DeclarationKind::kClosure:
      return ClosureSymbol(parts->unit_name, parts->ordinal);
  }
  throw InternalError("DeclarationSymbol: unknown declaration kind");
}

auto DefinitionSymbol(const CompilationUnit& unit, TypeId type)
    -> std::optional<std::string> {
  const std::optional<DeclarationParts> parts = PartsOf(unit, type);
  if (!parts.has_value()) {
    return std::nullopt;
  }
  switch (parts->kind) {
    case DeclarationKind::kClass:
      return ClassDefinitionSymbol(parts->unit_name, parts->name);
    case DeclarationKind::kStruct:
      return StructDefinitionSymbol(parts->unit_name, parts->name);
    case DeclarationKind::kClosure:
      return ClosureDefinitionSymbol(parts->unit_name, parts->ordinal);
  }
  throw InternalError("DefinitionSymbol: unknown declaration kind");
}

}  // namespace lyra::lir
