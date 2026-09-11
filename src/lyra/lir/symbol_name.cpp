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
    case SymbolCategory::kClassCallable:
      return 'm';
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

auto ClassSymbol(std::string_view unit_name, SymbolPart cls) -> std::string {
  return SymbolName(
      SymbolCategory::kClass, {SymbolPart::Name(unit_name), std::move(cls)});
}

auto ClassDefinitionSymbol(std::string_view unit_name, SymbolPart cls)
    -> std::string {
  return SymbolName(
      SymbolCategory::kClassDefinition,
      {SymbolPart::Name(unit_name), std::move(cls)});
}

auto StructDefinitionSymbol(std::string_view unit_name, SymbolPart record)
    -> std::string {
  return SymbolName(
      SymbolCategory::kStructDefinition,
      {SymbolPart::Name(unit_name), std::move(record)});
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

auto StaticPropertySymbol(
    std::string_view unit_name, SymbolPart cls, SymbolPart property)
    -> std::string {
  return SymbolName(
      SymbolCategory::kStaticProperty,
      {SymbolPart::Name(unit_name), std::move(cls), std::move(property)});
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

auto NamespaceVariableSymbol(std::string_view unit_name, SymbolPart variable)
    -> std::string {
  return SymbolName(
      SymbolCategory::kNamespaceVariable,
      {SymbolPart::Name(unit_name), std::move(variable)});
}

auto StructSymbol(std::string_view unit_name, SymbolPart record)
    -> std::string {
  return SymbolName(
      SymbolCategory::kStruct,
      {SymbolPart::Name(unit_name), std::move(record)});
}

auto TypeDescriptionSymbol(std::string_view unit_name, std::uint32_t ordinal)
    -> std::string {
  return SymbolName(
      SymbolCategory::kTypeDescription,
      {SymbolPart::Name(unit_name), SymbolPart::Ordinal(ordinal)});
}

auto ClosureSymbol(std::string_view unit_name, SymbolPart closure)
    -> std::string {
  return SymbolName(
      SymbolCategory::kClosure,
      {SymbolPart::Name(unit_name), std::move(closure)});
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
  // What the declaration contributes: the identifier its unit gave it, or the
  // position that unit counted it at where it was given none.
  SymbolPart part;
};

auto PartsOf(const CompilationUnit& unit, TypeId type)
    -> std::optional<DeclarationParts> {
  return unit.types.Get(type).Visit(
      Overloaded{
          [&](const ObjectType& o) -> std::optional<DeclarationParts> {
            return DeclarationParts{
                .kind = DeclarationKind::kClass,
                .unit_name = unit.name,
                .part = SymbolPartOf(
                    unit.classes.Get(o.class_id).name, o.class_id.value)};
          },
          [&](const ExternalUnitObjectType& e)
              -> std::optional<DeclarationParts> {
            const ExternalUnitObject& object =
                unit.external_unit_objects.Get(e.object);
            return DeclarationParts{
                .kind = DeclarationKind::kClass,
                .unit_name = object.unit_name,
                .part = SymbolPart::Name(object.class_name)};
          },
          [](const CrossUnitClassType& c) -> std::optional<DeclarationParts> {
            return DeclarationParts{
                .kind = DeclarationKind::kClass,
                .unit_name = c.unit_name,
                .part = SymbolPart::Name(c.class_name)};
          },
          [&](const ClosureType& c) -> std::optional<DeclarationParts> {
            // A closure has no declaration of the source to take a name from,
            // so what identifies it is the position its unit counted it at.
            return DeclarationParts{
                .kind = DeclarationKind::kClosure,
                .unit_name = unit.name,
                .part = SymbolPart::Ordinal(c.closure_id.value)};
          },
          [&](const StructType& s) -> std::optional<DeclarationParts> {
            // Like a closure, a gathered scope is no declaration of the
            // source, so its position is what identifies it.
            return DeclarationParts{
                .kind = DeclarationKind::kStruct,
                .unit_name = unit.name,
                .part = SymbolPart::Ordinal(s.struct_id.value)};
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
      return ClassSymbol(parts->unit_name, parts->part);
    case DeclarationKind::kStruct:
      return StructSymbol(parts->unit_name, parts->part);
    case DeclarationKind::kClosure:
      return ClosureSymbol(parts->unit_name, parts->part);
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
      return ClassDefinitionSymbol(parts->unit_name, parts->part);
    case DeclarationKind::kStruct:
      return StructDefinitionSymbol(parts->unit_name, parts->part);
    case DeclarationKind::kClosure:
      return ClosureDefinitionSymbol(parts->unit_name, parts->part);
  }
  throw InternalError("DefinitionSymbol: unknown declaration kind");
}

}  // namespace lyra::lir
