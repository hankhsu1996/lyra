#include "lyra/lir/symbol_name.hpp"

#include <cstdint>
#include <format>
#include <initializer_list>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/value_step.hpp"
#include "lyra/support/def_path.hpp"
#include "lyra/support/runtime_class.hpp"
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
    case SymbolCategory::kMethodEntry:
      return 'q';
    case SymbolCategory::kRecordMethod:
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
    case SymbolCategory::kTypeOperation:
      return 't';
    case SymbolCategory::kValueStep:
      return 's';
    case SymbolCategory::kBaseObjectCreation:
      return 'g';
    case SymbolCategory::kAllocatingCreation:
      return 'h';
    case SymbolCategory::kBaseObjectDestructor:
      return 'x';
    case SymbolCategory::kDeletingDestructor:
      return 'k';
    case SymbolCategory::kClassConstant:
      return 'c';
    case SymbolCategory::kDispatchTable:
      return 'w';
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

auto SymbolPart::LibraryCallback(support::LibraryCallback function)
    -> SymbolPart {
  const std::string_view name = support::LibraryCallbackName(function);
  return SymbolPart{std::format("$l{}:{}", name.size(), name)};
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
              const std::string name = named('c', cls.name);
              const std::string arguments = bound(cls.arguments);
              return name + arguments;
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

auto SymbolPartOf(const support::ClassName& name) -> SymbolPart {
  return std::visit(
      Overloaded{
          [](const support::DefPath& path) { return SymbolPartOf(path); },
          [](const support::UnnamedClass& unnamed) {
            return SymbolPart::Ordinal(unnamed.disambiguator);
          }},
      name);
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

auto MethodEntrySymbol(
    std::string_view unit_name, SymbolPart cls, SymbolPart method)
    -> std::string {
  return SymbolName(
      SymbolCategory::kMethodEntry,
      {SymbolPart::Name(unit_name), std::move(cls), std::move(method)});
}

auto RecordMethodSymbol(
    std::string_view unit_name, const support::DefPath& record,
    support::ValueOperation operation) -> std::string {
  return SymbolName(
      SymbolCategory::kRecordMethod,
      {SymbolPart::Name(unit_name), SymbolPartOf(record),
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

auto ObjectEntrySymbol(
    std::string_view unit_name, std::string_view body_name,
    std::optional<std::uint32_t> block) -> std::string {
  if (!block.has_value()) {
    return SymbolName(
        SymbolCategory::kObjectEntry,
        {SymbolPart::Name(unit_name), SymbolPart::Name(body_name)});
  }
  return SymbolName(
      SymbolCategory::kObjectEntry,
      {SymbolPart::Name(unit_name), SymbolPart::Name(body_name),
       SymbolPart::Ordinal(*block)});
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

namespace {

// `type` spelled onto `out`: a letter for its kind, then each fact that fixes
// it ended by `;`, then the types it is made of, each spelled the same way, so
// where each spelling ends is told by the spelling itself and no two types
// spell alike.
void SpellType(const CompilationUnit& unit, TypeId type, std::string& out) {
  const auto number = [&](std::uint64_t value) {
    out += std::format("{};", value);
  };
  const auto of = [&](TypeId part) { SpellType(unit, part, out); };
  const auto all = [&](const std::vector<TypeId>& parts) {
    number(parts.size());
    for (const TypeId part : parts) {
      of(part);
    }
  };
  const auto named = [&](std::string_view name) {
    out += SymbolPart::Name(name).encoded;
  };
  const auto kind = [&](char letter) { out += letter; };
  unit.types.Get(type).Visit(
      Overloaded{
          [&](const IntegralType& t) {
            kind('I');
            number(t.bit_width);
            number(std::to_underlying(t.signedness));
            number(std::to_underlying(t.state_kind));
          },
          [&](const UnpackedArrayType& t) {
            kind('A');
            number(t.size);
            of(t.element_type);
          },
          [&](const DynamicArrayType& t) {
            kind('D');
            of(t.element_type);
          },
          [&](const QueueType& t) {
            kind('Q');
            out += t.max_bound.has_value() ? std::format("{};", *t.max_bound)
                                           : std::string{"-;"};
            of(t.element_type);
          },
          [&](const AssociativeArrayType& t) {
            kind('M');
            of(t.key_type);
            of(t.element_type);
          },
          [&](const WildcardIndexType&) { kind('W'); },
          [&](const StringType&) { kind('S'); },
          [&](const AnyType&) { kind('Y'); },
          [&](const AnyReferenceType&) { kind('R'); },
          [&](const MachineCStringType&) { kind('c'); },
          [&](const MachineBoolType&) { kind('b'); },
          [&](const MachineIntType& t) {
            kind('i');
            number(BitsOf(t.width));
            number(std::to_underlying(t.signedness));
          },
          [&](const MachineFloatType& t) {
            kind('f');
            number(std::to_underlying(t.width));
          },
          [&](const MachineArrayType& t) {
            kind('a');
            number(t.size);
            of(t.element);
          },
          [&](const MachineSpanType& t) {
            kind('z');
            of(t.element);
          },
          [&](const MachineFunctionType& t) {
            kind('F');
            all(t.params);
            of(t.result);
          },
          [&](const RealType&) { kind('r'); },
          [&](const ShortRealType&) { kind('h'); },
          [&](const ChandleType&) { kind('C'); },
          [&](const VoidType&) { kind('v'); },
          [&](const EmptyType&) { kind('e'); },
          [&](const ObjectType& t) {
            kind('O');
            named(DefinitionSymbol(unit, t.class_id));
          },
          [&](const RuntimeClassType& t) {
            kind('L');
            number(std::to_underlying(t.which));
          },
          [&](const ClosureType& t) {
            kind('K');
            named(DefinitionSymbol(unit, t.closure_id));
          },
          [&](const ErasedCallableType& t) {
            kind('E');
            all(t.params);
            of(t.result);
          },
          [&](const RuntimeEffectsType&) { kind('x'); },
          [&](const FilesType&) { kind('g'); },
          [&](const DiagnosticType&) { kind('d'); },
          [&](const RuntimeLibraryType& t) {
            kind('l');
            number(std::to_underlying(t.kind));
          },
          [&](const CoroutineType& t) {
            kind('T');
            of(t.payload);
          },
          [&](const VariableRefType& t) {
            kind('V');
            number(std::to_underlying(t.mutability));
            of(t.pointee);
          },
          [&](const PointerType& t) {
            kind('P');
            number(std::to_underlying(t.ownership));
            number(std::to_underlying(t.mutability));
            of(t.pointee);
          },
          [&](const ManagedRefType& t) {
            kind('H');
            of(t.pointee);
          },
          [&](const WeakManagedRefType& t) {
            kind('w');
            of(t.pointee);
          },
          [&](const VectorType& t) {
            kind('Z');
            of(t.element);
          },
          [&](const TupleType& t) {
            kind('U');
            all(t.elements);
          },
          // A record is the declaration it names: the unit defining it and
          // where in that unit it is declared.
          [&](const RecordType& t) {
            kind('N');
            const RecordDecl& record = unit.records.Get(t.declaration);
            named(
                record.defining_unit.has_value()
                    ? std::string_view{*record.defining_unit}
                    : std::string_view{unit.name});
            out += SymbolPartOf(record.path).encoded;
            out += ';';
          },
          [&](const ResolvedType& t) {
            kind('n');
            of(t.value);
          },
          [&](const DriverType& t) {
            kind('j');
            of(t.value);
          },
          [&](const ObservableType& t) {
            kind('o');
            of(t.value);
          },
          [&](const SampledHistoryType& t) {
            kind('s');
            of(t.value);
          },
          [&](const EvaluationAttemptsType&) { kind('t'); },
          [&](const LoopBlocksType&) { kind('k'); },
          [&](const OpenWriteType& t) {
            kind('p');
            of(t.value);
          },
          [&](const DesignationType& t) {
            kind('q');
            of(t.value);
          }});
}

}  // namespace

auto SymbolPartOf(const CompilationUnit& unit, TypeId type) -> SymbolPart {
  std::string spelled;
  SpellType(unit, type, spelled);
  return SymbolPart{std::format("$y{}:{}", spelled.size(), spelled)};
}

auto TypeOperationSymbol(
    std::string_view unit_name, SymbolPart type,
    support::ValueOperation operation) -> std::string {
  return SymbolName(
      SymbolCategory::kTypeOperation,
      {SymbolPart::Name(unit_name), std::move(type),
       SymbolPart::Name(support::ValueOperationName(operation))});
}

auto ValueStepSymbol(
    std::string_view unit_name, SymbolPart type, ValueStep step)
    -> std::string {
  return SymbolName(
      SymbolCategory::kValueStep, {SymbolPart::Name(unit_name), std::move(type),
                                   SymbolPart::Name(ValueStepName(step))});
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
          [&](const ClosureType& c) {
            return DefinitionSymbol(unit, c.closure_id);
          }},
      *declaration);
}

auto QualifierOf(const CompilationUnit& unit, const support::ClassName& name)
    -> std::string_view {
  return std::visit(
      Overloaded{
          [&](const support::DefPath&) -> std::string_view {
            return unit.name;
          },
          [&](const support::UnnamedClass&) -> std::string_view {
            return unit.body_name;
          }},
      name);
}

auto DefinitionSymbol(const CompilationUnit& unit, ClassId id) -> std::string {
  const Class& cls = unit.classes.Get(id);
  return ClassDefinitionSymbol(
      cls.defining_unit.has_value() ? std::string_view{*cls.defining_unit}
                                    : QualifierOf(unit, cls.identity),
      SymbolPartOf(cls.identity));
}

// A closure has no declaration of the source to take a name from, so what
// identifies it is the position its unit counted it at.
auto DefinitionSymbol(const CompilationUnit& unit, ClosureId id)
    -> std::string {
  return SymbolName(
      SymbolCategory::kClosureDefinition,
      {SymbolPart::Name(unit.body_name), SymbolPart::Ordinal(id.value)});
}

auto CreationSymbol(std::string_view definition, Creation which)
    -> std::string {
  const SymbolCategory category = [&] {
    switch (which) {
      case Creation::kBaseObject:
        return SymbolCategory::kBaseObjectCreation;
      case Creation::kAllocating:
        return SymbolCategory::kAllocatingCreation;
    }
    throw InternalError("CreationSymbol: unknown creation");
  }();
  return SymbolName(category, {SymbolPart::Name(definition)});
}

auto DestructorSymbol(std::string_view definition, Destructor which)
    -> std::string {
  const SymbolCategory category = [&] {
    switch (which) {
      case Destructor::kBaseObject:
        return SymbolCategory::kBaseObjectDestructor;
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

auto InterfaceTableSymbol(
    std::string_view definition, std::string_view iface_definition)
    -> std::string {
  return SymbolName(
      SymbolCategory::kDispatchTable,
      {SymbolPart::Name(definition), SymbolPart::Name(iface_definition)});
}

}  // namespace lyra::lir
