#include "lyra/lowering/hir_to_mir/class_definition.hpp"

#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/lowering/hir_to_mir/library_entry_body.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/value_build.hpp"

namespace lyra::lowering::hir_to_mir {

auto BuildDefinitionRead(
    const mir::CompilationUnit& unit, mir::Block& block,
    const mir::DeclaredClassRef& of) -> mir::ExprId {
  const mir::ExprId definition = block.exprs.Add(
      mir::Expr{
          .data = mir::ReferenceExpr{.target = mir::DefinitionRef{.of = of}},
          .type = mir::ClassDefinitionType(unit.types)});
  return block.exprs.Add(
      mir::Expr{
          .data = mir::AddressOfExpr{.operand = definition},
          .type = mir::ClassDefinitionPointer(unit.types)});
}

namespace {

// What a class tells the runtime library is constants of the class, each
// stated as the expression that is its value. For a class `Mid` an instance of
// the design hierarchy is built of, declaring one task `bump`:
//
//   const ScopeCallable Mid::table[1] = {{"bump", &Mid::bump_entry}};
//   const ScopeInfo Mid::info = {unit, precision, nullptr, 0, table, 1, ...};
//   const ObjectDefinition Mid::definition = {&Scope::definition, ..., &info};
//
// A table is an array the class holds, and whatever points at one states two
// members for it: where the array is, and how many it holds.

auto LibraryType(const mir::CompilationUnit& unit, mir::RuntimeLibraryKind kind)
    -> mir::TypeId {
  return unit.types.Intern(mir::Type{mir::RuntimeLibraryType{.kind = kind}});
}

auto ReadOnlyPointerTo(const mir::CompilationUnit& unit, mir::TypeId pointee)
    -> mir::TypeId {
  return unit.types.Intern(
      mir::Type{mir::PointerType{
          .pointee = pointee,
          .ownership = mir::PointerOwnership::kBorrowed,
          .mutability = mir::Mutability::kReadOnly}});
}

auto MachineInt(
    const mir::CompilationUnit& unit, mir::Block& block, std::int64_t value,
    mir::MachineIntWidth width, mir::Signedness signedness) -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data = mir::MachineIntLiteral{.value = value},
          .type = unit.types.Intern(
              mir::Type{mir::MachineIntType{
                  .width = width, .signedness = signedness}})});
}

auto Name(
    const mir::CompilationUnit& unit, mir::Block& block, std::string_view text)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::MakeStringLiteral(
          unit.types.Intern(mir::Type{mir::MachineCStringType{}}),
          std::string{text}));
}

// A structure of the library, composed from `parts` in the order it declares
// its members, a nested structure's members standing where it does.
auto Record(
    const mir::CompilationUnit& unit, mir::Block& block,
    mir::RuntimeLibraryKind kind, std::vector<mir::ExprId> parts)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(parts)},
          .type = LibraryType(unit, kind)});
}

// The machine function type `callable` of `cls` has: every parameter it takes
// and what it results in.
auto FunctionTypeOf(
    const mir::CompilationUnit& unit, const mir::Class& cls,
    mir::CallableId callable) -> mir::TypeId {
  const mir::CallableCode& code = cls.callables.Get(callable).code;
  std::vector<mir::TypeId> params;
  params.reserve(code.params.size());
  for (const mir::LocalId param : code.params) {
    params.push_back(code.locals.Get(param).type);
  }
  return unit.types.Intern(
      mir::Type{mir::MachineFunctionType{
          .params = std::move(params), .result = code.result_type}});
}

// `callable` of class `id`, named as the code it is: `&Mid::bump_entry`.
auto Function(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ClassId id,
    const mir::Class& cls, mir::CallableId callable) -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::ReferenceExpr{
                  .target =
                      mir::FunctionRef{
                          .body =
                              mir::CallableTarget{
                                  .owner = id, .slot = callable}}},
          .type = FunctionTypeOf(unit, cls, callable)});
}

// The same with its prototype erased, as a table holding bodies of every
// prototype keeps one.
auto ErasedFunction(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ClassId id,
    const mir::Class& cls, mir::CallableId callable) -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CastExpr{
                  .operand = Function(unit, block, id, cls, callable)},
          .type = mir::ErasedFunction(unit.types)});
}

// Holds `build`, a value of `type`, as a constant of class `id`, and answers
// its address as a value of `block`.
auto ConstantAddress(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ClassId id,
    mir::Class& cls, mir::TypeId type, mir::ValueBuild build) -> mir::ExprId {
  const mir::ClassConstantId constant = cls.constants.Add(
      mir::ClassConstantDecl{.type = type, .initializer = std::move(build)});
  return block.exprs.Add(
      mir::MakeAddressOfExpr(
          block.exprs.Add(
              mir::Expr{
                  .data =
                      mir::ReferenceExpr{
                          .target =
                              mir::ClassConstantRef{
                                  .owner = id, .constant = constant}},
                  .type = type}),
          ReadOnlyPointerTo(unit, type)));
}

// A table of `count` values of type `element`, each written by `write` into
// the block it is handed: the array is held as a constant of class `id`, and
// the two members that point at it -- where the array is, and how many it
// holds -- are appended to `parts` as values of `block`.
void AppendTable(
    std::vector<mir::ExprId>& parts, const mir::CompilationUnit& unit,
    mir::Block& block, mir::ClassId id, mir::Class& cls, mir::TypeId element,
    std::size_t count,
    const std::function<mir::ExprId(mir::Block&, std::size_t)>& write) {
  mir::ValueBuild build;
  std::vector<mir::ExprId> elements;
  elements.reserve(count);
  for (std::size_t i = 0; i < count; ++i) {
    elements.push_back(write(build.body, i));
  }
  const mir::TypeId type = mir::MachineArrayOf(unit.types, element, count);
  build.value = build.body.exprs.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(elements)},
          .type = type});
  parts.push_back(
      ConstantAddress(unit, block, id, cls, type, std::move(build)));
  parts.push_back(MachineInt(
      unit, block, static_cast<std::int64_t>(count), mir::MachineIntWidth::k64,
      mir::Signedness::kUnsigned));
}

// A table of the bodies `named` reaches, each as a record of `kind`: the name,
// then the body with its prototype erased.
//
//   {{"bump", &Mid::bump_entry}, ...}
void AppendNamedBodyTable(
    std::vector<mir::ExprId>& parts, const mir::CompilationUnit& unit,
    mir::Block& block, mir::ClassId id, mir::Class& cls,
    mir::RuntimeLibraryKind kind, std::span<const mir::NamedCallable> named) {
  AppendTable(
      parts, unit, block, id, cls, LibraryType(unit, kind), named.size(),
      [&](mir::Block& entry, std::size_t i) {
        return Record(
            unit, entry, kind,
            {Name(unit, entry, named[i].name),
             ErasedFunction(unit, entry, id, cls, named[i].body)});
      });
}

// What a class of the design hierarchy states of its instances, as a constant
// of class `id`, and its address as a value of `block`: its timescale, then
// the bodies its export and subroutine names reach, then the classes it
// declares.
//
//   {unit, precision, exports, n, subroutines, n, classes, n}
auto ScopeInfoAddress(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ClassId id,
    mir::Class& cls, std::span<const mir::NamedCallable> exports,
    std::span<const mir::NamedCallable> subroutines) -> mir::ExprId {
  using Kind = mir::RuntimeLibraryKind;
  const mir::TypeId type = LibraryType(unit, Kind::kScopeInfo);
  mir::ValueBuild build;
  mir::Block& info = build.body;
  const std::vector<mir::ClassId> declares = cls.declares;
  std::vector<mir::ExprId> parts{
      MachineInt(
          unit, info, cls.time_resolution.unit_power, mir::MachineIntWidth::k8,
          mir::Signedness::kSigned),
      MachineInt(
          unit, info, cls.time_resolution.precision_power,
          mir::MachineIntWidth::k8, mir::Signedness::kSigned)};
  AppendNamedBodyTable(
      parts, unit, info, id, cls, Kind::kScopeCallable, exports);
  AppendNamedBodyTable(
      parts, unit, info, id, cls, Kind::kScopeCallable, subroutines);
  AppendTable(
      parts, unit, info, id, cls, LibraryType(unit, Kind::kScopeClass),
      declares.size(), [&](mir::Block& entry, std::size_t i) {
        return Record(
            unit, entry, Kind::kScopeClass,
            {Name(unit, entry, *unit.GetClass(declares[i]).name),
             BuildDefinitionRead(
                 unit, entry,
                 mir::IntraUnitClassRef{.class_id = declares[i]})});
      });
  build.value = Record(unit, info, Kind::kScopeInfo, std::move(parts));
  return ConstantAddress(unit, block, id, cls, type, std::move(build));
}

// What the definition of a class holds beyond the class it extends. Each list
// is empty, and `scope` absent, for a class that answers nothing of that kind.
struct DefinitionContents {
  // The properties a name reaches, the body answering where each field is in
  // field order, and the body each method's name runs.
  std::span<const mir::NamedField> property_names;
  std::span<const mir::CallableId> property_slots;
  std::span<const mir::NamedCallable> bodies;
  // What a class of the design hierarchy states of its instances.
  struct Scope {
    std::span<const mir::NamedCallable> exports;
    std::span<const mir::NamedCallable> subroutines;
  };
  std::optional<Scope> scope;
};

// States the definition of class `id`: the class it extends, the three tables
// a name is answered from, and what it states of its instances where it stands
// in the design hierarchy.
//
//   {&Base::definition, property_names, n, bodies, n, property_slots, n, &info}
void StateDefinition(
    const mir::CompilationUnit& unit, mir::ClassId id, mir::Class& cls,
    const DefinitionContents& contents) {
  using Kind = mir::RuntimeLibraryKind;
  mir::ValueBuild build;
  mir::Block& block = build.body;
  const std::optional<mir::DeclaredClassRef> base = mir::DeclaredBase(cls);
  std::vector<mir::ExprId> parts{
      base.has_value()
          ? BuildDefinitionRead(unit, block, *base)
          : block.exprs.Add(
                mir::Expr{
                    .data = mir::NullLiteral{},
                    .type = mir::ClassDefinitionPointer(unit.types)})};
  AppendTable(
      parts, unit, block, id, cls, LibraryType(unit, Kind::kResolvedProperty),
      contents.property_names.size(), [&](mir::Block& entry, std::size_t i) {
        const mir::NamedField& property = contents.property_names[i];
        return Record(
            unit, entry, Kind::kResolvedProperty,
            {Name(unit, entry, property.name),
             BuildDefinitionRead(
                 unit, entry, mir::IntraUnitClassRef{.class_id = id}),
             MachineInt(
                 unit, entry, property.slot.value, mir::MachineIntWidth::k32,
                 mir::Signedness::kUnsigned)});
      });
  AppendNamedBodyTable(
      parts, unit, block, id, cls, Kind::kDeclaredBody, contents.bodies);
  const mir::TypeId slot_type = unit.types.Intern(
      mir::Type{mir::MachineFunctionType{
          .params = {mir::ErasedPointer(unit.types)},
          .result = mir::ErasedPointer(unit.types)}});
  AppendTable(
      parts, unit, block, id, cls, slot_type, contents.property_slots.size(),
      [&](mir::Block& entry, std::size_t i) {
        return Function(unit, entry, id, cls, contents.property_slots[i]);
      });
  parts.push_back(
      contents.scope.has_value()
          ? ScopeInfoAddress(
                unit, block, id, cls, contents.scope->exports,
                contents.scope->subroutines)
          : block.exprs.Add(
                mir::Expr{
                    .data = mir::NullLiteral{},
                    .type = ReadOnlyPointerTo(
                        unit, LibraryType(unit, Kind::kScopeInfo))}));
  build.value = Record(unit, block, Kind::kObjectDefinition, std::move(parts));
  cls.object_definition_initializer = std::move(build);
}

}  // namespace

void StateNamedClassDefinition(
    const mir::CompilationUnit& unit, mir::ClassId id, mir::Class& cls) {
  StateDefinition(
      unit, id, cls,
      DefinitionContents{
          .property_names = {},
          .property_slots = {},
          .bodies = {},
          .scope = std::nullopt});
}

void StateNameReachedDefinition(
    const mir::CompilationUnit& unit, mir::ClassId id, mir::Class& cls,
    std::span<const InheritedBehavior> inherited) {
  // A body of a class of the source language is handed the object as nothing
  // in particular, since the referrer cannot name the class.
  const mir::TypeId object = mir::ErasedPointer(unit.types);
  std::vector<mir::CallableId> property_slots;
  property_slots.reserve(cls.fields.size());
  for (const mir::FieldId slot : cls.fields.Ids()) {
    property_slots.push_back(AddFieldAddressEntry(unit, id, cls, slot, object));
  }
  const std::vector<mir::NamedCallable> named = cls.named_callables;
  std::vector<mir::NamedCallable> bodies =
      NamedEntries(unit, id, cls, named, object);
  // A name the class states itself is the behavior a call through it means
  // (LRM 8.26.6.1), so only the rest are answered for the class it extends.
  for (const InheritedBehavior& behavior : inherited) {
    if (mir::CallableNamed(named, behavior.name).has_value()) {
      continue;
    }
    bodies.push_back(
        mir::NamedCallable{
            .name = behavior.name,
            .body = AddDispatchingEntry(
                unit, cls, behavior.slot,
                Prototype{
                    .params = {behavior.params.begin(), behavior.params.end()},
                    .result = behavior.result},
                object)});
  }
  const std::vector<mir::NamedField> property_names = cls.named_fields;
  StateDefinition(
      unit, id, cls,
      DefinitionContents{
          .property_names = property_names,
          .property_slots = property_slots,
          .bodies = bodies,
          .scope = std::nullopt});
}

void StateScopeDefinition(
    const mir::CompilationUnit& unit, mir::ClassId id, mir::Class& cls,
    std::span<const mir::NamedCallable> exports) {
  // A body of a scope is handed the scope every instance is.
  const mir::TypeId scope = unit.builtins.scope_ptr;
  // A DPI-C import is reached by its program-global foreign name rather than
  // through an instance (LRM 35.4), so only a body the class defines here
  // answers a name on its scope.
  const std::vector<mir::NamedCallable> named = cls.named_callables;
  std::vector<mir::NamedCallable> subroutines;
  for (const mir::NamedCallable& name : named) {
    if (std::holds_alternative<mir::DefinedHere>(
            mir::FormOf(cls.callables.Get(name.body)))) {
      subroutines.push_back(
          mir::NamedCallable{
              .name = name.name,
              .body = EntryOf(unit, id, cls, name.body, scope)});
    }
  }
  std::vector<mir::NamedCallable> entered;
  entered.reserve(exports.size());
  for (const mir::NamedCallable& exported : exports) {
    entered.push_back(
        mir::NamedCallable{
            .name = exported.name,
            .body = EntryOf(unit, id, cls, exported.body, scope)});
  }
  StateDefinition(
      unit, id, cls,
      DefinitionContents{
          .property_names = {},
          .property_slots = {},
          .bodies = {},
          .scope = DefinitionContents::Scope{
              .exports = entered, .subroutines = subroutines}});
}

}  // namespace lyra::lowering::hir_to_mir
