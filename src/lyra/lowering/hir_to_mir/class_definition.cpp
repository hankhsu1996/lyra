#include "lyra/lowering/hir_to_mir/class_definition.hpp"

#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
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
// the design hierarchy is built of, exporting one task as `bump` (LRM 35.5):
//
//   const ScopeCallable Mid::table[1] = {{"bump", &Mid::bump_entry}};
//   const ScopeInfo Mid::info = {unit, precision, table, 1};
//   const ObjectDefinition Mid::definition = {&Scope::definition, &info};
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
// the bodies its export names reach.
//
//   {unit, precision, exports, n}
auto ScopeInfoAddress(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ClassId id,
    mir::Class& cls, std::span<const mir::NamedCallable> exports)
    -> mir::ExprId {
  using Kind = mir::RuntimeLibraryKind;
  const mir::TypeId type = LibraryType(unit, Kind::kScopeInfo);
  mir::ValueBuild build;
  mir::Block& info = build.body;
  std::vector<mir::ExprId> parts{
      MachineInt(
          unit, info, cls.time_resolution.unit_power, mir::MachineIntWidth::k8,
          mir::Signedness::kSigned),
      MachineInt(
          unit, info, cls.time_resolution.precision_power,
          mir::MachineIntWidth::k8, mir::Signedness::kSigned)};
  AppendNamedBodyTable(
      parts, unit, info, id, cls, Kind::kScopeCallable, exports);
  build.value = Record(unit, info, Kind::kScopeInfo, std::move(parts));
  return ConstantAddress(unit, block, id, cls, type, std::move(build));
}

// States the definition of `cls`: the class it extends, and what it states of
// its instances, which `instances` writes into the block it is handed.
//
//   {&Base::definition, &info}
void StateDefinition(
    const mir::CompilationUnit& unit, mir::Class& cls,
    const std::function<mir::ExprId(mir::Block&)>& instances) {
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
  parts.push_back(instances(block));
  build.value = Record(
      unit, block, mir::RuntimeLibraryKind::kObjectDefinition,
      std::move(parts));
  cls.object_definition_initializer = std::move(build);
}

}  // namespace

void StateNamedClassDefinition(
    const mir::CompilationUnit& unit, mir::Class& cls) {
  StateDefinition(unit, cls, [&](mir::Block& block) {
    return block.exprs.Add(
        mir::Expr{
            .data = mir::NullLiteral{},
            .type = ReadOnlyPointerTo(
                unit, LibraryType(unit, mir::RuntimeLibraryKind::kScopeInfo))});
  });
}

void StateScopeDefinition(
    const mir::CompilationUnit& unit, mir::ClassId id, mir::Class& cls,
    std::span<const mir::NamedCallable> exports) {
  // A body of a scope is handed the scope every instance is.
  const mir::TypeId scope = unit.builtins.scope_ptr;
  std::vector<mir::NamedCallable> entered;
  entered.reserve(exports.size());
  for (const mir::NamedCallable& exported : exports) {
    entered.push_back(
        mir::NamedCallable{
            .name = exported.name,
            .body = EntryOf(unit, id, cls, exported.body, scope)});
  }
  StateDefinition(unit, cls, [&](mir::Block& block) {
    return ScopeInfoAddress(unit, block, id, cls, entered);
  });
}

}  // namespace lyra::lowering::hir_to_mir
