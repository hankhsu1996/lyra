#include "lyra/lowering/hir_to_mir/class_definition.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <functional>
#include <map>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/scope_instantiations.hpp"
#include "lyra/hir/specialization_ref.hpp"
#include "lyra/lowering/hir_to_mir/design_instantiations.hpp"
#include "lyra/lowering/hir_to_mir/library_entry_body.hpp"
#include "lyra/mir/block_body_ordinal.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/value_build.hpp"
#include "lyra/support/dump_kind.hpp"

namespace lyra::lowering::hir_to_mir {

auto BuildDefinitionRead(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ClassId of)
    -> mir::ExprId {
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
          unit.builtins.machine_c_string, std::string{text}));
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

// The address of `constant`, a value of `type`, as a value of `block`.
auto AddressOfConstant(
    const mir::CompilationUnit& unit, mir::Block& block,
    const mir::ClassConstantRef& constant, mir::TypeId type) -> mir::ExprId {
  return block.exprs.Add(
      mir::MakeAddressOfExpr(
          block.exprs.Add(
              mir::Expr{
                  .data = mir::ReferenceExpr{.target = constant},
                  .type = type}),
          ReadOnlyPointerTo(unit, type)));
}

// Holds `build`, a value of `type`, as a constant of class `id`, and answers
// its address as a value of `block`.
auto ConstantAddress(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ClassId id,
    mir::Class& cls, mir::TypeId type, mir::ValueBuild build) -> mir::ExprId {
  const mir::ClassConstantId constant = cls.constants.Add(
      mir::ClassConstantDecl{.type = type, .initializer = std::move(build)});
  return AddressOfConstant(
      unit, block, mir::ClassConstantRef{.owner = id, .constant = constant},
      type);
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
// of class `id`, and its address as a value of `block`: its timescale, how what
// an instance starts is run, what a dump records it as, then the bodies its
// export names reach.
//
//   {unit, precision, execution, dumped_as, exports, n}
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
          mir::MachineIntWidth::k8, mir::Signedness::kSigned),
      MachineInt(
          unit, info, static_cast<std::uint8_t>(cls.execution),
          mir::MachineIntWidth::k8, mir::Signedness::kUnsigned),
      MachineInt(
          unit, info, static_cast<std::uint8_t>(cls.dumped_as),
          mir::MachineIntWidth::k8, mir::Signedness::kUnsigned)};
  AppendNamedBodyTable(
      parts, unit, info, id, cls, Kind::kScopeCallable, exports);
  build.value = Record(unit, info, Kind::kScopeInfo, std::move(parts));
  return ConstantAddress(unit, block, id, cls, type, std::move(build));
}

}  // namespace

//   {"count", 7, 0, 8, kReg, 1, 1, kInput, 2}
auto DumpEntryAddress(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ClassId id,
    mir::Class& cls, const hir::DumpedDeclaration& declaration) -> mir::ExprId {
  using Kind = mir::RuntimeLibraryKind;
  mir::ValueBuild build;
  mir::Block& entry = build.body;
  const hir::DumpedDeclaration::Range range = declaration.range.value_or(
      hir::DumpedDeclaration::Range{.left = 0, .right = 0});
  const hir::DumpedDeclaration::Port port = declaration.port.value_or(
      hir::DumpedDeclaration::Port{
          .direction = support::DumpPortDirection::kInput, .position = 0});
  build.value = Record(
      unit, entry, Kind::kDumpEntry,
      {Name(unit, entry, declaration.identifier),
       MachineInt(
           unit, entry, range.left, mir::MachineIntWidth::k64,
           mir::Signedness::kSigned),
       MachineInt(
           unit, entry, range.right, mir::MachineIntWidth::k64,
           mir::Signedness::kSigned),
       MachineInt(
           unit, entry, declaration.width, mir::MachineIntWidth::k32,
           mir::Signedness::kUnsigned),
       MachineInt(
           unit, entry, static_cast<std::uint8_t>(declaration.kind),
           mir::MachineIntWidth::k8, mir::Signedness::kUnsigned),
       MachineInt(
           unit, entry, declaration.range.has_value() ? 1 : 0,
           mir::MachineIntWidth::k8, mir::Signedness::kUnsigned),
       MachineInt(
           unit, entry, declaration.port.has_value() ? 1 : 0,
           mir::MachineIntWidth::k8, mir::Signedness::kUnsigned),
       MachineInt(
           unit, entry, static_cast<std::uint8_t>(port.direction),
           mir::MachineIntWidth::k8, mir::Signedness::kUnsigned),
       MachineInt(
           unit, entry, port.position, mir::MachineIntWidth::k32,
           mir::Signedness::kUnsigned)});
  return ConstantAddress(
      unit, block, id, cls, LibraryType(unit, Kind::kDumpEntry),
      std::move(build));
}

//   {"blk", kBegin}
auto DumpBlockAddress(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ClassId id,
    mir::Class& cls, const hir::DumpedBlock& dumped) -> mir::ExprId {
  using Kind = mir::RuntimeLibraryKind;
  mir::ValueBuild build;
  mir::Block& entry = build.body;
  build.value = Record(
      unit, entry, Kind::kDumpBlock,
      {Name(unit, entry, dumped.identifier),
       MachineInt(
           unit, entry, static_cast<std::uint8_t>(dumped.kind),
           mir::MachineIntWidth::k8, mir::Signedness::kUnsigned)});
  return ConstantAddress(
      unit, block, id, cls, LibraryType(unit, Kind::kDumpBlock),
      std::move(build));
}

namespace {

// One body of a compiled unit, named by both: the unit, which is what it
// publishes and what every referrer names, and the body it is compiled to,
// which is what an object is made of.
struct BodyRef {
  std::string unit;
  std::string body;
};

// States the definition of `cls`: the class it extends, and what it states of
// its instances, which `instances` writes into the block it is handed.
//
//   {&Base::definition, &info}
void StateDefinition(
    const mir::CompilationUnit& unit, mir::Class& cls,
    const std::function<mir::ExprId(mir::Block&)>& instances) {
  mir::ValueBuild build;
  mir::Block& block = build.body;
  const std::optional<mir::ClassId> base = mir::DeclaredBase(cls);
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

// The object entry of the body `made` names, or the entry it publishes for its
// block body `built`, at the type `entry` its unit declared it with, read with
// that prototype erased, as a value of `block`.
//
//   (Erased)&Child::sv_create
auto ErasedObjectEntry(
    const mir::CompilationUnit& unit, mir::Block& block, const BodyRef& made,
    std::optional<mir::BlockBodyOrdinal> built, mir::TypeId entry)
    -> mir::ExprId {
  const mir::ExprId declared = block.exprs.Add(
      mir::Expr{
          .data =
              mir::ReferenceExpr{
                  .target =
                      mir::ExternalObjectEntryRef{
                          .unit_name = made.unit,
                          .body_name = made.body,
                          .block = built}},
          .type = entry});
  return block.exprs.Add(
      mir::Expr{
          .data = mir::CastExpr{.operand = declared},
          .type = mir::ErasedFunction(unit.types)});
}

// The construction tables the design root states: one per scope of each
// specialization an object is made of -- the specialization's own, and one per
// block it builds at each place of each of its generates, inside it -- each
// specialization's stated the first time an instantiation makes it and named
// by every later one.
//
//   const Instantiation entries[2] = {{(Erased)&Child::sv_create, &child},
//                                     ...};
//   const Instantiation g_places[2] = {{(Erased)&Top::sv_create_block_0, &g0},
//                                      {(Erased)&Top::sv_create_block_0, &g1}};
//   const Instantiation c_places[2] = {{nullptr, nullptr},
//                                      {(Erased)&Top::sv_create_block_1, &c1}};
//   const GeneratePlaces generates[2] = {{g_places, 2}, {c_places, 2}};
//   const ConstructionTable top = {entries, 2, generates, 2};
class ConstructionTables {
 public:
  ConstructionTables(
      mir::CompilationUnit& unit, mir::ClassId id, mir::Class& cls,
      const DesignInstantiations& instantiations,
      const std::map<std::string, mir::TypeId, std::less<>>& entry_types)
      : unit_(&unit),
        id_(id),
        cls_(&cls),
        instantiations_(&instantiations),
        entry_types_(&entry_types) {
  }

  // The table of the specialization `made`, which is stated once.
  auto Of(const hir::SpecializationRef& made) -> mir::ClassConstantRef {
    if (const auto stated = of_specialization_.find(made.specialization);
        stated != of_specialization_.end()) {
      return stated->second;
    }
    const SpecializationMade& found = Made(made);
    const mir::ClassConstantRef table =
        Of(BodyRef{.unit = made.unit, .body = found.body}, found.made);
    of_specialization_.emplace(made.specialization, table);
    return table;
  }

 private:
  // The table of a scope of the body `owner` laid out as `layout`: at each
  // instantiation it declares, how the specialization made there is made,
  // then at each place of each of its generates, how the block built there is
  // made.
  auto Of(const BodyRef& owner, const hir::ScopeInstantiations& layout)
      -> mir::ClassConstantRef {
    const std::span<const hir::SpecializationRef> instantiated =
        layout.Instantiated();
    std::vector<mir::ClassConstantRef> tables;
    std::vector<BodyRef> bodies;
    tables.reserve(instantiated.size());
    bodies.reserve(instantiated.size());
    for (const hir::SpecializationRef& made : instantiated) {
      tables.push_back(Of(made));
      bodies.push_back(BodyRef{.unit = made.unit, .body = Made(made).body});
    }
    // A block's table is stated before the table of the scope building it,
    // which names it.
    const std::span<const hir::GeneratePlaces> generates = layout.Generates();
    std::vector<std::vector<std::optional<mir::ClassConstantRef>>> block_tables;
    block_tables.reserve(generates.size());
    for (const hir::GeneratePlaces& generate : generates) {
      std::vector<std::optional<mir::ClassConstantRef>>& of_generate =
          block_tables.emplace_back();
      of_generate.reserve(generate.places.size());
      for (const std::optional<hir::PlacedBlock>& block : generate.places) {
        of_generate.push_back(
            block.has_value() ? std::optional{Of(owner, block->made)}
                              : std::nullopt);
      }
    }

    using Kind = mir::RuntimeLibraryKind;
    mir::ValueBuild build;
    mir::Block& table = build.body;
    std::vector<mir::ExprId> parts;
    AppendTable(
        parts, *unit_, table, id_, *cls_,
        LibraryType(*unit_, Kind::kInstantiation), instantiated.size(),
        [&](mir::Block& entry, std::size_t i) {
          return EntryRecord(
              entry, bodies[i], std::nullopt, EntryTypeOf(bodies[i].unit),
              tables[i]);
        });
    AppendTable(
        parts, *unit_, table, id_, *cls_,
        LibraryType(*unit_, Kind::kGeneratePlaces), generates.size(),
        [&](mir::Block& at_generate, std::size_t g) {
          const hir::GeneratePlaces& generate = generates[g];
          const mir::TypeId entry_type =
              BlockEntryType(*unit_, generate.handed_an_index);
          std::vector<mir::ExprId> places;
          AppendTable(
              places, *unit_, at_generate, id_, *cls_,
              LibraryType(*unit_, Kind::kInstantiation), generate.places.size(),
              [&](mir::Block& entry, std::size_t k) {
                const std::optional<hir::PlacedBlock>& block =
                    generate.places[k];
                return block.has_value()
                           ? EntryRecord(
                                 entry, owner, BlockBodyOrdinalOf(block->body),
                                 entry_type, *block_tables[g][k])
                           : NoEntryRecord(entry);
              });
          return Record(
              *unit_, at_generate, Kind::kGeneratePlaces, std::move(places));
        });
    build.value =
        Record(*unit_, table, Kind::kConstructionTable, std::move(parts));
    return mir::ClassConstantRef{
        .owner = id_,
        .constant = cls_->constants.Add(
            mir::ClassConstantDecl{
                .type = mir::ConstructionTableType(unit_->types),
                .initializer = std::move(build)})};
  }

  // How one object is made at an entry of a table: the object entry of the
  // body `made`, or the entry it publishes for its block body `built`, at the
  // type `entry_type` that entry has, and the table the object is built with.
  auto EntryRecord(
      mir::Block& entry, const BodyRef& made,
      std::optional<mir::BlockBodyOrdinal> built, mir::TypeId entry_type,
      mir::ClassConstantRef table) -> mir::ExprId {
    return Record(
        *unit_, entry, mir::RuntimeLibraryKind::kInstantiation,
        {ErasedObjectEntry(*unit_, entry, made, built, entry_type),
         AddressOfConstant(
             *unit_, entry, table, mir::ConstructionTableType(unit_->types))});
  }

  // A place no object of the body builds a block at -- an alternative it did
  // not select -- which holds no entry, and the body's code never reads one
  // there.
  auto NoEntryRecord(mir::Block& entry) -> mir::ExprId {
    return Record(
        *unit_, entry, mir::RuntimeLibraryKind::kInstantiation,
        {entry.exprs.Add(
             mir::Expr{
                 .data = mir::NullLiteral{},
                 .type = mir::ErasedFunction(unit_->types)}),
         entry.exprs.Add(
             mir::Expr{
                 .data = mir::NullLiteral{},
                 .type = mir::ConstructionTablePointer(unit_->types)})});
  }

  // What the specialization `made` builds, as its lowering recorded it.
  auto Made(const hir::SpecializationRef& made) const
      -> const SpecializationMade& {
    const auto found =
        instantiations_->by_specialization.find(made.specialization);
    if (found == instantiations_->by_specialization.end()) {
      throw InternalError(
          std::format(
              "hir_to_mir: an object of specialization '{}' is made, and what "
              "it builds was never recorded -- please report this as a bug",
              made.specialization));
    }
    return found->second;
  }

  auto EntryTypeOf(const std::string& unit) const -> mir::TypeId {
    const auto found = entry_types_->find(unit);
    if (found == entry_types_->end()) {
      throw InternalError(
          std::format(
              "hir_to_mir: an object of unit '{}' is made, and how its object "
              "entry is declared was never taken from what it published -- "
              "please report this as a bug",
              unit));
    }
    return found->second;
  }

  mir::CompilationUnit* unit_;
  mir::ClassId id_;
  mir::Class* cls_;
  const DesignInstantiations* instantiations_;
  const std::map<std::string, mir::TypeId, std::less<>>* entry_types_;
  std::map<std::string, mir::ClassConstantRef, std::less<>> of_specialization_;
};

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

auto BlockEntryType(mir::CompilationUnit& unit, bool handed_its_index)
    -> mir::TypeId {
  std::vector<mir::TypeId> params{
      unit.builtins.scope_ptr, mir::ConstructionTablePointer(unit.types)};
  if (handed_its_index) params.push_back(unit.builtins.integer);
  const mir::TypeId owned = unit.types.Intern(
      mir::Type{mir::PointerType{
          .pointee = unit.types.Get(unit.builtins.scope_ptr)
                         .Get<mir::PointerType>()
                         .pointee,
          .ownership = mir::PointerOwnership::kUnique}});
  return unit.types.Intern(
      mir::Type{mir::MachineFunctionType{
          .params = std::move(params), .result = owned}});
}

auto StateConstructionTables(
    mir::CompilationUnit& unit, mir::ClassId id, mir::Class& cls,
    const hir::SpecializationRef& root,
    const DesignInstantiations& instantiations,
    const std::map<std::string, mir::TypeId, std::less<>>& entry_types)
    -> mir::ClassConstantRef {
  return ConstructionTables(unit, id, cls, instantiations, entry_types)
      .Of(root);
}

}  // namespace lyra::lowering::hir_to_mir
