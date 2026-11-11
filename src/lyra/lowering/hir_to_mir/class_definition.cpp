#include "lyra/lowering/hir_to_mir/class_definition.hpp"

#include <algorithm>
#include <array>
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
#include <variant>
#include <vector>

#include "lyra/abi/library_records.hpp"
#include "lyra/abi/records.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/time.hpp"
#include "lyra/hir/lowering_ref.hpp"
#include "lyra/hir/owned_child_step.hpp"
#include "lyra/hir/scope_instantiations.hpp"
#include "lyra/hir/unit_definition.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/design_instantiations.hpp"
#include "lyra/lowering/hir_to_mir/library_entry_body.hpp"
#include "lyra/mir/block_body_ordinal.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/constant_id.hpp"
#include "lyra/mir/constant_value.hpp"
#include "lyra/mir/dispatch_table.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/library_record_builder.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/dump_kind.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/support/scope_kind.hpp"

namespace lyra::lowering::hir_to_mir {

auto BuildDefinitionRead(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ClassId of)
    -> mir::ExprId {
  const mir::ExprId definition = block.Add(
      mir::Expr{
          .data = mir::ReferenceExpr{.target = mir::DefinitionRef{.of = of}},
          .type = mir::LibraryTypeOf(
              unit.types, support::LibraryDecl::kObjectDefinition)});
  return block.Add(
      mir::Expr{
          .data = mir::AddressOfExpr{.operand = definition},
          .type = mir::ClassDefinitionPointer(unit.types)});
}

namespace {

// What a class tells the runtime library is constants of the unit, each stated
// as the value it is: a structure the library declares, composed from a part
// for each member it declares. For a class `Mid` an instance of the design
// hierarchy is built of, exporting one task as `bump` (LRM 35.5):
//
//   const array<ScopeCallable, 1> exports = {{{"bump", &Mid::bump_entry}}};
//   const ScopeInfo info = {{&definition, ConstantSpan(&declared)},
//                           ConstantSpan(&exports), ...};
//   const array<InterfaceRow, 0> rows = {};
//   const ObjectDefinition Mid::definition =
//       {nullptr, &info, nullptr, &Mid::end, ConstantSpan(&rows),
//        &Mid::methods};
//
// A table is an array constant of the unit, and whatever points at one holds a
// view of all of it.

auto ReadOnlyPointerTo(const mir::CompilationUnit& unit, mir::TypeId pointee)
    -> mir::TypeId {
  return unit.types.Intern(
      mir::Type{mir::PointerType{
          .pointee = pointee,
          .ownership = mir::PointerOwnership::kBorrowed,
          .mutability = mir::Mutability::kReadOnly}});
}

auto Null() -> mir::ConstantValue {
  return mir::ConstantValue{mir::NullValue{}};
}

auto Name(std::string_view text) -> mir::ConstantValue {
  return mir::ConstantValue{mir::CStringValue{.text = std::string{text}}};
}

auto Number(std::int64_t value) -> mir::ConstantValue {
  return mir::ConstantValue{mir::MachineIntValue{.value = value}};
}

// The address of what `to` names, which naming it in a body is a value of
// `type`.
auto PointerTo(mir::ReferenceTarget to, mir::TypeId type)
    -> mir::ConstantValue {
  return mir::ConstantValue{
      mir::PointerValue{.to = std::move(to), .type = type}};
}

// `callable` of class `id`, as the code it is: `&Mid::bump_entry`.
auto Function(
    const mir::CompilationUnit& unit, mir::ClassId id, const mir::Class& cls,
    mir::CallableId callable) -> mir::ConstantValue {
  return PointerTo(
      mir::FunctionRef{
          .body = mir::CallableTarget{.owner = id, .slot = callable}},
      mir::FunctionTypeOf(unit.types, cls.callables.Get(callable).code));
}

// What the address of a record of the library's structure `Record` is a value
// of.
template <typename Record>
auto RecordPointerType(const mir::CompilationUnit& unit) -> mir::TypeId {
  return ReadOnlyPointerTo(
      unit, mir::LibraryTypeOf(unit.types, abi::LibraryDeclOf<Record>()));
}

// The address of `constant`, a constant of the unit.
auto AddressOf(const mir::CompilationUnit& unit, mir::ConstantId constant)
    -> mir::ConstantValue {
  return PointerTo(
      mir::ConstantRef{.constant = constant},
      unit.constants.Get(constant).type);
}

// A table of `count` entries of the library's structure `Entry`, each the value
// `write` answers for its position: an array constant of the unit holding the
// entries, and a view of all of it.
template <typename Entry>
auto Table(
    const mir::CompilationUnit& unit, std::size_t count,
    const std::function<mir::ConstantValue(std::size_t)>& write)
    -> mir::ConstantValue {
  std::vector<mir::ConstantValue> entries;
  entries.reserve(count);
  for (std::size_t i = 0; i < count; ++i) {
    entries.push_back(write(i));
  }
  return mir::ViewOf(
      unit, mir::LibraryTypeOf(unit.types, abi::LibraryDeclOf<Entry>()),
      std::move(entries));
}

// A body answering a callback the library makes on an object, and the
// prototype of the entry the library enters it through, which hands it the
// object as the library does.
struct LibraryBody {
  mir::CallableTarget body;
  mir::TypeId prototype;
};

// The entry the library enters for `body`, named at its own prototype, which
// the member holding it declares as the library's; none where no body answers.
auto EntryOrNone(const std::optional<LibraryBody>& body) -> mir::ConstantValue {
  if (!body.has_value()) {
    return Null();
  }
  return PointerTo(mir::DispatchEntryRef{.body = body->body}, body->prototype);
}

// The callback answering each phase of a scope of the design hierarchy, by
// the member of its record that holds it.
constexpr std::array kScopePhases{
    std::pair{
        &abi::ScopeInfo::resolve, support::LibraryCallback::kScopeResolve},
    std::pair{
        &abi::ScopeInfo::initialize,
        support::LibraryCallback::kScopeInitialize},
    std::pair{
        &abi::ScopeInfo::create_processes,
        support::LibraryCallback::kScopeCreateProcesses},
    std::pair{
        &abi::ScopeInfo::describe, support::LibraryCallback::kScopeDescribe}};

// The callback answering each question `randomize()` asks of an object of a
// source class (LRM 18.6), by the member of its record that holds it: those
// handed the call's problem, and the hooks before and after.
constexpr std::array kRandomizationOnProblem{
    std::pair{
        &abi::ClassObjectInfo::rand_variables,
        support::LibraryCallback::kClassRandVariables},
    std::pair{
        &abi::ClassObjectInfo::constrain,
        support::LibraryCallback::kClassConstrain},
    std::pair{
        &abi::ClassObjectInfo::rand_objects,
        support::LibraryCallback::kClassRandObjects}};
constexpr std::array kRandomizationHooks{
    std::pair{
        &abi::ClassObjectInfo::pre_randomize,
        support::LibraryCallback::kClassPreRandomize},
    std::pair{
        &abi::ClassObjectInfo::post_randomize,
        support::LibraryCallback::kClassPostRandomize}};

// The body answering each callback the library makes on an object of a class,
// where one does.
using LibraryBodies =
    std::map<support::LibraryCallback, std::optional<LibraryBody>>;

auto BodyFor(const LibraryBodies& bodies, support::LibraryCallback callback)
    -> const std::optional<LibraryBody>& {
  const auto found = bodies.find(callback);
  if (found == bodies.end()) {
    throw InternalError(
        "hir_to_mir: a definition names a callback whose body was never "
        "looked for -- please report this as a bug");
  }
  return found->second;
}

// Each member `answers` names of the record `record` composes, holding the
// entry of the body answering its callback.
template <typename Record, typename Answers>
void SetEntries(
    mir::LibraryRecordBuilder<Record>& record, const Answers& answers,
    const LibraryBodies& bodies) {
  for (const auto& [member, callback] : answers) {
    record.Set(member, EntryOrNone(BodyFor(bodies, callback)));
  }
}

// What a scope states of itself, as the record the library reads it from: the
// address of the record of its definition, and a view of the addresses of the
// records of the scopes it declares, of each family.
auto ScopeDeclarationsRecord(
    const mir::CompilationUnit& unit,
    const mir::ScopeDeclarations& scope_declarations)
    -> mir::LibraryRecordBuilder<abi::ScopeDeclarations> {
  using abi::ScopeDeclarations;
  const auto addresses_of = [&](const std::vector<mir::ConstantId>& records) {
    std::vector<mir::ConstantValue> addresses;
    addresses.reserve(records.size());
    for (const mir::ConstantId record : records) {
      addresses.push_back(AddressOf(unit, record));
    }
    return addresses;
  };
  mir::LibraryRecordBuilder<ScopeDeclarations> record(unit);
  record.Set(
      &ScopeDeclarations::definition,
      AddressOf(unit, scope_declarations.definition));
  record.Set(
      &ScopeDeclarations::elaborated,
      mir::ViewOf(
          unit, RecordPointerType<abi::ElaboratedScopeRecord>(unit),
          addresses_of(scope_declarations.elaborated)));
  record.Set(
      &ScopeDeclarations::lexical,
      mir::ViewOf(
          unit, RecordPointerType<abi::LexicalScopeRecord>(unit),
          addresses_of(scope_declarations.lexical)));
  return record;
}

// What a class of the design hierarchy states of its instances, as a constant
// of the unit, and its address: what they state of themselves, the bodies its
// exports name, then the body of each of its phases.
auto ScopeInfoAddress(
    const mir::CompilationUnit& unit, mir::ClassId id, const mir::Class& cls,
    const LibraryBodies& bodies) -> mir::ConstantValue {
  using abi::ScopeInfo;
  mir::LibraryRecordBuilder<ScopeInfo> info(unit);
  info.Set(
      &ScopeInfo::scope_declarations,
      ScopeDeclarationsRecord(unit, *cls.scope_declarations).Build());
  info.Set(
      &ScopeInfo::exports,
      Table<abi::ScopeCallable>(unit, cls.exports.size(), [&](std::size_t i) {
        using abi::ScopeCallable;
        const mir::NamedCallable& exported = cls.exports[i];
        mir::LibraryRecordBuilder<ScopeCallable> callable(unit);
        callable.Set(&ScopeCallable::name, Name(exported.name));
        callable.Set(
            &ScopeCallable::entry, Function(unit, id, cls, exported.body));
        return std::move(callable).Build();
      }));
  SetEntries(info, kScopePhases, bodies);
  return AddressOf(unit, std::move(info).Intern());
}

// What a definition record states of a definition of one form: its kind, and
// each directive in force where it was written that a definition of the form
// has, a record's own value standing for one it does not have. A class
// lowered alone and a text of the tool's are no scope of the run, so no record
// is made of them.
struct RecordedDefinition {
  support::DefinitionKind kind = support::DefinitionKind::kModule;
  support::DefaultNetType default_net_type = support::DefaultNetType::kWire;
  support::UnconnectedDrive unconnected_drive =
      support::UnconnectedDrive::kNone;
  bool celldefine = false;
  bool automatic = false;
};

auto DefinitionKindOf(hir::DesignElementKind kind) -> support::DefinitionKind {
  switch (kind) {
    case hir::DesignElementKind::kModule:
      return support::DefinitionKind::kModule;
    case hir::DesignElementKind::kInterface:
      return support::DefinitionKind::kInterface;
    case hir::DesignElementKind::kProgram:
      return support::DefinitionKind::kProgram;
  }
  throw InternalError("DefinitionKindOf: a design element of no kind");
}

auto RecordedDefinitionOf(const hir::DefinitionForm& form)
    -> RecordedDefinition {
  return std::visit(
      Overloaded{
          [](const hir::DesignElementDefinition& element) {
            return RecordedDefinition{
                .kind = DefinitionKindOf(element.kind),
                .default_net_type = element.default_net_type,
                .unconnected_drive = element.unconnected_drive,
                .celldefine = element.celldefine,
                .automatic = element.automatic};
          },
          [](const hir::PackageDefinition& package) {
            return RecordedDefinition{
                .kind = support::DefinitionKind::kPackage,
                .default_net_type = package.default_net_type,
                .automatic = package.automatic};
          },
          [](const hir::CompilationUnitScope&) {
            return RecordedDefinition{
                .kind = support::DefinitionKind::kCompilationUnit};
          },
          [](const hir::DesignRoot&) {
            return RecordedDefinition{
                .kind = support::DefinitionKind::kDesignRoot};
          },
          [](const hir::ClassSpecialization&) -> RecordedDefinition {
            throw InternalError(
                "RecordedDefinitionOf: a class lowered alone is no scope of "
                "the run");
          },
          [](const hir::ToolText&) -> RecordedDefinition {
            throw InternalError(
                "RecordedDefinitionOf: a text of the tool's is no scope of "
                "the run");
          }},
      form);
}

}  // namespace

//   {&blk, "count", 7, 0, 8, kReg, 1, 1, kInput, 2}
auto DumpEntryAddress(
    const mir::CompilationUnit& unit, mir::Builder block,
    const hir::DumpedDeclaration& declaration,
    std::optional<mir::ConstantId> enclosing) -> mir::ExprId {
  using abi::DumpEntry;
  const hir::DumpedDeclaration::Range range = declaration.range.value_or(
      hir::DumpedDeclaration::Range{.left = 0, .right = 0});
  const hir::DumpedDeclaration::Port port = declaration.port.value_or(
      hir::DumpedDeclaration::Port{
          .direction = support::DumpPortDirection::kInput, .position = 0});
  mir::LibraryRecordBuilder<DumpEntry> entry(unit);
  entry.Set(
      &DumpEntry::enclosing,
      enclosing.has_value() ? AddressOf(unit, *enclosing) : Null());
  entry.Set(&DumpEntry::identifier, Name(declaration.identifier));
  entry.Set(&DumpEntry::left, Number(range.left));
  entry.Set(&DumpEntry::right, Number(range.right));
  entry.Set(&DumpEntry::width, Number(declaration.width));
  entry.Set(
      &DumpEntry::kind, Number(static_cast<std::uint8_t>(declaration.kind)));
  entry.Set(
      &DumpEntry::has_range, Number(declaration.range.has_value() ? 1 : 0));
  entry.Set(&DumpEntry::has_port, Number(declaration.port.has_value() ? 1 : 0));
  entry.Set(
      &DumpEntry::port_direction,
      Number(static_cast<std::uint8_t>(port.direction)));
  entry.Set(&DumpEntry::port_position, Number(port.position));
  return ConstantAddressIn(unit, block, std::move(entry).Intern());
}

//   {"Top", "work", "Top", -9, -12, 1, kModule, kWire, kNone, 0, 0}
auto DefinitionRecordConstant(
    const mir::CompilationUnit& unit, const hir::CompilationUnit& hir)
    -> mir::ConstantId {
  using abi::DefinitionRecord;
  const hir::UnitDefinition& definition = hir.definition;
  const TimeResolution& time = hir.root_scope.time_resolution;
  mir::LibraryRecordBuilder<DefinitionRecord> record(unit);
  record.Set(&DefinitionRecord::name, Name(hir.source_name));
  record.Set(&DefinitionRecord::library, Name(definition.library));
  record.Set(&DefinitionRecord::cell, Name(definition.cell));
  record.Set(&DefinitionRecord::time_unit_power, Number(time.unit_power));
  record.Set(
      &DefinitionRecord::time_precision_power, Number(time.precision_power));
  record.Set(
      &DefinitionRecord::time_stated,
      Number(hir.root_scope.time_resolution_stated ? 1 : 0));
  const RecordedDefinition recorded = RecordedDefinitionOf(definition.form);
  record.Set(
      &DefinitionRecord::kind,
      Number(static_cast<std::uint8_t>(recorded.kind)));
  record.Set(
      &DefinitionRecord::default_net_type,
      Number(static_cast<std::uint8_t>(recorded.default_net_type)));
  record.Set(
      &DefinitionRecord::unconnected_drive,
      Number(static_cast<std::uint8_t>(recorded.unconnected_drive)));
  record.Set(
      &DefinitionRecord::celldefine, Number(recorded.celldefine ? 1 : 0));
  record.Set(&DefinitionRecord::automatic, Number(recorded.automatic ? 1 : 0));
  return std::move(record).Intern();
}

//   {&definition, {&run, &step}}
auto ScopeDeclarationsAddress(
    const mir::CompilationUnit& unit, mir::Builder block,
    const mir::ScopeDeclarations& scope_declarations) -> mir::ExprId {
  return ConstantAddressIn(
      unit, block, ScopeDeclarationsRecord(unit, scope_declarations).Intern());
}

//   {"u", kModuleArray, 0}
auto ElaboratedScopeRecordConstant(
    const mir::CompilationUnit& unit, support::ElaboratedScopeKind kind,
    const std::string& name, bool implicitly_named) -> mir::ConstantId {
  using abi::ElaboratedScopeRecord;
  mir::LibraryRecordBuilder<ElaboratedScopeRecord> record(unit);
  record.Set(&ElaboratedScopeRecord::name, Name(name));
  record.Set(
      &ElaboratedScopeRecord::kind, Number(static_cast<std::uint8_t>(kind)));
  record.Set(
      &ElaboratedScopeRecord::implicitly_named,
      Number(implicitly_named ? 1 : 0));
  return std::move(record).Intern();
}

//   {"blk", &enclosing, 3, kBegin}
auto LexicalScopeRecordConstant(
    const mir::CompilationUnit& unit, support::LexicalScopeKind kind,
    const std::optional<std::string>& name,
    std::optional<mir::ConstantId> enclosing, std::uint32_t position)
    -> mir::ConstantId {
  using abi::LexicalScopeRecord;
  mir::LibraryRecordBuilder<LexicalScopeRecord> record(unit);
  record.Set(&LexicalScopeRecord::name, Name(name.value_or("")));
  record.Set(
      &LexicalScopeRecord::enclosing,
      enclosing.has_value() ? AddressOf(unit, *enclosing) : Null());
  record.Set(&LexicalScopeRecord::position, Number(position));
  record.Set(
      &LexicalScopeRecord::kind, Number(static_cast<std::uint8_t>(kind)));
  return std::move(record).Intern();
}

auto ConstantAddressIn(
    const mir::CompilationUnit& unit, mir::Builder block,
    mir::ConstantId constant) -> mir::ExprId {
  const mir::TypeId type = unit.constants.Get(constant).type;
  return block.Add(
      mir::MakeAddressOfExpr(
          block.Add(
              mir::Expr{
                  .data =
                      mir::ReferenceExpr{
                          .target = mir::ConstantRef{.constant = constant}},
                  .type = type}),
          ReadOnlyPointerTo(unit, type)));
}

namespace {

// One body of a compiled unit, named by both: the unit, which is what it
// publishes and what every referrer names, and the body it is compiled to,
// which is what an object is made of.
struct BodyRef {
  std::string unit;
  std::string body;

  auto operator==(const BodyRef&) const -> bool = default;
};

// The object entry of the body `made` names, or the entry it publishes for its
// block body `built`, named at the type `entry` its unit declared it with,
// which a table holds at the prototype it holds every entry at.
//
//   (Erased)&Child::sv_create
auto ObjectEntry(
    const BodyRef& made, std::optional<mir::BlockBodyOrdinal> built,
    mir::TypeId entry) -> mir::ConstantValue {
  return PointerTo(
      mir::ExternalObjectEntryRef{
          .unit_name = made.unit, .body_name = made.body, .block = built},
      entry);
}

// The construction tables the design root states: one per scope of each
// lowering an object is made of -- the lowering's own, and one per
// block it builds at each place of each of its generates, inside it -- each
// lowering's stated the first time an instantiation makes it and named
// by every later one.
//
//   const Instantiation entries[2] = {{(Erased)&Child::sv_create, &child},
//                                     ...};
//   const Instantiation g_places[2] = {{(Erased)&Top::sv_create_block_0, &g0},
//                                      {(Erased)&Top::sv_create_block_0, &g1}};
//   const Instantiation c_places[2] = {{nullptr, nullptr},
//                                      {(Erased)&Top::sv_create_block_1, &c1}};
//   const GeneratePlaces generates[2] = {{ConstantSpan(&g_places)},
//                                        {ConstantSpan(&c_places)}};
//   const ConstructionTable top = {ConstantSpan(&entries),
//                                  ConstantSpan(&generates)};
class ConstructionTables {
 public:
  ConstructionTables(
      const mir::CompilationUnit& unit,
      const DesignInstantiations& instantiations,
      const std::map<std::string, mir::TypeId, std::less<>>& entry_types)
      : unit_(&unit),
        instantiations_(&instantiations),
        entry_types_(&entry_types) {
    for (const auto& [_, made] : instantiations.by_lowering) {
      Gather(made.body, std::nullopt, made.made);
    }
  }

  // The table of the lowering `made`, which is stated once.
  auto Of(const hir::LoweringRef& made) -> mir::ConstantId {
    if (const auto stated = of_lowering_.find(made.lowering);
        stated != of_lowering_.end()) {
      return stated->second;
    }
    const LoweringMade& found = Made(made);
    const mir::ConstantId table =
        Of(BodyRef{.unit = made.unit, .body = found.body}, std::nullopt,
           found.made);
    of_lowering_.emplace(made.lowering, table);
    return table;
  }

 private:
  // What one entry of a table makes: an object of the body `made`, or a block
  // of its block body `block`, built with the table `table`.
  struct Entry {
    BodyRef made;
    std::optional<mir::BlockBodyOrdinal> block;
    mir::ConstantId table;

    auto operator==(const Entry&) const -> bool = default;
  };

  // The places of one generate in a table: whether the blocks are handed the
  // index their loop stands at, and the entry at each place, absent where no
  // block stands.
  struct Places {
    bool handed_an_index = false;
    std::vector<std::optional<Entry>> places;
  };

  // What a table holds: an entry per object of each instantiation, then the
  // places of each generate.
  struct Content {
    std::vector<Entry> instances;
    std::vector<Places> generates;
  };

  // Where blocks of one generate stand among the scopes the design builds of
  // one body: the body, the block body whose scope holds the generate, none
  // for the body's own, the generate, and one place of it, or every place
  // where `place` is none.
  struct GeneratePlace {
    std::string body;
    std::optional<hir::BlockBodyId> scope;
    std::size_t generate = 0;
    std::optional<std::size_t> place;

    // The place `k` of the same generate.
    [[nodiscard]] auto At(std::size_t k) const -> GeneratePlace {
      GeneratePlace at = *this;
      at.place = k;
      return at;
    }

    auto operator<=>(const GeneratePlace&) const = default;
  };

  // Records, for every place of every scope the design builds, each block
  // built there, under the place and under the whole generate.
  void Gather(
      const std::string& body, std::optional<hir::BlockBodyId> scope,
      const hir::ScopeInstantiations& layout) {
    const std::span<const hir::GeneratePlaces> generates = layout.Generates();
    for (std::size_t g = 0; g < generates.size(); ++g) {
      for (std::size_t k = 0; k < generates[g].places.size(); ++k) {
        const hir::PlacedBlock& block = generates[g].places[k];
        if (!block.body.has_value()) continue;
        const GeneratePlace generate{
            .body = body, .scope = scope, .generate = g, .place = std::nullopt};
        built_at_[generate].push_back(&block);
        built_at_[generate.At(k)].push_back(&block);
        Gather(body, block.body, block.made);
      }
    }
  }

  // The entry of the block `block` of the body `owner`.
  auto EntryOf(const BodyRef& owner, const hir::PlacedBlock& block) -> Entry {
    return Entry{
        .made = owner,
        .block = BlockBodyOrdinalOf(*block.body),
        .table = Of(owner, block.body, block.made)};
  }

  // The entry every block built at `at` anywhere holds, where they all hold
  // one, and nothing otherwise. That is a question of the place alone, so two
  // scopes answer it alike whichever of them is asked first.
  auto UniformEntryAt(const BodyRef& owner, const GeneratePlace& at)
      -> std::optional<Entry> {
    if (const auto known = uniform_entries_.find(at);
        known != uniform_entries_.end()) {
      return known->second;
    }
    std::optional<Entry> held;
    if (const auto built = built_at_.find(at); built != built_at_.end()) {
      std::vector<Entry> entries;
      for (const hir::PlacedBlock* block : built->second) {
        entries.push_back(EntryOf(owner, *block));
      }
      if (std::ranges::all_of(entries, [&](const Entry& entry) {
            return entry == entries.front();
          })) {
        held = entries.front();
      }
    }
    uniform_entries_.emplace(at, held);
    return held;
  }

  // The table of a scope of the body `owner` laid out as `layout` -- the
  // body's own scope, or that of its block body `scope`: at each
  // instantiation it declares, how the lowering made there is made,
  // then at each place of each of its generates, how the block built there is
  // made. Scopes whose tables hold the same entries are made with one table
  // -- the blocks of a loop handed different values that build alike, above
  // all -- so the tables follow what objects differ in rather than how many
  // there are. A table named by another is stated before it.
  auto Of(
      const BodyRef& owner, std::optional<hir::BlockBodyId> scope,
      const hir::ScopeInstantiations& layout) -> mir::ConstantId {
    Content content;
    for (const hir::LoweringRef& made : layout.Instantiated()) {
      content.instances.push_back(
          Entry{
              .made = BodyRef{.unit = made.unit, .body = Made(made).body},
              .block = std::nullopt,
              .table = Of(made)});
    }
    const std::span<const hir::GeneratePlaces> generates = layout.Generates();
    for (std::size_t g = 0; g < generates.size(); ++g) {
      const GeneratePlace generate{
          .body = owner.body,
          .scope = scope,
          .generate = g,
          .place = std::nullopt};
      Places& placed = content.generates.emplace_back(
          Places{
              .handed_an_index = generates[g].handed_an_index, .places = {}});
      // A place an object built no block at -- an alternative it did not
      // select (LRM 27.5) -- is never read there, so all it decides is which
      // tables are one, and it holds what that place holds wherever a block
      // was built at it.
      for (std::size_t k = 0; k < generates[g].places.size(); ++k) {
        const hir::PlacedBlock& block = generates[g].places[k];
        placed.places.push_back(
            block.body.has_value() ? std::optional{EntryOf(owner, block)}
                                   : UniformEntryAt(owner, generate.At(k)));
      }
      // Places that all hold one entry list it once, and the runtime reads it
      // at every place: every block the generate builds wherever it stands,
      // whichever places and how many this object built -- a loop's count is
      // its construction's (LRM 27.4) -- or failing that the places here.
      if (const std::optional<Entry> held = UniformEntryAt(owner, generate)) {
        placed.places = {held};
      } else if (std::ranges::all_of(placed.places, [&](const auto& at) {
                   return at == placed.places.front();
                 })) {
        placed.places.resize(std::min<std::size_t>(placed.places.size(), 1));
      }
    }
    return State(content);
  }

  // The table holding `content`, which tables holding the same entries share.
  auto State(const Content& content) -> mir::ConstantId {
    using abi::ConstructionTable;
    using abi::GeneratePlaces;
    using abi::Instantiation;
    mir::LibraryRecordBuilder<ConstructionTable> table(*unit_);
    table.Set(
        &ConstructionTable::instantiations,
        Table<Instantiation>(
            *unit_, content.instances.size(), [&](std::size_t i) {
              const Entry& made = content.instances[i];
              return EntryRecord(made, EntryTypeOf(made.made.unit));
            }));
    table.Set(
        &ConstructionTable::generates,
        Table<GeneratePlaces>(
            *unit_, content.generates.size(), [&](std::size_t g) {
              const Places& generate = content.generates[g];
              const mir::TypeId entry_type =
                  BlockEntryType(*unit_, generate.handed_an_index);
              mir::LibraryRecordBuilder<GeneratePlaces> places(*unit_);
              places.Set(
                  &GeneratePlaces::places,
                  Table<Instantiation>(
                      *unit_, generate.places.size(), [&](std::size_t k) {
                        const std::optional<Entry>& block = generate.places[k];
                        return block.has_value()
                                   ? EntryRecord(*block, entry_type)
                                   : NoEntryRecord();
                      }));
              return std::move(places).Build();
            }));
    return std::move(table).Intern();
  }

  // The record of the entry `made`, its entry named at the type `entry_type`
  // that entry has.
  auto EntryRecord(const Entry& made, mir::TypeId entry_type)
      -> mir::ConstantValue {
    using abi::Instantiation;
    mir::LibraryRecordBuilder<Instantiation> record(*unit_);
    record.Set(
        &Instantiation::entry, ObjectEntry(made.made, made.block, entry_type));
    record.Set(&Instantiation::table, AddressOf(*unit_, made.table));
    return std::move(record).Build();
  }

  // A place this object built no block at -- an alternative it did not select
  // -- that no one entry fills, which holds no entry; the body's code never
  // reads one there.
  auto NoEntryRecord() -> mir::ConstantValue {
    using abi::Instantiation;
    mir::LibraryRecordBuilder<Instantiation> record(*unit_);
    record.Set(&Instantiation::entry, Null());
    record.Set(&Instantiation::table, Null());
    return std::move(record).Build();
  }

  // What the lowering `made` builds, as its lowering recorded it.
  [[nodiscard]] auto Made(const hir::LoweringRef& made) const
      -> const LoweringMade& {
    const auto found = instantiations_->by_lowering.find(made.lowering);
    if (found == instantiations_->by_lowering.end()) {
      throw InternalError(
          std::format(
              "hir_to_mir: an object of lowering '{}' is made, and what "
              "it builds was never recorded -- please report this as a bug",
              made.lowering));
    }
    return found->second;
  }

  [[nodiscard]] auto EntryTypeOf(const std::string& unit) const -> mir::TypeId {
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

  const mir::CompilationUnit* unit_;
  const DesignInstantiations* instantiations_;
  const std::map<std::string, mir::TypeId, std::less<>>* entry_types_;
  std::map<std::string, mir::ConstantId, std::less<>> of_lowering_;
  // Every block built at each place of every scope the design builds.
  std::map<GeneratePlace, std::vector<const hir::PlacedBlock*>> built_at_;
  // The one entry each place, or each generate, holds wherever a block was
  // built at it, once worked out.
  std::map<GeneratePlace, std::optional<Entry>> uniform_entries_;
};

// What the definition of one class states that its lineage decides, read
// before any class of the unit is changed, since stating the definition adds
// to the class it states.
struct LineageFacts {
  bool is_interface = false;
  bool is_class_object = false;
  // The body answering each callback the library makes on an object of the
  // class.
  LibraryBodies bodies;
  std::vector<mir::DispatchedBehavior> dispatch;
  std::vector<mir::InterfaceTable> interface_tables;
};

// The root a class's lineage ends at, where it ends at one: an interface
// extends none (LRM 8.26).
auto RootOf(const mir::CompilationUnit& unit, mir::ClassId id)
    -> std::optional<mir::ClassRef> {
  mir::ClassId at = id;
  while (const std::optional<mir::ClassId> base =
             mir::DeclaredBase(unit.GetClass(at))) {
    at = *base;
  }
  return unit.GetClass(at).base;
}

// The prototypes a body the library reaches through a class's definition is
// entered at. Every body answering a library callback is handed the object as
// the root every object is, then what every body answering that callback takes
// after its receiver -- the call's problem, for what `randomize()` asks of the
// variables, the constraints and the random objects (LRM 18.6) -- answering
// nothing. It follows from the callback alone, so a body of a class another
// unit defines, declared here without its signature, is entered at it too.
struct EntryPrototypes {
  mir::TypeId on_object;
  mir::TypeId on_object_and_problem;
  // The body answering the instance an object keeps for its class (LRM 6.22)
  // is handed the object and answers the instance.
  mir::TypeId instance_of_object;

  [[nodiscard]] auto Of(support::LibraryCallback callback) const
      -> mir::TypeId {
    switch (callback) {
      case support::LibraryCallback::kClassRandVariables:
      case support::LibraryCallback::kClassConstrain:
      case support::LibraryCallback::kClassRandObjects:
        return on_object_and_problem;
      case support::LibraryCallback::kScopeResolve:
      case support::LibraryCallback::kScopeInitialize:
      case support::LibraryCallback::kScopeCreateProcesses:
      case support::LibraryCallback::kScopeDescribe:
      case support::LibraryCallback::kClassPreRandomize:
      case support::LibraryCallback::kClassPostRandomize:
        return on_object;
    }
    throw InternalError("hir_to_mir: unknown library callback");
  }
};

// Adds to `bodies` the body answering each callback `answers` names, where one
// does, with the prototype of its entry.
template <typename Answers>
void AddBodies(
    LibraryBodies& bodies, const mir::CompilationUnit& unit, mir::ClassId id,
    const Answers& answers, const EntryPrototypes& prototypes) {
  for (const auto& [_, callback] : answers) {
    bodies.emplace(
        callback, mir::CallbackBodyOf(unit, id, callback)
                      .transform([&](const mir::CallableTarget& body) {
                        return LibraryBody{
                            .body = body, .prototype = prototypes.Of(callback)};
                      }));
  }
}

// What an object of `id`, a class another unit defines, dispatches on: its
// behaviors, answered by that unit.
auto DeclaredDispatchOf(const mir::CompilationUnit& unit, mir::ClassId id)
    -> std::vector<mir::DispatchedBehavior> {
  std::vector<mir::DispatchedBehavior> dispatch;
  for (const mir::VirtualSlot& behavior : mir::DispatchTableOf(unit, id)) {
    dispatch.push_back(
        mir::DispatchedBehavior{
            .behavior = behavior, .answered_by = std::nullopt});
  }
  return dispatch;
}

auto FactsOf(
    const mir::CompilationUnit& unit, mir::ClassId id,
    const EntryPrototypes& prototypes) -> LineageFacts {
  const mir::Class& cls = unit.GetClass(id);
  LineageFacts facts{
      .is_interface = cls.is_interface,
      .is_class_object = false,
      .bodies = {},
      .dispatch = {},
      .interface_tables = {}};
  if (cls.is_interface) {
    return facts;
  }
  const std::optional<mir::ClassRef> root = RootOf(unit, id);
  facts.is_class_object =
      root.has_value() &&
      std::holds_alternative<mir::ClassObjectRootRef>(*root);
  // A class whose instances are scopes of the hierarchy runs the phases on
  // them; what a unit published of its object has no instance of its own.
  if (cls.scope_declarations.has_value()) {
    AddBodies(facts.bodies, unit, id, kScopePhases, prototypes);
  }
  if (facts.is_class_object) {
    AddBodies(facts.bodies, unit, id, kRandomizationOnProblem, prototypes);
    AddBodies(facts.bodies, unit, id, kRandomizationHooks, prototypes);
  }
  for (const mir::VirtualSlot& behavior : mir::DispatchTableOf(unit, id)) {
    facts.dispatch.push_back(
        mir::DispatchedBehavior{
            .behavior = behavior,
            .answered_by = mir::DispatchedBody(unit, id, behavior)});
  }
  for (const mir::ClassId iface : mir::InterfacesOf(unit, id)) {
    mir::InterfaceTable table{.of = iface, .answered_by = {}};
    for (const mir::VirtualSlot& requirement :
         mir::RequirementsOf(unit, iface)) {
      const auto answer = std::ranges::find_if(
          cls.conforming, [&](const mir::ConformingBehavior& conforming) {
            return conforming.interface_behavior == requirement;
          });
      if (answer == cls.conforming.end()) {
        throw InternalError(
            "hir_to_mir: a class states nothing about a method of an interface "
            "class it implements -- please report this as a bug");
      }
      table.answered_by.push_back(
          answer->answered_by.and_then([&](const mir::VirtualSlot& answering) {
            return mir::DispatchedBody(unit, id, answering);
          }));
    }
    facts.interface_tables.push_back(std::move(table));
  }
  return facts;
}

// Gives each body of `cls`, class `id`, that something reaches without knowing
// the object's class its entry: every one taking part in dispatch, answering
// a callback the library makes on an object, or answering the instance the
// object keeps, that the class defines. One answering a callback is entered at
// that callback's prototype, the one answering the instance at its own, and one
// taking part in dispatch at its own, handed the object as the root.
void AddEntries(
    const mir::CompilationUnit& unit, mir::ClassId id, mir::Class& cls,
    const EntryPrototypes& prototypes) {
  const mir::TypeId root = mir::ObjectRootPointer(unit.types);
  std::vector<mir::CallableId> entered;
  for (const mir::CallableId callable : cls.callables.Ids()) {
    const mir::CallableDecl& decl = cls.callables.Get(callable);
    const bool reached_by_definition =
        decl.virtual_dispatch.has_value() ||
        cls.CallbackAnsweredBy(callable).has_value() ||
        cls.declaring_instance == callable;
    if (reached_by_definition &&
        std::holds_alternative<mir::DefinedHere>(mir::FormOf(decl))) {
      entered.push_back(callable);
    }
  }
  for (const mir::CallableId method : entered) {
    const std::optional<support::LibraryCallback> callback =
        cls.CallbackAnsweredBy(method);
    const mir::TypeId prototype = [&] {
      if (callback.has_value()) return prototypes.Of(*callback);
      if (cls.declaring_instance == method) {
        return prototypes.instance_of_object;
      }
      return EntryPrototypeOf(unit, cls.callables.Get(method).code, root);
    }();
    cls.entries.push_back(
        mir::MethodEntry{
            .method = method,
            .entry = EntryOf(unit, id, cls, method, prototype)});
  }
}

// What `randomize()` asks of an object of a class, as a constant of the unit,
// and its address.
auto ClassObjectInfoAddress(
    const mir::CompilationUnit& unit, const LibraryBodies& bodies)
    -> mir::ConstantValue {
  using abi::ClassObjectInfo;
  mir::LibraryRecordBuilder<ClassObjectInfo> info(unit);
  SetEntries(info, kRandomizationOnProblem, bodies);
  SetEntries(info, kRandomizationHooks, bodies);
  return AddressOf(unit, std::move(info).Intern());
}

// The address of the definition of `of`, a class of this unit or another.
auto DefinitionAddress(const mir::CompilationUnit& unit, mir::ClassId of)
    -> mir::ConstantValue {
  return PointerTo(
      mir::DefinitionRef{.of = of},
      mir::LibraryTypeOf(unit.types, support::LibraryDecl::kObjectDefinition));
}

// The definition of class `id`, `cls`, from what its lineage decides.
//
//   {&Base::definition, &info, &randomization, &end, ConstantSpan(&rows),
//    &methods, &declaring_instance_entry, ConstantSpan(&implemented)}
auto DefinitionOf(
    const mir::CompilationUnit& unit, mir::ClassId id, const mir::Class& cls,
    const LineageFacts& facts) -> mir::ConstantValue {
  using abi::InterfaceReach;
  using abi::InterfaceRow;
  using abi::ObjectDefinition;
  mir::LibraryRecordBuilder<ObjectDefinition> definition(unit);
  const std::optional<mir::ClassId> base = mir::DeclaredBase(cls);
  definition.Set(
      &ObjectDefinition::base,
      base.has_value() ? DefinitionAddress(unit, *base) : Null());
  definition.Set(
      &ObjectDefinition::scope,
      cls.scope_declarations.has_value()
          ? ScopeInfoAddress(unit, id, cls, facts.bodies)
          : Null());
  definition.Set(
      &ObjectDefinition::class_object,
      facts.is_class_object ? ClassObjectInfoAddress(unit, facts.bodies)
                            : Null());
  definition.Set(
      &ObjectDefinition::end,
      facts.is_interface ? Null()
                         : PointerTo(
                               mir::DestructorRef{.of = id},
                               definition.TypeOf(&ObjectDefinition::end)));
  definition.Set(
      &ObjectDefinition::interfaces,
      Table<InterfaceRow>(
          unit, facts.interface_tables.size(), [&](std::size_t i) {
            const mir::ClassId iface = facts.interface_tables[i].of;
            mir::LibraryRecordBuilder<InterfaceRow> row(unit);
            row.Set(&InterfaceRow::of, DefinitionAddress(unit, iface));
            row.Set(
                &InterfaceRow::table,
                PointerTo(
                    mir::InterfaceTableRef{.of = id, .iface = iface},
                    row.TypeOf(&InterfaceRow::table)));
            return std::move(row).Build();
          }));
  definition.Set(
      &ObjectDefinition::methods,
      facts.is_interface ? Null()
                         : PointerTo(
                               mir::DispatchTableRef{.of = id},
                               definition.TypeOf(&ObjectDefinition::methods)));
  definition.Set(
      &ObjectDefinition::declaring_instance,
      cls.declaring_instance.has_value()
          ? PointerTo(
                mir::DispatchEntryRef{
                    .body =
                        mir::CallableTarget{
                            .owner = id, .slot = *cls.declaring_instance}},
                definition.TypeOf(&ObjectDefinition::declaring_instance))
          : Null());
  definition.Set(
      &ObjectDefinition::interface_reaches,
      Table<InterfaceReach>(
          unit, cls.interface_reaches.size(), [&](std::size_t i) {
            const mir::InterfaceReach& named = cls.interface_reaches[i];
            mir::LibraryRecordBuilder<InterfaceReach> entry(unit);
            entry.Set(&InterfaceReach::of, DefinitionAddress(unit, named.of));
            entry.Set(
                &InterfaceReach::reach, Function(unit, id, cls, named.reach));
            return std::move(entry).Build();
          }));
  return std::move(definition).Build();
}

}  // namespace

void StateObjectDefinitions(mir::CompilationUnit& unit) {
  const std::optional<mir::TypeId> problem =
      ConstraintProblemParamTypeOf(unit, hir::SubroutineKind::kConstraintBlock);
  if (!problem.has_value()) {
    throw InternalError(
        "hir_to_mir: a constraint block takes no problem -- please report this "
        "as a bug");
  }
  // A body handed only the object is entered as the library enters a phase,
  // and one handed the call's problem too takes it as every body answering
  // such a callback declares it.
  const EntryPrototypes prototypes{
      .on_object = mir::LibraryRecordMemberType(unit, &abi::ScopeInfo::resolve),
      .on_object_and_problem = unit.types.Intern(
          mir::Type{mir::MachineFunctionType{
              .params = {mir::ObjectRootPointer(unit.types), *problem},
              .result = unit.builtins.void_type}}),
      .instance_of_object = mir::LibraryRecordMemberType(
          unit, &abi::ObjectDefinition::declaring_instance)};
  std::vector<std::optional<LineageFacts>> facts;
  std::vector<std::vector<mir::DispatchedBehavior>> declared;
  facts.reserve(unit.classes.size());
  declared.reserve(unit.classes.size());
  for (const mir::ClassId id : unit.classes.Ids()) {
    const bool elsewhere = mir::IsDefinedElsewhere(unit, id);
    facts.push_back(
        elsewhere ? std::nullopt
                  : std::optional<LineageFacts>{FactsOf(unit, id, prototypes)});
    declared.push_back(
        elsewhere && !unit.GetClass(id).is_interface
            ? DeclaredDispatchOf(unit, id)
            : std::vector<mir::DispatchedBehavior>{});
  }
  unit.classes = std::move(unit.classes)
                     .TransformValues([&](mir::ClassId id, mir::Class cls) {
                       const std::optional<LineageFacts>& stated =
                           facts[id.value];
                       if (!stated.has_value()) {
                         cls.dispatch = std::move(declared[id.value]);
                         return cls;
                       }
                       AddEntries(unit, id, cls, prototypes);
                       cls.dispatch = stated->dispatch;
                       cls.interface_tables = stated->interface_tables;
                       cls.definition = DefinitionOf(unit, id, cls, *stated);
                       return cls;
                     });
}

auto BlockEntryType(const mir::CompilationUnit& unit, bool handed_its_index)
    -> mir::TypeId {
  std::vector<mir::TypeId> params{
      unit.builtins.scope_ptr, unit.builtins.hierarchy_segment,
      mir::ConstructionTablePointer(unit.types)};
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
    const mir::CompilationUnit& unit, const hir::LoweringRef& root,
    const DesignInstantiations& instantiations,
    const std::map<std::string, mir::TypeId, std::less<>>& entry_types)
    -> mir::ConstantId {
  return ConstructionTables(unit, instantiations, entry_types).Of(root);
}

}  // namespace lyra::lowering::hir_to_mir
