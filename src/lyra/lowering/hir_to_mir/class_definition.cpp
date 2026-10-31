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

#include "lyra/abi/records.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/hir/owned_child_step.hpp"
#include "lyra/hir/scope_instantiations.hpp"
#include "lyra/hir/specialization_ref.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/design_instantiations.hpp"
#include "lyra/lowering/hir_to_mir/library_entry_body.hpp"
#include "lyra/mir/block_body_ordinal.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/dispatch_table.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/library_records.hpp"
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
// stated as the expression that is its value: a structure the library
// declares, composed from a part for each member it declares. For a class
// `Mid` an instance of the design hierarchy is built of, exporting one task as
// `bump` (LRM 35.5):
//
//   const array<ScopeCallable, 1> Mid::exports =
//       {{{"bump", &Mid::bump_entry}}};
//   const ScopeInfo Mid::info =
//       {{unit, precision, ...}, ConstantSpan(&exports), ...};
//   const array<InterfaceRow, 0> Mid::rows = {};
//   const ObjectDefinition Mid::definition =
//       {nullptr, &info, nullptr, &Mid::end, ConstantSpan(&rows),
//        &Mid::methods};
//
// A table is an array the class holds, and whatever points at one holds a
// view of all of it, made from the array's address.

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

auto Null(mir::Block& block, mir::TypeId type) -> mir::ExprId {
  return block.exprs.Add(mir::Expr{.data = mir::NullLiteral{}, .type = type});
}

auto Name(
    const mir::CompilationUnit& unit, mir::Block& block, std::string_view text)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::MakeStringLiteral(
          unit.builtins.machine_c_string, std::string{text}));
}

// The types the library declares the members of `Record` at, in its order.
template <typename Record>
auto MembersOf(const mir::CompilationUnit& unit)
    -> std::span<const mir::TypeId> {
  const std::optional<std::span<const mir::TypeId>> members =
      unit.LibraryRecordMembers(mir::LibraryRecordKind<Record>());
  if (!members.has_value()) {
    throw InternalError(
        "hir_to_mir: a unit declares no structure of the library a constant "
        "is stated as -- please report this as a bug");
  }
  return *members;
}

// The type the library declares `member` at.
template <typename Record, typename Member>
auto MemberTypeOf(const mir::CompilationUnit& unit, Member Record::* member)
    -> mir::TypeId {
  return MembersOf<Record>(unit)[abi::PositionOf(member)];
}

// A structure of the library composed from a part for each of its members,
// each part named by the member it fills, so it stands where the library
// declares that member and is built at the type the library declares it at.
//
//   LibraryRecordBuilder<abi::ScopeCallable> entry(unit, block);
//   entry.Set(&abi::ScopeCallable::name, name);
//   entry.Set(&abi::ScopeCallable::entry, body);
//   std::move(entry).Build()  // {name, body}
template <typename Record>
class LibraryRecordBuilder {
 public:
  static constexpr mir::RuntimeLibraryKind kKind =
      mir::LibraryRecordKind<Record>();

  LibraryRecordBuilder(const mir::CompilationUnit& unit, mir::Block& block)
      : unit_(&unit), block_(&block) {
    parts_.resize(MembersOf<Record>(unit).size());
  }

  // The type the library declares `member` at.
  template <typename Member>
  [[nodiscard]] auto TypeOf(Member Record::* member) const -> mir::TypeId {
    return MemberTypeOf(*unit_, member);
  }

  template <typename Member>
  void Set(Member Record::* member, mir::ExprId part) {
    std::optional<mir::ExprId>& at = parts_[abi::PositionOf(member)];
    if (at.has_value()) {
      throw InternalError(
          "hir_to_mir: a member of a structure of the library is given "
          "twice -- please report this as a bug");
    }
    at = part;
  }

  // `member` holding the number `value`, at the type of the member.
  template <typename Member>
  void SetNumber(Member Record::* member, std::int64_t value) {
    Set(member, block_->exprs.Add(
                    mir::Expr{
                        .data = mir::MachineIntLiteral{.value = value},
                        .type = TypeOf(member)}));
  }

  // `member` holding nothing.
  template <typename Member>
  void SetNull(Member Record::* member) {
    Set(member, Null(*block_, TypeOf(member)));
  }

  // The structure, once every member is given.
  [[nodiscard]] auto Build() && -> mir::ExprId {
    std::vector<mir::ExprId> parts;
    parts.reserve(parts_.size());
    for (const std::optional<mir::ExprId>& part : parts_) {
      if (!part.has_value()) {
        throw InternalError(
            "hir_to_mir: a structure of the library is composed with a "
            "member left out -- please report this as a bug");
      }
      parts.push_back(*part);
    }
    return block_->exprs.Add(
        mir::Expr{
            .data = mir::CompositeExpr{.parts = std::move(parts)},
            .type = LibraryType(*unit_, kKind)});
  }

 private:
  const mir::CompilationUnit* unit_;
  mir::Block* block_;
  std::vector<std::optional<mir::ExprId>> parts_;
};

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
          .type = mir::FunctionTypeOf(
              unit.types, cls.callables.Get(callable).code)});
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

// Holds `build` as a constant of class `id`, at the type of the value it is
// built as, and answers its address as a value of `block`.
auto ConstantAddress(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ClassId id,
    mir::Class& cls, mir::ValueBuild build) -> mir::ExprId {
  const mir::TypeId type = build.body.exprs.Get(build.value).type;
  const mir::ClassConstantId constant = cls.constants.Add(
      mir::ClassConstantDecl{.type = type, .initializer = std::move(build)});
  return AddressOfConstant(
      unit, block, mir::ClassConstantRef{.owner = id, .constant = constant},
      type);
}

// A table of `count` entries of the library's structure `Entry`, each written
// by `write` into the block it is handed: the entries are held as an array
// constant of class `id`, which owns them, and the table, a value of `block`,
// is a view of all of that array, made from its address.
template <typename Entry>
auto Table(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ClassId id,
    mir::Class& cls, std::size_t count,
    const std::function<mir::ExprId(mir::Block&, std::size_t)>& write)
    -> mir::ExprId {
  const mir::TypeId entry = LibraryType(unit, mir::LibraryRecordKind<Entry>());
  mir::ValueBuild build;
  std::vector<mir::ExprId> elements;
  elements.reserve(count);
  for (std::size_t i = 0; i < count; ++i) {
    elements.push_back(write(build.body, i));
  }
  build.value = build.body.exprs.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(elements)},
          .type = mir::MachineArrayOf(unit.types, entry, count)});
  const mir::ExprId entries =
      ConstantAddress(unit, block, id, cls, std::move(build));
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{.callee = mir::Construct{}, .arguments = {entries}},
          .type = unit.types.Intern(
              mir::Type{mir::MachineSpanType{.element = entry}})});
}

// A body answering a callback the library makes on an object, and the
// prototype of the entry the library enters it through, which hands it the
// object as the library does.
struct LibraryBody {
  mir::CallableTarget body;
  mir::TypeId prototype;
};

// The entry the library enters for `body`, held at `member_type`, the
// prototype the library declares the member holding it at, as a value of
// `block`; none where no body answers. The entry is named at its own prototype
// and read at the library's.
auto EntryOrNone(
    mir::Block& block, const std::optional<LibraryBody>& body,
    mir::TypeId member_type) -> mir::ExprId {
  if (!body.has_value()) {
    return Null(block, member_type);
  }
  const mir::ExprId named = block.exprs.Add(
      mir::Expr{
          .data =
              mir::ReferenceExpr{
                  .target = mir::DispatchEntryRef{.body = body->body}},
          .type = body->prototype});
  return block.exprs.Add(
      mir::Expr{.data = mir::CastExpr{.operand = named}, .type = member_type});
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
    mir::Block& block, LibraryRecordBuilder<Record>& record,
    const Answers& answers, const LibraryBodies& bodies) {
  for (const auto& [member, callback] : answers) {
    record.Set(
        member,
        EntryOrNone(block, BodyFor(bodies, callback), record.TypeOf(member)));
  }
}

// What a class of the design hierarchy states of its instances, as a constant
// of class `id`, and its address as a value of `block`: its timescale and how
// what an instance starts is run, what a dump records it as, the bodies its
// exports name, then the body of each of its phases.
auto ScopeInfoAddress(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ClassId id,
    mir::Class& cls, const LibraryBodies& bodies) -> mir::ExprId {
  using abi::ScopeInfo;
  using abi::ScopeMetadata;
  mir::ValueBuild build;
  mir::Block& at = build.body;
  LibraryRecordBuilder<ScopeMetadata> metadata(unit, at);
  metadata.SetNumber(
      &ScopeMetadata::time_unit_power, cls.time_resolution.unit_power);
  metadata.SetNumber(
      &ScopeMetadata::time_precision_power,
      cls.time_resolution.precision_power);
  metadata.SetNumber(
      &ScopeMetadata::execution, static_cast<std::uint8_t>(cls.execution));
  metadata.SetNumber(
      &ScopeMetadata::dumped_as, static_cast<std::uint8_t>(cls.dumped_as));
  LibraryRecordBuilder<ScopeInfo> info(unit, at);
  info.Set(&ScopeInfo::metadata, std::move(metadata).Build());
  info.Set(
      &ScopeInfo::exports,
      Table<abi::ScopeCallable>(
          unit, at, id, cls, cls.exports.size(),
          [&](mir::Block& entry, std::size_t i) {
            using abi::ScopeCallable;
            const mir::NamedCallable& exported = cls.exports[i];
            LibraryRecordBuilder<ScopeCallable> callable(unit, entry);
            callable.Set(
                &ScopeCallable::name, Name(unit, entry, exported.name));
            callable.Set(
                &ScopeCallable::entry,
                entry.exprs.Add(
                    mir::Expr{
                        .data =
                            mir::CastExpr{
                                .operand = Function(
                                    unit, entry, id, cls, exported.body)},
                        .type = callable.TypeOf(&ScopeCallable::entry)}));
            return std::move(callable).Build();
          }));
  SetEntries(at, info, kScopePhases, bodies);
  build.value = std::move(info).Build();
  return ConstantAddress(unit, block, id, cls, std::move(build));
}

}  // namespace

//   {"count", 7, 0, 8, kReg, 1, 1, kInput, 2}
auto DumpEntryAddress(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ClassId id,
    mir::Class& cls, const hir::DumpedDeclaration& declaration) -> mir::ExprId {
  using abi::DumpEntry;
  mir::ValueBuild build;
  const hir::DumpedDeclaration::Range range = declaration.range.value_or(
      hir::DumpedDeclaration::Range{.left = 0, .right = 0});
  const hir::DumpedDeclaration::Port port = declaration.port.value_or(
      hir::DumpedDeclaration::Port{
          .direction = support::DumpPortDirection::kInput, .position = 0});
  LibraryRecordBuilder<DumpEntry> entry(unit, build.body);
  entry.Set(
      &DumpEntry::identifier, Name(unit, build.body, declaration.identifier));
  entry.SetNumber(&DumpEntry::left, range.left);
  entry.SetNumber(&DumpEntry::right, range.right);
  entry.SetNumber(&DumpEntry::width, declaration.width);
  entry.SetNumber(
      &DumpEntry::kind, static_cast<std::uint8_t>(declaration.kind));
  entry.SetNumber(&DumpEntry::has_range, declaration.range.has_value() ? 1 : 0);
  entry.SetNumber(&DumpEntry::has_port, declaration.port.has_value() ? 1 : 0);
  entry.SetNumber(
      &DumpEntry::port_direction, static_cast<std::uint8_t>(port.direction));
  entry.SetNumber(&DumpEntry::port_position, port.position);
  build.value = std::move(entry).Build();
  return ConstantAddress(unit, block, id, cls, std::move(build));
}

//   {"blk", kBegin}
auto DumpBlockAddress(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ClassId id,
    mir::Class& cls, const hir::DumpedBlock& dumped) -> mir::ExprId {
  using abi::DumpBlock;
  mir::ValueBuild build;
  LibraryRecordBuilder<DumpBlock> entry(unit, build.body);
  entry.Set(&DumpBlock::identifier, Name(unit, build.body, dumped.identifier));
  entry.SetNumber(&DumpBlock::kind, static_cast<std::uint8_t>(dumped.kind));
  build.value = std::move(entry).Build();
  return ConstantAddress(unit, block, id, cls, std::move(build));
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
// block body `built`, at the type `entry` its unit declared it with, read at
// `erased`, the prototype a table holds every entry at, as a value of `block`.
//
//   (Erased)&Child::sv_create
auto ErasedObjectEntry(
    mir::Block& block, const BodyRef& made,
    std::optional<mir::BlockBodyOrdinal> built, mir::TypeId entry,
    mir::TypeId erased) -> mir::ExprId {
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
      mir::Expr{.data = mir::CastExpr{.operand = declared}, .type = erased});
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
//   const GeneratePlaces generates[2] = {{ConstantSpan(&g_places)},
//                                        {ConstantSpan(&c_places)}};
//   const ConstructionTable top = {ConstantSpan(&entries),
//                                  ConstantSpan(&generates)};
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
    for (const auto& [_, made] : instantiations.by_specialization) {
      Gather(made.body, std::nullopt, made.made);
    }
  }

  // The table of the specialization `made`, which is stated once.
  auto Of(const hir::SpecializationRef& made) -> mir::ClassConstantRef {
    if (const auto stated = of_specialization_.find(made.specialization);
        stated != of_specialization_.end()) {
      return stated->second;
    }
    const SpecializationMade& found = Made(made);
    const mir::ClassConstantRef table =
        Of(BodyRef{.unit = made.unit, .body = found.body}, std::nullopt,
           found.made);
    of_specialization_.emplace(made.specialization, table);
    return table;
  }

 private:
  // What one entry of a table makes: an object of the body `made`, or a block
  // of its block body `block`, built with the table `table`.
  struct Entry {
    BodyRef made;
    std::optional<mir::BlockBodyOrdinal> block;
    mir::ClassConstantRef table;

    auto operator==(const Entry&) const -> bool = default;
  };

  // The places of one generate in a table: whether the blocks are handed the
  // index their loop stands at, and the entry at each place, absent where no
  // block stands.
  struct Places {
    bool handed_an_index = false;
    std::vector<std::optional<Entry>> places;

    auto operator==(const Places&) const -> bool = default;
  };

  // What a table holds: an entry per object of each instantiation, then the
  // places of each generate.
  struct Content {
    std::vector<Entry> instances;
    std::vector<Places> generates;

    auto operator==(const Content&) const -> bool = default;
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
  // instantiation it declares, how the specialization made there is made,
  // then at each place of each of its generates, how the block built there is
  // made. Scopes whose tables hold the same entries are made with one table
  // -- the blocks of a loop handed different values that build alike, above
  // all -- so the tables follow what objects differ in rather than how many
  // there are. A table named by another is stated before it.
  auto Of(
      const BodyRef& owner, std::optional<hir::BlockBodyId> scope,
      const hir::ScopeInstantiations& layout) -> mir::ClassConstantRef {
    Content content;
    for (const hir::SpecializationRef& made : layout.Instantiated()) {
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
    for (const auto& [held, table] : stated_) {
      if (held == content) return table;
    }
    const mir::ClassConstantRef table = State(content);
    stated_.emplace_back(std::move(content), table);
    return table;
  }

  // States a table holding `content`.
  auto State(const Content& content) -> mir::ClassConstantRef {
    using abi::ConstructionTable;
    using abi::GeneratePlaces;
    using abi::Instantiation;
    mir::ValueBuild build;
    LibraryRecordBuilder<ConstructionTable> table(*unit_, build.body);
    table.Set(
        &ConstructionTable::instantiations,
        Table<Instantiation>(
            *unit_, build.body, id_, *cls_, content.instances.size(),
            [&](mir::Block& entry, std::size_t i) {
              const Entry& made = content.instances[i];
              return EntryRecord(entry, made, EntryTypeOf(made.made.unit));
            }));
    table.Set(
        &ConstructionTable::generates,
        Table<GeneratePlaces>(
            *unit_, build.body, id_, *cls_, content.generates.size(),
            [&](mir::Block& at_generate, std::size_t g) {
              const Places& generate = content.generates[g];
              const mir::TypeId entry_type =
                  BlockEntryType(*unit_, generate.handed_an_index);
              LibraryRecordBuilder<GeneratePlaces> places(*unit_, at_generate);
              places.Set(
                  &GeneratePlaces::places,
                  Table<Instantiation>(
                      *unit_, at_generate, id_, *cls_, generate.places.size(),
                      [&](mir::Block& entry, std::size_t k) {
                        const std::optional<Entry>& block = generate.places[k];
                        return block.has_value()
                                   ? EntryRecord(entry, *block, entry_type)
                                   : NoEntryRecord(entry);
                      }));
              return std::move(places).Build();
            }));
    build.value = std::move(table).Build();
    return mir::ClassConstantRef{
        .owner = id_,
        .constant = cls_->constants.Add(
            mir::ClassConstantDecl{
                .type = mir::ConstructionTableType(unit_->types),
                .initializer = std::move(build)})};
  }

  // The record of the entry `made`, its entry read at the type `entry_type`
  // that entry has.
  auto EntryRecord(mir::Block& entry, const Entry& made, mir::TypeId entry_type)
      -> mir::ExprId {
    using abi::Instantiation;
    LibraryRecordBuilder<Instantiation> record(*unit_, entry);
    record.Set(
        &Instantiation::entry, ErasedObjectEntry(
                                   entry, made.made, made.block, entry_type,
                                   record.TypeOf(&Instantiation::entry)));
    record.Set(
        &Instantiation::table, AddressOfConstant(
                                   *unit_, entry, made.table,
                                   mir::ConstructionTableType(unit_->types)));
    return std::move(record).Build();
  }

  // A place this object built no block at -- an alternative it did not select
  // -- that no one entry fills, which holds no entry; the body's code never
  // reads one there.
  auto NoEntryRecord(mir::Block& entry) -> mir::ExprId {
    using abi::Instantiation;
    LibraryRecordBuilder<Instantiation> record(*unit_, entry);
    record.SetNull(&Instantiation::entry);
    record.SetNull(&Instantiation::table);
    return std::move(record).Build();
  }

  // What the specialization `made` builds, as its lowering recorded it.
  [[nodiscard]] auto Made(const hir::SpecializationRef& made) const
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

  mir::CompilationUnit* unit_;
  mir::ClassId id_;
  mir::Class* cls_;
  const DesignInstantiations* instantiations_;
  const std::map<std::string, mir::TypeId, std::less<>>* entry_types_;
  std::map<std::string, mir::ClassConstantRef, std::less<>> of_specialization_;
  // Each table stated, with what it holds.
  std::vector<std::pair<Content, mir::ClassConstantRef>> stated_;
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
  bool is_interface_class = false;
  bool is_class_object = false;
  // Whether instances of the class stand in the design hierarchy and are built
  // of it, which a body for its first phase says.
  bool stands_in_hierarchy = false;
  // The body answering each callback the library makes on an object of the
  // class.
  LibraryBodies bodies;
  std::vector<mir::DispatchedBehavior> dispatch;
  std::vector<mir::InterfaceTable> interface_tables;
};

// The root a class's lineage ends at, where it ends at one: an interface class
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

// The prototype every body answering a library callback is entered at: handed
// the object as the root every object is, then what every body answering that
// callback takes after its receiver -- the call's problem, for what
// `randomize()` asks of the variables, the constraints and the random objects
// (LRM 18.6) -- answering nothing. It follows from the callback alone, so a
// body of a class another unit defines, declared here without its signature,
// is entered at it too.
struct CallbackPrototypes {
  mir::TypeId on_object;
  mir::TypeId on_object_and_problem;

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
    const Answers& answers, const CallbackPrototypes& prototypes) {
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
    const CallbackPrototypes& prototypes) -> LineageFacts {
  const mir::Class& cls = unit.GetClass(id);
  LineageFacts facts{
      .is_interface_class = cls.is_interface_class,
      .is_class_object = false,
      .stands_in_hierarchy = false,
      .bodies = {},
      .dispatch = {},
      .interface_tables = {}};
  if (cls.is_interface_class) {
    return facts;
  }
  const std::optional<mir::ClassRef> root = RootOf(unit, id);
  facts.is_class_object =
      root.has_value() &&
      std::holds_alternative<mir::ManagedObjectRootRef>(*root);
  // Only a class an instance is built of gives its phases a body; what a unit
  // published of its object is built of no instance alone.
  facts.stands_in_hierarchy =
      mir::CallbackBodyOf(unit, id, kScopePhases.front().second).has_value();
  if (facts.stands_in_hierarchy) {
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

// The address of a table `id`'s dispatch is laid out in, as a value of
// `block` of type `type`: what `target` names.
auto TableAddress(
    mir::Block& block, mir::ReferenceTarget target, mir::TypeId type)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data = mir::ReferenceExpr{.target = std::move(target)},
          .type = type});
}

// Gives each body of `cls`, class `id`, that something reaches without knowing
// the object's class its entry: every one taking part in dispatch or answering
// a callback the library makes on an object, that the class defines. One
// answering a callback is entered at that callback's prototype, and one taking
// part in dispatch at its own, handed the object as the root.
void AddEntries(
    const mir::CompilationUnit& unit, mir::ClassId id, mir::Class& cls,
    const CallbackPrototypes& prototypes) {
  const mir::TypeId root = mir::ObjectRootPointer(unit.types);
  std::vector<mir::CallableId> entered;
  for (const mir::CallableId callable : cls.callables.Ids()) {
    const mir::CallableDecl& decl = cls.callables.Get(callable);
    const bool reached_by_definition =
        decl.virtual_dispatch.has_value() || cls.CallbackAnsweredBy(callable);
    if (reached_by_definition &&
        std::holds_alternative<mir::DefinedHere>(mir::FormOf(decl))) {
      entered.push_back(callable);
    }
  }
  for (const mir::CallableId method : entered) {
    const std::optional<support::LibraryCallback> callback =
        cls.CallbackAnsweredBy(method);
    const mir::TypeId prototype =
        callback.has_value()
            ? prototypes.Of(*callback)
            : EntryPrototypeOf(unit, cls.callables.Get(method).code, root);
    cls.entries.push_back(
        mir::MethodEntry{
            .method = method,
            .entry = EntryOf(unit, id, cls, method, prototype)});
  }
}

// What `randomize()` asks of an object of class `id`, as a constant of the
// class, and its address as a value of `block`.
auto ClassObjectInfoAddress(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ClassId id,
    mir::Class& cls, const LibraryBodies& bodies) -> mir::ExprId {
  mir::ValueBuild build;
  LibraryRecordBuilder<abi::ClassObjectInfo> info(unit, build.body);
  SetEntries(build.body, info, kRandomizationOnProblem, bodies);
  SetEntries(build.body, info, kRandomizationHooks, bodies);
  build.value = std::move(info).Build();
  return ConstantAddress(unit, block, id, cls, std::move(build));
}

// The definition of class `id`, `cls`, from what its lineage decides, adding
// to `cls` the constants it takes.
//
//   {&Base::definition, &info, &randomization, &end, ConstantSpan(&rows),
//    &methods}
auto DefinitionOf(
    const mir::CompilationUnit& unit, mir::ClassId id, mir::Class& cls,
    const LineageFacts& facts) -> mir::ValueBuild {
  using abi::InterfaceRow;
  using abi::ObjectDefinition;
  mir::ValueBuild build;
  mir::Block& block = build.body;
  LibraryRecordBuilder<ObjectDefinition> definition(unit, block);
  if (const std::optional<mir::ClassId> base = mir::DeclaredBase(cls)) {
    definition.Set(
        &ObjectDefinition::base, BuildDefinitionRead(unit, block, *base));
  } else {
    definition.SetNull(&ObjectDefinition::base);
  }
  if (facts.stands_in_hierarchy) {
    definition.Set(
        &ObjectDefinition::scope,
        ScopeInfoAddress(unit, block, id, cls, facts.bodies));
  } else {
    definition.SetNull(&ObjectDefinition::scope);
  }
  if (facts.is_class_object) {
    definition.Set(
        &ObjectDefinition::class_object,
        ClassObjectInfoAddress(unit, block, id, cls, facts.bodies));
  } else {
    definition.SetNull(&ObjectDefinition::class_object);
  }
  if (facts.is_interface_class) {
    definition.SetNull(&ObjectDefinition::end);
  } else {
    definition.Set(
        &ObjectDefinition::end,
        block.exprs.Add(
            mir::Expr{
                .data =
                    mir::ReferenceExpr{.target = mir::DestructorRef{.of = id}},
                .type = definition.TypeOf(&ObjectDefinition::end)}));
  }
  definition.Set(
      &ObjectDefinition::interfaces,
      Table<InterfaceRow>(
          unit, block, id, cls, facts.interface_tables.size(),
          [&](mir::Block& entry, std::size_t i) {
            const mir::ClassId iface = facts.interface_tables[i].of;
            LibraryRecordBuilder<InterfaceRow> row(unit, entry);
            row.Set(&InterfaceRow::of, BuildDefinitionRead(unit, entry, iface));
            row.Set(
                &InterfaceRow::table,
                TableAddress(
                    entry, mir::InterfaceTableRef{.of = id, .iface = iface},
                    row.TypeOf(&InterfaceRow::table)));
            return std::move(row).Build();
          }));
  if (facts.is_interface_class) {
    definition.SetNull(&ObjectDefinition::methods);
  } else {
    definition.Set(
        &ObjectDefinition::methods,
        TableAddress(
            block, mir::DispatchTableRef{.of = id},
            definition.TypeOf(&ObjectDefinition::methods)));
  }
  build.value = std::move(definition).Build();
  return build;
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
  const CallbackPrototypes prototypes{
      .on_object = MemberTypeOf(unit, &abi::ScopeInfo::resolve),
      .on_object_and_problem = unit.types.Intern(
          mir::Type{mir::MachineFunctionType{
              .params = {mir::ObjectRootPointer(unit.types), *problem},
              .result = unit.builtins.void_type}})};
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
        elsewhere && !unit.GetClass(id).is_interface_class
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
                       cls.object_definition_initializer =
                           DefinitionOf(unit, id, cls, *stated);
                       return cls;
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
