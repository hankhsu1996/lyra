#include "lyra/lowering/hir_to_mir/class_definition.hpp"

#include <algorithm>
#include <array>
#include <compare>
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
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/value_build.hpp"
#include "lyra/support/dump_kind.hpp"
#include "lyra/support/runtime_class.hpp"

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
//   const ObjectDefinition Mid::definition =
//       {nullptr, &info, nullptr, &Mid::end, nullptr, 0, &Mid::methods};
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

// A pointer to the root every object is: how an entry is handed the object,
// whatever class it is of.
auto ObjectRootPointer(const mir::CompilationUnit& unit) -> mir::TypeId {
  return unit.types.Intern(
      mir::Type{mir::PointerType{
          .pointee = unit.types.Intern(
              mir::Type{mir::RuntimeClassType{
                  .which = support::RuntimeClass::kObject}}),
          .ownership = mir::PointerOwnership::kBorrowed}});
}

// `code`, a body the library enters at the type `entry` its record states, as
// a value of `block`; none of that type where `code` is none.
auto EntryOrNone(
    mir::Block& block, std::optional<mir::ExprId> code, mir::TypeId entry)
    -> mir::ExprId {
  if (code.has_value()) {
    return *code;
  }
  return block.exprs.Add(mir::Expr{.data = mir::NullLiteral{}, .type = entry});
}

// The entry of `body`, the body the library enters for one of the callbacks
// it makes on an object, as a value of `block`. `entry` is the entry's
// machine function type, which is the library's for that callback.
auto EntryReference(
    mir::Block& block, const mir::CallableTarget& body, mir::TypeId entry)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::ReferenceExpr{.target = mir::DispatchEntryRef{.body = body}},
          .type = entry});
}

// The bodies an instance of a class of the design hierarchy is entered with in
// each phase, in the order the runtime library's scope record lists them.
constexpr std::array kScopePhases{
    support::LibraryCallback::kScopeResolve,
    support::LibraryCallback::kScopeInitialize,
    support::LibraryCallback::kScopeCreateProcesses,
    support::LibraryCallback::kScopeDescribe};

// What `randomize()` asks of an object of a source class, in the order the
// runtime library's record lists them.
constexpr std::array kRandomization{
    support::LibraryCallback::kClassRandVariables,
    support::LibraryCallback::kClassConstrain,
    support::LibraryCallback::kClassRandObjects,
    support::LibraryCallback::kClassPreRandomize,
    support::LibraryCallback::kClassPostRandomize};

using LibraryBodies = std::vector<std::optional<mir::CallableTarget>>;

// The machine function types of the entries the library enters an object's
// class through, each the type the library's record states: handed the
// object, and for what `randomize()` asks the variables, the constraints and
// the random objects of, the call's problem too (LRM 18.6); and the one ending
// an object, handed its storage.
struct LibraryEntryTypes {
  mir::TypeId on_object;
  mir::TypeId on_object_and_problem;
  mir::TypeId on_storage;
};

[[nodiscard]] auto EntryTypeOf(
    const LibraryEntryTypes& types, support::LibraryCallback callback)
    -> mir::TypeId {
  switch (callback) {
    case support::LibraryCallback::kClassRandVariables:
    case support::LibraryCallback::kClassConstrain:
    case support::LibraryCallback::kClassRandObjects:
      return types.on_object_and_problem;
    case support::LibraryCallback::kScopeResolve:
    case support::LibraryCallback::kScopeInitialize:
    case support::LibraryCallback::kScopeCreateProcesses:
    case support::LibraryCallback::kScopeDescribe:
    case support::LibraryCallback::kClassPreRandomize:
    case support::LibraryCallback::kClassPostRandomize:
      return types.on_object;
  }
  throw InternalError("hir_to_mir: unknown library callback");
}

// The entries of `bodies`, the bodies the library enters for `callbacks`
// in order, as values of `block`.
auto LibraryEntries(
    mir::Block& block, const LibraryEntryTypes& types,
    std::span<const support::LibraryCallback> callbacks,
    const LibraryBodies& bodies) -> std::vector<mir::ExprId> {
  std::vector<mir::ExprId> entries;
  entries.reserve(callbacks.size());
  for (std::size_t i = 0; i < callbacks.size(); ++i) {
    const mir::TypeId entry = EntryTypeOf(types, callbacks[i]);
    entries.push_back(EntryOrNone(
        block, bodies[i].transform([&](const mir::CallableTarget& body) {
          return EntryReference(block, body, entry);
        }),
        entry));
  }
  return entries;
}

// What a class of the design hierarchy states of its instances, as a constant
// of class `id`, and its address as a value of `block`: its timescale, how what
// an instance starts is run, what a dump records it as, the bodies its exports
// name, then the body of each of its phases.
//
//   {unit, precision, execution, dumped_as, exports, n, &resolve, ...}
auto ScopeInfoAddress(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ClassId id,
    mir::Class& cls, const LibraryEntryTypes& types,
    const LibraryBodies& phases) -> mir::ExprId {
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
      parts, unit, info, id, cls, Kind::kScopeCallable, cls.exports);
  for (const mir::ExprId phase :
       LibraryEntries(info, types, kScopePhases, phases)) {
    parts.push_back(phase);
  }
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

  auto operator==(const BodyRef&) const -> bool = default;
};

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
    using Kind = mir::RuntimeLibraryKind;
    mir::ValueBuild build;
    mir::Block& table = build.body;
    std::vector<mir::ExprId> parts;
    AppendTable(
        parts, *unit_, table, id_, *cls_,
        LibraryType(*unit_, Kind::kInstantiation), content.instances.size(),
        [&](mir::Block& entry, std::size_t i) {
          const Entry& made = content.instances[i];
          return EntryRecord(entry, made, EntryTypeOf(made.made.unit));
        });
    AppendTable(
        parts, *unit_, table, id_, *cls_,
        LibraryType(*unit_, Kind::kGeneratePlaces), content.generates.size(),
        [&](mir::Block& at_generate, std::size_t g) {
          const Places& generate = content.generates[g];
          const mir::TypeId entry_type =
              BlockEntryType(*unit_, generate.handed_an_index);
          std::vector<mir::ExprId> places;
          AppendTable(
              places, *unit_, at_generate, id_, *cls_,
              LibraryType(*unit_, Kind::kInstantiation), generate.places.size(),
              [&](mir::Block& entry, std::size_t k) {
                const std::optional<Entry>& block = generate.places[k];
                return block.has_value()
                           ? EntryRecord(entry, *block, entry_type)
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

  // The record of the entry `made`, its entry read at the type `entry_type`
  // that entry has.
  auto EntryRecord(mir::Block& entry, const Entry& made, mir::TypeId entry_type)
      -> mir::ExprId {
    return Record(
        *unit_, entry, mir::RuntimeLibraryKind::kInstantiation,
        {ErasedObjectEntry(*unit_, entry, made.made, made.block, entry_type),
         AddressOfConstant(
             *unit_, entry, made.table,
             mir::ConstructionTableType(unit_->types))});
  }

  // A place this object built no block at -- an alternative it did not select
  // -- that no one entry fills, which holds no entry; the body's code never
  // reads one there.
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
  // The body of each phase, where instances of the class stand in the design
  // hierarchy and are built of it.
  std::optional<LibraryBodies> phases;
  LibraryBodies randomization;
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

auto LibraryBodiesOf(
    const mir::CompilationUnit& unit, mir::ClassId id,
    std::span<const support::LibraryCallback> callbacks) -> LibraryBodies {
  LibraryBodies bodies;
  bodies.reserve(callbacks.size());
  for (const support::LibraryCallback callback : callbacks) {
    bodies.push_back(mir::CallbackBodyOf(unit, id, callback));
  }
  return bodies;
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

auto FactsOf(const mir::CompilationUnit& unit, mir::ClassId id)
    -> LineageFacts {
  const mir::Class& cls = unit.GetClass(id);
  LineageFacts facts{
      .is_interface_class = cls.is_interface_class,
      .is_class_object = false,
      .phases = std::nullopt,
      .randomization = {},
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
  if (mir::CallbackBodyOf(unit, id, kScopePhases.front()).has_value()) {
    facts.phases = LibraryBodiesOf(unit, id, kScopePhases);
  }
  if (facts.is_class_object) {
    facts.randomization = LibraryBodiesOf(unit, id, kRandomization);
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

// A null pointer to a record of the library of kind `kind`, as a value of
// `block`.
auto NoRecord(
    const mir::CompilationUnit& unit, mir::Block& block,
    mir::RuntimeLibraryKind kind) -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data = mir::NullLiteral{},
          .type = ReadOnlyPointerTo(unit, LibraryType(unit, kind))});
}

// The address of a table `id`'s dispatch is laid out in, as a value of
// `block`: what `target` names.
auto TableAddress(
    const mir::CompilationUnit& unit, mir::Block& block,
    mir::ReferenceTarget target) -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data = mir::ReferenceExpr{.target = std::move(target)},
          .type = mir::ErasedPointer(unit.types)});
}

// Gives each body of `cls`, class `id`, that something reaches without knowing
// the object's class its entry: every one taking part in dispatch or answering
// a callback the library makes on an object, that the class defines.
void AddEntries(
    const mir::CompilationUnit& unit, mir::ClassId id, mir::Class& cls) {
  const mir::TypeId root = ObjectRootPointer(unit);
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
    cls.entries.push_back(
        mir::MethodEntry{
            .method = method, .entry = EntryOf(unit, id, cls, method, root)});
  }
}

// The definition of class `id`, `cls`, from what its lineage decides, adding
// to `cls` the constants it takes.
//
//   {&Base::definition, &info, &randomization, &end, rows, n, &methods}
auto DefinitionOf(
    const mir::CompilationUnit& unit, mir::ClassId id, mir::Class& cls,
    const LibraryEntryTypes& types, const LineageFacts& facts)
    -> mir::ValueBuild {
  using Kind = mir::RuntimeLibraryKind;
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
  parts.push_back(
      facts.phases.has_value()
          ? ScopeInfoAddress(unit, block, id, cls, types, *facts.phases)
          : NoRecord(unit, block, Kind::kScopeInfo));
  if (facts.is_class_object) {
    mir::ValueBuild info;
    info.value = Record(
        unit, info.body, Kind::kClassObjectInfo,
        LibraryEntries(info.body, types, kRandomization, facts.randomization));
    parts.push_back(ConstantAddress(
        unit, block, id, cls, LibraryType(unit, Kind::kClassObjectInfo),
        std::move(info)));
  } else {
    parts.push_back(NoRecord(unit, block, Kind::kClassObjectInfo));
  }
  parts.push_back(EntryOrNone(
      block,
      facts.is_interface_class
          ? std::nullopt
          : std::optional{block.exprs.Add(
                mir::Expr{
                    .data =
                        mir::ReferenceExpr{
                            .target = mir::DestructorRef{.of = id}},
                    .type = types.on_storage})},
      types.on_storage));
  AppendTable(
      parts, unit, block, id, cls, LibraryType(unit, Kind::kInterfaceRow),
      facts.interface_tables.size(), [&](mir::Block& entry, std::size_t i) {
        const mir::ClassId iface = facts.interface_tables[i].of;
        return Record(
            unit, entry, Kind::kInterfaceRow,
            {BuildDefinitionRead(unit, entry, iface),
             TableAddress(
                 unit, entry,
                 mir::InterfaceTableRef{.of = id, .iface = iface})});
      });
  parts.push_back(
      facts.is_interface_class
          ? block.exprs.Add(
                mir::Expr{
                    .data = mir::NullLiteral{},
                    .type = mir::ErasedPointer(unit.types)})
          : TableAddress(unit, block, mir::DispatchTableRef{.of = id}));
  build.value = Record(unit, block, Kind::kObjectDefinition, std::move(parts));
  return build;
}

}  // namespace

void StateObjectDefinitions(mir::CompilationUnit& unit) {
  const mir::TypeId root = ObjectRootPointer(unit);
  const mir::TypeId void_type = unit.builtins.void_type;
  const std::optional<mir::TypeId> problem =
      ConstraintProblemParamTypeOf(unit, hir::SubroutineKind::kConstraintBlock);
  if (!problem.has_value()) {
    throw InternalError(
        "hir_to_mir: a constraint block takes no problem -- please report this "
        "as a bug");
  }
  const LibraryEntryTypes types{
      .on_object = unit.types.Intern(
          mir::Type{
              mir::MachineFunctionType{.params = {root}, .result = void_type}}),
      .on_object_and_problem = unit.types.Intern(
          mir::Type{mir::MachineFunctionType{
              .params = {root, *problem}, .result = void_type}}),
      .on_storage = unit.types.Intern(
          mir::Type{mir::MachineFunctionType{
              .params = {mir::ErasedPointer(unit.types)},
              .result = void_type}})};
  std::vector<std::optional<LineageFacts>> facts;
  std::vector<std::vector<mir::DispatchedBehavior>> declared;
  facts.reserve(unit.classes.size());
  declared.reserve(unit.classes.size());
  for (const mir::ClassId id : unit.classes.Ids()) {
    const bool elsewhere = mir::IsDefinedElsewhere(unit, id);
    facts.push_back(
        elsewhere ? std::nullopt
                  : std::optional<LineageFacts>{FactsOf(unit, id)});
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
                       AddEntries(unit, id, cls);
                       cls.dispatch = stated->dispatch;
                       cls.interface_tables = stated->interface_tables;
                       cls.object_definition_initializer =
                           DefinitionOf(unit, id, cls, types, *stated);
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
