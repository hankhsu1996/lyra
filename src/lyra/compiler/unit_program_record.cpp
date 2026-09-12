#include "lyra/compiler/unit_program_record.hpp"

#include <algorithm>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/foreign_linkage.hpp"

namespace lyra::compiler {

namespace {

// A foreign name may be declared several times, and each declaration lowers its
// own copy (LRM 35.5.4 requires them to agree). One entry per name is what the
// program's foreign name space holds, so a repeat within a unit collapses here
// rather than at each reader.
auto AlreadyRecorded(
    const std::vector<ForeignName>& names, std::string_view linkage_name)
    -> bool {
  return std::ranges::any_of(names, [&](const ForeignName& recorded) {
    return recorded.linkage_name == linkage_name;
  });
}

// Whether the unit settles where any name lands while the design elaborates. A
// coordinate type exists in a unit's pool exactly when the unit formed one, so
// this reads the whole answer off the types rather than looking for the places
// a coordinate is used.
auto SettlesAnElaboratedCoordinate(const mir::CompilationUnit& unit) -> bool {
  return std::ranges::any_of(unit.types.Ids(), [&](mir::TypeId id) {
    const auto* library = unit.types.Get(id).As<mir::RuntimeLibraryType>();
    return library != nullptr &&
           (library->kind == mir::RuntimeLibraryKind::kPropertyCoordinate ||
            library->kind == mir::RuntimeLibraryKind::kBehaviorCoordinate);
  });
}

// The prototype a foreign declaration publishes: the callable's own signature,
// so the two cannot drift.
auto PrototypeOf(
    mir::TypePool& into, const mir::CompilationUnit& unit,
    const mir::CallableCode& code) -> mir::TypeId {
  std::vector<mir::TypeId> params;
  params.reserve(code.params.size());
  for (const mir::LocalId param : code.params) {
    params.push_back(
        AdoptForeignType(into, unit.types, code.locals.Get(param).type));
  }
  return into.Intern(
      mir::Type{mir::MachineFunctionType{
          .params = std::move(params),
          .result = AdoptForeignType(into, unit.types, code.result_type)}});
}

}  // namespace

auto AdoptForeignType(
    mir::TypePool& into, const mir::TypePool& from, mir::TypeId id)
    -> mir::TypeId {
  const mir::Type& data = from.Get(id);
  if (const auto* function = data.As<mir::MachineFunctionType>()) {
    std::vector<mir::TypeId> params;
    params.reserve(function->params.size());
    for (const mir::TypeId param : function->params) {
      params.push_back(AdoptForeignType(into, from, param));
    }
    return into.Intern(
        mir::Type{mir::MachineFunctionType{
            .params = std::move(params),
            .result = AdoptForeignType(into, from, function->result)}});
  }
  if (const auto* pointer = data.As<mir::PointerType>()) {
    return into.Intern(
        mir::Type{mir::PointerType{
            .pointee = AdoptForeignType(into, from, pointer->pointee),
            .ownership = pointer->ownership,
            .mutability = pointer->mutability}});
  }
  if (data.Is<mir::VoidType>() || data.Is<mir::MachineIntType>() ||
      data.Is<mir::MachineFloatType>() || data.Is<mir::MachineCStringType>() ||
      data.Is<mir::RuntimeLibraryType>()) {
    return into.Intern(data);
  }
  throw InternalError(
      "AdoptForeignType: this type does not cross a foreign boundary");
}

auto ProgramRecordOf(const mir::CompilationUnit& unit) -> UnitProgramRecord {
  UnitProgramRecord record;
  record.unit_name = unit.name;
  record.settles_an_elaborated_coordinate = SettlesAnElaboratedCoordinate(unit);
  if (mir::BroughtUpNamespaceOf(unit) != nullptr) {
    record.namespace_bring_up = NamespaceBringUp{
        .initializer_unit_reads = unit.direct_initializer_unit_reads};
  }

  // A DPI-C name belongs to no class (LRM 35.4, 35.7), so what the unit's own
  // namespace declares is the whole of what it names. Whether the design
  // supplies the body is what separates an export from an import; neither side
  // carries a tag.
  for (const mir::CallableDecl& callable : unit.callables) {
    if (!callable.foreign.has_value() ||
        AlreadyRecorded(record.foreign_names, callable.foreign->foreign_name)) {
      continue;
    }
    record.foreign_names.push_back(
        ForeignName{
            .linkage_name = callable.foreign->foreign_name,
            .prototype = PrototypeOf(record.foreign_types, unit, callable.code),
            .body = callable.code.body.has_value()
                        ? ForeignBody{DefinedByTheUnit{}}
                        : ForeignBody{DefinedByTheForeignSide{}}});
  }

  // An entry reached through a scope is compiled once per specialization of
  // that scope, so it sits on the scope rather than in the unit's namespace and
  // the walk above does not see it.
  for (const mir::ForeignScopeEntry& entry : unit.foreign_scope_entries) {
    if (AlreadyRecorded(record.foreign_names, entry.linkage.foreign_name)) {
      continue;
    }
    record.foreign_names.push_back(
        ForeignName{
            .linkage_name = entry.linkage.foreign_name,
            .prototype = AdoptForeignType(
                record.foreign_types, unit.types, entry.signature),
            .body = DefinedByTheProgram{}});
  }
  return record;
}

}  // namespace lyra::compiler
