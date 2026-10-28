#include "lyra/lowering/mir_to_lir/unit_lowerer.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/class_id.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/function_id.hpp"
#include "lyra/lir/symbol_name.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lowering/mir_to_lir/function_lowerer.hpp"
#include "lyra/mir/behavior_ordinal.hpp"
#include "lyra/mir/block_body_ordinal.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_constant_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/dispatch_table.hpp"
#include "lyra/mir/enum_table_id.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant_id.hpp"
#include "lyra/mir/record_decl.hpp"
#include "lyra/mir/static_variable_id.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/value_build.hpp"
#include "lyra/support/runtime_class.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::lowering::mir_to_lir {

namespace {

// Where the block body an entry makes a block of stands among its body's, or
// none for the entry making an object of the body itself.
auto BlockOrdinalOf(std::optional<mir::BlockBodyOrdinal> block)
    -> std::optional<std::uint32_t> {
  if (!block.has_value()) return std::nullopt;
  return block->value;
}

// A class every body of the unit states is one the link keeps one of; one
// another unit names is this body's own, defined once; and one nothing outside
// this artifact names -- what realizes a scope of this body, or what the
// lowering built for its own use -- is linked within it alone.
auto ClassLinkage(const mir::CompilationUnit& unit, mir::ClassId id)
    -> lir::Linkage {
  if (mir::IsStatedByEveryBody(unit, id)) return lir::Linkage::kShared;
  if (mir::IsPublished(unit, id)) return lir::Linkage::kExternal;
  return lir::Linkage::kInternal;
}

// A body of the namespace something outside this artifact calls -- by its
// linkage name, its identifier, or as an entry the unit publishes -- is
// defined here once; one reached only by its position is this artifact's own.
auto UnitCallableLinkage(const mir::NamespaceReach& reach) -> lir::Linkage {
  return std::visit(
      Overloaded{
          [](const mir::ReachedByLinkageName&) {
            return lir::Linkage::kExternal;
          },
          [](const mir::ReachedByName&) { return lir::Linkage::kExternal; },
          [](const mir::ReachedByBringUpEntry&) {
            return lir::Linkage::kExternal;
          },
          [](const mir::ReachedAsObjectEntry&) {
            return lir::Linkage::kExternal;
          },
          [](const mir::ReachedInAnotherUnit&) {
            return lir::Linkage::kExternal;
          },
          [](const mir::ReachedByPosition&) {
            return lir::Linkage::kInternal;
          }},
      reach);
}

auto TranslateRecordKind(mir::RecordKind kind) -> lir::RecordKind {
  switch (kind) {
    case mir::RecordKind::kStruct:
      return lir::RecordKind::kStruct;
    case mir::RecordKind::kUnion:
      return lir::RecordKind::kUnion;
    case mir::RecordKind::kTaggedUnion:
      return lir::RecordKind::kTaggedUnion;
  }
  throw InternalError("mir_to_lir: unknown record kind");
}

}  // namespace

auto UnitLowerer::Run() -> diag::Result<lir::CompilationUnit> {
  // Every identity the unit will hold is taken before any body is lowered,
  // because a body may name a function whose own body is lowered later --
  // including itself, and a body may build a closure whose own body is lowered
  // after it.
  std::vector<ClassIdentities> classes;
  classes.reserve(mir_->classes.size());
  for (const mir::ClassId id : mir_->classes.Ids()) {
    classes.push_back(TakeClassIdentities(mir_->GetClass(id)));
  }
  class_identities_ = {mir_->classes.size(), std::move(classes)};

  // So is every record this unit names, since what another unit published
  // may hold one -- a specialization of its generic on this unit's record.
  std::vector<lir::RecordId> records;
  records.reserve(mir_->records.size());
  for (std::size_t i = 0; i < mir_->records.size(); ++i) {
    records.push_back(out_.records.Declare());
  }
  record_identities_ = {mir_->records.size(), std::move(records)};

  std::vector<ClosureIdentities> closures;
  closures.reserve(mir_->closures.size());
  for (std::size_t i = 0; i < mir_->closures.size(); ++i) {
    closures.push_back(
        ClosureIdentities{
            .declaration = out_.closures.Declare(),
            .invoke = out_.functions.Declare()});
  }
  closure_identities_ = {mir_->closures.size(), std::move(closures)};

  // What every record is made of is settled before any body lowers, since a
  // body reaching a member of one reads it -- a method of the record
  // included, which is why each method's function is only reserved here. A
  // record is a declaration of the unit another unit names (LRM 7.2, 7.3),
  // which no body's code adds to, so every body states it alike and the link
  // keeps one.
  for (const mir::RecordId id : mir_->records.Ids()) {
    const mir::RecordDecl& decl = mir_->GetRecord(id);
    // A record another unit declares is that unit's to link.
    lir::RecordDecl lowered{
        .defining_unit = decl.defining_unit,
        .path = decl.path,
        .linkage = decl.defining_unit.has_value() ? lir::Linkage::kExternal
                                                  : lir::Linkage::kShared,
        .kind = TranslateRecordKind(decl.kind),
        .elements = TranslateTypes(decl.elements),
        .methods = {}};
    for (const mir::RecordMethod& method : mir_->GetRecordMethods(id)) {
      lowered.methods.push_back(
          lir::RecordMethod{
              .answers = method.answers, .function = out_.functions.Declare()});
    }
    out_.records.Define(RecordDeclaration(id), std::move(lowered));
  }
  // Each operation a type whose operations the unit states answers is
  // reserved the same way, since a body of the unit may call it. Two types MIR
  // tells apart and LIR does not are one representation with one function per
  // operation, which the first of them states: a type's bodies read only what
  // LIR keeps of it, so the two bodies are the same.
  std::vector<const mir::CallableCode*> stating;
  for (const mir::TypeOperation& stated : mir_->type_operations) {
    const lir::TypeId type = TranslateType(stated.type);
    const auto [at, reserved] = type_operation_functions_.try_emplace(
        std::pair{type, stated.answers}, lir::FunctionId{});
    if (reserved) {
      at->second = out_.functions.Declare();
      out_.type_operations.push_back(
          lir::TypeOperation{
              .type = type, .answers = stated.answers, .function = at->second});
      stating.push_back(&stated.code);
    }
  }

  // A variable the unit's namespace owns -- a package's (LRM 26.2), a
  // `$unit` scope's (LRM 3.12.1) -- is one cell for the whole program that no
  // instance holds, so the unit publishes it under a symbol instead. Every
  // reader reaches it by that name, this unit's own bodies included, since a
  // namespace has no instance for a receiver to arrive through.
  // A variable of another unit's namespace is that unit's to hold.
  for (const mir::StaticVariableId id : mir_->static_variables.Ids()) {
    if (mir_->static_variables.Get(id).defining_unit.has_value()) continue;
    out_.static_storage.push_back(
        lir::StaticStorage{
            .symbol = StaticVariableSymbolOf(id),
            .type = TranslateType(mir_->static_variables.Get(id).type)});
  }

  // A cell a class owns rather than an object of it (LRM 8.9) is that same one
  // cell for the whole program, under a name qualified one step further. Below
  // here there is no class for it to hang on -- only storage a symbol reaches
  // -- so it joins the list a namespace variable is on, and the class it was
  // declared by survives only in the name.
  for (const mir::ClassId id : mir_->classes.Ids()) {
    const mir::Class& cls = mir_->GetClass(id);
    // A class another unit defines holds its cells there.
    if (cls.defining_unit.has_value()) continue;
    for (const mir::StaticPropertyId prop_id : cls.static_properties.Ids()) {
      out_.static_storage.push_back(
          lir::StaticStorage{
              .symbol = StaticPropertySymbolOf(
                  mir::StaticPropertyRef{.owner = id, .prop = prop_id}),
              .type = TranslateType(cls.static_properties.Get(prop_id).type)});
    }
  }

  // The symbol a name declared on a scope is reached by (LRM 35.5.3): its
  // definition names only the name and the prototype, so every unit declaring
  // such a scope writes the same one and whatever resolves names across
  // artifacts keeps one of them. What the unit's own namespace owns is the
  // other of the two, and goes with the bodies below.
  for (const mir::ForeignScopeEntry& shared : mir_->foreign_scope_entries) {
    auto fn =
        FunctionLowerer(*this, shared.definition, shared.linkage.foreign_name)
            .Run();
    if (!fn) {
      return std::unexpected(std::move(fn.error()));
    }
    fn->linkage = lir::Linkage::kShared;
    out_.functions.Add(*std::move(fn));
  }

  // A callable the unit's namespace owns -- a package's own body (LRM 26.3) --
  // is a body like any other and becomes a function of the unit. A DPI-C
  // import is reached as a foreign symbol and defined elsewhere.
  for (const mir::CallableId id : mir_->callables.Ids()) {
    const mir::CallableDecl& callable = mir_->callables.Get(id);
    if (!std::holds_alternative<mir::DefinedHere>(mir::FormOf(callable))) {
      continue;
    }
    auto fn =
        FunctionLowerer(*this, callable.code, UnitCallableSymbol(id)).Run();
    if (!fn) {
      return std::unexpected(std::move(fn.error()));
    }
    fn->linkage = UnitCallableLinkage(mir::NamespaceReachOf(*mir_, id));
    out_.functions.Add(*std::move(fn));
  }

  // A record's method is a function under a symbol composed from the record
  // and the operation it answers, which is how every unit reaching it names it.
  for (const mir::RecordId id : mir_->records.Ids()) {
    const mir::RecordDecl& decl = mir_->GetRecord(id);
    const mir::RecordMethods& methods = mir_->GetRecordMethods(id);
    const lir::RecordDecl& lowered = out_.records.Get(RecordDeclaration(id));
    for (std::size_t i = 0; i < methods.size(); ++i) {
      const mir::RecordMethod& method = methods[i];
      auto fn =
          FunctionLowerer(
              *this, method.code,
              lir::RecordMethodSymbol(mir_->name, decl.path, method.answers))
              .Run();
      if (!fn) {
        return std::unexpected(std::move(fn.error()));
      }
      fn->linkage = lowered.linkage;
      out_.functions.Define(lowered.methods[i].function, *std::move(fn));
    }
  }

  for (const mir::ClassId id : mir_->classes.Ids()) {
    const mir::Class& declared = mir_->GetClass(id);
    if (declared.defining_unit.has_value()) {
      out_.classes.Define(
          class_identities_.Get(id).lir_class, LowerDeclaredClass(declared));
      continue;
    }
    auto cls = LowerClass(id, declared);
    if (!cls) {
      return std::unexpected(std::move(cls.error()));
    }
    out_.classes.Define(class_identities_.Get(id).lir_class, *std::move(cls));
  }
  if (const mir::RootedTree* tree = mir::RootedTreeOf(*mir_)) {
    out_.root = class_identities_.Get(tree->root).lir_class;
  }

  // A closure's captures are the storage its values own, and its invoke is a
  // function like any other body's, reading them through the receiver it takes.
  // Only the body that builds the closure names it, so it is linked within
  // this artifact alone.
  for (const mir::ClosureId id : mir_->closures.Ids()) {
    const mir::ClosureDecl& decl = mir_->GetClosure(id);
    lir::Closure closure;
    closure.captures.reserve(decl.fields.size());
    for (const mir::FieldId field : decl.fields.Ids()) {
      closure.captures.push_back(
          lir::Member{.type = TranslateType(decl.fields.Get(field).type)});
    }
    closure.invoke = ClosureFunction(id);
    auto fn =
        FunctionLowerer(
            *this, decl, lir::ClosureInvokeSymbol(out_.body_name, id.value))
            .Run();
    if (!fn) {
      return std::unexpected(std::move(fn.error()));
    }
    fn->linkage = lir::Linkage::kInternal;
    out_.functions.Define(closure.invoke, *std::move(fn));
    out_.closures.Define(ClosureDeclaration(id), std::move(closure));
  }

  // An operation a type with no declaration answers is a function no other
  // unit names, under a symbol composed from the type and the operation, which
  // is named once every declaration the type may name is.
  for (std::size_t i = 0; i < stating.size(); ++i) {
    const lir::TypeOperation stated = out_.type_operations[i];
    auto fn = FunctionLowerer(
                  *this, *stating[i],
                  lir::TypeOperationSymbol(
                      out_.body_name, lir::SymbolPartOf(out_, stated.type),
                      stated.answers))
                  .Run();
    if (!fn) {
      return std::unexpected(std::move(fn.error()));
    }
    out_.functions.Define(stated.function, *std::move(fn));
  }

  // An integral constant is its type and its bits here, which is the whole of
  // a value of an integral type.
  base::Translation<lir::IntegralConstantId, lir::IntegralConstantDecl>
      constants(mir_->integral_constants.size());
  for (const mir::IntegralConstantId id : mir_->integral_constants.Ids()) {
    const mir::IntegralConstantDecl& decl = mir_->integral_constants.Get(id);
    constants.Append(
        lir::IntegralConstantDecl{
            .type = TranslateType(decl.type),
            .value = lir::IntegralConstant{
                .value_words = decl.value.value_words,
                .state_words = decl.value.state_words}});
  }
  out_.integral_constants = std::move(constants);

  // An enumeration's member table is its base type and its members' names and
  // bits here.
  base::Translation<lir::EnumTableId, lir::EnumTableDecl> tables(
      mir_->enum_tables.size());
  for (const mir::EnumTableId id : mir_->enum_tables.Ids()) {
    const mir::EnumType& enumeration = mir_->enum_tables.Get(id);
    lir::EnumTableDecl table{
        .base = out_.types.Intern(TranslateType(mir::Type{enumeration.base})),
        .members = {}};
    table.members.reserve(enumeration.members.size());
    for (const mir::EnumMember& member : enumeration.members) {
      table.members.push_back(
          lir::EnumTableMember{
              .name = member.name,
              .value = lir::IntegralConstant{
                  .value_words = member.value.value_words,
                  .state_words = member.value.state_words}});
    }
    tables.Append(std::move(table));
  }
  out_.enum_tables = std::move(tables);
  return std::move(out_);
}

auto BringUpEntrySymbol(std::string_view unit_name, mir::BringUpEntry entry)
    -> std::string {
  switch (entry) {
    case mir::BringUpEntry::kInstallStorage:
      return lir::NamespaceStorageInstallSymbol(unit_name);
    case mir::BringUpEntry::kInitializeStorage:
      return lir::NamespaceStorageInitializeSymbol(unit_name);
  }
  throw InternalError("mir_to_lir: unknown bring-up entry");
}

auto UnitLowerer::UnitCallableSymbol(mir::CallableId id) const -> std::string {
  return std::visit(
      Overloaded{
          // A foreign name is program-global and crosses as itself (LRM 35.4).
          [](const mir::ReachedByLinkageName& r) {
            return std::string{r.name};
          },
          [&](const mir::ReachedByName& r) {
            return lir::NamespaceCallableSymbol(
                mir_->name, lir::SymbolPart::Name(r.name));
          },
          // A namespace is brought up by the unit's name, which is how the
          // design root calls it.
          [&](const mir::ReachedByBringUpEntry& r) {
            return BringUpEntrySymbol(mir_->name, r.entry);
          },
          // Making an object of one body is making an object of that body's
          // classes, so the entry is the body's.
          [&](const mir::ReachedAsObjectEntry& r) {
            return lir::ObjectEntrySymbol(
                mir_->name, mir_->body_name, BlockOrdinalOf(r.block));
          },
          // Another unit spells a body of its namespace the same way.
          [](const mir::ReachedInAnotherUnit& r) {
            return lir::NamespaceCallableSymbol(
                r.unit_name, lir::SymbolPart::Name(r.name));
          },
          [&](const mir::ReachedByPosition& r) {
            return lir::NamespaceCallableSymbol(
                out_.body_name, lir::SymbolPart::Ordinal(r.slot.value));
          }},
      mir::NamespaceReachOf(*mir_, id));
}

auto UnitLowerer::BaseType(const mir::ClassRef& base) -> lir::TypeId {
  const auto library_class = [&](support::RuntimeClass which) {
    return out_.types.Intern(lir::Type{lir::RuntimeClassType{.which = which}});
  };
  return std::visit(
      Overloaded{
          [&](const mir::ClassId& declared) {
            return ClassValueType(declared);
          },
          [&](const mir::ObjectTreeRootRef&) {
            return library_class(support::RuntimeClass::kScope);
          },
          [&](const mir::ManagedObjectRootRef&) {
            return library_class(support::RuntimeClass::kClassObject);
          }},
      base);
}

namespace {

// What the name every body of a dispatching method's behavior is reached by
// contributes to a symbol.
auto DispatchNamePart(const mir::DispatchName& name) -> lir::SymbolPart {
  return std::visit(
      Overloaded{
          [](std::string_view source) { return lir::SymbolPart::Name(source); },
          [](support::LibraryCallback function) {
            return lir::SymbolPart::LibraryCallback(function);
          }},
      name);
}

}  // namespace

auto UnitLowerer::ClassBodySymbol(
    lir::SymbolPart class_part, const mir::Class& cls, mir::CallableId id) const
    -> std::string {
  // An entry is named by the method it enters, which a class of another unit
  // whose table holds it names it by.
  for (const mir::MethodEntry& entry : cls.entries) {
    if (entry.entry == id) {
      return lir::MethodEntrySymbol(
          ClassQualifier(cls), std::move(class_part),
          DispatchNamePart(mir::DispatchNameOf(*mir_, cls, entry.method)));
    }
  }
  // A body the library calls is named by the callback it answers, which a
  // derived class of another unit calls it by.
  const std::optional<support::LibraryCallback> callback =
      cls.CallbackAnsweredBy(id);
  return lir::ClassCallableSymbol(
      ClassQualifier(cls), std::move(class_part),
      callback.has_value()
          ? lir::SymbolPart::LibraryCallback(*callback)
          : lir::SymbolPartOf(mir::NameOf(cls.named_callables, id), id.value));
}

auto UnitLowerer::EntryConstant(
    const mir::CallableTarget& method, lir::TypeId prototype)
    -> lir::TableBody {
  const mir::Class& owner = mir_->GetClass(method.owner);
  if (!owner.defining_unit.has_value()) {
    for (const mir::MethodEntry& entry : owner.entries) {
      if (entry.method == method.slot) {
        return lir::ConstantFunction{
            .function = MethodFunction(method.owner, entry.entry)};
      }
    }
    throw InternalError(
        "mir_to_lir: a method of a class of this unit is reached through an "
        "entry the class does not have -- please report this as a bug");
  }
  return lir::ConstantExternalFunction{
      .symbol = lir::MethodEntrySymbol(
          ClassQualifier(owner), ClassPart(method.owner),
          DispatchNamePart(mir::DispatchNameOf(*mir_, owner, method.slot))),
      .prototype = prototype};
}

auto UnitLowerer::TakeClassIdentities(const mir::Class& cls)
    -> ClassIdentities {
  // Only a callable this program defines becomes a function of the unit, so a
  // bodyless one takes no function identity and answers with none. Which
  // behavior a callable introduces is the other question, answered from the
  // same walk and independent of it: a callable may have a body and introduce
  // nothing, introduce something and have no body (LRM 8.21), both, or neither.
  std::vector<std::optional<lir::FunctionId>> methods;
  std::vector<std::optional<lir::DispatchOrdinal>> ordinals;
  methods.reserve(cls.callables.size());
  ordinals.reserve(cls.callables.size());
  for (const mir::CallableId callable : cls.callables.Ids()) {
    methods.push_back(
        std::holds_alternative<mir::DefinedHere>(
            mir::FormOf(cls.callables.Get(callable)))
            ? std::optional{out_.functions.Declare()}
            : std::nullopt);
    ordinals.push_back(cls.IntroductionOrdinal(callable).transform(
        [](mir::BehaviorOrdinal ordinal) {
          return lir::DispatchOrdinal{.value = ordinal.value};
        }));
  }
  return ClassIdentities{
      .lir_class = out_.classes.Declare(),
      .constructor = cls.constructor.has_value()
                         ? std::optional{out_.functions.Declare()}
                         : std::nullopt,
      .methods = {cls.callables.size(), std::move(methods)},
      .ordinals = {cls.callables.size(), std::move(ordinals)}};
}

auto UnitLowerer::LowerClass(mir::ClassId owner, const mir::Class& cls)
    -> diag::Result<lir::Class> {
  lir::Class out;
  out.identity = cls.identity;
  out.linkage = ClassLinkage(*mir_, owner);
  const std::string_view qualifier = lir::QualifierOf(out_, cls.identity);
  out.base = cls.base.transform(
      [&](const mir::ClassRef& base) { return BaseType(base); });

  for (const mir::FieldId id : cls.fields.Ids()) {
    out.members.push_back(
        lir::Member{.type = TranslateType(cls.fields.Get(id).type)});
  }
  out.dispatch = LowerDispatch(cls);
  out.interface_tables = LowerInterfaceTables(cls);

  const ClassIdentities& identities = class_identities_.Get(owner);

  // A class's bodies become functions of the program, and a body's own name is
  // unique only within its class -- so the class qualifies it, being itself
  // unique program-wide.
  if (cls.constructor.has_value()) {
    const lir::FunctionId function = ConstructorFunction(owner);
    auto constructor = FunctionLowerer(
                           *this, cls.constructor->code,
                           lir::ConstructorSymbol(qualifier, ClassPart(owner)))
                           .Run();
    if (!constructor) {
      return std::unexpected(std::move(constructor.error()));
    }
    constructor->linkage = out.linkage;
    out_.functions.Define(function, *std::move(constructor));
  }

  // Only a callable this program defines becomes a function: a DPI-C import is
  // reached as a foreign symbol and a pure virtual has no implementation here
  // (LRM 8.21).
  for (const mir::CallableId cid : cls.callables.Ids()) {
    const std::optional<lir::FunctionId>& body = identities.methods.Get(cid);
    if (!body.has_value()) {
      continue;
    }
    auto fn = FunctionLowerer(
                  *this, cls.callables.Get(cid).code,
                  ClassBodySymbol(ClassPart(owner), cls, cid))
                  .Run();
    if (!fn) {
      return std::unexpected(std::move(fn.error()));
    }
    fn->linkage = out.linkage;
    out_.functions.Define(*body, *std::move(fn));
  }
  const std::string definition = DefinitionSymbolOf(owner);
  for (const mir::ClassConstantId constant : cls.constants.Ids()) {
    const mir::ValueBuild& initializer =
        cls.constants.Get(constant).initializer;
    out_.constants.push_back(
        lir::GlobalConstant{
            .symbol = lir::ClassConstantSymbol(definition, constant.value),
            .linkage = lir::Linkage::kInternal,
            .initializer = LowerConstant(initializer, initializer.value)});
  }
  out_.constants.push_back(
      lir::GlobalConstant{
          .symbol = definition,
          .linkage = out.linkage,
          .initializer = LowerConstant(
              cls.object_definition_initializer,
              cls.object_definition_initializer.value)});
  return out;
}

auto UnitLowerer::DefinitionSymbolOf(mir::ClassId of) const -> std::string {
  const mir::Class& cls = mir_->GetClass(of);
  return lir::ClassDefinitionSymbol(ClassQualifier(cls), ClassPart(of));
}

auto UnitLowerer::StaticVariableSymbolOf(mir::StaticVariableId variable) const
    -> std::string {
  return lir::NamespaceVariableSymbol(
      mir::DefiningUnitOf(
          *mir_, mir_->static_variables.Get(variable).defining_unit),
      lir::SymbolPartOf(
          mir::NameOf(mir_->named_static_variables, variable), variable.value));
}

auto UnitLowerer::StaticPropertySymbolOf(
    const mir::StaticPropertyRef& ref) const -> std::string {
  const mir::Class& cls = mir_->GetClass(ref.owner);
  return lir::StaticPropertySymbol(
      ClassQualifier(cls), ClassPart(ref.owner),
      lir::SymbolPartOf(
          mir::NameOf(cls.named_static_properties, ref.prop), ref.prop.value));
}

auto UnitLowerer::ClassPart(mir::ClassId id) const -> lir::SymbolPart {
  return lir::SymbolPartOf(mir_->GetClass(id).identity);
}

auto UnitLowerer::ClassQualifier(const mir::Class& cls) const
    -> std::string_view {
  if (cls.defining_unit.has_value()) {
    return *cls.defining_unit;
  }
  return lir::QualifierOf(out_, cls.identity);
}

auto UnitLowerer::LowerDeclaredClass(const mir::Class& cls) -> lir::Class {
  lir::Class record{
      .defining_unit = cls.defining_unit,
      .identity = cls.identity,
      .linkage = lir::Linkage::kExternal,
      .base = cls.base.transform(
          [&](const mir::ClassRef& base) { return BaseType(base); }),
      .members = {},
      .dispatch = LowerDispatch(cls),
      .interface_tables = {}};
  record.members.reserve(cls.fields.size());
  for (const mir::FieldId field : cls.fields.Ids()) {
    record.members.push_back(
        lir::Member{.type = TranslateType(cls.fields.Get(field).type)});
  }
  return record;
}

auto UnitLowerer::ClassConstantSymbolOf(
    const mir::ClassConstantRef& constant) const -> std::string {
  return lir::ClassConstantSymbol(
      DefinitionSymbolOf(constant.owner), constant.constant.value);
}

auto UnitLowerer::LowerConstant(const mir::ValueBuild& build, mir::ExprId id)
    -> lir::Constant {
  const mir::Expr& expr = build.body.exprs.Get(id);
  const auto not_data = [](std::string_view what) -> lir::Constant {
    throw InternalError(
        std::format(
            "mir_to_lir: a constant is built of {}, which is no data -- please "
            "report this as a bug",
            what));
  };
  const auto parts_of = [&](std::span<const mir::ExprId> parts) {
    std::vector<lir::Constant> out;
    out.reserve(parts.size());
    for (const mir::ExprId part : parts) {
      out.push_back(LowerConstant(build, part));
    }
    return out;
  };
  return std::visit(
      Overloaded{
          [](const mir::StringLiteral& s) -> lir::Constant {
            return {lir::ConstantString{.text = s.value}};
          },
          [](const mir::NullLiteral&) -> lir::Constant {
            return {lir::ConstantNull{}};
          },
          [](const mir::MachineIntLiteral& i) -> lir::Constant {
            return {lir::ConstantInt{.value = i.value}};
          },
          // Read as a value, a body is its address; nothing else a reference
          // names is data until its address is taken.
          [&](const mir::ReferenceExpr& r) -> lir::Constant {
            return std::visit(
                Overloaded{
                    [&](const mir::FunctionRef& f) -> lir::Constant {
                      return {lir::ConstantFunction{
                          .function =
                              MethodFunction(f.body.owner, f.body.slot)}};
                    },
                    [&](const mir::ExternalObjectEntryRef& e) -> lir::Constant {
                      return {lir::ConstantExternalFunction{
                          .symbol = lir::ObjectEntrySymbol(
                              e.unit_name, e.body_name,
                              BlockOrdinalOf(e.block)),
                          .prototype = TranslateType(expr.type)}};
                    },
                    [&](const mir::DispatchEntryRef& e) -> lir::Constant {
                      return std::visit(
                          Overloaded{
                              [](lir::ConstantFunction f) -> lir::Constant {
                                return {f};
                              },
                              [](lir::ConstantExternalFunction f)
                                  -> lir::Constant { return {std::move(f)}; }},
                          EntryConstant(e.body, TranslateType(expr.type)));
                    },
                    [&](const mir::DestructorRef& d) -> lir::Constant {
                      return {
                          lir::ConstantDestructor{.cls = ClassValueType(d.of)}};
                    },
                    [&](const mir::DispatchTableRef& t) -> lir::Constant {
                      return {lir::ConstantDispatchTable{
                          .cls = ClassValueType(t.of)}};
                    },
                    [&](const mir::InterfaceTableRef& t) -> lir::Constant {
                      return {lir::ConstantInterfaceTable{
                          .cls = ClassValueType(t.of),
                          .iface = ClassValueType(t.iface)}};
                    },
                    [&](const mir::ClassConstantRef&) {
                      return not_data("a constant read whole");
                    },
                    [&](const mir::LocalRef&) { return not_data("a local"); },
                    [&](const mir::DefinitionRef&) {
                      return not_data("a definition read whole");
                    },
                    [&](const mir::EnumTableRef&) {
                      return not_data("an enumeration's member table");
                    },
                    [&](const mir::IntegralConstantRef&) {
                      return not_data("an integral constant");
                    },
                    [&](const mir::StaticPropertyRef&) {
                      return not_data("a static property");
                    },
                    [&](const mir::StaticVariableRef&) {
                      return not_data("a static variable");
                    }},
                r.target);
          },
          // Retyping an address changes nothing about which address it is.
          [&](const mir::CastExpr& c) -> lir::Constant {
            return LowerConstant(build, c.operand);
          },
          [&](const mir::AddressOfExpr& a) -> lir::Constant {
            const auto* named = std::get_if<mir::ReferenceExpr>(
                &build.body.exprs.Get(a.operand).data);
            if (named == nullptr) {
              return not_data("the address of something no name reaches");
            }
            return std::visit(
                Overloaded{
                    [&](const mir::DefinitionRef& d) -> lir::Constant {
                      return {lir::ConstantAddress{
                          .symbol = DefinitionSymbolOf(d.of)}};
                    },
                    [&](const mir::ClassConstantRef& c) -> lir::Constant {
                      return {lir::ConstantAddress{
                          .symbol = ClassConstantSymbolOf(c)}};
                    },
                    [&](const mir::FunctionRef&) {
                      return not_data("the address of a body's address");
                    },
                    [&](const mir::DispatchEntryRef&) {
                      return not_data("the address of a body's address");
                    },
                    [&](const mir::DestructorRef&) {
                      return not_data("the address of a body's address");
                    },
                    [&](const mir::DispatchTableRef&) {
                      return not_data("the address of a table's address");
                    },
                    [&](const mir::InterfaceTableRef&) {
                      return not_data("the address of a table's address");
                    },
                    [&](const mir::ExternalObjectEntryRef&) {
                      return not_data("the address of a body's address");
                    },
                    [&](const mir::LocalRef&) {
                      return not_data("the address of a local");
                    },
                    [&](const mir::EnumTableRef&) {
                      return not_data(
                          "the address of an enumeration's member table");
                    },
                    [&](const mir::IntegralConstantRef&) {
                      return not_data("the address of an integral constant");
                    },
                    [&](const mir::StaticPropertyRef&) {
                      return not_data("the address of a static property");
                    },
                    [&](const mir::StaticVariableRef&) {
                      return not_data("the address of a static variable");
                    }},
                named->target);
          },
          // What is composed is what the type says: a structure of the library
          // from its members, or an array from its elements.
          [&](const mir::CompositeExpr& c) -> lir::Constant {
            const mir::Type& type = mir_->types.Get(expr.type);
            if (const auto* record = type.As<mir::RuntimeLibraryType>()) {
              return {lir::ConstantRecord{
                  .kind = TranslateRuntimeLibrary(record->kind)
                              .Get<lir::RuntimeLibraryType>()
                              .kind,
                  .parts = parts_of(c.parts)}};
            }
            if (type.Is<mir::MachineArrayType>()) {
              return {lir::ConstantArray{.elements = parts_of(c.parts)}};
            }
            return not_data("a composite of some other type");
          },
          [&](const mir::MachineBoolLiteral&) {
            return not_data("a machine boolean");
          },
          [&](const mir::MachineFloatLiteral&) {
            return not_data("a machine float");
          },
          [&](const mir::UnaryExpr&) { return not_data("a unary operation"); },
          [&](const mir::BinaryExpr&) {
            return not_data("a binary operation");
          },
          [&](const mir::ConditionalExpr&) {
            return not_data("a conditional");
          },
          [&](const mir::BlockExpr&) { return not_data("a block"); },
          [&](const mir::AssignExpr&) { return not_data("an assignment"); },
          [&](const mir::CallExpr&) { return not_data("a call"); },
          [&](const mir::NewExpr&) { return not_data("an allocation"); },
          [&](const mir::DerefExpr&) { return not_data("a dereference"); },
          [&](const mir::BorrowExpr&) { return not_data("a reference"); },
          [&](const mir::MoveExpr&) { return not_data("a move"); },
          [&](const mir::CopyExpr&) { return not_data("a read"); },
          [&](const mir::DiscriminantExpr&) {
            return not_data("a read of a tag");
          },
          [&](const mir::FieldAccessExpr&) {
            return not_data("a field access");
          },
          [&](const mir::ClosureExpr&) { return not_data("a closure"); },
          [&](const mir::UnionExpr&) { return not_data("a union"); },
          [&](const mir::AwaitExpr&) { return not_data("an await"); },
          [&](const mir::WaitExpr&) { return not_data("a wait"); },
          [&](const mir::VectorGetExpr&) {
            return not_data("a vector element");
          }},
      expr.data);
}

auto UnitLowerer::SlotRef(const mir::VirtualSlot& slot)
    -> lir::StatedDispatchRef {
  const std::optional<lir::DispatchOrdinal>& ordinal =
      class_identities_.Get(slot.owner_class).ordinals.Get(slot.slot);
  // A dispatch names the callable that introduced the behavior, which is the
  // one identity every class answering it agrees on, so a callable naming no
  // introduction is a producer that built the slot identity wrongly.
  if (!ordinal.has_value()) {
    throw InternalError(
        "mir_to_lir: a dispatch names a callable that introduces no behavior");
  }
  return lir::StatedDispatchRef{
      .introduced_by = ClassValueType(slot.owner_class), .ordinal = *ordinal};
}

auto UnitLowerer::TableBodyPrototype() const -> lir::TypeId {
  return out_.types.Intern(
      lir::Type{lir::MachineFunctionType{
          .params = {},
          .result = out_.types.Intern(lir::Type{lir::VoidType{}})}});
}

auto UnitLowerer::LowerDispatch(const mir::Class& cls)
    -> std::vector<lir::DispatchedBehavior> {
  const lir::TypeId erased = TableBodyPrototype();
  std::vector<lir::DispatchedBehavior> dispatch;
  dispatch.reserve(cls.dispatch.size());
  for (const mir::DispatchedBehavior& dispatched : cls.dispatch) {
    dispatch.push_back(
        lir::DispatchedBehavior{
            .behavior = SlotRef(dispatched.behavior),
            .answered_by = dispatched.answered_by.transform(
                [&](const mir::CallableTarget& body) {
                  return EntryConstant(body, erased);
                })});
  }
  return dispatch;
}

auto UnitLowerer::LowerInterfaceTables(const mir::Class& cls)
    -> std::vector<lir::InterfaceTable> {
  const lir::TypeId erased = TableBodyPrototype();
  std::vector<lir::InterfaceTable> tables;
  tables.reserve(cls.interface_tables.size());
  for (const mir::InterfaceTable& implemented : cls.interface_tables) {
    lir::InterfaceTable table{
        .of = ClassValueType(implemented.of), .answered_by = {}};
    for (const std::optional<mir::CallableTarget>& body :
         implemented.answered_by) {
      table.answered_by.push_back(
          body.transform([&](const mir::CallableTarget& answered) {
            return EntryConstant(answered, erased);
          }));
    }
    tables.push_back(std::move(table));
  }
  return tables;
}

auto UnitLowerer::MethodFunction(
    mir::ClassId owner, mir::CallableId callable) const -> lir::FunctionId {
  const std::optional<lir::FunctionId>& fn =
      class_identities_.Get(owner).methods.Get(callable);
  if (!fn.has_value()) {
    throw InternalError(
        "mir_to_lir: callable has no body, so it is no function of this unit");
  }
  return *fn;
}

auto UnitLowerer::CallableCallTarget(
    mir::ClassId owner, mir::CallableId callable) const -> lir::CallTarget {
  const mir::Class& cls = mir_->GetClass(owner);
  if (!cls.defining_unit.has_value()) {
    return lir::CallTarget{
        lir::FunctionTarget{.function = MethodFunction(owner, callable)}};
  }
  return lir::CallTarget{lir::SymbolTarget{
      .symbol = ClassBodySymbol(ClassPart(owner), cls, callable)}};
}

auto UnitLowerer::ConstructorCallTarget(mir::ClassId cls) const
    -> lir::CallTarget {
  const mir::Class& declared = mir_->GetClass(cls);
  if (!declared.defining_unit.has_value()) {
    return lir::CallTarget{
        lir::FunctionTarget{.function = ConstructorFunction(cls)}};
  }
  return lir::CallTarget{lir::SymbolTarget{
      .symbol =
          lir::ConstructorSymbol(ClassQualifier(declared), ClassPart(cls))}};
}

auto UnitLowerer::ConstructorFunction(mir::ClassId cls) const
    -> lir::FunctionId {
  const std::optional<lir::FunctionId>& constructor =
      class_identities_.Get(cls).constructor;
  if (!constructor.has_value()) {
    throw InternalError(
        "mir_to_lir: a construction enters a class no object is built of -- "
        "please report this as a bug");
  }
  return *constructor;
}

auto UnitLowerer::ClosureFunction(mir::ClosureId closure) const
    -> lir::FunctionId {
  return closure_identities_.Get(closure).invoke;
}

auto UnitLowerer::ClosureDeclaration(mir::ClosureId closure) const
    -> lir::ClosureId {
  return closure_identities_.Get(closure).declaration;
}

auto UnitLowerer::RecordDeclaration(mir::RecordId record) const
    -> lir::RecordId {
  return record_identities_.Get(record);
}

auto UnitLowerer::TypeOperationFunction(
    mir::TypeId type, support::ValueOperation answers) -> lir::FunctionId {
  const auto found =
      type_operation_functions_.find(std::pair{TranslateType(type), answers});
  if (found != type_operation_functions_.end()) {
    return found->second;
  }
  throw InternalError(
      std::format(
          "mir_to_lir: a call names the operation \"{}\" of a type the unit "
          "states no such operation for -- please report this as a bug",
          support::ValueOperationName(answers)));
}

auto UnitLowerer::MachineBoolType() -> lir::TypeId {
  return TranslateType(mir_->builtins.machine_bool);
}

auto UnitLowerer::ClosureValueType(mir::ClosureId closure) -> lir::TypeId {
  return out_.types.Intern(
      lir::Type{lir::ClosureType{.closure_id = ClosureDeclaration(closure)}});
}

auto UnitLowerer::ClassValueType(mir::ClassId cls) -> lir::TypeId {
  return out_.types.Intern(
      lir::Type{
          lir::ObjectType{.class_id = class_identities_.Get(cls).lir_class}});
}

}  // namespace lyra::lowering::mir_to_lir
