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
#include "lyra/mir/callable.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_constant_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/enum_table_id.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant_id.hpp"
#include "lyra/mir/record_decl.hpp"
#include "lyra/mir/static_variable_id.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/value_build.hpp"
#include "lyra/support/def_path.hpp"
#include "lyra/support/runtime_class.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::lowering::mir_to_lir {

namespace {

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

  // So is every struct this unit declares, since what another unit published
  // may hold one -- a specialization of its generic on this unit's struct.
  std::vector<lir::RecordId> records;
  records.reserve(mir_->records.size());
  for (std::size_t i = 0; i < mir_->records.size(); ++i) {
    records.push_back(out_.records.Declare());
  }
  record_identities_ = {mir_->records.size(), std::move(records)};

  // What each class of another unit published, taken whole: a property step on
  // one names a slot counted out of the published list, and a class extending
  // one is placed after all of its storage and fills its table from all of its
  // bodies. A body of it is reached by the symbol its unit emits it under,
  // composed from the names that unit composed it from.
  for (const mir::ExternalClass& cls : mir_->external_classes) {
    lir::ExternalClass record{
        .unit_name = cls.unit_name,
        .class_path = cls.class_path,
        .base = cls.base.transform(
            [&](const mir::ClassRef& base) { return BaseType(base); }),
        .members = {},
        .dispatch = {},
        .implements = {}};
    for (const mir::DeclaredClassRef& iface : cls.implements) {
      record.implements.push_back(ClassRefValueType(iface));
    }
    record.members.reserve(cls.fields.size() + cls.private_field_types.size());
    for (const mir::FieldId id : cls.fields.Ids()) {
      record.members.push_back(
          lir::Member{.type = TranslateType(cls.fields.Get(id).type)});
    }
    for (const mir::TypeId type : cls.private_field_types) {
      record.members.push_back(lir::Member{.type = TranslateType(type)});
    }
    const auto body = [&](std::string_view method) {
      return lir::ClassCallableSymbol(
          cls.unit_name, lir::SymbolPartOf(cls.class_path),
          lir::SymbolPart::Name(method));
    };
    for (const mir::PublishedBehavior& behavior : cls.behaviors) {
      record.dispatch.introduces.push_back(
          behavior.is_pure ? std::nullopt : std::optional{body(behavior.name)});
    }
    for (const mir::PublishedOverride& overriding : cls.overrides) {
      record.dispatch.overrides.push_back(
          lir::Override{
              .behavior = ExternalMethodRef(
                  overriding.behavior.unit_name, overriding.behavior.class_path,
                  overriding.behavior.ordinal),
              .body = body(overriding.method)});
    }
    out_.external_classes.push_back(std::move(record));
  }

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
  for (const mir::ExternalRecord& external : mir_->external_records) {
    out_.external_records.push_back(
        lir::ExternalRecord{
            .declaration = TranslateDeclaration(external.declaration),
            .kind = TranslateRecordKind(external.kind),
            .elements = TranslateTypes(external.elements)});
  }
  for (const mir::RecordId id : mir_->records.Ids()) {
    const mir::RecordDecl& decl = mir_->GetRecord(id);
    lir::RecordDecl lowered{
        .path = decl.path,
        .linkage = lir::Linkage::kShared,
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
  for (const mir::StaticVariableId id : mir_->static_variables.Ids()) {
    out_.static_storage.push_back(
        lir::StaticStorage{
            .symbol = lir::NamespaceVariableSymbol(
                mir_->name,
                lir::SymbolPartOf(
                    mir::NameOf(mir_->named_static_variables, id), id.value)),
            .type = TranslateType(mir_->static_variables.Get(id).type)});
  }

  // A cell a class owns rather than an object of it (LRM 8.9) is that same one
  // cell for the whole program, under a name qualified one step further. Below
  // here there is no class for it to hang on -- only storage a symbol reaches
  // -- so it joins the list a namespace variable is on, and the class it was
  // declared by survives only in the name.
  for (const mir::ClassId id : mir_->classes.Ids()) {
    const mir::Class& cls = mir_->GetClass(id);
    for (const mir::StaticPropertyId prop_id : cls.static_properties.Ids()) {
      out_.static_storage.push_back(
          lir::StaticStorage{
              .symbol = lir::StaticPropertySymbol(
                  lir::QualifierOf(out_, cls.path),
                  lir::SymbolPartOf(cls.path, id.value),
                  lir::SymbolPartOf(
                      mir::NameOf(cls.named_static_properties, prop_id),
                      prop_id.value)),
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

  // An operation a type with no declaration answers is a function no other
  // unit names, so its symbol is counted, within this body.
  for (std::size_t i = 0; i < stating.size(); ++i) {
    auto fn = FunctionLowerer(
                  *this, *stating[i],
                  lir::TypeOperationSymbol(
                      lir::QualifierOf(out_, std::nullopt),
                      static_cast<std::uint32_t>(i)))
                  .Run();
    if (!fn) {
      return std::unexpected(std::move(fn.error()));
    }
    out_.functions.Define(out_.type_operations[i].function, *std::move(fn));
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
    auto cls = LowerClass(id, mir_->GetClass(id));
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
    auto fn = FunctionLowerer(
                  *this, decl,
                  lir::ClosureInvokeSymbol(
                      lir::QualifierOf(out_, std::nullopt), id.value))
                  .Run();
    if (!fn) {
      return std::unexpected(std::move(fn.error()));
    }
    fn->linkage = lir::Linkage::kInternal;
    out_.functions.Define(closure.invoke, *std::move(fn));
    out_.closures.Define(ClosureDeclaration(id), std::move(closure));
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
          [&](const mir::ReachedAsObjectEntry&) {
            return lir::ObjectEntrySymbol(mir_->name, mir_->body_name);
          },
          [&](const mir::ReachedByPosition& r) {
            return lir::NamespaceCallableSymbol(
                lir::QualifierOf(out_, std::nullopt),
                lir::SymbolPart::Ordinal(r.slot.value));
          }},
      mir::NamespaceReachOf(*mir_, id));
}

auto UnitLowerer::ClassRefValueType(const mir::DeclaredClassRef& of)
    -> lir::TypeId {
  return std::visit(
      Overloaded{
          [&](const mir::IntraUnitClassRef& intra) {
            return ClassValueType(intra.class_id);
          },
          [&](const mir::CrossUnitClassRef& cross) {
            return ExternalClassValueType(cross.unit_name, cross.class_path);
          }},
      of);
}

auto UnitLowerer::BaseType(const mir::ClassRef& base) -> lir::TypeId {
  const auto library_class = [&](support::RuntimeClass which) {
    return out_.types.Intern(lir::Type{lir::RuntimeClassType{.which = which}});
  };
  return std::visit(
      Overloaded{
          [&](const mir::IntraUnitClassRef& intra) {
            return ClassRefValueType(intra);
          },
          [&](const mir::CrossUnitClassRef& cross) {
            return ClassRefValueType(cross);
          },
          [&](const mir::ObjectTreeRootRef&) {
            return library_class(support::RuntimeClass::kScope);
          },
          [&](const mir::ManagedObjectRootRef&) {
            return library_class(support::RuntimeClass::kObject);
          }},
      base);
}

auto UnitLowerer::ClassBodySymbol(
    lir::SymbolPart class_part, const mir::Class& cls, mir::CallableId id) const
    -> std::string {
  return lir::ClassCallableSymbol(
      lir::QualifierOf(out_, cls.path), std::move(class_part),
      lir::SymbolPartOf(mir::NameOf(cls.named_callables, id), id.value));
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
  out.path = cls.path;
  out.linkage = ClassLinkage(*mir_, owner);
  const std::string_view qualifier = lir::QualifierOf(out_, cls.path);
  out.base = cls.base.transform(
      [&](const mir::ClassRef& base) { return BaseType(base); });

  for (const mir::FieldId id : cls.fields.Ids()) {
    out.members.push_back(
        lir::Member{.type = TranslateType(cls.fields.Get(id).type)});
  }

  const ClassIdentities& identities = class_identities_.Get(owner);

  // A class's bodies become functions of the program, and a body's own name is
  // unique only within its class -- so the class qualifies it, being itself
  // unique program-wide.
  if (cls.constructor.has_value()) {
    const lir::FunctionId function = ConstructorFunction(owner);
    auto constructor =
        FunctionLowerer(
            *this, cls, *cls.constructor,
            lir::ConstructorPrologueSymbol(
                lir::ClassDefinitionSymbol(
                    qualifier, lir::SymbolPartOf(cls.path, owner.value))),
            lir::ConstructorSymbol(
                qualifier, lir::SymbolPartOf(cls.path, owner.value)))
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
    auto fn =
        FunctionLowerer(
            *this, cls.callables.Get(cid).code,
            ClassBodySymbol(lir::SymbolPartOf(cls.path, owner.value), cls, cid))
            .Run();
    if (!fn) {
      return std::unexpected(std::move(fn.error()));
    }
    fn->linkage = out.linkage;
    out_.functions.Define(*body, *std::move(fn));
  }
  out.dispatch = LowerDispatch(owner, cls);
  for (const mir::DeclaredClassRef& iface : cls.implements) {
    out.implements.push_back(ClassRefValueType(iface));
  }
  for (const mir::ConformingBehavior& answered : cls.conforming) {
    out.conforming.push_back(
        lir::ConformingBehavior{
            .interface_behavior = SlotRef(answered.interface_behavior),
            .answered_by = answered.answered_by.transform(
                [&](const mir::VirtualSlot& slot) { return SlotRef(slot); })});
  }
  const std::string definition =
      DefinitionSymbolOf(mir::IntraUnitClassRef{.class_id = owner});
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

auto UnitLowerer::DefinitionSymbolOf(const mir::DeclaredClassRef& of) const
    -> std::string {
  return std::visit(
      Overloaded{
          [&](const mir::IntraUnitClassRef& intra) {
            const std::optional<support::DefPath>& path =
                mir_->GetClass(intra.class_id).path;
            return lir::ClassDefinitionSymbol(
                lir::QualifierOf(out_, path),
                lir::SymbolPartOf(path, intra.class_id.value));
          },
          [](const mir::CrossUnitClassRef& cross) {
            return lir::ClassDefinitionSymbol(
                cross.unit_name, lir::SymbolPartOf(cross.class_path));
          }},
      of);
}

auto UnitLowerer::ClassConstantSymbolOf(
    const mir::ClassConstantRef& constant) const -> std::string {
  return lir::ClassConstantSymbol(
      DefinitionSymbolOf(mir::IntraUnitClassRef{.class_id = constant.owner}),
      constant.constant.value);
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
                          .symbol =
                              lir::ObjectEntrySymbol(e.unit_name, e.body_name),
                          .prototype = TranslateType(expr.type)}};
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
                    },
                    [&](const mir::ExternalUnitVariableRef&) {
                      return not_data("another unit's variable");
                    },
                    [&](const mir::ExternalStaticPropertyRef&) {
                      return not_data("another unit's static property");
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
                    },
                    [&](const mir::ExternalUnitVariableRef&) {
                      return not_data("the address of another unit's variable");
                    },
                    [&](const mir::ExternalStaticPropertyRef&) {
                      return not_data(
                          "the address of another unit's static property");
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
          [&](const mir::DynamicCastExpr&) {
            return not_data("a checked conversion");
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
  return std::visit(
      Overloaded{
          [&](const mir::LocalVirtualSlot& local) {
            return MethodRef(local.owner_class, local.slot);
          },
          [&](const mir::ExternalVirtualSlot& external) {
            return ExternalMethodRef(
                external.unit_name, external.class_path, external.ordinal);
          }},
      slot);
}

auto UnitLowerer::LowerDispatch(mir::ClassId owner, const mir::Class& cls)
    -> lir::ClassDispatch {
  lir::ClassDispatch dispatch;
  for (const mir::CallableId cid : cls.callables.Ids()) {
    const mir::CallableDecl& callable = cls.callables.Get(cid);
    if (!callable.virtual_dispatch.has_value()) {
      continue;
    }
    // Every body of the class is emitted under the symbol it composes here, so
    // naming one by it is naming the function.
    const std::optional<std::string> body =
        class_identities_.Get(owner).methods.Get(cid).has_value()
            ? std::optional{ClassBodySymbol(
                  lir::SymbolPartOf(cls.path, owner.value), cls, cid)}
            : std::nullopt;
    const auto override_with_body = [&](lir::StatedDispatchRef behavior) {
      // Overriding a behavior without a body leaves it as the lineage already
      // had it (LRM 8.21, one abstract class extending another).
      if (body.has_value()) {
        dispatch.overrides.push_back(
            lir::Override{.behavior = behavior, .body = *body});
      }
    };
    std::visit(
        Overloaded{
            [&](const mir::IntroducesVirtualSlot&) {
              dispatch.introduces.push_back(body);
            },
            [&](const mir::OverridesIntraUnitSlot& overridden) {
              override_with_body(
                  MethodRef(overridden.slot_owner, overridden.slot_id));
            },
            [&](const mir::OverridesExternalSlot& overridden) {
              override_with_body(ExternalMethodRef(
                  overridden.unit_name, overridden.class_path,
                  overridden.ordinal));
            },
            // The library's class introduced it, at the position its class
            // declares it among its own.
            [&](const mir::OverridesLibraryVirtual& overridden) {
              override_with_body(
                  lir::StatedDispatchRef{
                      .introduced_by = out_.types.Intern(
                          lir::Type{lir::RuntimeClassType{
                              .which = support::DeclaringClassOf(
                                  overridden.function)}}),
                      .ordinal = lir::DispatchOrdinal{
                          support::OrdinalOf(overridden.function)}});
            }},
        *callable.virtual_dispatch);
  }
  return dispatch;
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

auto UnitLowerer::MethodRef(mir::ClassId owner, mir::CallableId callable)
    -> lir::StatedDispatchRef {
  const std::optional<lir::DispatchOrdinal>& ordinal =
      class_identities_.Get(owner).ordinals.Get(callable);
  // A dispatch names the callable that introduced the behavior, which is the
  // one identity every class answering it agrees on, so a callable naming no
  // introduction is a producer that built the slot identity wrongly.
  if (!ordinal.has_value()) {
    throw InternalError(
        "mir_to_lir: a dispatch names a callable that introduces no behavior");
  }
  return lir::StatedDispatchRef{
      .introduced_by = ClassValueType(owner), .ordinal = *ordinal};
}

auto UnitLowerer::ExternalMethodRef(
    const std::string& unit_name, const support::DefPath& class_path,
    mir::BehaviorOrdinal ordinal) const -> lir::StatedDispatchRef {
  return lir::StatedDispatchRef{
      .introduced_by = ExternalClassValueType(unit_name, class_path),
      .ordinal = lir::DispatchOrdinal{ordinal.value}};
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

auto UnitLowerer::ExternalClassValueType(
    const std::string& unit_name, const support::DefPath& class_path) const
    -> lir::TypeId {
  return out_.types.Intern(
      lir::Type{lir::CrossUnitClassType{
          .unit_name = unit_name, .class_path = class_path}});
}

}  // namespace lyra::lowering::mir_to_lir
