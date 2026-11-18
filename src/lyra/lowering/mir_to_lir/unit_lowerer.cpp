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
#include "lyra/lir/constant_id.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/function_id.hpp"
#include "lyra/lir/symbol_name.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lowering/mir_to_lir/function_lowerer.hpp"
#include "lyra/mir/block_body_ordinal.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/constant_id.hpp"
#include "lyra/mir/constant_value.hpp"
#include "lyra/mir/dispatch_table.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/linkage.hpp"
#include "lyra/mir/record_decl.hpp"
#include "lyra/mir/static_variable_id.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_declaration_ref.hpp"
#include "lyra/mir/verify.hpp"
#include "lyra/support/library_callback.hpp"
#include "lyra/support/library_decl.hpp"
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

// What a record is laid out as below MIR. An untagged union is a structure of
// its one storage, which every member is read and written through as MIR
// states (LRM 7.3), so nothing below MIR tells it from one; a tagged union
// keeps its members overlaid and its tag (LRM 7.3.2).
auto TranslateRecordKind(mir::RecordKind kind) -> lir::RecordKind {
  switch (kind) {
    case mir::RecordKind::kStruct:
    case mir::RecordKind::kUnion:
      return lir::RecordKind::kStruct;
    case mir::RecordKind::kTaggedUnion:
      return lir::RecordKind::kTaggedUnion;
  }
  throw InternalError("mir_to_lir: unknown record kind");
}

// A call of an operation the language does not give its type is a call no
// program states, so it is the compiler's own defect.
[[noreturn]] void RefuseOperationNotGiven(support::ValueOperation answers) {
  throw InternalError(
      std::format(
          "mir_to_lir: a call names the operation \"{}\" of a type the "
          "language gives no such operation -- please report this as a bug",
          support::ValueOperationName(answers)));
}

}  // namespace

auto UnitLowerer::LowerUnit(const mir::CompilationUnit& mir)
    -> diag::Result<UnitLowerer> {
  mir::RequireLegalized(mir, "mir_to_lir");
  UnitLowerer lowerer(mir);
  if (auto populated = lowerer.Populate(); !populated) {
    return std::unexpected(std::move(populated.error()));
  }
  return lowerer;
}

auto UnitLowerer::Populate() -> diag::Result<void> {
  // Every identity a declaration gives the unit is taken before any body is
  // lowered, because a body may name a function whose own body is lowered
  // later -- including itself, and a body may build a closure whose own body
  // is lowered after it. Only a callable this unit defines becomes a
  // function of the unit, whoever declares it, so a bodyless one takes no
  // function identity and is reached by the symbol whoever defines it emits.
  std::vector<std::optional<lir::FunctionId>> functions(
      mir_->callables.size(), std::nullopt);
  for (const mir::CallableId id : mir_->callables.Ids()) {
    if (mir::IsDefinedHere(mir_->callables.Get(id))) {
      functions.at(id.value) = out_.functions.Declare();
    }
  }
  functions_ = {mir_->callables.size(), std::move(functions)};

  std::vector<std::optional<lir::DispatchOrdinal>> ordinals(
      mir_->callables.size(), std::nullopt);
  std::vector<lir::ClassId> classes;
  classes.reserve(mir_->classes.size());
  for (const mir::ClassId id : mir_->classes.Ids()) {
    TakeDispatchOrdinals(id, ordinals);
    classes.push_back(out_.classes.Declare());
  }
  class_identities_ = {mir_->classes.size(), std::move(classes)};
  ordinals_ = {mir_->callables.size(), std::move(ordinals)};

  // So is every record this unit names, since what another unit published
  // may hold one -- a specialization of its generic on this unit's record.
  std::vector<lir::RecordId> records;
  records.reserve(mir_->records.size());
  for (std::size_t i = 0; i < mir_->records.size(); ++i) {
    records.push_back(out_.records.Declare());
  }
  record_identities_ = {mir_->records.size(), std::move(records)};

  std::vector<lir::ClosureId> closures;
  closures.reserve(mir_->closures.size());
  for (std::size_t i = 0; i < mir_->closures.size(); ++i) {
    closures.push_back(out_.closures.Declare());
  }
  closure_identities_ = {mir_->closures.size(), std::move(closures)};

  // What every record is made of is settled before any body lowers, since a
  // body reaching a member of one reads it -- a method of the record
  // included. A method of a record another unit declares is that unit's
  // function.
  for (const mir::RecordId id : mir_->records.Ids()) {
    const mir::RecordDecl& decl = mir_->GetRecord(id);
    lir::RecordDecl lowered{
        .defining_unit = decl.defining_unit,
        .path = decl.path,
        .linkage = mir::LinkageOf(*mir_, id),
        .kind = TranslateRecordKind(mir::KindOf(decl)),
        .elements = TranslateTypes(mir::ComponentsOf(decl)),
        .methods = {}};
    if (!decl.defining_unit.has_value()) {
      for (const mir::CallableId method : mir_->RecordMethodsOf(id)) {
        lowered.methods.push_back(
            lir::RecordMethod{
                .answers = mir::AnsweredOperationOf(*mir_, method),
                .function = FunctionOf(method)});
      }
    }
    out_.records.Define(RecordDeclaration(id), std::move(lowered));
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

  // The symbol a name declared on a scope is reached by (LRM 35.5.3). What
  // the unit's own namespace owns is the other of the two, and goes with the
  // bodies below.
  for (const mir::ForeignScopeEntry& shared : mir_->foreign_scope_entries) {
    auto fn =
        FunctionLowerer(*this, shared.definition, shared.linkage.foreign_name)
            .Run();
    if (!fn) {
      return std::unexpected(std::move(fn.error()));
    }
    fn->linkage = mir::ForeignScopeEntryLinkage();
    out_.functions.Add(*std::move(fn));
  }

  // A callable the unit's namespace owns -- a package's own body (LRM 26.3) --
  // is a body like any other and becomes a function of the unit. A DPI-C
  // import is reached as a foreign symbol and defined elsewhere.
  for (const mir::CallableId id : mir_->NamespaceCallables()) {
    const mir::CallableDecl& callable = mir_->callables.Get(id);
    if (!mir::IsDefinedHere(callable)) {
      continue;
    }
    auto fn =
        FunctionLowerer(*this, callable.code, NamespaceCallableSymbolOf(id))
            .Run();
    if (!fn) {
      return std::unexpected(std::move(fn.error()));
    }
    fn->linkage = mir::LinkageOf(*mir_, mir::CallableTarget{.callable = id});
    out_.functions.Define(FunctionOf(id), *std::move(fn));
  }

  // A record's method is a function under a symbol composed from the record
  // and the operation it answers, which is how every unit reaching it names it.
  for (const mir::RecordId id : mir_->records.Ids()) {
    const mir::RecordDecl& decl = mir_->GetRecord(id);
    if (decl.defining_unit.has_value()) continue;
    const lir::RecordDecl& lowered = out_.records.Get(RecordDeclaration(id));
    for (const mir::CallableId method : mir_->RecordMethodsOf(id)) {
      auto fn = FunctionLowerer(
                    *this, mir_->callables.Get(method).code,
                    RecordMethodSymbolOf(method))
                    .Run();
      if (!fn) {
        return std::unexpected(std::move(fn.error()));
      }
      fn->linkage = lowered.linkage;
      out_.functions.Define(FunctionOf(method), *std::move(fn));
    }
  }

  for (const mir::ClassId id : mir_->classes.Ids()) {
    const mir::Class& declared = mir_->GetClass(id);
    if (declared.defining_unit.has_value()) {
      out_.classes.Define(LirClass(id), LowerDeclaredClass(id, declared));
      continue;
    }
    auto cls = LowerClass(id, declared);
    if (!cls) {
      return std::unexpected(std::move(cls.error()));
    }
    out_.classes.Define(LirClass(id), *std::move(cls));
  }
  if (const mir::RootedTree* tree = mir::RootedTreeOf(*mir_)) {
    out_.root = LirClass(tree->root);
  }

  // A closure's captures are the storage its values own, and its invoke is a
  // function like any other body's, reading them through the receiver it takes.
  for (const mir::ClosureId id : mir_->closures.Ids()) {
    const mir::ClosureDecl& decl = mir_->GetClosure(id);
    lir::Closure closure;
    closure.captures.reserve(decl.fields.size());
    for (const mir::FieldId field : decl.fields.Ids()) {
      closure.captures.push_back(
          lir::Member{.type = TranslateType(decl.fields.Get(field).type)});
    }
    const mir::CallableId body = mir_->BodyOf(id);
    closure.invoke = FunctionOf(body);
    auto fn = FunctionLowerer(
                  *this, id, lir::ClosureInvokeSymbol(out_.body_name, id.value))
                  .Run();
    if (!fn) {
      return std::unexpected(std::move(fn.error()));
    }
    fn->linkage = mir::LinkageOf(*mir_, mir::CallableTarget{.callable = body});
    out_.functions.Define(closure.invoke, *std::move(fn));
    out_.closures.Define(ClosureDeclaration(id), std::move(closure));
  }

  StateClassSteps();
  return {};
}

void UnitLowerer::MarkOperationUsed(const lir::TypeOperation& operation) {
  const auto stated = mir_type_of_.find(operation.type);
  if (stated == mir_type_of_.end()) {
    throw InternalError(
        std::format(
            "mir_to_lir: the operation \"{}\" is asked of a type no type of "
            "the unit's model translates to -- please report this as a bug",
            support::ValueOperationName(operation.answers)));
  }
  if (visited_.insert(operation).second) {
    pending_.push_back(
        PendingOperation{.operation = operation, .mir_type = stated->second});
  }
}

auto UnitLowerer::StateTypeOperations(const OperationBodyProvider& bodies)
    -> diag::Result<bool> {
  std::vector<PendingOperation> pending = std::exchange(pending_, {});
  for (const auto& [operation, mir_type] : pending) {
    std::optional<mir::CallableCode> body = bodies(mir_type, operation.answers);
    const auto called = out_.type_operations.find(operation);
    if (!body.has_value()) {
      if (called != out_.type_operations.end()) {
        RefuseOperationNotGiven(operation.answers);
      }
      out_.type_operations.emplace(operation, std::nullopt);
      continue;
    }
    const lir::FunctionId function = called != out_.type_operations.end()
                                         ? *called->second
                                         : out_.functions.Declare();
    out_.type_operations.insert_or_assign(operation, function);
    auto fn = FunctionLowerer(
                  *this, *body,
                  lir::TypeOperationSymbol(
                      out_.body_name, lir::SymbolPartOf(out_, operation.type),
                      operation.answers))
                  .Run();
    if (!fn) {
      return std::unexpected(std::move(fn.error()));
    }
    fn->linkage = mir::LinkageOf(
        *mir_, mir::TypeOperationSource{
                   .type = mir_type, .answers = operation.answers});
    out_.functions.Define(function, *std::move(fn));
  }
  return !pending.empty();
}

auto UnitLowerer::Finish() && -> lir::CompilationUnit {
  // Every body is lowered by now, so every constant one names is in the pool.
  // A definition every body states alike reads its own artifact's copy of
  // each, which holds what every other copy holds.
  std::vector<lir::ConstantDecl> constants;
  constants.reserve(mir_->constants.size());
  for (const mir::ConstantId id : mir_->constants.Ids()) {
    const mir::ConstantDecl& constant = mir_->constants.Get(id);
    constants.push_back(
        lir::ConstantDecl{
            .type = TranslateType(constant.type),
            .value = LowerConstant(constant.type, constant.value)});
  }
  out_.constants = base::Translation<lir::ConstantId, lir::ConstantDecl>(
      mir_->constants.size(), std::move(constants));
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

auto UnitLowerer::NamespaceCallableSymbolOf(mir::CallableId id) const
    -> std::string {
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
          // Named by the callback it answers, as a class's body answering it
          // is.
          [&](const mir::ReachedAsLibraryCallback& r) {
            return lir::NamespaceCallableSymbol(
                out_.body_name, lir::SymbolPart::LibraryCallback(r.callback));
          },
          [&](const mir::ReachedByPosition& r) {
            return lir::NamespaceCallableSymbol(
                out_.body_name, lir::SymbolPart::Ordinal(r.position.value));
          }},
      mir::NamespaceReachOf(*mir_, id));
}

auto UnitLowerer::BaseType(const mir::ClassRef& base) -> lir::TypeId {
  const auto library_class = [&](support::LibraryDecl decl) {
    return out_.types.Intern(
        lir::Type{lir::LibraryType{.decl = decl, .args = {}}});
  };
  return std::visit(
      Overloaded{
          [&](const mir::ClassId& declared) {
            return ClassValueType(declared);
          },
          [&](const mir::ObjectTreeRootRef&) {
            return library_class(support::LibraryDecl::kScope);
          },
          [&](const mir::ClassObjectRootRef&) {
            return library_class(support::LibraryDecl::kClassObject);
          },
          [&](const mir::ObjectRootRef&) {
            return library_class(support::LibraryDecl::kObjectRoot);
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
          },
          [](mir::DeclaringInstanceName) {
            return lir::SymbolPart::DeclaringInstance();
          }},
      name);
}

}  // namespace

auto UnitLowerer::ClassBodySymbolOf(mir::CallableId id) const -> std::string {
  const auto* owner = std::get_if<mir::DeclaredByClass>(&mir_->OwnerOf(id));
  if (owner == nullptr) {
    throw InternalError(
        std::format(
            "mir_to_lir: callable {} is named as a body of a class, which no "
            "class declares -- please report this as a bug",
            id.value));
  }
  const mir::ClassId declaring = owner->cls;
  const mir::Class& cls = mir_->GetClass(declaring);
  // An entry is named by the method it enters, which a class of another unit
  // whose table holds it names it by.
  for (const mir::MethodEntry& entry : cls.entries) {
    if (entry.entry == id) {
      return lir::MethodEntrySymbol(
          ClassQualifier(cls), ClassPart(declaring),
          DispatchNamePart(mir::DispatchNameOf(*mir_, entry.method)));
    }
  }
  // A body the library calls is named by the callback it answers, which a
  // derived class of another unit calls it by.
  const std::optional<support::LibraryCallback> callback =
      cls.CallbackAnsweredBy(id);
  return lir::ClassCallableSymbol(
      ClassQualifier(cls), ClassPart(declaring),
      callback.has_value()
          ? lir::SymbolPart::LibraryCallback(*callback)
          : lir::SymbolPartOf(
                mir::NameOf(cls.named_callables, id), owner->ordinal.value));
}

auto UnitLowerer::EntryBody(mir::CallableId method, lir::TypeId prototype)
    -> lir::TableBody {
  const mir::ClassId declaring = mir::DeclaringClassOf(*mir_, method);
  const mir::Class& owner = mir_->GetClass(declaring);
  // The entry a class of this unit gives a method is a callable of the class,
  // entered as any other is.
  if (const std::optional<mir::CallableId> entry = owner.EntryOf(method)) {
    return ReferentOfCallable(*entry, prototype);
  }
  // A class another unit defines states no entry, and every unit names the
  // entry of each of its methods by the method it enters.
  if (!owner.defining_unit.has_value()) {
    throw InternalError(
        "mir_to_lir: a method of a class of this unit is reached through an "
        "entry the class does not have -- please report this as a bug");
  }
  const std::string_view qualifier = ClassQualifier(owner);
  lir::SymbolPart class_part = ClassPart(declaring);
  return lir::ExternalFunctionReferent{
      .symbol = lir::MethodEntrySymbol(
          qualifier, std::move(class_part),
          DispatchNamePart(mir::DispatchNameOf(*mir_, method))),
      .prototype = prototype};
}

void UnitLowerer::TakeDispatchOrdinals(
    mir::ClassId cls,
    std::vector<std::optional<lir::DispatchOrdinal>>& ordinals) const {
  // Which behavior a callable introduces is independent of whether this
  // unit defines it: a callable may have a body and introduce nothing,
  // introduce something and have no body (LRM 8.21), both, or neither.
  const std::vector<mir::CallableId> introduced =
      mir::IntroducedBehaviors(*mir_, cls);
  for (std::uint32_t at = 0; at < introduced.size(); ++at) {
    ordinals.at(introduced[at].value) = lir::DispatchOrdinal{.value = at};
  }
}

auto UnitLowerer::LowerClass(mir::ClassId owner, const mir::Class& cls)
    -> diag::Result<lir::Class> {
  lir::Class out;
  out.identity = cls.identity;
  out.linkage = mir::LinkageOf(*mir_, owner);
  out.base = cls.base.transform(
      [&](const mir::ClassRef& base) { return BaseType(base); });

  for (const mir::FieldId id : cls.fields.Ids()) {
    out.members.push_back(
        lir::Member{.type = TranslateType(cls.fields.Get(id).type)});
  }
  out.dispatch = LowerDispatch(cls);
  out.interface_tables = LowerInterfaceTables(cls);

  // A class's bodies become functions of the program, and a body's own name is
  // unique only within its class -- so the class qualifies it, being itself
  // unique program-wide.
  const mir::DeclaredCallables& declared = mir_->CallablesOf(owner);
  if (declared.constructor.has_value() &&
      functions_.Get(*declared.constructor).has_value()) {
    const lir::FunctionId function = FunctionOf(*declared.constructor);
    auto constructor =
        FunctionLowerer(
            *this, mir_->callables.Get(*declared.constructor).code,
            ConstructorSymbolOf(owner))
            .Run();
    if (!constructor) {
      return std::unexpected(std::move(constructor.error()));
    }
    constructor->linkage = out.linkage;
    out_.functions.Define(function, *std::move(constructor));
  }

  // Only a callable this unit defines becomes a function: a pure virtual has
  // no implementation here (LRM 8.21).
  for (const mir::CallableId cid : declared.callables) {
    const std::optional<lir::FunctionId>& body = functions_.Get(cid);
    if (!body.has_value()) {
      continue;
    }
    const mir::CallableCode& code = mir_->callables.Get(cid).code;
    std::string symbol = ClassBodySymbolOf(cid);
    auto fn = FunctionLowerer(*this, code, std::move(symbol)).Run();
    if (!fn) {
      return std::unexpected(std::move(fn.error()));
    }
    fn->linkage = out.linkage;
    out_.functions.Define(*body, *std::move(fn));
  }
  if (cls.definition.has_value()) {
    const mir::TypeId definition = mir::LibraryTypeOf(
        mir_->types, support::LibraryDecl::kObjectDefinition);
    out_.definitions.push_back(
        lir::GlobalConstant{
            .symbol = DefinitionSymbolOf(owner),
            .linkage = out.linkage,
            .type = TranslateType(definition),
            .initializer = LowerConstant(definition, *cls.definition)});
  }
  return out;
}

auto UnitLowerer::DefinitionSymbolOf(mir::ClassId of) const -> std::string {
  const mir::Class& cls = mir_->GetClass(of);
  const std::string_view qualifier = ClassQualifier(cls);
  lir::SymbolPart class_part = ClassPart(of);
  return lir::ClassDefinitionSymbol(qualifier, std::move(class_part));
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
  const std::string_view qualifier = ClassQualifier(cls);
  lir::SymbolPart class_part = ClassPart(ref.owner);
  return lir::StaticPropertySymbol(
      qualifier, std::move(class_part),
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

auto UnitLowerer::LowerDeclaredClass(mir::ClassId id, const mir::Class& cls)
    -> lir::Class {
  lir::Class record{
      .defining_unit = cls.defining_unit,
      .identity = cls.identity,
      .linkage = mir::LinkageOf(*mir_, id),
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

auto UnitLowerer::LowerConstant(
    mir::TypeId type, const mir::ConstantValue& value) -> lir::Constant {
  const mir::Type& held = mir_->types.Get(type);
  return std::visit(
      Overloaded{
          [](const mir::IntegralValue& v) -> lir::Constant {
            return {lir::ConstantIntegral{
                .value = lir::IntegralConstant{
                    .value_words = v.planes.value_words,
                    .state_words = v.planes.state_words}}};
          },
          [](const mir::MachineIntValue& v) -> lir::Constant {
            return {lir::ConstantInt{.value = v.value}};
          },
          [](const mir::NullValue&) -> lir::Constant {
            return {lir::ConstantNull{}};
          },
          [](const mir::CStringValue& v) -> lir::Constant {
            return {
                lir::ConstantPointer{.to = lir::TextReferent{.text = v.text}}};
          },
          [&](const mir::PointerValue& v) -> lir::Constant {
            return {lir::ConstantPointer{.to = ReferentOf(v.to, v.type)}};
          },
          // A structure's parts are of the types the library declares its
          // members at.
          [&](const mir::RecordValue& v) -> lir::Constant {
            const support::LibraryDecl decl = held.Get<mir::LibraryType>().decl;
            const std::span<const mir::TypeId> members =
                *mir_->LibraryRecordMembers(decl);
            lir::ConstantRecord record{.decl = decl, .parts = {}};
            record.parts.reserve(v.parts.size());
            for (std::size_t i = 0; i < v.parts.size(); ++i) {
              record.parts.push_back(LowerConstant(members[i], v.parts[i]));
            }
            return {std::move(record)};
          },
          [&](const mir::ArrayValue& v) -> lir::Constant {
            const mir::TypeId element =
                held.Get<mir::MachineArrayType>().element;
            lir::ConstantArray array;
            array.elements.reserve(v.elements.size());
            for (const mir::ConstantValue& part : v.elements) {
              array.elements.push_back(LowerConstant(element, part));
            }
            return {std::move(array)};
          },
          [](const mir::SpanValue& v) -> lir::Constant {
            return {lir::ConstantSpan{.array = TranslateConstant(v.array)}};
          }},
      value.value);
}

auto UnitLowerer::ReferentOf(
    const mir::ReferenceTarget& target, mir::TypeId named) -> lir::Referent {
  return std::visit(
      Overloaded{
          [](const mir::LocalRef&) -> lir::Referent {
            throw InternalError(
                "mir_to_lir: the address of a local is held where only an "
                "address fixed before the program runs may be -- please "
                "report this as a bug");
          },
          [](const mir::ConstantRef& r) -> lir::Referent {
            return lir::ConstantReferent{
                .constant = TranslateConstant(r.constant)};
          },
          [&](const mir::DefinitionRef& r) -> lir::Referent {
            return lir::DefinitionReferent{.cls = LirClass(r.of)};
          },
          [&](const mir::StaticVariableRef& r) -> lir::Referent {
            return lir::SharedStorageReferent{
                .symbol = StaticVariableSymbolOf(r.variable)};
          },
          [&](const mir::StaticPropertyRef& r) -> lir::Referent {
            return lir::SharedStorageReferent{
                .symbol = StaticPropertySymbolOf(r)};
          },
          // A body this unit defines is its function; a body of another
          // unit's namespace is reached by the symbol that unit emits it
          // under, as a call reaches it.
          [&](const mir::FunctionRef& f) -> lir::Referent {
            return lir::ReferentOf(
                ReferentOfCallable(f.callable, TranslateType(named)));
          },
          [&](const mir::ExternalObjectEntryRef& e) -> lir::Referent {
            return lir::ExternalFunctionReferent{
                .symbol = lir::ObjectEntrySymbol(
                    e.unit_name, e.body_name, BlockOrdinalOf(e.block)),
                .prototype = TranslateType(named)};
          },
          [&](const mir::DispatchEntryRef& e) -> lir::Referent {
            return lir::ReferentOf(EntryBody(e.method, TranslateType(named)));
          },
          [&](const mir::DestructorRef& d) -> lir::Referent {
            return lir::DestructorReferent{.cls = LirClass(d.of)};
          },
          [&](const mir::DispatchTableRef& t) -> lir::Referent {
            return lir::DispatchTableReferent{.cls = LirClass(t.of)};
          },
          [&](const mir::InterfaceTableRef& t) -> lir::Referent {
            return lir::InterfaceTableReferent{
                .cls = LirClass(t.of), .iface = LirClass(t.iface)};
          }},
      target);
}

auto UnitLowerer::LirClass(mir::ClassId cls) const -> lir::ClassId {
  return class_identities_.Get(cls);
}

auto UnitLowerer::SlotRef(const mir::VirtualSlot& slot)
    -> lir::StatedDispatchRef {
  const std::optional<lir::DispatchOrdinal>& ordinal =
      ordinals_.Get(slot.introduced_by);
  // A dispatch names the callable that introduced the behavior, which is the
  // one identity every class answering it agrees on, so a callable naming no
  // introduction is a producer that built the slot identity wrongly.
  if (!ordinal.has_value()) {
    throw InternalError(
        "mir_to_lir: a dispatch names a callable that introduces no behavior");
  }
  return lir::StatedDispatchRef{
      .introduced_by =
          ClassValueType(mir::DeclaringClassOf(*mir_, slot.introduced_by)),
      .ordinal = *ordinal};
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
            .answered_by =
                dispatched.answered_by.transform([&](mir::CallableId body) {
                  return EntryBody(body, erased);
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
    for (const std::optional<mir::CallableId>& body : implemented.answered_by) {
      table.answered_by.push_back(body.transform([&](mir::CallableId answered) {
        return EntryBody(answered, erased);
      }));
    }
    tables.push_back(std::move(table));
  }
  return tables;
}

auto UnitLowerer::FunctionOf(mir::CallableId callable) const
    -> lir::FunctionId {
  const std::optional<lir::FunctionId>& fn = functions_.Get(callable);
  if (!fn.has_value()) {
    throw InternalError(
        std::format(
            "mir_to_lir: callable {} is no function of this unit, which "
            "defines no body for it -- please report this as a bug",
            callable.value));
  }
  return *fn;
}

auto UnitLowerer::EnteredAs(mir::CallableId callable) const -> CallableEntered {
  const mir::CallableDecl& decl = mir_->callables.Get(callable);
  return std::visit(
      Overloaded{
          // A callable this unit defines is its function, whoever declares it.
          [&](const mir::DefinedHere&) -> CallableEntered {
            return lir::FunctionTarget{.function = FunctionOf(callable)};
          },
          // A name in the DPI-C name space is reached as an external-linkage
          // symbol the execution session resolves (LRM 35.4); nothing about
          // the call names a unit or a class.
          [&](const mir::DefinedByForeignCode&) -> CallableEntered {
            return lir::ForeignTarget{
                .symbol = std::get<mir::DeclaredByNamespace>(decl.owner)
                              .foreign->foreign_name};
          },
          // Reached by the symbol the unit defining it emits it under.
          [&](const mir::DefinedByAnotherUnit&) -> CallableEntered {
            return lir::SymbolTarget{.symbol = SymbolOfElsewhere(callable)};
          },
          [&](const mir::LeftAbstract&) -> CallableEntered {
            throw InternalError(
                std::format(
                    "mir_to_lir: callable {}, which its class leaves to what "
                    "extends it, is entered other than by dispatch -- please "
                    "report this as a bug",
                    callable.value));
          }},
      mir::FormOf(decl));
}

auto UnitLowerer::SymbolOfElsewhere(mir::CallableId callable) const
    -> std::string {
  return std::visit(
      Overloaded{
          [&](const mir::DeclaredByClass&) {
            return ClassBodySymbolOf(callable);
          },
          [&](const mir::ConstructsClass& owner) {
            return ConstructorSymbolOf(owner.cls);
          },
          [&](const mir::DeclaredByNamespace&) {
            return NamespaceCallableSymbolOf(callable);
          },
          [&](const mir::DeclaredByRecord&) {
            return RecordMethodSymbolOf(callable);
          },
          [&](const mir::DeclaredByClosure&) -> std::string {
            throw InternalError(
                std::format(
                    "mir_to_lir: the body of a closure, callable {}, is "
                    "defined elsewhere -- please report this as a bug",
                    callable.value));
          }},
      mir_->OwnerOf(callable));
}

auto UnitLowerer::ReferentOfCallable(
    mir::CallableId callable, lir::TypeId prototype) const -> lir::TableBody {
  return std::visit(
      Overloaded{
          [](const lir::FunctionTarget& target) -> lir::TableBody {
            return lir::FunctionReferent{.function = target.function};
          },
          [&](lir::SymbolTarget target) -> lir::TableBody {
            return lir::ExternalFunctionReferent{
                .symbol = std::move(target.symbol), .prototype = prototype};
          },
          [&](const lir::ForeignTarget&) -> lir::TableBody {
            throw InternalError(
                std::format(
                    "mir_to_lir: the address of callable {}, a function the "
                    "foreign program defines, is taken -- please report this "
                    "as a bug",
                    callable.value));
          }},
      EnteredAs(callable));
}

auto UnitLowerer::CallTargetOf(mir::CallableId callable) const
    -> lir::CallTarget {
  return std::visit(
      Overloaded{
          [](lir::FunctionTarget target) { return lir::CallTarget{target}; },
          [](lir::SymbolTarget target) {
            return lir::CallTarget{std::move(target)};
          },
          [](lir::ForeignTarget target) {
            return lir::CallTarget{std::move(target)};
          }},
      EnteredAs(callable));
}

auto UnitLowerer::ConstructorSymbolOf(mir::ClassId cls) const -> std::string {
  return lir::ConstructorSymbol(
      ClassQualifier(mir_->GetClass(cls)), ClassPart(cls));
}

auto UnitLowerer::RecordMethodSymbolOf(mir::CallableId method) const
    -> std::string {
  const auto* owner =
      std::get_if<mir::DeclaredByRecord>(&mir_->OwnerOf(method));
  if (owner == nullptr) {
    throw InternalError(
        std::format(
            "mir_to_lir: callable {} is named as a method of a record, which "
            "no record declares -- please report this as a bug",
            method.value));
  }
  const mir::TypeDeclarationRef declaration =
      mir::RecordDeclarationOf(*mir_, owner->record);
  return lir::RecordMethodSymbol(
      declaration.unit_name, declaration.path, owner->answers);
}

auto UnitLowerer::ConstructionCallTargetOf(mir::ClassId cls) const
    -> lir::CallTarget {
  const std::optional<mir::CallableId>& constructor =
      mir_->CallablesOf(cls).constructor;
  if (!constructor.has_value()) {
    throw InternalError(
        "mir_to_lir: a construction enters a class no object is built of -- "
        "please report this as a bug");
  }
  return CallTargetOf(*constructor);
}

auto UnitLowerer::ClosureDeclaration(mir::ClosureId closure) const
    -> lir::ClosureId {
  return closure_identities_.Get(closure);
}

auto UnitLowerer::RecordDeclaration(mir::RecordId record) const
    -> lir::RecordId {
  return record_identities_.Get(record);
}

auto UnitLowerer::TypeOperationFunction(
    mir::TypeId type, support::ValueOperation answers) -> lir::FunctionId {
  const lir::TypeOperation operation{
      .type = TranslateType(type), .answers = answers};
  const auto found = out_.type_operations.find(operation);
  if (found != out_.type_operations.end()) {
    if (!found->second.has_value()) {
      RefuseOperationNotGiven(answers);
    }
    return *found->second;
  }
  const lir::FunctionId function = out_.functions.Declare();
  out_.type_operations.emplace(operation, function);
  MarkOperationUsed(operation);
  return function;
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
      lir::Type{lir::ObjectType{.class_id = LirClass(cls)}});
}

}  // namespace lyra::lowering::mir_to_lir
