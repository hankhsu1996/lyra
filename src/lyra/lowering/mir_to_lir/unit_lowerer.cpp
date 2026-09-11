#include "lyra/lowering/mir_to_lir/unit_lowerer.hpp"

#include <cstddef>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/class_id.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/declaration_name.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/function_id.hpp"
#include "lyra/lowering/mir_to_lir/function_lowerer.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/packed_type_descriptor.hpp"
#include "lyra/mir/static_variable_id.hpp"

namespace lyra::lowering::mir_to_lir {

auto StaticVariableSymbol(
    std::string_view unit_name, std::string_view variable_name) -> std::string {
  return std::format("{}.{}", unit_name, variable_name);
}

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

  // What each unit this one references promised about its object, taken whole
  // and before any body lowers: a member step names a position counted out of
  // that whole list. Every identity comes first, because one record's members
  // may name another of them and have to resolve to an identity that exists,
  // whichever order the two were reached in.
  external_unit_object_identities_ =
      base::Translation<mir::ExternalUnitObjectId, lir::ExternalUnitObjectId>{
          mir_->external_unit_objects.size()};
  for (std::size_t i = 0; i < mir_->external_unit_objects.size(); ++i) {
    external_unit_object_identities_.Append(
        out_.external_unit_objects.Declare());
  }
  for (const mir::ExternalUnitObjectId id : mir_->external_unit_objects.Ids()) {
    out_.external_unit_objects.Define(
        external_unit_object_identities_.Get(id),
        LowerExternalUnitObject(mir_->external_unit_objects.Get(id)));
  }

  // What each class of another unit promised, taken whole: a property step on
  // one names a slot counted out of that whole list.
  for (const mir::ExternalClass& cls : mir_->external_classes) {
    lir::ExternalClass record{
        .unit_name = cls.unit_name,
        .class_name = cls.class_name,
        .base = {},
        .members = {}};
    if (cls.base.has_value()) {
      record.base = lir::CrossUnitBase{
          .unit_name = cls.base->unit_name, .class_name = cls.base->class_name};
    }
    record.members.reserve(cls.fields.size());
    for (const mir::FieldId id : cls.fields.Ids()) {
      const mir::FieldDecl& field = cls.fields.Get(id);
      record.members.push_back(
          lir::Member{.name = field.name, .type = TranslateType(field.type)});
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

  std::vector<lir::StructId> structs;
  structs.reserve(mir_->structs.size());
  for (std::size_t i = 0; i < mir_->structs.size(); ++i) {
    structs.push_back(out_.structs.Declare());
  }
  struct_identities_ = {mir_->structs.size(), std::move(structs)};

  // A variable the unit's namespace owns -- a package's (LRM 26.2), a
  // `$unit` scope's (LRM 3.12.1) -- is one cell for the whole program that no
  // instance holds, so the unit publishes it under a symbol instead. Every
  // reader reaches it by that name, this unit's own bodies included, since a
  // namespace has no instance for a receiver to arrive through.
  for (const mir::StaticVariableId id : mir_->static_variables.Ids()) {
    const mir::StaticVariableDecl& variable = mir_->static_variables.Get(id);
    out_.static_storage.push_back(
        lir::StaticStorage{
            .symbol = StaticVariableSymbol(mir_->name, variable.name),
            .type = TranslateType(variable.type)});
  }

  // A callable the unit's namespace owns -- a package's own body (LRM 26.3) --
  // is a body like any other and becomes a function of the unit. Only one this
  // program defines does: a DPI-C import is reached as a foreign symbol and
  // declares no body here.
  for (const mir::CallableId id : mir_->callables.Ids()) {
    const mir::CallableDecl& callable = mir_->callables.Get(id);
    if (!callable.code.body.has_value()) {
      continue;
    }
    auto fn =
        FunctionLowerer(*this, callable.code, UnitCallableSymbol(callable))
            .Run();
    if (!fn) {
      return std::unexpected(std::move(fn.error()));
    }
    out_.functions.Add(*std::move(fn));
  }

  for (const mir::ClassId id : mir_->classes.Ids()) {
    auto cls = LowerClass(id, mir_->GetClass(id));
    if (!cls) {
      return std::unexpected(std::move(cls.error()));
    }
    out_.classes.Define(class_identities_.Get(id).lir_class, *std::move(cls));
  }
  if (mir_->root.has_value()) {
    out_.root = class_identities_.Get(*mir_->root).lir_class;
  }

  // A closure's captures are the storage its values own, and its invoke is a
  // function like any other body's, reading them through the receiver it takes.
  for (const mir::ClosureId id : mir_->closures.Ids()) {
    const mir::ClosureDecl& decl = mir_->GetClosure(id);
    lir::Closure closure;
    closure.name = ClosureSymbol(id);
    closure.captures.reserve(decl.fields.size());
    for (const mir::FieldId field : decl.fields.Ids()) {
      closure.captures.push_back(
          lir::Member{
              .name = decl.fields.Get(field).name,
              .type = TranslateType(decl.fields.Get(field).type)});
    }
    closure.invoke = ClosureFunction(id);
    auto fn =
        FunctionLowerer(*this, decl, std::format("{}.invoke", closure.name))
            .Run();
    if (!fn) {
      return std::unexpected(std::move(fn.error()));
    }
    out_.functions.Define(closure.invoke, *std::move(fn));
    out_.closures.Define(ClosureDeclaration(id), std::move(closure));
  }

  // A struct's fields are the storage its values own. Nothing else of it
  // lowers: it declares no body, so there is no function to reserve and none to
  // fill in beside the declaration.
  for (const mir::StructId id : mir_->structs.Ids()) {
    const mir::StructDecl& decl = mir_->GetStruct(id);
    lir::Struct record;
    record.name = StructSymbol(id);
    record.fields.reserve(decl.fields.size());
    for (const mir::FieldId field : decl.fields.Ids()) {
      record.fields.push_back(
          lir::Member{
              .name = decl.fields.Get(field).name,
              .type = TranslateType(decl.fields.Get(field).type)});
    }
    out_.structs.Define(StructDeclaration(id), std::move(record));
  }

  // Descriptions are lowered after the bodies: each one translates types of
  // its own, and what receives them below is indexed by LIR type, so its
  // extent is settled only once nothing more will translate. Building a value
  // is an instruction sequence at this layer, so a description is a function
  // here, and the type it describes is what reaches it.
  std::vector<std::pair<lir::TypeId, lir::FunctionId>> described;
  for (const mir::TypeId id : mir::DescribedPackedTypes(*mir_)) {
    const mir::PackedTypeDescription description =
        mir::DescribePackedType(*mir_, id);
    // A description is unique only within its unit, while the whole program
    // links into one name space, so the unit qualifies it -- the same reason a
    // namespace callable is qualified.
    auto fn = FunctionLowerer::LowerDescription(
        *this, description,
        std::format("{}.{}", mir_->name, mir::PackedTypeDescriptionName(id)));
    if (!fn) {
      return std::unexpected(std::move(fn.error()));
    }
    described.emplace_back(
        TranslateType(id), out_.functions.Add(*std::move(fn)));
  }
  std::vector<std::optional<lir::FunctionId>> initializers(
      out_.types.size(), std::nullopt);
  for (const auto& [type, initializer] : described) {
    initializers[type.value] = initializer;
  }
  out_.packed_type_initializers = {out_.types.size(), std::move(initializers)};

  // A type reached during lowering had no LIR mirror; surface it now, once the
  // whole unit has been walked, rather than from the non-failing translator.
  if (type_error_.has_value()) {
    return std::unexpected(std::move(*type_error_));
  }
  return std::move(out_);
}

auto UnitLowerer::UnitCallableSymbol(const mir::CallableDecl& callable) const
    -> std::string {
  // A foreign name is program-global and stands on its own (LRM 35.4). Every
  // other namespace callable is unique only within its unit, while the whole
  // program links into one name space, so the unit qualifies it -- the same
  // reason a class qualifies the bodies it owns.
  if (callable.foreign.has_value()) {
    return callable.LinkedName();
  }
  return std::format("{}.{}", mir_->name, callable.name);
}

auto UnitLowerer::ClassSymbol(const mir::Class& cls) const -> std::string {
  return lir::ClassLinkageName(mir_->name, cls.name);
}

auto UnitLowerer::ClosureSymbol(mir::ClosureId closure) const -> std::string {
  // A closure's ordinal is counted within its unit while the whole program
  // links into one name space, so the unit qualifies it -- the same reason a
  // class and a namespace callable are qualified.
  return std::format("{}.closure_{}", mir_->name, closure.value);
}

auto UnitLowerer::StructSymbol(mir::StructId record) const -> std::string {
  // A struct's name is unique within the unit that declares it while the whole
  // program links into one name space, so the unit qualifies it -- the same
  // reason a class and a closure are qualified.
  return std::format("{}.{}", mir_->name, mir_->GetStruct(record).name);
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
  ClassIdentities identities{
      .lir_class = out_.classes.Declare(),
      .constructor = out_.functions.Declare(),
      .methods = {},
      .ordinals = {},
      .introduces = {}};
  for (const mir::CallableId callable : cls.callables.Ids()) {
    const mir::CallableDecl& decl = cls.callables.Get(callable);
    const std::optional<lir::FunctionId> body =
        decl.code.body.has_value() ? std::optional{out_.functions.Declare()}
                                   : std::nullopt;
    std::optional<lir::DispatchOrdinal> ordinal;
    if (mir::IntroducesSlot(decl.virtual_dispatch)) {
      ordinal = lir::DispatchOrdinal{
          .value = static_cast<std::uint32_t>(identities.introduces.size())};
      identities.introduces.push_back(body);
    }
    methods.push_back(body);
    ordinals.push_back(ordinal);
  }
  identities.methods = {cls.callables.size(), std::move(methods)};
  identities.ordinals = {cls.callables.size(), std::move(ordinals)};
  return identities;
}

auto UnitLowerer::LowerExternalUnitObject(const mir::ExternalUnitObject& object)
    -> lir::ExternalUnitObject {
  lir::ExternalUnitObject out{
      .unit_name = object.unit_name,
      .class_name = object.class_name,
      .members = {}};
  out.members.reserve(object.fields.size());
  for (const mir::FieldDecl& field : object.fields) {
    out.members.push_back(
        lir::Member{.name = field.name, .type = TranslateType(field.type)});
  }
  return out;
}

auto UnitLowerer::LowerClass(mir::ClassId owner, const mir::Class& cls)
    -> diag::Result<lir::Class> {
  lir::Class out;
  out.name = ClassSymbol(cls);
  if (cls.base.has_value()) {
    out.base = LowerBase(*cls.base);
  }

  for (const mir::FieldId id : cls.fields.Ids()) {
    const mir::FieldDecl& field = cls.fields.Get(id);
    out.members.push_back(
        lir::Member{.name = field.name, .type = TranslateType(field.type)});
  }

  // The behaviors the class introduces were settled with the ordinals naming
  // them, so what is left is to hand the list over.
  const ClassIdentities& identities = class_identities_.Get(owner);
  out.introduces = identities.introduces;

  // A class's bodies become functions of the program, and a body's own name is
  // unique only within its class -- so the class symbol qualifies it, being
  // itself unique program-wide.
  auto constructor =
      FunctionLowerer(*this, cls, lir::ConstructorSymbolName(out.name)).Run();
  if (!constructor) {
    return std::unexpected(std::move(constructor.error()));
  }
  out_.functions.Define(identities.constructor, *std::move(constructor));
  out.constructor = identities.constructor;

  // Which of the class's bodies a hierarchical name may end at, by the
  // identifier such a name spells. That identifier is the whole of the
  // identity: it is what the scope is asked for at run time and what the
  // declaration was written under, so the two meet on it and on nothing else.
  std::unordered_set<std::string_view> published;
  for (const mir::AbiAdapterId aid : cls.abi_adapters.Ids()) {
    const mir::AbiAdapter& adapter = cls.abi_adapters.Get(aid);
    if (const auto* entry =
            std::get_if<mir::SubroutineEntry>(&adapter.published)) {
      published.insert(entry->name);
    }
  }

  // Only a callable this program defines becomes a function: a DPI-C import is
  // reached as a foreign symbol and a pure virtual has no implementation here
  // (LRM 8.21). Which behavior a callable takes over is the other question, and
  // the two do not gate each other: a body may take over nothing, and only a
  // body can take one over.
  for (const mir::CallableId cid : cls.callables.Ids()) {
    const mir::CallableDecl& callable = cls.callables.Get(cid);
    const std::optional<lir::FunctionId>& body = identities.methods.Get(cid);
    if (body.has_value()) {
      auto fn = FunctionLowerer(
                    *this, callable.code,
                    std::format("{}.{}", out.name, callable.name))
                    .Run();
      if (!fn) {
        return std::unexpected(std::move(fn.error()));
      }
      out_.functions.Define(*body, *std::move(fn));
      if (published.contains(callable.name)) {
        out.subroutines.push_back(
            lir::PublishedSubroutine{.name = callable.name, .body = *body});
      }
    }
    if (const std::optional<lir::DispatchTakeover> taken =
            TakenOver(callable, body)) {
      out.takeovers.push_back(*taken);
    }
  }
  return out;
}

auto UnitLowerer::TakenOver(
    const mir::CallableDecl& callable,
    const std::optional<lir::FunctionId>& body)
    -> std::optional<lir::DispatchTakeover> {
  // Taking a behavior over without a body would leave it exactly as the
  // lineage already had it (LRM 8.21 again, one abstract class extending
  // another), so it states nothing.
  if (!callable.virtual_dispatch.has_value() || !body.has_value()) {
    return std::nullopt;
  }
  return std::visit(
      Overloaded{
          [](const mir::IntroducesVirtualSlot&)
              -> std::optional<lir::DispatchTakeover> { return std::nullopt; },
          [&](const mir::OverridesIntraUnitSlot& taken)
              -> std::optional<lir::DispatchTakeover> {
            return lir::DispatchTakeover{
                .method = MethodRef(taken.slot_owner, taken.slot_id),
                .body = *body};
          },
          [&](const mir::OverridesExternalSlot& taken)
              -> std::optional<lir::DispatchTakeover> {
            return lir::DispatchTakeover{
                .method =
                    lir::DispatchRef{
                        .introduced_by = ExternalClassValueType(
                            taken.unit_name, taken.class_name),
                        .ordinal = lir::DispatchOrdinal{taken.ordinal.value}},
                .body = *body};
          }},
      *callable.virtual_dispatch);
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
    -> lir::DispatchRef {
  const std::optional<lir::DispatchOrdinal>& ordinal =
      class_identities_.Get(owner).ordinals.Get(callable);
  // A dispatch names the callable that introduced the behavior, which is the
  // one identity every class answering it agrees on, so a callable naming no
  // introduction is a producer that built the slot identity wrongly.
  if (!ordinal.has_value()) {
    throw InternalError(
        "mir_to_lir: a dispatch names a callable that introduces no behavior");
  }
  return lir::DispatchRef{
      .introduced_by = ClassValueType(owner), .ordinal = *ordinal};
}

auto UnitLowerer::ConstructorFunction(mir::ClassId cls) const
    -> lir::FunctionId {
  return class_identities_.Get(cls).constructor;
}

auto UnitLowerer::ClosureFunction(mir::ClosureId closure) const
    -> lir::FunctionId {
  return closure_identities_.Get(closure).invoke;
}

auto UnitLowerer::ClosureDeclaration(mir::ClosureId closure) const
    -> lir::ClosureId {
  return closure_identities_.Get(closure).declaration;
}

auto UnitLowerer::StructDeclaration(mir::StructId record) const
    -> lir::StructId {
  return struct_identities_.Get(record);
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

auto UnitLowerer::StructValueType(mir::StructId record) -> lir::TypeId {
  return out_.types.Intern(
      lir::Type{lir::StructType{.struct_id = StructDeclaration(record)}});
}

auto UnitLowerer::ExternalUnitObjectValueType(mir::ExternalUnitObjectId object)
    -> lir::TypeId {
  return out_.types.Intern(
      lir::Type{lir::ExternalUnitObjectType{
          .object = external_unit_object_identities_.Get(object)}});
}

auto UnitLowerer::ExternalClassValueType(
    const std::string& unit_name, const std::string& class_name) const
    -> lir::TypeId {
  return out_.types.Intern(
      lir::Type{lir::CrossUnitClassType{
          .unit_name = unit_name, .class_name = class_name}});
}

auto UnitLowerer::PromisedClass(
    const std::string& unit_name, const std::string& class_name) const
    -> const mir::ExternalClass& {
  const mir::ExternalClass* promised =
      mir::FindExternalClass(Mir().external_classes, unit_name, class_name);
  if (promised == nullptr) {
    throw InternalError(
        "mir_to_lir: a reference names a class of another unit that no "
        "consumed promise describes");
  }
  return *promised;
}

auto UnitLowerer::ProductOf(std::vector<lir::TypeId> components)
    -> lir::TypeId {
  if (const auto it = product_memo_.find(components);
      it != product_memo_.end()) {
    return it->second;
  }
  const lir::TypeId id =
      out_.types.Intern(lir::Type{lir::TupleType{.elements = components}});
  product_memo_.emplace(std::move(components), id);
  return id;
}

auto UnitLowerer::LowerBase(const mir::ClassRef& base) const -> lir::Base {
  return std::visit(
      Overloaded{
          [this](const mir::IntraUnitClassRef& i) -> lir::Base {
            return lir::Base{lir::IntraUnitBase{
                .class_id = class_identities_.Get(i.class_id).lir_class}};
          },
          [](const mir::CrossUnitClassRef& e) -> lir::Base {
            return lir::Base{lir::CrossUnitBase{
                .unit_name = e.unit_name, .class_name = e.class_name}};
          },
          [](const mir::RuntimeClassRef& e) -> lir::Base {
            return lir::Base{lir::RuntimeBase{.symbol = e.symbol}};
          }},
      base);
}

}  // namespace lyra::lowering::mir_to_lir
