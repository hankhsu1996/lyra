#include "lyra/lowering/mir_to_lir/unit_lowerer.hpp"

#include <cstddef>
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
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant_id.hpp"
#include "lyra/mir/static_variable_id.hpp"
#include "lyra/mir/struct_decl.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_descriptor_id.hpp"
#include "lyra/mir/value_build.hpp"
#include "lyra/support/runtime_class.hpp"

namespace lyra::lowering::mir_to_lir {

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

  // Which unit and class each unit this one references promised of its object,
  // taken before any body lowers, since a body naming one resolves against an
  // identity that has to exist by then. A record names no other, so each is
  // minted carrying its value.
  external_unit_object_identities_ =
      base::Translation<mir::ExternalUnitObjectId, lir::ExternalUnitObjectId>{
          mir_->external_unit_objects.size()};
  for (const mir::ExternalUnitObjectId id : mir_->external_unit_objects.Ids()) {
    const mir::ExternalUnitObject& object = mir_->external_unit_objects.Get(id);
    external_unit_object_identities_.Append(out_.external_unit_objects.Add(
        lir::ExternalUnitObject{
            .unit_name = object.unit_name, .class_name = object.class_name}));
  }

  // What each class of another unit promised, taken whole: a property step on
  // one names a slot counted out of the published list, and a class extending
  // one is placed after all of its storage and fills its table from all of its
  // bodies. A body of it is reached by the symbol its unit emits it under,
  // composed from the names that unit composed it from.
  for (const mir::ExternalClass& cls : mir_->external_classes) {
    lir::ExternalClass record{
        .unit_name = cls.unit_name,
        .class_name = cls.class_name,
        .base = cls.base.transform(
            [&](const mir::ClassRef& base) { return BaseType(base); }),
        .members = {},
        .dispatch = {},
        .implements = {}};
    for (const mir::CrossUnitClassRef& iface : cls.implements) {
      record.implements.push_back(
          ExternalClassValueType(iface.unit_name, iface.class_name));
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
          cls.unit_name, lir::SymbolPart::Name(cls.class_name),
          lir::SymbolPart::Name(method));
    };
    for (const mir::PromisedBehavior& behavior : cls.behaviors) {
      record.dispatch.introduces.push_back(
          behavior.is_pure ? std::nullopt : std::optional{body(behavior.name)});
    }
    for (const mir::PromisedOverride& overriding : cls.overrides) {
      record.dispatch.overrides.push_back(
          lir::Override{
              .behavior = ExternalMethodRef(
                  overriding.behavior.unit_name, overriding.behavior.class_name,
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

  std::vector<lir::StructId> structs;
  structs.reserve(mir_->structs.size());
  for (std::size_t i = 0; i < mir_->structs.size(); ++i) {
    structs.push_back(out_.structs.Declare());
  }
  struct_identities_ = {mir_->structs.size(), std::move(structs)};

  // What every struct is made of is settled before any body lowers, since a
  // body reaching a component of one reads it -- a method of the struct
  // included, which is why each method's function is only reserved here.
  for (const mir::ExternalStruct& external : mir_->external_structs) {
    out_.external_structs.push_back(
        lir::ExternalStruct{
            .declaration = TranslateDeclaration(external.declaration),
            .elements = TranslateTypes(external.elements)});
  }
  for (const mir::StructId id : mir_->structs.Ids()) {
    const mir::StructDecl& decl = mir_->GetStruct(id);
    lir::Struct lowered{
        .name = decl.name,
        .elements = TranslateTypes(decl.elements),
        .methods = {}};
    for (const mir::StructMethod& method : decl.methods) {
      lowered.methods.push_back(
          lir::StructMethod{
              .answers = method.answers, .function = out_.functions.Declare()});
    }
    out_.structs.Define(StructDeclaration(id), std::move(lowered));
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
                  mir_->name, lir::SymbolPartOf(cls.name, id.value),
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
    fn->definition = lir::Definition::kShared;
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
    out_.functions.Add(*std::move(fn));
  }

  // A struct's method is a function under a symbol composed from the struct
  // and the operation it answers, which is how every unit reaching it names it.
  for (const mir::StructId id : mir_->structs.Ids()) {
    const mir::StructDecl& decl = mir_->GetStruct(id);
    const lir::Struct& lowered = out_.structs.Get(StructDeclaration(id));
    for (std::size_t i = 0; i < decl.methods.size(); ++i) {
      const mir::StructMethod& method = decl.methods[i];
      auto fn =
          FunctionLowerer(
              *this, method.code,
              lir::StructMethodSymbol(mir_->name, decl.name, method.answers))
              .Run();
      if (!fn) {
        return std::unexpected(std::move(fn.error()));
      }
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
                  *this, decl, lir::ClosureInvokeSymbol(mir_->name, id.value))
                  .Run();
    if (!fn) {
      return std::unexpected(std::move(fn.error()));
    }
    out_.functions.Define(closure.invoke, *std::move(fn));
    out_.closures.Define(ClosureDeclaration(id), std::move(closure));
  }

  // Building a value is an instruction sequence at this layer, so each value
  // the unit holds is a function here and the entry holding it is what reaches
  // that function. What each is built from was settled upstream, so this reads
  // two finished sets and takes them in either order.
  base::Translation<lir::IntegralConstantId, lir::FunctionId> constants(
      mir_->integral_constants.size());
  for (const mir::IntegralConstantId id : mir_->integral_constants.Ids()) {
    auto fn = FunctionLowerer::LowerValueBuild(
        *this, mir_->builds.constants.Get(id),
        lir::IntegralConstantSymbol(mir_->name, id.value));
    if (!fn) {
      return std::unexpected(std::move(fn.error()));
    }
    constants.Append(out_.functions.Add(*std::move(fn)));
  }
  out_.integral_constant_initializers = std::move(constants);

  base::Translation<lir::TypeDescriptorId, lir::FunctionId> descriptors(
      mir_->type_descriptors.size());
  for (const mir::TypeDescriptorId id : mir_->type_descriptors.Ids()) {
    // A description is unique only within its unit, while the whole program
    // links into one name space, so the unit qualifies it -- the same reason a
    // namespace callable is qualified.
    auto fn = FunctionLowerer::LowerValueBuild(
        *this, mir_->builds.descriptors.Get(id),
        lir::TypeDescriptionSymbol(mir_->name, id.value));
    if (!fn) {
      return std::unexpected(std::move(fn.error()));
    }
    descriptors.Append(out_.functions.Add(*std::move(fn)));
  }
  out_.type_descriptor_initializers = std::move(descriptors);
  return std::move(out_);
}

auto MintedEntrySymbol(std::string_view unit_name, mir::MintedEntry entry)
    -> std::string {
  switch (entry) {
    case mir::MintedEntry::kInstallStorage:
      return lir::NamespaceStorageInstallSymbol(unit_name);
    case mir::MintedEntry::kInitializeStorage:
      return lir::NamespaceStorageInitializeSymbol(unit_name);
    case mir::MintedEntry::kMakeObject:
      return lir::ObjectEntrySymbol(unit_name);
  }
  throw InternalError("mir_to_lir: unknown minted entry");
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
          [&](const mir::ReachedByMintedEntry& r) {
            return MintedEntrySymbol(mir_->name, r.entry);
          },
          [&](const mir::ReachedByPosition& r) {
            return lir::NamespaceCallableSymbol(
                mir_->name, lir::SymbolPart::Ordinal(r.slot.value));
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
            return ExternalClassValueType(cross.unit_name, cross.class_name);
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
    mir::ClassId owner, const mir::Class& cls, mir::CallableId id) const
    -> std::string {
  return lir::ClassCallableSymbol(
      mir_->name, lir::SymbolPartOf(cls.name, owner.value),
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
  out.name = cls.name;
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
                    mir_->name, lir::SymbolPartOf(cls.name, owner.value))),
            lir::ConstructorSymbol(
                mir_->name, lir::SymbolPartOf(cls.name, owner.value)))
            .Run();
    if (!constructor) {
      return std::unexpected(std::move(constructor.error()));
    }
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
                  ClassBodySymbol(owner, cls, cid))
                  .Run();
    if (!fn) {
      return std::unexpected(std::move(fn.error()));
    }
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
          .linkage = lir::Linkage::kExternal,
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
            return lir::ClassDefinitionSymbol(
                mir_->name,
                lir::SymbolPartOf(
                    mir_->GetClass(intra.class_id).name, intra.class_id.value));
          },
          [](const mir::CrossUnitClassRef& cross) {
            return lir::ClassDefinitionSymbol(
                cross.unit_name, lir::SymbolPart::Name(cross.class_name));
          }},
      of);
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
  const auto constant_symbol = [&](const mir::ClassConstantRef& ref) {
    return lir::ClassConstantSymbol(
        DefinitionSymbolOf(mir::IntraUnitClassRef{.class_id = ref.owner}),
        ref.constant.value);
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
                    [&](const mir::ClassConstantRef&) {
                      return not_data("a constant read whole");
                    },
                    [&](const mir::LocalRef&) { return not_data("a local"); },
                    [&](const mir::DefinitionRef&) {
                      return not_data("a definition read whole");
                    },
                    [&](const mir::TypeDescriptorRef&) {
                      return not_data("a type's description");
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
                      return {
                          lir::ConstantAddress{.symbol = constant_symbol(c)}};
                    },
                    [&](const mir::FunctionRef&) {
                      return not_data("the address of a body's address");
                    },
                    [&](const mir::LocalRef&) {
                      return not_data("the address of a local");
                    },
                    [&](const mir::TypeDescriptorRef&) {
                      return not_data("the address of a type's description");
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
          [&](const mir::IncDecExpr&) { return not_data("an increment"); },
          [&](const mir::CallExpr&) { return not_data("a call"); },
          [&](const mir::DerefExpr&) { return not_data("a dereference"); },
          [&](const mir::MoveExpr&) { return not_data("a move"); },
          [&](const mir::FieldAccessExpr&) {
            return not_data("a field access");
          },
          [&](const mir::ClosureExpr&) { return not_data("a closure"); },
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
                external.unit_name, external.class_name, external.ordinal);
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
            ? std::optional{ClassBodySymbol(owner, cls, cid)}
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
                  overridden.unit_name, overridden.class_name,
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
    const std::string& unit_name, const std::string& class_name,
    mir::BehaviorOrdinal ordinal) const -> lir::StatedDispatchRef {
  return lir::StatedDispatchRef{
      .introduced_by = ExternalClassValueType(unit_name, class_name),
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

auto UnitLowerer::ExternalClassValueType(
    const std::string& unit_name, const std::string& class_name) const
    -> lir::TypeId {
  return out_.types.Intern(
      lir::Type{lir::CrossUnitClassType{
          .unit_name = unit_name, .class_name = class_name}});
}

}  // namespace lyra::lowering::mir_to_lir
