#include "lyra/lowering/mir_to_lir/unit_lowerer.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/lir/class_id.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/function_id.hpp"
#include "lyra/lowering/mir_to_lir/function_lowerer.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/static_variable_id.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::mir_to_lir {

auto StaticVariableSymbol(
    std::string_view unit_name, std::string_view variable_name) -> std::string {
  return std::format("{}.{}", unit_name, variable_name);
}

auto UnitLowerer::Run() -> diag::Result<lir::CompilationUnit> {
  for (const mir::ClassId id : mir_->classes.Ids()) {
    if (!mir_->classes.IsDefined(id)) {
      return diag::Fail(
          diag::DiagCode::kUnsupportedTypeKind,
          "mir_to_lir: undefined class in unit");
    }
  }
  for (const mir::ClosureId id : mir_->closures.Ids()) {
    if (!mir_->closures.IsDefined(id)) {
      return diag::Fail(
          diag::DiagCode::kUnsupportedTypeKind,
          "mir_to_lir: undefined closure in unit");
    }
  }

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
  // that whole list.
  external_unit_object_identities_ =
      base::Translation<mir::ExternalUnitObjectId, lir::ExternalUnitObjectId>{
          mir_->external_unit_objects.size()};
  for (const mir::ExternalUnitObject& object : mir_->external_unit_objects) {
    external_unit_object_identities_.Append(
        out_.external_unit_objects.Add(LowerExternalUnitObject(object)));
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
  // A class's name is the declaring unit's own statement and is unique only
  // there, while the whole program links into one name space, so the unit
  // qualifies it. A referrer composes the same symbol from the unit and class
  // its signature named, which is what lets the two agree with no shared table.
  return std::format("{}.{}", mir_->name, cls.name);
}

auto UnitLowerer::ClosureSymbol(mir::ClosureId closure) const -> std::string {
  // A closure's ordinal is counted within its unit while the whole program
  // links into one name space, so the unit qualifies it -- the same reason a
  // class and a namespace callable are qualified.
  return std::format("{}.closure_{}", mir_->name, closure.value);
}

auto UnitLowerer::TakeClassIdentities(const mir::Class& cls)
    -> ClassIdentities {
  // Only a callable this program defines becomes a function of the unit, so a
  // bodyless one takes no function identity and answers with none.
  std::vector<std::optional<lir::FunctionId>> methods;
  methods.reserve(cls.callables.size());
  ClassIdentities identities{
      .lir_class = out_.classes.Declare(),
      .constructor = out_.functions.Declare(),
      .methods = {}};
  for (const mir::CallableId callable : cls.callables.Ids()) {
    methods.push_back(
        cls.callables.Get(callable).code.body.has_value()
            ? std::optional{out_.functions.Declare()}
            : std::nullopt);
  }
  identities.methods = {cls.callables.size(), std::move(methods)};
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

  // A class's bodies become functions of the program, and a body's own name is
  // unique only within its class -- so the class symbol qualifies it, being
  // itself unique program-wide.
  const ClassIdentities& identities = class_identities_.Get(owner);
  auto constructor =
      FunctionLowerer(
          *this, cls.constructor.code, std::format("{}.constructor", out.name))
          .Run();
  if (!constructor) {
    return std::unexpected(std::move(constructor.error()));
  }
  out_.functions.Define(identities.constructor, *std::move(constructor));
  out.constructor = identities.constructor;

  // Only a callable this program defines becomes a function: a DPI-C import is
  // reached as a foreign symbol and a pure virtual has no implementation here.
  // The interface lists the rest in arena order, so a method's position in the
  // list is the slot a dispatch indexes.
  for (const mir::CallableId cid : cls.callables.Ids()) {
    const mir::CallableDecl& callable = cls.callables.Get(cid);
    if (!callable.code.body.has_value()) continue;
    auto fn =
        FunctionLowerer(
            *this, callable.code, std::format("{}.{}", out.name, callable.name))
            .Run();
    if (!fn) {
      return std::unexpected(std::move(fn.error()));
    }
    out_.functions.Define(*identities.methods.Get(cid), *std::move(fn));
    out.methods.push_back(*identities.methods.Get(cid));
  }
  return out;
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

auto UnitLowerer::ClosureFunction(mir::ClosureId closure) const
    -> lir::FunctionId {
  return closure_identities_.Get(closure).invoke;
}

auto UnitLowerer::ClosureDeclaration(mir::ClosureId closure) const
    -> lir::ClosureId {
  return closure_identities_.Get(closure).declaration;
}

auto UnitLowerer::BorrowedPointerTo(lir::TypeId pointee) -> lir::TypeId {
  const auto it = pointer_memo_.find(pointee);
  if (it != pointer_memo_.end()) {
    return it->second;
  }
  const lir::TypeId id = out_.types.Add(
      lir::Type{
          .data = lir::PointerType{
              .pointee = pointee,
              .ownership = lir::PointerOwnership::kBorrowed,
              .mutability = lir::Mutability::kMutable}});
  pointer_memo_.emplace(pointee, id);
  return id;
}

auto UnitLowerer::MachineBoolType() -> lir::TypeId {
  if (!machine_bool_type_.has_value()) {
    machine_bool_type_ = out_.types.Add(
        lir::Type{
            .data = lir::MachineIntType{
                .bit_width = 1, .signedness = lir::Signedness::kUnsigned}});
  }
  return *machine_bool_type_;
}

auto UnitLowerer::VoidType() -> lir::TypeId {
  if (!void_type_.has_value()) {
    void_type_ = out_.types.Add(lir::Type{.data = lir::VoidType{}});
  }
  return *void_type_;
}

auto UnitLowerer::FlatPackedType(std::uint64_t width, bool four_state)
    -> lir::TypeId {
  const std::pair<std::uint64_t, bool> shape{width, four_state};
  if (const auto it = flat_packed_memo_.find(shape);
      it != flat_packed_memo_.end()) {
    return it->second;
  }
  const lir::TypeId id = out_.types.Add(
      lir::Type{
          .data = lir::PackedArrayType{
              .atom = four_state ? lir::BitAtom::kLogic : lir::BitAtom::kBit,
              .signedness = lir::Signedness::kUnsigned,
              .dims = {lir::PackedRange{
                  .left = static_cast<std::int64_t>(width) - 1, .right = 0}},
              .form = lir::PackedArrayForm::kExplicit}});
  flat_packed_memo_.emplace(shape, id);
  return id;
}

auto UnitLowerer::LowerBase(const mir::ClassRef& base) const -> lir::Base {
  return std::visit(
      Overloaded{
          [this](const mir::IntraUnitClassRef& i) -> lir::Base {
            return lir::Base{lir::IntraUnitBase{
                .class_id = class_identities_.Get(i.class_id).lir_class}};
          },
          [](const mir::ExternalClassRef& e) -> lir::Base {
            return lir::Base{
                lir::ExternalBase{.qualified_name = e.qualified_name}};
          }},
      base);
}

}  // namespace lyra::lowering::mir_to_lir
