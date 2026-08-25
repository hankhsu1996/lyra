#include "lyra/lowering/mir_to_lir/unit_lowerer.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
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
#include "lyra/mir/type.hpp"

namespace lyra::lowering::mir_to_lir {

auto UnitLowerer::Run() -> diag::Result<lir::CompilationUnit> {
  for (std::size_t i = 0; i < mir_->classes.size(); ++i) {
    if (!mir_->classes.IsDefined(mir::ClassId{static_cast<std::uint32_t>(i)})) {
      return diag::Fail(
          diag::DiagCode::kUnsupportedTypeKind,
          "mir_to_lir: undefined class in unit");
    }
  }
  for (std::size_t i = 0; i < mir_->closures.size(); ++i) {
    if (!mir_->closures.IsDefined(
            mir::ClosureId{static_cast<std::uint32_t>(i)})) {
      return diag::Fail(
          diag::DiagCode::kUnsupportedTypeKind,
          "mir_to_lir: undefined closure in unit");
    }
  }
  PlanFunctions();

  for (std::size_t i = 0; i < mir_->classes.size(); ++i) {
    const mir::ClassId id{static_cast<std::uint32_t>(i)};
    auto cls = LowerClass(id, mir_->GetClass(id));
    if (!cls) {
      return std::unexpected(std::move(cls.error()));
    }
    const lir::ClassId added = out_.classes.Add(*std::move(cls));
    if (mir_->root.has_value() && id.value == mir_->root->value) {
      out_.root = added;
    }
  }

  // A closure's invoke is a function like any other body's; the captures its
  // referencing site supplies arrive as its leading parameters.
  for (std::size_t i = 0; i < mir_->closures.size(); ++i) {
    const mir::ClosureId id{static_cast<std::uint32_t>(i)};
    auto fn = FunctionLowerer(
                  *this, mir_->GetClosure(id), std::format("closure_{}", i))
                  .Run();
    if (!fn) {
      return std::unexpected(std::move(fn.error()));
    }
    if (out_.functions.Add(*std::move(fn)) != closure_functions_[i]) {
      throw InternalError(
          "mir_to_lir: a lowered function landed on an identity other than the "
          "one planned for it");
    }
  }

  // A type reached during lowering had no LIR mirror; surface it now, once the
  // whole unit has been walked, rather than from the non-failing translator.
  if (type_error_.has_value()) {
    return std::unexpected(std::move(*type_error_));
  }
  return std::move(out_);
}

void UnitLowerer::PlanFunctions() {
  std::uint32_t next = 0;
  class_functions_.resize(mir_->classes.size());
  for (std::size_t i = 0; i < mir_->classes.size(); ++i) {
    const mir::Class& cls =
        mir_->GetClass(mir::ClassId{static_cast<std::uint32_t>(i)});
    ClassFunctions& planned = class_functions_[i];
    planned.constructor = lir::FunctionId{next++};
    planned.methods.resize(cls.callables.size());
    for (std::uint32_t c = 0; c < cls.callables.size(); ++c) {
      if (cls.callables.Get(mir::CallableId{c}).code.body.has_value()) {
        planned.methods[c] = lir::FunctionId{next++};
      }
    }
  }
  closure_functions_.reserve(mir_->closures.size());
  for (std::size_t i = 0; i < mir_->closures.size(); ++i) {
    closure_functions_.push_back(lir::FunctionId{next++});
  }
}

auto UnitLowerer::LowerClass(mir::ClassId owner, const mir::Class& cls)
    -> diag::Result<lir::Class> {
  lir::Class out;
  out.name = cls.name;
  if (cls.base.has_value()) {
    out.base = LowerBase(*cls.base);
  }

  for (std::size_t i = 0; i < cls.fields.size(); ++i) {
    const mir::FieldDecl& field =
        cls.fields.Get(mir::FieldId{static_cast<std::uint32_t>(i)});
    out.members.push_back(
        lir::Member{.name = field.name, .type = TranslateType(field.type)});
  }

  // A class's bodies become functions of the unit, so each takes a name that is
  // unique across it: the owning class qualifies the body's own name, which is
  // only unique within that class.
  const ClassFunctions& planned = class_functions_[owner.value];
  auto constructor =
      FunctionLowerer(
          *this, cls.constructor.code, std::format("{}.constructor", cls.name))
          .Run();
  if (!constructor) {
    return std::unexpected(std::move(constructor.error()));
  }
  if (out_.functions.Add(*std::move(constructor)) != planned.constructor) {
    throw InternalError(
        "mir_to_lir: a lowered function landed on an identity other than the "
        "one planned for it");
  }
  out.constructor = planned.constructor;

  // Only a callable this program defines becomes a function: a DPI-C import is
  // reached as a foreign symbol and a pure virtual has no implementation here.
  // The interface lists the rest in arena order, so a method's position in the
  // list is the slot a dispatch indexes.
  for (std::size_t i = 0; i < cls.callables.size(); ++i) {
    const mir::CallableId cid{static_cast<std::uint32_t>(i)};
    const mir::CallableDecl& callable = cls.callables.Get(cid);
    if (!callable.code.body.has_value()) continue;
    auto fn =
        FunctionLowerer(
            *this, callable.code, std::format("{}.{}", cls.name, callable.name))
            .Run();
    if (!fn) {
      return std::unexpected(std::move(fn.error()));
    }
    const lir::FunctionId planned_id = MethodFunction(owner, cid);
    if (out_.functions.Add(*std::move(fn)) != planned_id) {
      throw InternalError(
          "mir_to_lir: a lowered function landed on an identity other than the "
          "one planned for it");
    }
    out.methods.push_back(planned_id);
  }
  return out;
}

auto UnitLowerer::MethodFunction(
    mir::ClassId owner, mir::CallableId callable) const -> lir::FunctionId {
  const ClassFunctions& planned = class_functions_.at(owner.value);
  if (callable.value >= planned.methods.size() ||
      !planned.methods[callable.value].has_value()) {
    throw InternalError(
        "mir_to_lir: callable has no body, so it is no function of this unit");
  }
  return *planned.methods[callable.value];
}

auto UnitLowerer::ClosureFunction(mir::ClosureId closure) const
    -> lir::FunctionId {
  return closure_functions_.at(closure.value);
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

auto UnitLowerer::LowerBase(const mir::ClassRef& base) -> lir::Base {
  return std::visit(
      Overloaded{
          [](const mir::IntraUnitClassRef& i) -> lir::Base {
            return lir::Base{
                lir::IntraUnitBase{.class_id = lir::ClassId{i.class_id.value}}};
          },
          [](const mir::ExternalClassRef& e) -> lir::Base {
            return lir::Base{
                lir::ExternalBase{.qualified_name = e.qualified_name}};
          }},
      base);
}

}  // namespace lyra::lowering::mir_to_lir
