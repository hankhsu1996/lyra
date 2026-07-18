#include "lyra/lowering/mir_to_lir/unit_lowerer.hpp"

#include <cstddef>
#include <cstdint>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lowering/mir_to_lir/function_lowerer.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::mir_to_lir {

auto UnitLowerer::Run() -> diag::Result<lir::CompilationUnit> {
  for (std::size_t i = 0; i < mir_->classes.size(); ++i) {
    const mir::ClassId id{static_cast<std::uint32_t>(i)};
    if (!mir_->classes.IsDefined(id)) {
      return diag::Fail(
          diag::DiagCode::kUnsupportedTypeKind,
          "mir_to_lir: undefined class in unit");
    }
    auto cls = LowerClass(
        lir::ClassId{static_cast<std::uint32_t>(i)}, mir_->GetClass(id));
    if (!cls) {
      return std::unexpected(std::move(cls.error()));
    }
    const lir::ClassId added = out_.classes.Add(*std::move(cls));
    if (mir_->root.has_value() && id.value == mir_->root->value) {
      out_.root = added;
    }
  }
  // A type reached during lowering had no LIR mirror; surface it now, once the
  // whole unit has been walked, rather than from the non-failing translator.
  if (type_error_.has_value()) {
    return std::unexpected(std::move(*type_error_));
  }
  return std::move(out_);
}

auto UnitLowerer::LowerClass(lir::ClassId class_id, const mir::Class& cls)
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

  auto constructor =
      FunctionLowerer(*this, cls.constructor.code, "constructor", class_id)
          .Run();
  if (!constructor) {
    return std::unexpected(std::move(constructor.error()));
  }
  out.constructor = *std::move(constructor);

  // Only bodied callables become LIR functions, appended in arena order; a
  // DPI-C import (external) is a foreign symbol reached by a `ForeignTarget`,
  // so it takes no slot here. `Callee` resolution reaches a method through the
  // same slot assignment `MethodSlot` memoizes, so each append must land
  // exactly on the slot it assigns.
  const mir::ClassId owner{class_id.value};
  for (std::size_t i = 0; i < cls.callables.size(); ++i) {
    const mir::CallableId cid{static_cast<std::uint32_t>(i)};
    const mir::CallableDecl& callable = cls.callables.Get(cid);
    const auto* internal = std::get_if<mir::InternalCallable>(&callable.impl);
    if (internal == nullptr) continue;
    if (out.methods.size() != MethodSlot(owner, cid).index) {
      throw InternalError(
          "mir_to_lir: LIR method slot diverged from the callable arena "
          "compaction");
    }
    auto fn =
        FunctionLowerer(*this, internal->code, callable.name, class_id).Run();
    if (!fn) {
      return std::unexpected(std::move(fn.error()));
    }
    out.methods.push_back(*std::move(fn));
  }
  return out;
}

auto UnitLowerer::MethodSlot(mir::ClassId owner, mir::CallableId callable)
    -> lir::MethodRef {
  auto it = method_slot_memo_.find(owner);
  if (it == method_slot_memo_.end()) {
    const mir::Class& cls = mir_->GetClass(owner);
    std::vector<std::optional<lir::MethodRef>> slots(cls.callables.size());
    std::uint32_t index = 0;
    for (std::uint32_t i = 0; i < cls.callables.size(); ++i) {
      if (std::holds_alternative<mir::InternalCallable>(
              cls.callables.Get(mir::CallableId{i}).impl)) {
        slots[i] = lir::MethodRef{
            .class_id = lir::ClassId{owner.value}, .index = index++};
      }
    }
    it = method_slot_memo_.emplace(owner, std::move(slots)).first;
  }
  if (callable.value >= it->second.size() ||
      !it->second[callable.value].has_value()) {
    throw InternalError(
        "mir_to_lir: callable has no LIR method slot in its class");
  }
  return *it->second[callable.value];
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
