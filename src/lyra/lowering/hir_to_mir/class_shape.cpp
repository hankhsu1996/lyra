#include "lyra/lowering/hir_to_mir/class_shape.hpp"

#include <cstddef>
#include <optional>
#include <string>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

auto CanonicalVirtualSlot(
    mir::ClassId self_owner, mir::CallableId self_slot,
    const mir::VirtualDispatchRole& role) -> mir::VirtualSlot {
  return std::visit(
      Overloaded{
          [&](const mir::IntroducesVirtualSlot&) -> mir::VirtualSlot {
            return mir::LocalVirtualSlot{
                .owner_class = self_owner, .slot = self_slot};
          },
          [](const mir::OverridesIntraUnitSlot& s) -> mir::VirtualSlot {
            return mir::LocalVirtualSlot{
                .owner_class = s.slot_owner, .slot = s.slot_id};
          },
          [](const mir::OverridesExternalSlot& e) -> mir::VirtualSlot {
            return mir::ExternalVirtualSlot{
                .unit_name = e.unit_name,
                .class_name = e.class_name,
                .ordinal = e.ordinal};
          },
          // Only the library calls one of its own virtual functions, so no
          // call of the source names the behavior such a method answers.
          [](const mir::OverridesLibraryVirtual&) -> mir::VirtualSlot {
            throw InternalError(
                "CanonicalVirtualSlot: a call names a phase of the library's "
                "scope, which only the library calls -- please report this as "
                "a bug");
          }},
      role);
}

auto AsIntroducer(
    const mir::TypePool& types, mir::Block& block, mir::ExprId receiver,
    const mir::VirtualSlot& slot) -> mir::ExprId {
  const mir::TypeId introducer = types.Intern(
      std::visit(
          Overloaded{
              [](const mir::LocalVirtualSlot& local) {
                return mir::Type{
                    mir::ObjectType{.class_id = local.owner_class}};
              },
              [](const mir::ExternalVirtualSlot& external) {
                return mir::Type{mir::CrossUnitClassType{
                    .unit_name = external.unit_name,
                    .class_name = external.class_name}};
              }},
          slot));
  const auto& held =
      types.Get(block.exprs.Get(receiver).type).Get<mir::PointerType>();
  if (held.pointee == introducer) {
    return receiver;
  }
  return block.exprs.Add(
      mir::Expr{
          .data = mir::CastExpr{.operand = receiver},
          .type = types.Intern(
              mir::Type{mir::PointerType{
                  .pointee = introducer,
                  .ownership = held.ownership,
                  .mutability = held.mutability}})});
}

auto ClassShape::AddNamedField(std::string name, mir::TypeId type)
    -> mir::FieldId {
  const mir::FieldId slot = fields.Add(mir::FieldDecl{.type = type});
  named_fields.push_back(
      mir::NamedField{.name = std::move(name), .slot = slot});
  return slot;
}

auto ClassShape::AddField(mir::TypeId type) -> mir::FieldId {
  return fields.Add(mir::FieldDecl{.type = type});
}

auto ClassShape::OpenClass() const -> mir::Class {
  mir::Class cls{
      .name = name,
      .aliases = aliases,
      .base = base,
      .implements = implements,
      .conforming = {},
      .is_final = is_final,
      .is_interface_class = is_interface_class,
      .self_pointer_type = self_pointer_type,
      .time_resolution = time_resolution,
      .fields = fields,
      .named_fields = named_fields,
      .constructor = std::nullopt,
      .contained = contained,
      .callables = {},
      .static_properties = static_properties,
      .named_static_properties = named_static_properties,
      .named_callables = {},
      .constants = {},
      .object_definition_initializer = {}};
  for (std::size_t i = 0; i < callable_signatures.size(); ++i) {
    cls.callables.Declare();
  }
  return cls;
}

}  // namespace lyra::lowering::hir_to_mir
