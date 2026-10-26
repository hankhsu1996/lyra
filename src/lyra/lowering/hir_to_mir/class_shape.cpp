#include "lyra/lowering/hir_to_mir/class_shape.hpp"

#include <cstddef>
#include <optional>
#include <string>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
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
            return mir::VirtualSlot{
                .owner_class = self_owner, .slot = self_slot};
          },
          [](const mir::OverridesVirtualSlot& o) -> mir::VirtualSlot {
            return o.overridden;
          }},
      role);
}

auto AsIntroducer(
    const mir::TypePool& types, mir::Block& block, mir::ExprId receiver,
    const mir::VirtualSlot& slot) -> mir::ExprId {
  const mir::TypeId introducer =
      types.Intern(mir::Type{mir::ObjectType{.of = slot.owner_class}});
  const auto& held =
      types.Get(block.exprs.Get(receiver).type).Get<mir::PointerType>();
  return ObjectAs(
      block, receiver,
      types.Intern(
          mir::Type{mir::PointerType{
              .pointee = introducer,
              .ownership = held.ownership,
              .mutability = held.mutability}}));
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
      .defining_unit = std::nullopt,
      .identity = identity,
      .base = base,
      .implements = implements,
      .conforming = {},
      .entries = {},
      .dispatch = {},
      .interface_tables = {},
      .is_final = is_final,
      .is_interface_class = is_interface_class,
      .self_pointer_type = self_pointer_type,
      .time_resolution = time_resolution,
      .execution = execution,
      .dumped_as = dumped_as,
      .fields = fields,
      .named_fields = named_fields,
      .constructor = std::nullopt,
      .contained = contained,
      .callables = {},
      .static_properties = static_properties,
      .named_static_properties = named_static_properties,
      .named_callables = {},
      .library_callbacks = library_callbacks,
      .exports = {},
      .constants = {},
      .object_definition_initializer = {}};
  for (std::size_t i = 0; i < callable_signatures.size(); ++i) {
    cls.callables.Declare();
  }
  return cls;
}

}  // namespace lyra::lowering::hir_to_mir
