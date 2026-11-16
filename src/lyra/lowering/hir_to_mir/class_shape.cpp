#include "lyra/lowering/hir_to_mir/class_shape.hpp"

#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

auto CanonicalVirtualSlot(
    mir::CallableId self, const mir::VirtualDispatchRole& role)
    -> mir::VirtualSlot {
  return std::visit(
      Overloaded{
          [&](const mir::IntroducesVirtualSlot&) -> mir::VirtualSlot {
            return mir::VirtualSlot{.introduced_by = self};
          },
          [](const mir::OverridesVirtualSlot& o) -> mir::VirtualSlot {
            return o.overridden;
          }},
      role);
}

auto AsIntroducer(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId receiver,
    const mir::VirtualSlot& slot) -> mir::ExprId {
  const mir::TypePool& types = unit.types;
  const mir::TypeId introducer = types.Intern(
      mir::Type{mir::ObjectType{
          .of = mir::DeclaringClassOf(unit, slot.introduced_by)}});
  const auto& held =
      types.Get(block.Get(receiver).type).Get<mir::PointerType>();
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
      .is_interface = is_interface,
      .self_pointer_type = self_pointer_type,
      .scope_declarations = scope_declarations,
      .fields = fields,
      .named_fields = named_fields,
      .contained = contained,
      .static_properties = static_properties,
      .named_static_properties = named_static_properties,
      .named_callables = {},
      .library_callbacks = library_callbacks,
      .declaring_instance = std::nullopt,
      .interface_reaches = {},
      .exports = {},
      .definition = std::nullopt};
  return cls;
}

}  // namespace lyra::lowering::hir_to_mir
