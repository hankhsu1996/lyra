#include "lyra/lowering/hir_to_mir/class_shape.hpp"

#include <cstddef>
#include <string>
#include <utility>

#include "lyra/mir/class.hpp"

namespace lyra::lowering::hir_to_mir {

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
      .base = base,
      .implements = implements,
      .is_final = is_final,
      .is_interface_class = is_interface_class,
      .self_pointer_type = self_pointer_type,
      .time_resolution = time_resolution,
      .fields = fields,
      .named_fields = named_fields,
      .constructor = {},
      .contained = contained,
      .callables = {},
      .abi_adapters = {},
      .static_constants = {},
      .static_properties = static_properties,
      .named_static_properties = named_static_properties,
      .named_callables = {},
      .declares = declares};
  for (std::size_t i = 0; i < callable_signatures.size(); ++i) {
    cls.callables.Declare();
  }
  return cls;
}

}  // namespace lyra::lowering::hir_to_mir
