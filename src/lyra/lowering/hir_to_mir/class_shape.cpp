#include "lyra/lowering/hir_to_mir/class_shape.hpp"

#include <cstddef>

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class.hpp"

namespace lyra::lowering::hir_to_mir {

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
      .constructor = {},
      .contained = contained,
      .callables = {},
      .abi_adapters = {},
      .static_constants = {},
      .static_properties = static_properties,
      .static_init = mir::CallableCode::Defined()};
  for (std::size_t i = 0; i < callable_signatures.size(); ++i) {
    cls.callables.Declare();
  }
  return cls;
}

}  // namespace lyra::lowering::hir_to_mir
