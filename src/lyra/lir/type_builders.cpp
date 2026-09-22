#include "lyra/lir/type_builders.hpp"

#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"

namespace lyra::lir {

auto CellOf(const TypePool& types, TypeId value_type) -> TypeId {
  return types.Intern(Type{ObservableType{.value = value_type}});
}

}  // namespace lyra::lir
