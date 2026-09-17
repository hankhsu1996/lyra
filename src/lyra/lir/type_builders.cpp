#include "lyra/lir/type_builders.hpp"

#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"

namespace lyra::lir {

auto CellOf(const TypePool& types, TypeId value_type) -> TypeId {
  return types.Intern(Type{ObservableType{.value = value_type}});
}

auto ReferenceToCellOf(
    const TypePool& types, TypeId value_type, Mutability mutability) -> TypeId {
  return types.Intern(
      Type{RefType{
          .pointee = CellOf(types, value_type), .mutability = mutability}});
}

}  // namespace lyra::lir
