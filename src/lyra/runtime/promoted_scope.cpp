#include "lyra/runtime/promoted_scope.hpp"

#include "lyra/runtime/managed_object.hpp"
#include "lyra/runtime/object_ref.hpp"

namespace lyra::runtime {

PromotedScopeRef::PromotedScopeRef(const ObjectDefinition* definition)
    : held_(MakeManagedObject(definition)) {
}

auto PromotedScopeRef::Storage() const -> ManagedObject* {
  return held_.View<ManagedObject>();
}

}  // namespace lyra::runtime
