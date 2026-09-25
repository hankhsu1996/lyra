#include "lyra/runtime/managed_object.hpp"

#include <memory>
#include <utility>

#include "lyra/runtime/class_definition.hpp"
#include "lyra/runtime/class_value.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/support/member_layout.hpp"
#include "lyra/value/object_ref.hpp"

namespace lyra::runtime {

static_assert(
    ClassValue::MembersAt(sizeof(ManagedObject)) ==
    support::MembersAt(support::ValueHolder::kObject));

auto MakeManagedObject(const ObjectDefinition* definition) -> value::ObjectRef {
  std::shared_ptr<ManagedObject> owned(
      ClassValue::Make<ManagedObject>(definition, definition));
  return RefToObject(std::move(owned));
}

}  // namespace lyra::runtime
