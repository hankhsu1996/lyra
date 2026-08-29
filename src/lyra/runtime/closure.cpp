#include "lyra/runtime/closure.hpp"

#include <cstdint>
#include <memory>
#include <span>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/generated_call_scope.hpp"

namespace lyra::runtime {

ClosureValue::ClosureValue(
    const ClosureDefinition* definition, std::span<void* const> captures)
    : definition_(definition) {
  if (definition_ == nullptr || definition_->invoke == nullptr) {
    throw InternalError("ClosureValue: the closure has no body to run");
  }
  const std::span<const MemberStorageDescriptor> schema =
      definition_->captures.Descriptors();
  if (captures.size() != schema.size()) {
    throw InternalError(
        "ClosureValue: the construction does not initialize every capture");
  }
  captures_.reserve(schema.size());
  for (std::uint32_t i = 0; i < schema.size(); ++i) {
    captures_.push_back(std::make_unique<MemberStorage>(schema[i]));
    captures_.back()->AdoptFrom(captures[i]);
  }
}

auto ClosureValue::Capture(std::uint32_t index) -> void* {
  return captures_.at(index)->HeldValue();
}

void ClosureValue::Invoke() {
  // The body is generated code, so it runs in a scope of its own like every
  // other stretch the runtime enters: the values it materializes are released
  // when it returns, and the captures it reads are not among them.
  GeneratedCallScope scope;
  definition_->invoke(this);
}

}  // namespace lyra::runtime
