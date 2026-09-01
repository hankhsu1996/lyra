#include "lyra/runtime/closure.hpp"

#include <cstdint>
#include <memory>
#include <span>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/generated_call_scope.hpp"

namespace lyra::runtime {

namespace {

auto HasBody(const ClosureBody& body) -> bool {
  return std::visit(
      Overloaded{
          [](const SynchronousBody& b) { return b.run != nullptr; },
          [](const CoroutineBody& b) { return b.start != nullptr; }},
      body);
}

}  // namespace

ClosureValue::ClosureValue(
    const ClosureDefinition* definition, std::span<void* const> captures)
    : definition_(definition) {
  if (definition_ == nullptr || !HasBody(definition_->body)) {
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
  const auto* body = std::get_if<SynchronousBody>(&definition_->body);
  if (body == nullptr) {
    throw InternalError(
        "ClosureValue: a body that completes as a coroutine is entered through "
        "the coroutine protocol rather than run to completion -- please report "
        "this as a bug");
  }
  // The body is generated code, so it runs in a scope of its own like every
  // other stretch the runtime enters: the values it materializes are released
  // when it returns, and the captures it reads are not among them.
  GeneratedCallScope scope;
  body->run(this);
}

auto ClosureValue::Start() -> void* {
  const auto* body = std::get_if<CoroutineBody>(&definition_->body);
  if (body == nullptr) {
    throw InternalError(
        "ClosureValue: an ordinary body has no handle to yield -- please "
        "report this as a bug");
  }
  // No scope is pushed here. A coroutine body's stretches each run in the scope
  // naming its activation frame, which is the driver's to establish and which
  // its later resumptions need too; a scope opened here would cover only the
  // first stretch and would name no frame.
  return body->start(this);
}

}  // namespace lyra::runtime
