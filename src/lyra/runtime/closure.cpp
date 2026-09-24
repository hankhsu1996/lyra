#include "lyra/runtime/closure.hpp"

#include <cstdint>
#include <span>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/erased_value.hpp"
#include "lyra/runtime/scope_program.hpp"
#include "lyra/value/runtime_value.hpp"

namespace lyra::runtime {

namespace {

auto HasBody(const ClosureBody& body) -> bool {
  return std::visit(
      Overloaded{
          [](const SynchronousBody& b) { return b.run != nullptr; },
          [](const CoroutineBody& b) { return b.start != nullptr; },
          [](const PerElementBody& b) { return b.run != nullptr; },
          [](const ValueBody& b) { return b.run != nullptr; }},
      body);
}

// The capture schema, checked before any storage is built from it, because a
// closure with no body is a linkage failure rather than a value that could run.
auto CaptureSchemaOf(const ClosureDefinition* definition)
    -> MemberStorageSchema {
  if (definition == nullptr || !HasBody(definition->body)) {
    throw InternalError("ClosureValue: the closure has no body to run");
  }
  return definition->captures;
}

}  // namespace

ClosureValue::ClosureValue(
    const ClosureDefinition* definition, std::span<void* const> captures)
    : definition_(definition), captures_(CaptureSchemaOf(definition)) {
  if (captures.size() != captures_.Size()) {
    throw InternalError(
        "ClosureValue: the construction does not initialize every capture");
  }
  for (std::uint32_t i = 0; i < captures_.Size(); ++i) {
    captures_.Adopt(i, captures[i]);
  }
}

auto ClosureValue::Capture(std::uint32_t index) -> void* {
  return captures_.Held(index);
}

void ClosureValue::Invoke() {
  const auto* body = std::get_if<SynchronousBody>(&definition_->body);
  if (body == nullptr) {
    throw InternalError(
        "ClosureValue: this body is not one run to completion -- please "
        "report this as a bug");
  }
  body->run(this);
}

auto ClosureValue::Start() -> void* {
  const auto* body = std::get_if<CoroutineBody>(&definition_->body);
  if (body == nullptr) {
    throw InternalError(
        "ClosureValue: this body is not one entered as a coroutine -- please "
        "report this as a bug");
  }
  return body->start(this);
}

auto ClosureValue::RunPerElement(
    const value::RuntimeValue& item, const value::RuntimeValue& index)
    -> value::RuntimeValue {
  const auto* body = std::get_if<PerElementBody>(&definition_->body);
  if (body == nullptr) {
    throw InternalError(
        "ClosureValue: this body is not one run per entry -- please report "
        "this as a bug");
  }
  // The element and the index are borrowed for the call: the container holds
  // them and the body only reads them. The answer is built in storage given
  // here, and taken out of it.
  AnswerStorage answer{};
  return TakeValue(
      body->result_domain,
      body->run(this, HandleOf(item), HandleOf(index), answer.bytes.data()));
}

auto ClosureValue::RunValue() -> value::RuntimeValue {
  const auto* body = std::get_if<ValueBody>(&definition_->body);
  if (body == nullptr) {
    throw InternalError(
        "ClosureValue: this body is not one that answers a value on its own "
        "-- please report this as a bug");
  }
  // The answer is built in storage given here, and taken out of it.
  AnswerStorage answer{};
  return TakeValue(body->result_domain, body->run(this, answer.bytes.data()));
}

}  // namespace lyra::runtime
