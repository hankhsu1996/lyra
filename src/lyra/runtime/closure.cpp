#include "lyra/runtime/closure.hpp"

#include <format>
#include <memory>
#include <new>
#include <string_view>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::runtime {

namespace {

// The definition, checked before any storage is allocated to its shape,
// because a closure with no definition is a linkage failure rather than a value
// that could run.
auto Checked(const ClosureDefinition* definition) -> const ClosureDefinition* {
  if (definition == nullptr) {
    throw InternalError("ClosureValue: the closure has no definition");
  }
  return definition;
}

// The entry a closure is run through under one protocol, where its body
// answers to that one.
template <typename Entry>
auto Entered(Entry entry, std::string_view protocol) -> Entry {
  if (entry == nullptr) {
    throw InternalError(
        std::format(
            "ClosureValue: this body is not one {} -- please report this as a "
            "bug",
            protocol));
  }
  return entry;
}

}  // namespace

void EndClosure::operator()(ClosureValue* closure) const noexcept {
  std::destroy_at(closure);
  ::operator delete(closure);
}

// Every capture is an object of this library, so the alignment the captures ask
// for is at most what every allocation already gives.
auto ClosureValue::Make(const ClosureDefinition* definition) -> OwnedClosure {
  void* storage = ::operator new(Checked(definition)->size);
  return OwnedClosure(::new (storage) ClosureValue(definition));
}

ClosureValue::ClosureValue(const ClosureDefinition* definition)
    : definition_(definition) {
}

ClosureValue::~ClosureValue() {
  definition_->end_captures(this);
}

void ClosureValue::Invoke() {
  Entered(definition_->run, "run to completion")(this);
}

auto ClosureValue::Start() -> void* {
  return Entered(definition_->start, "entered as a coroutine")(this);
}

auto ClosureValue::RunPerElement(const void* item, const void* index)
    -> value::AnyValue {
  const auto run = Entered(definition_->run_per_element, "run per entry");
  // The element and the index are borrowed for the call: the container holds
  // them and the body only reads them.
  return value::AnyValue::Built(*definition_->result_type, [&](void* out) {
    run(this, item, index, out);
  });
}

auto ClosureValue::RunValue() -> value::AnyValue {
  return value::AnyValue::Built(
      *definition_->result_type, [&](void* out) { RunValueInto(out); });
}

auto ClosureValue::RunWatchedBit() -> value::FourStateBit {
  value::Logic bit;
  RunValueInto(&bit);
  return bit.Lsb();
}

auto ClosureValue::RunTruth() -> bool {
  value::Bit holds;
  RunValueInto(&holds);
  return holds.IsTruthy();
}

void ClosureValue::RunValueInto(void* out) {
  Entered(definition_->run_value, "that answers a value on its own")(this, out);
}

}  // namespace lyra::runtime
