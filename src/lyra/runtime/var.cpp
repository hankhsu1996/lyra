#include "lyra/runtime/var.hpp"

#include <optional>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

auto WriteBits(
    value::PackedArrayRef& bits, const value::PackedArray& value, bool watched)
    -> std::optional<Change> {
  const std::optional<value::BitPositions> reached =
      watched ? bits.Reached() : std::nullopt;
  if (!reached.has_value()) {
    bits = value;
    return std::nullopt;
  }
  KeptPart<value::PackedArray> kept(bits.Root(), *reached);
  bits = value;
  return kept.ChangeTo(bits.Root());
}

KeptPart<value::PackedArray>::KeptPart(const value::PackedArray& part)
    : KeptPart(part, {.lsb = 0, .width = part.BitWidth()}) {
}

KeptPart<value::PackedArray>::KeptPart(
    const value::PackedArray& storage, value::BitPositions reached)
    : reached_(Change::Reaching(storage, reached)) {
}

KeptPart<value::PackedArray>::~KeptPart() = default;

auto KeptPart<value::PackedArray>::ChangeTo(const value::PackedArray& part)
    -> std::optional<Change> {
  reached_.SetAfter(part);
  if (reached_.Unmoved()) {
    return std::nullopt;
  }
  return reached_;
}

RareWriteState::~RareWriteState() = default;

VariableCell::VariableCell() = default;
VariableCell::~VariableCell() = default;

void ErasedReference::Report(const Change& change) const {
  std::visit(
      Overloaded{
          [](std::monostate) {},
          [&](VariableCell* variable) {
            current_runtime().WakeParkedOn(variable->Members(), change);
          },
          [](GcObject* object) { object->PublishChange(); }},
      holder);
}

auto ErasedReference::ReportsTo() const -> Observable* {
  return std::visit(
      Overloaded{
          [](std::monostate) -> Observable* { return nullptr; },
          [](VariableCell* variable) -> Observable* { return variable; },
          [](GcObject* object) -> Observable* {
            return &object->EventSource();
          }},
      holder);
}

void ErasedReference::AdmitStep() const {
  if (!Admits()) {
    throw SimulationError(
        "lending part of a variable a procedural continuous assignment holds "
        "is not yet supported");
  }
}

auto ErasedReference::Part(void* part, value::Formation formed) const
    -> ErasedReference {
  switch (formed) {
    case value::Formation::kExisting:
      return {.holder = holder, .storage = part};
    case value::Formation::kMade:
      if (Watched()) {
        Report(Change::Whole());
      }
      return {.holder = holder, .storage = part};
    case value::Formation::kNowhere:
      return {.holder = std::monostate{}, .storage = part};
  }
  throw InternalError("ErasedReference::Part: unknown formation");
}

}  // namespace lyra::runtime
