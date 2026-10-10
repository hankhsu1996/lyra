#include "lyra/runtime/var.hpp"

#include <memory>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/trigger.hpp"

namespace lyra::runtime {

RareWriteState::RareWriteState() = default;
RareWriteState::~RareWriteState() = default;

VariableCell::VariableCell() = default;
VariableCell::~VariableCell() = default;

void VariableCell::InstallRare(std::unique_ptr<RareWriteState> rare) {
  rare_ = std::move(rare);
}

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
