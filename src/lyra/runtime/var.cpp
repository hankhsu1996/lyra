#include "lyra/runtime/var.hpp"

#include <bit>
#include <cstdint>
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

namespace {

// The low bit of a place's word says the rest is the address of a member a
// reference was bound into; an address of either kind is of an object aligned
// past it.
constexpr std::uintptr_t kThroughAMember = 1;

}  // namespace

WatchedPlace::WatchedPlace(Observable* reported_to)
    : word_(std::bit_cast<std::uintptr_t>(reported_to)) {
}

auto WatchedPlace::Through(const ErasedReference& reference) -> WatchedPlace {
  if (reference.member == nullptr) {
    return WatchedPlace{reference.ReportsTo()};
  }
  WatchedPlace place;
  place.word_ =
      std::bit_cast<std::uintptr_t>(reference.member) | kThroughAMember;
  return place;
}

auto WatchedPlace::FromWord(void* word) -> WatchedPlace {
  WatchedPlace place;
  place.word_ = std::bit_cast<std::uintptr_t>(word);
  return place;
}

auto WatchedPlace::Word() const -> void* {
  return std::bit_cast<void*>(word_);
}

auto WatchedPlace::Member() const -> const ErasedReference* {
  if ((word_ & kThroughAMember) == 0) {
    return nullptr;
  }
  return std::bit_cast<const ErasedReference*>(word_ & ~kThroughAMember);
}

auto WatchedPlace::PathOf(std::span<const std::int64_t> steps) const
    -> PartPath {
  const PartPath within{steps};
  if (const ErasedReference* member = Member()) {
    return member->part.Join(within);
  }
  return within;
}

auto WatchedPlace::ReportedTo() const -> Observable* {
  if (const ErasedReference* member = Member()) {
    return member->ReportsTo();
  }
  return std::bit_cast<Observable*>(word_);
}

void ErasedReference::Report(
    const PartPath& within, const Change& change) const {
  std::visit(
      Overloaded{
          [](std::monostate) {},
          [&](VariableCell* variable) {
            current_runtime().WakeParkedOn(
                *variable, part.Join(within), change);
          },
          [](GcObject* object) { object->PublishChange(); },
          // What a net shows moves as the net resolves, and the net tells its
          // own waits then; nothing writes it through a reference.
          [](HeldByNet) {}},
      holder);
}

auto ErasedReference::ReportsTo() const -> Observable* {
  return std::visit(
      Overloaded{
          [](std::monostate) -> Observable* { return nullptr; },
          [](VariableCell* variable) -> Observable* { return variable; },
          [](GcObject* object) -> Observable* {
            return &object->EventSource();
          },
          [](HeldByNet held) -> Observable* { return held.net; }},
      holder);
}

void ErasedReference::DriveContinuously() const {
  std::visit(
      Overloaded{
          [](std::monostate) {},
          [](VariableCell* variable) {
            current_runtime().DriveContinuously(*variable);
          },
          [](GcObject*) {}, [](HeldByNet) {}},
      holder);
}

void ErasedReference::AdmitStep() const {
  if (OverridesWrites()) {
    throw SimulationError(
        "lending part of a variable a procedural continuous assignment holds "
        "is not yet supported");
  }
}

auto ErasedReference::Part(
    void* storage, value::Formation formed, const PartPath& within) const
    -> ErasedReference {
  switch (formed) {
    case value::Formation::kExisting:
      return {.holder = holder, .storage = storage, .part = part.Join(within)};
    case value::Formation::kMade:
      if (Watched()) {
        Report(PartPath{}, Change::Whole());
      }
      return {.holder = holder, .storage = storage, .part = part.Join(within)};
    case value::Formation::kNowhere:
      return {.holder = std::monostate{}, .storage = storage};
  }
  throw InternalError("ErasedReference::Part: unknown formation");
}

}  // namespace lyra::runtime
