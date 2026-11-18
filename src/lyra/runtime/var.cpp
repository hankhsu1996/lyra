#include "lyra/runtime/var.hpp"

#include <algorithm>
#include <bit>
#include <cstdint>
#include <limits>
#include <memory>
#include <optional>
#include <span>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/owned_call.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/kept_copy.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::runtime {

RareWriteState::RareWriteState() = default;
RareWriteState::~RareWriteState() = default;

VariableCell::VariableCell() = default;
VariableCell::~VariableCell() = default;

void VariableCell::InstallRare(std::unique_ptr<RareWriteState> rare) {
  rare_ = std::move(rare);
}

void VariableCell::WritePart(OwnedCall write) {
  rare_->WritePart(std::move(write));
}

auto VariableCell::ReportsTo() const -> WatchedPlace {
  return WatchedPlace{this};
}

namespace {

// The low bit of a place's word says the rest is the address of a member a
// reference was bound into; an address of either kind is of an object aligned
// past it.
constexpr std::uintptr_t kThroughAMember = 1;

}  // namespace

WatchedPlace::WatchedPlace(const Observable* reported_to)
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

void ErasedReference::WritePart(OwnedCall write) const {
  if (VariableCell* const* variable = std::get_if<VariableCell*>(&holder)) {
    (*variable)->WritePart(std::move(write));
  }
}

void ErasedReference::AdmitStep() const {
  if (OverridesWrites()) {
    throw SimulationError(
        "lending part of a variable a procedural continuous assignment holds "
        "is not yet supported");
  }
}

// A start past the array holds none of its elements, and an end the sum would
// overflow lies past the array too.
auto PositionsASliceWrites(
    std::optional<std::int64_t> start, std::int64_t count, std::int64_t size)
    -> std::ranges::iota_view<std::int64_t, std::int64_t> {
  if (!start.has_value() || count <= 0 || *start >= size) {
    return {};
  }
  const std::int64_t first = std::max<std::int64_t>(*start, 0);
  const std::int64_t end =
      *start > std::numeric_limits<std::int64_t>::max() - count
          ? size
          : std::min(*start + count, size);
  return {first, std::max(first, end)};
}

auto ChangeBetween(
    const value::ValueType& type, const void* before, const void* now)
    -> std::optional<Change> {
  if (const value::IntegralValueType* integral = type.AsIntegral()) {
    const value::LoadedWords kept = integral->Load(before);
    Change change = Change::Reaching(
        kept.View(),
        value::BitPositions{.lsb = 0, .width = integral->Shape().width});
    change.SetAfter(integral->Load(now).View());
    if (change.Unmoved()) {
      return std::nullopt;
    }
    return change;
  }
  if (type.BitIdentical(before, now)) {
    return std::nullopt;
  }
  return Change::Whole();
}

void ExchangeLaidOut(const value::ValueType& type, void* live, void* kept) {
  const value::KeptCopy held(type, live);
  type.Assign(live, kept);
  type.Assign(kept, held.Bytes());
}

void OriginalKeeper::KeepBits(
    const PartPath& within, value::IntegralView live,
    value::BitPositions bits) const {
  if (place == nullptr) {
    return;
  }
  place->KeepBitsOriginal(named.Join(within), live, bits);
}

// Only a variable keeps anything: a class object's properties are never what
// an `always_comb` or `always_latch` is triggered by (LRM 9.2.2.2.1), and a
// net's shown value moves as the net resolves.
auto KeeperThrough(const ErasedReference& lent) -> OriginalKeeper {
  VariableCell* const* variable = std::get_if<VariableCell*>(&lent.holder);
  if (variable == nullptr || !lent.NamesItsPart()) {
    return {};
  }
  return {.place = *variable, .named = lent.part};
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

auto LeaseObject(const ErasedReference& reference) -> LeasedReference {
  GcObject* const* object = std::get_if<GcObject*>(&reference.holder);
  if (object == nullptr) {
    throw InternalError(
        "LeaseObject: a reference leased for the object it names storage of "
        "names storage no object holds -- please report this as a bug");
  }
  return {StorageLease((*object)->weak_from_this().lock()), reference};
}

}  // namespace lyra::runtime
