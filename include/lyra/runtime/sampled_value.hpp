#pragma once

#include <optional>
#include <type_traits>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/time.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/wait.hpp"

namespace lyra::runtime {

// The sampled value of a place (LRM 16.5.1): what it held in the Preponed
// region of the current time slot, and its default sampled value through time
// zero. Preponed is the previous slot's Postponed (LRM 4.4.2.1), and nothing
// changes in Postponed (LRM 4.4.2.9), so the value is taken there, once, in a
// slot the place changed in; in any other slot the place still holds it.
//
// It learns of a change the way a wait does, enrolled on the place through
// whatever member the place was reached through, so it follows a net from its
// own value to the object its one driver carries and back, and under a force,
// with nothing of its own to say what the net shows. What it costs follows how
// often the place changes, and a place nothing samples pays nothing.
class SampledValue {
 public:
  SampledValue(const SampledValue&) = delete;
  auto operator=(const SampledValue&) -> SampledValue& = delete;
  // Non-movable: the place it is enrolled on points at its membership.
  SampledValue(SampledValue&&) = delete;
  auto operator=(SampledValue&&) -> SampledValue& = delete;
  // Defined in the library's own source, because a class with a virtual
  // function whose every definition is in a header is emitted into every unit
  // that builds one.
  virtual ~SampledValue();

  // A change reached the place, so the value it holds at the end of this slot
  // is the next slot's Preponed value; it is taken then, once however often the
  // place changes before.
  void Reached();

 protected:
  // Enrolled on the place `watched` names, for its whole life.
  explicit SampledValue(const Trigger& watched);

 private:
  // Takes what the place holds now.
  virtual void Take() = 0;

  // The slot a taking is already submitted for, none before the first change.
  std::optional<SimTime> submitted_for_;
  WaitMembership membership_;
};

// A place read as a value of its own by `kRead`, which a sampled value takes
// what the place holds through.
template <class Place, auto kRead>
class ReadThrough {
 public:
  explicit ReadThrough(const Place& place) : place_(&place) {
  }

  [[nodiscard]] auto Get() const -> decltype(auto) {
    return (place_->*kRead)();
  }

 private:
  const Place* place_;
};

// The sampled value of a place read from `Source`, whatever holds it, by its
// `Get()`.
template <class Source>
class SampledValueOf final : public SampledValue {
 public:
  using ValueType = std::remove_cvref_t<decltype(std::declval<Source>().Get())>;

  // Answers `default_value` until the end of the first slot the place changes
  // in.
  SampledValueOf(Source source, ValueType default_value, const Trigger& watched)
      : SampledValue(watched),
        source_(std::move(source)),
        taken_(std::move(default_value)) {
  }
  SampledValueOf(const SampledValueOf&) = delete;
  auto operator=(const SampledValueOf&) -> SampledValueOf& = delete;
  SampledValueOf(SampledValueOf&&) = delete;
  auto operator=(SampledValueOf&&) -> SampledValueOf& = delete;
  ~SampledValueOf() override = default;

  [[nodiscard]] auto Get() const -> const ValueType& {
    return taken_;
  }

 private:
  void Take() override {
    taken_ = source_.Get();
  }

  Source source_;
  ValueType taken_;
};

// The value `sampled` keeps. Every place a sampled read names is armed before
// anything reads it, so one that was not is a lowering defect.
template <class Source>
auto SampledOf(const SampledValueOf<Source>* sampled) ->
    typename SampledValueOf<Source>::ValueType const& {
  if (sampled == nullptr) {
    throw InternalError(
        "a sampled value was read from a place nothing armed to answer for "
        "one");
  }
  return sampled->Get();
}

}  // namespace lyra::runtime
