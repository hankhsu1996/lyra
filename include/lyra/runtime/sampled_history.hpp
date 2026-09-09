#pragma once

#include <cstddef>
#include <cstdint>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

// What the ticks of one clocking event have settled for one expression (LRM
// 16.9.3). A value change function compares the sampled value of this time step
// against the most recent entry; `$past(e, k)` reads the kth.
//
// Only strictly prior ticks are ever read (LRM 16.9.3), so what is recorded
// here is never a tick of the time step a reader is asking in.
//
// It is installed already full of the expression's default sampled value, which
// is exactly what the standard requires before the kth prior tick exists -- so
// a read has no empty case and nothing here counts ticks. Depth is fixed where
// the declaration is, because the count `$past` names is an elaboration-time
// constant, so nothing grows.
template <value::LyraValue T>
class SampledHistory {
 public:
  SampledHistory() = default;

  // Non-movable for the reason every member storage is: a place resolves to
  // this address, so it has to stay put once anything has named it.
  SampledHistory(const SampledHistory&) = delete;
  auto operator=(const SampledHistory&) -> SampledHistory& = delete;
  SampledHistory(SampledHistory&&) = delete;
  auto operator=(SampledHistory&&) -> SampledHistory& = delete;
  ~SampledHistory() = default;

  // Fills every entry with the expression's default sampled value. Run where
  // the design is activated, which is after every declaration initializer has
  // run (LRM 10.5) and before any procedure can write -- so what the subject
  // evaluates to there is the default sampled value the standard defines (LRM
  // 16.5.1).
  void Install(
      const T& default_sampled_value, const value::PackedArray& depth) {
    const std::int64_t entries = depth.ToInt64();
    if (entries < 1) {
      throw InternalError(
          "SampledHistory::Install: a history reaching no prior tick answers "
          "nothing, and a tick count is one or greater (LRM 16.9.3)");
    }
    entries_.assign(static_cast<std::size_t>(entries), default_sampled_value);
    newest_ = 0;
  }

  // Records what this tick settled. The entry that falls off the end is the one
  // no read can name, which is what bounds this to the declared depth.
  void Push(const T& value) {
    RequireInstalled();
    newest_ = (newest_ + 1) % entries_.size();
    entries_[newest_] = value;
  }

  // What the tick `ticks_back` ticks before this one settled, counting from 1
  // for the most recent. Reaching past what has happened answers with the
  // default sampled value, because that is what the entry still holds.
  [[nodiscard]] auto At(const value::PackedArray& ticks_back) const
      -> const T& {
    RequireInstalled();
    const std::int64_t back = ticks_back.ToInt64();
    if (back < 1 || std::cmp_greater(back, entries_.size())) {
      throw InternalError(
          "SampledHistory::At: a read reaches a tick this history was not "
          "declared deep enough to keep");
    }
    const auto offset = static_cast<std::size_t>(back - 1);
    return entries_[(newest_ + entries_.size() - offset) % entries_.size()];
  }

 private:
  void RequireInstalled() const {
    if (entries_.empty()) {
      throw InternalError(
          "SampledHistory: a history was reached before the design activated "
          "it, so it holds no default sampled value to answer with");
    }
  }

  // A ring rather than a shifted sequence: the depth is fixed, so recording a
  // tick moves where the newest entry is instead of moving every entry.
  std::vector<T> entries_;
  std::size_t newest_ = 0;
};

}  // namespace lyra::runtime
