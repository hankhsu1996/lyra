#include "lyra/runtime/read_report.hpp"

#include <algorithm>
#include <cstdint>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/observation.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/value/packed.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

namespace {

// How deep reports may nest before a report stops following calls. It bounds
// only a cycle of calls: no program without one nests reports past the depth
// its own calls nest, which is far below this.
constexpr std::int64_t kReportDepthBound = 64;

}  // namespace

ReadReport::ReadReport(bool an_evaluation_reports)
    : an_evaluation_reports_(an_evaluation_reports) {
}

auto ReadReport::Empty() -> ReadReport {
  return ReadReport{true};
}

auto ReadReport::ForImplicitList() -> ReadReport {
  return ReadReport{false};
}

ReadReport::ReadReport(ReadReport&&) noexcept = default;
auto ReadReport::operator=(ReadReport&&) noexcept -> ReadReport& = default;
ReadReport::~ReadReport() = default;

void ReadReport::Reach(Trigger trigger) {
  (calls_on_handles_ == 0 ? read_directly_ : through_handles_)
      .push_back(std::move(trigger));
}

// A place is watched only for being reached: the process decides by its own
// evaluation, so nothing is asked where the change happens, and an implicit
// list wakes on any change to what it lists.
void ReadReport::Add(
    Observable* place, const value::PackedArray& lsb_bit_offset,
    const value::PackedArray& bit_width) {
  Reach(Trigger{place, Observation::OnReaching(), lsb_bit_offset, bit_width});
}

void ReadReport::AddThroughHandle(
    Observable* place, const value::PackedArray& lsb_bit_offset,
    const value::PackedArray& bit_width) {
  through_handles_.emplace_back(
      place, Observation::OnReaching(), lsb_bit_offset, bit_width);
}

void ReadReport::EnterCallOnHandle() {
  ++calls_on_handles_;
}

void ReadReport::LeaveCallOnHandle() {
  --calls_on_handles_;
}

void ReadReport::AddWrite(
    Observable* place, const value::PackedArray& lsb_bit_offset,
    const value::PackedArray& bit_width) {
  writes_.push_back(
      Written{
          .place = place,
          .bits = {
              .lsb = static_cast<std::uint64_t>(lsb_bit_offset.ToInt64()),
              .width = static_cast<std::uint64_t>(bit_width.ToInt64())}});
}

void ReadReport::SettleAsImplicitList() {
  // Bits `[first, end)` of a place.
  using Interval = std::pair<std::uint64_t, std::uint64_t>;
  // What the list watches at one place: all of it, or these intervals of its
  // bits. A place is listed once however many reads reached it, since a write
  // there is tested once per leaf.
  struct Watched {
    Observable* place = nullptr;
    bool whole = false;
    std::vector<Interval> intervals;
  };
  std::vector<Watched> watched;
  for (const Trigger& read : read_directly_) {
    std::vector<Interval> intervals{
        {read.reads.lsb, read.reads.lsb + read.reads.width}};
    bool taken_whole = false;
    for (const Written& write : writes_) {
      if (write.place != read.observable) continue;
      if (write.bits.width == 0) {
        taken_whole = true;
        break;
      }
      if (read.reads.width == 0) continue;
      const std::uint64_t first = write.bits.lsb;
      const std::uint64_t end = write.bits.lsb + write.bits.width;
      std::vector<Interval> kept;
      for (const auto& [lo, hi] : intervals) {
        if (end <= lo || hi <= first) {
          kept.emplace_back(lo, hi);
          continue;
        }
        if (lo < first) kept.emplace_back(lo, first);
        if (end < hi) kept.emplace_back(end, hi);
      }
      intervals = std::move(kept);
    }
    if (taken_whole) continue;
    auto at = std::ranges::find(watched, read.observable, &Watched::place);
    if (at == watched.end()) {
      at = watched.insert(
          watched.end(),
          Watched{.place = read.observable, .whole = false, .intervals = {}});
    }
    if (read.reads.width == 0) {
      at->whole = true;
    } else {
      at->intervals.insert(
          at->intervals.end(), intervals.begin(), intervals.end());
    }
  }

  std::vector<Trigger> left;
  const auto leaf = [&](Observable* place, value::BitPositions reads) {
    Trigger trigger;
    trigger.observable = place;
    trigger.observation = Observation::OnReaching();
    trigger.reads = reads;
    left.push_back(std::move(trigger));
  };
  for (Watched& place : watched) {
    if (place.whole) {
      leaf(place.place, {});
      continue;
    }
    std::ranges::sort(place.intervals);
    std::vector<Interval> merged;
    for (const Interval& interval : place.intervals) {
      if (!merged.empty() && interval.first <= merged.back().second) {
        merged.back().second = std::max(merged.back().second, interval.second);
      } else {
        merged.push_back(interval);
      }
    }
    for (const auto& [lo, hi] : merged) {
      leaf(place.place, {.lsb = lo, .width = hi - lo});
    }
  }
  read_directly_ = std::move(left);
  through_handles_.clear();
  writes_.clear();
}

void ReadReport::AddEveryObject() {
  Trigger trigger;
  trigger.observable = &current_runtime().EveryObject();
  through_handles_.push_back(std::move(trigger));
}

auto ReadReport::TakeTriggers() -> std::vector<Trigger> {
  std::vector<Trigger> triggers = std::exchange(read_directly_, {});
  triggers.insert(
      triggers.end(), std::make_move_iterator(through_handles_.begin()),
      std::make_move_iterator(through_handles_.end()));
  through_handles_.clear();
  // A wait watches what was written as well (LRM 9.4.2 leaves nothing out),
  // so the writes only an implicit list reads are dropped with the rest.
  writes_.clear();
  return triggers;
}

auto ReadReport::Enter() -> std::int64_t {
  if (depth_ == kReportDepthBound) {
    AddEveryObject();
    return 0;
  }
  ++depth_;
  return 1;
}

void ReadReport::Leave() {
  --depth_;
}

// A report a function makes nests in the report of whatever called it, so the
// call the evaluation made is the one that leaves nothing open.
auto ReadReport::RunsTheBody() const -> std::int64_t {
  return an_evaluation_reports_ && depth_ == 0 ? 1 : 0;
}

void RefuseReport(std::string_view why) {
  throw SimulationError(
      "waiting on an expression that calls this function is not yet "
      "supported: " +
      std::string{why});
}

}  // namespace lyra::runtime
