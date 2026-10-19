#include "lyra/runtime/read_report.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <span>
#include <string_view>
#include <tuple>
#include <utility>
#include <vector>

#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/object_change.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/observation.hpp"
#include "lyra/runtime/part_path.hpp"
#include "lyra/runtime/running_state.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::runtime {

namespace {

// How deep the functions an implicit list follows may nest before it stops
// following calls. It bounds only a cycle of calls: no program without one
// nests past the depth its own calls nest, which is far below this.
constexpr std::int64_t kReportDepthBound = 64;

// An order on what a reached place is watched for -- the place, the part of
// it, the positions of that part -- so that one reached many times, by every
// call of a function a loop makes, is watched once. Every place an evaluation
// reaches is watched only for being reached, so nothing else tells two apart.
auto LeafKey(const Trigger& trigger) {
  return std::tuple{
      trigger.observable, trigger.through, trigger.part.IsInexact(),
      trigger.reads.lsb, trigger.reads.width};
}

auto LeafBefore(const Trigger& a, const Trigger& b) -> bool {
  if (LeafKey(a) != LeafKey(b)) return LeafKey(a) < LeafKey(b);
  return std::ranges::lexicographical_compare(a.part.Steps(), b.part.Steps());
}

auto SameLeaf(const Trigger& a, const Trigger& b) -> bool {
  return LeafKey(a) == LeafKey(b) && a.part == b.part;
}

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

// A place is watched only for being reached: the process decides by its own
// evaluation, so nothing is asked where the change happens, and an implicit
// list wakes on any change to what it lists.
void ReadReport::Add(
    Observable* place, std::span<const std::int64_t> steps,
    std::int64_t lsb_bit_offset, std::int64_t bit_width) {
  Add(WatchedPlace{place}, steps, lsb_bit_offset, bit_width);
}

void ReadReport::Add(
    WatchedPlace place, std::span<const std::int64_t> steps,
    std::int64_t lsb_bit_offset, std::int64_t bit_width) {
  reached_.emplace_back(
      place, Observation::OnReaching(), steps, lsb_bit_offset, bit_width);
}

void ReadReport::AddObject(GcObject* object) {
  if (object != nullptr) {
    std::shared_ptr<GcObject> owned = object->weak_from_this().lock();
    if (owned == nullptr) return;
    objects_.push_back(std::move(owned));
  }
  reached_.emplace_back(
      WatchedPlace{EventSourceOf(object)}, Observation::OnReaching(),
      std::span<const std::int64_t>{}, 0, 0);
}

void ReadReport::AddWrite(
    Observable* place, std::span<const std::int64_t> steps,
    std::int64_t lsb_bit_offset, std::int64_t bit_width) {
  AddWrite(WatchedPlace{place}, steps, lsb_bit_offset, bit_width);
}

void ReadReport::AddWrite(
    WatchedPlace place, std::span<const std::int64_t> steps,
    std::int64_t lsb_bit_offset, std::int64_t bit_width) {
  writes_.push_back(
      Written{
          .place = place.ReportedTo(),
          .part = place.PathOf(steps),
          .bits = {
              .lsb = static_cast<std::uint64_t>(lsb_bit_offset),
              .width = static_cast<std::uint64_t>(bit_width)}});
}

void ReadReport::SettleAsImplicitList() {
  // Positions `[first, end)` of a part: bits of a packed value, elements of
  // an array.
  using Interval = std::pair<std::uint64_t, std::uint64_t>;
  // What the list watches of one part of a place: all of it, or these
  // intervals of its positions. A part is listed once however many reads
  // reached it, since a write there is tested once per leaf.
  struct Watched {
    Observable* place = nullptr;
    const ErasedReference* through = nullptr;
    PartPath part;
    bool whole = false;
    std::vector<Interval> intervals;
  };
  std::vector<Watched> watched;
  for (const Trigger& read : reached_) {
    std::vector<Interval> intervals{
        {read.reads.lsb, read.reads.lsb + read.reads.width}};
    bool taken_whole = false;
    for (const Written& write : writes_) {
      // An inexact path says only that the write was somewhere in a part, so
      // it takes out nothing.
      if (write.place != read.observable || write.part.IsInexact()) {
        continue;
      }
      const std::size_t read_depth = read.part.Steps().size();
      const std::size_t write_depth = write.part.Steps().size();
      const bool write_whole = write.bits.width == 0;
      // The positions of the read's part the write covers.
      std::uint64_t first = 0;
      std::uint64_t end = 0;
      if (write.part.IsPrefixOf(read.part)) {
        // The write is of the read's part or of one holding it: all of that
        // part, or the positions it names, which one level up are the element
        // the read's part lies in.
        const bool holds_read =
            write_depth < read_depth &&
            write.bits.lsb <= read.part.Steps()[write_depth] &&
            read.part.Steps()[write_depth] - write.bits.lsb < write.bits.width;
        if (write_whole || holds_read) {
          taken_whole = true;
          break;
        }
        if (write_depth != read_depth) continue;
        first = write.bits.lsb;
        end = write.bits.lsb + write.bits.width;
      } else if (
          write_whole && write_depth == read_depth + 1 &&
          read.part.IsPrefixOf(write.part)) {
        // The write is of one element of the array the read names some of.
        first = write.part.Steps()[read_depth];
        end = first + 1;
      } else {
        continue;
      }
      if (read.reads.width == 0) continue;
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
    auto at = std::ranges::find_if(watched, [&](const Watched& listed) {
      return listed.place == read.observable &&
             listed.through == read.through && listed.part == read.part;
    });
    if (at == watched.end()) {
      at = watched.insert(
          watched.end(), Watched{
                             .place = read.observable,
                             .through = read.through,
                             .part = read.part,
                             .whole = false,
                             .intervals = {}});
    }
    if (read.reads.width == 0) {
      at->whole = true;
    } else {
      at->intervals.insert(
          at->intervals.end(), intervals.begin(), intervals.end());
    }
  }

  std::vector<Trigger> left;
  const auto leaf = [&](const Watched& place, value::BitPositions reads) {
    Trigger trigger;
    trigger.observable = place.place;
    trigger.through = place.through;
    trigger.part = place.part;
    trigger.observation = Observation::OnReaching();
    trigger.reads = reads;
    left.push_back(std::move(trigger));
  };
  for (Watched& place : watched) {
    if (place.whole) {
      leaf(place, {});
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
      leaf(place, {.lsb = lo, .width = hi - lo});
    }
  }
  reached_ = std::move(left);
  writes_.clear();
}

auto ReadReport::TakeReached() -> ReachedPlaces {
  ReachedPlaces reached{
      .triggers = std::exchange(reached_, {}),
      .objects = std::exchange(objects_, {})};
  std::ranges::sort(reached.triggers, LeafBefore);
  const auto repeated_leaves = std::ranges::unique(reached.triggers, SameLeaf);
  reached.triggers.erase(repeated_leaves.begin(), repeated_leaves.end());
  std::ranges::sort(reached.objects);
  const auto repeated_objects = std::ranges::unique(reached.objects);
  reached.objects.erase(repeated_objects.begin(), repeated_objects.end());
  // A wait watches what was written as well (LRM 9.4.2 leaves nothing out),
  // so the writes only an implicit list reads are dropped with the rest.
  writes_.clear();
  return reached;
}

auto ReadReport::Enter() -> std::int64_t {
  if (depth_ == kReportDepthBound) {
    return 0;
  }
  ++depth_;
  return 1;
}

void ReadReport::Leave() {
  --depth_;
}

auto ReadReport::RunsTheBody() const -> std::int64_t {
  return an_evaluation_reports_ ? 1 : 0;
}

void ReadReport::BeginEvaluation(RuntimeEffects& effects) {
  displaced_ = std::exchange(effects.Running().evaluation_report, this);
}

// The end runs on every way out of the evaluation, one of which is the frame
// being released where it stands as the run ends, with nothing running.
void ReadReport::EndEvaluation(RuntimeEffects& effects) noexcept {
  if (RunningState* running = effects.TryRunning(); running != nullptr) {
    running->evaluation_report = displaced_;
  }
}

auto EvaluationInProgress(RuntimeEffects& effects) -> ReadReport* {
  RunningState* running = effects.TryRunning();
  return running == nullptr ? nullptr : running->evaluation_report;
}

void RefuseReport(std::string_view why) {
  throw SimulationError(
      "waiting on an expression that calls this function is not yet "
      "supported, because {}",
      why);
}

}  // namespace lyra::runtime
