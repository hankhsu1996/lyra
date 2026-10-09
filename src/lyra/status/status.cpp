#include "lyra/status/status.hpp"

#include <algorithm>
#include <chrono>
#include <condition_variable>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <format>
#include <functional>
#include <memory>
#include <mutex>
#include <string>
#include <string_view>
#include <thread>
#include <utility>
#include <vector>

#include "lyra/status/render.hpp"

namespace lyra::status {

namespace {

using Clock = std::chrono::steady_clock;
using std::chrono_literals::operator""ms;
using std::chrono_literals::operator""s;

// A command that is over before this has shown nothing, which is every command
// a person does not have to wait for.
constexpr std::chrono::milliseconds kFirstLookInPlace = 1s;
constexpr std::chrono::milliseconds kBetweenLooksInPlace = 100ms;

// A stream that keeps what it is sent is sent little: nothing for a command
// that takes seconds, twice in the first half minute for one that does not,
// and once a minute from then on, which is often enough for whoever decides
// from silence that a command is stuck.
constexpr std::chrono::milliseconds kFirstLine = 10s;
constexpr std::chrono::milliseconds kSecondLine = 30s;
constexpr std::chrono::milliseconds kBetweenLines = 60s;

// A phase over sooner than the smallest time that is written took no time
// anyone would read, and a phase with nothing to do is one of those.
constexpr std::chrono::milliseconds kShortestPhaseListed = 100ms;

// Lines longer than the terminal is wide would wrap, and what was drawn could
// then not be found again to be erased. With wrapping off the terminal cuts a
// line at its edge instead.
constexpr std::string_view kWrapOff = "\x1b[?7l";
constexpr std::string_view kWrapOn = "\x1b[?7h";

// What the terminal is told of how far the command has got, in the form
// ConEmu introduced and other terminals took up: a share of the whole, that
// work is going on with no share to give, and that there is nothing to show.
constexpr std::string_view kNoProgress = "\x1b]9;4;0;0\x07";
constexpr std::string_view kProgressOfUnknownShare = "\x1b]9;4;3;0\x07";

struct PieceState {
  std::uint64_t id = 0;
  std::string name;
  Clock::time_point began;
};

struct PhaseState {
  std::string label;
  Clock::time_point began;
  // How long the phase had already taken before it was last begun.
  std::chrono::milliseconds taken_before{};
  std::size_t pieces = 0;
  std::size_t pieces_done = 0;
  std::size_t up_to_date = 0;
  // In the order they began.
  std::vector<PieceState> under_way;
  std::size_t widest_name = 0;
};

struct State {
  std::mutex lock;
  std::condition_variable ended;
  Look look;
  Clock::time_point began = Clock::now();
  // The innermost phase is last.
  std::vector<PhaseState> phases;
  // The phases that are over, in the order they ended.
  std::vector<PhaseDone> done;
  std::size_t errors = 0;
  std::uint64_t pieces_begun = 0;
  // How many lines of the terminal the status takes up, the cursor resting at
  // the end of the last.
  std::size_t lines_drawn = 0;
  bool told_the_terminal = false;
  // Set by whoever is about to print, and lifted when a phase next begins.
  bool withheld = false;
  bool ever_shown = false;
  bool ending = false;
  std::thread display;
};

// Never destroyed: a status may be taken off the terminal by a thread that
// outlives every static the process tears down as it exits.
auto TheState() -> State& {
  static State& state = *std::make_unique<State>().release();
  return state;
}

void Write(std::string_view text) {
  std::fwrite(text.data(), 1, text.size(), stderr);
  std::fflush(stderr);
}

auto Elapsed(Clock::time_point from, Clock::time_point to)
    -> std::chrono::milliseconds {
  return std::chrono::duration_cast<std::chrono::milliseconds>(to - from);
}

auto SnapshotOf(const State& state, Clock::time_point now) -> Snapshot {
  const PhaseState& phase = state.phases.back();
  Snapshot snapshot{
      .phase = phase.label,
      .phase_elapsed = phase.taken_before + Elapsed(phase.began, now),
      .elapsed = Elapsed(state.began, now),
      .pieces = phase.pieces,
      .pieces_done = phase.pieces_done,
      .up_to_date = phase.up_to_date,
      .errors = state.errors,
      .under_way = {},
      .widest_name = phase.widest_name};
  for (const PieceState& piece : phase.under_way) {
    if (!piece.name.empty()) {
      snapshot.under_way.push_back(
          PieceUnderWay{
              .name = piece.name, .elapsed = Elapsed(piece.began, now)});
    }
  }
  return snapshot;
}

// What takes the lines drawn off the terminal and leaves the cursor where the
// first of them began.
auto Erasure(const State& state) -> std::string {
  std::string erasure;
  if (state.lines_drawn > 1) {
    erasure += std::format("\x1b[{}A", state.lines_drawn - 1);
  }
  if (state.lines_drawn > 0) {
    erasure += "\r\x1b[J";
  }
  if (state.told_the_terminal) {
    erasure += kNoProgress;
  }
  return erasure;
}

void TakeOff(State& state) {
  const std::string erasure = Erasure(state);
  state.lines_drawn = 0;
  state.told_the_terminal = false;
  if (!erasure.empty()) {
    Write(erasure);
  }
}

// What the terminal is told of how far the phase under way has got.
auto ShareDone(const Snapshot& snapshot) -> std::string {
  if (snapshot.pieces == 0) {
    return std::string(kProgressOfUnknownShare);
  }
  return std::format(
      "\x1b]9;4;1;{}\x07", snapshot.pieces_done * 100 / snapshot.pieces);
}

// Redraws the status as the phases that are over and, below them, the phase
// under way where there is one. Between two phases the lines of the ones that
// are over stay, so the status does not vanish and come back.
void DrawInPlace(State& state, Clock::time_point now) {
  std::vector<std::string> lines =
      RenderPhasesDone(state.done, state.look.color);
  std::string told;
  if (!state.phases.empty()) {
    const Snapshot snapshot = SnapshotOf(state, now);
    for (std::string& line : RenderInPlace(snapshot, state.look.color)) {
      lines.push_back(std::move(line));
    }
    told = ShareDone(snapshot);
  }
  std::string drawn = Erasure(state);
  drawn += kWrapOff;
  for (std::size_t i = 0; i < lines.size(); ++i) {
    if (i > 0) {
      drawn += '\n';
    }
    drawn += lines[i];
  }
  drawn += kWrapOn;
  state.lines_drawn = lines.size();
  state.told_the_terminal = state.look.tells_the_terminal && !told.empty();
  if (state.told_the_terminal) {
    drawn += told;
  }
  state.ever_shown = state.ever_shown || !lines.empty();
  Write(drawn);
}

// Shows the state as it is now, which while someone else is printing is
// nothing.
void ShowNow(State& state, Clock::time_point now) {
  if (state.withheld) {
    TakeOff(state);
    return;
  }
  switch (state.look.display) {
    case Display::kNone:
      return;
    case Display::kInPlace:
      DrawInPlace(state, now);
      return;
    case Display::kPeriodicLines:
      if (!state.phases.empty()) {
        state.ever_shown = true;
        Write(RenderLine(SnapshotOf(state, now)) + "\n");
      }
      return;
  }
}

// How long after one look the next is due.
auto UntilNextLook(Display display, std::chrono::milliseconds last)
    -> std::chrono::milliseconds {
  switch (display) {
    case Display::kNone:
    case Display::kInPlace:
      return kBetweenLooksInPlace;
    case Display::kPeriodicLines:
      return last < kSecondLine ? kSecondLine - last : kBetweenLines;
  }
  return kBetweenLines;
}

void LookUntilEnded(State& state) {
  std::unique_lock lock(state.lock);
  std::chrono::milliseconds due =
      state.look.display == Display::kInPlace ? kFirstLookInPlace : kFirstLine;
  while (!state.ended.wait_until(
      lock, state.began + due, [&] { return state.ending; })) {
    ShowNow(state, Clock::now());
    due += UntilNextLook(state.look.display, due);
  }
}

}  // namespace

Shown::Shown(Look look) {
  State& state = TheState();
  const std::scoped_lock lock(state.lock);
  state.look = look;
  state.began = Clock::now();
  state.ending = false;
  if (look.display != Display::kNone) {
    state.display = std::thread(LookUntilEnded, std::ref(state));
  }
}

Shown::~Shown() {
  State& state = TheState();
  {
    const std::scoped_lock lock(state.lock);
    state.ending = true;
  }
  state.ended.notify_all();
  if (state.display.joinable()) {
    state.display.join();
  }
  const std::scoped_lock lock(state.lock);
  TakeOff(state);
  state.look = Look{};
}

Phase::Phase(std::string_view label) {
  State& state = TheState();
  const std::scoped_lock lock(state.lock);
  // A phase that begins again straight after it ended is the same phase going
  // on, so it takes back what it had counted.
  PhaseDone resumed{
      .phase = std::string(label), .elapsed = {}, .up_to_date = 0};
  if (!state.done.empty() && state.done.back().phase == label) {
    resumed = std::move(state.done.back());
    state.done.pop_back();
  }
  state.phases.push_back(
      PhaseState{
          .label = std::move(resumed.phase),
          .began = Clock::now(),
          .taken_before = resumed.elapsed,
          .pieces = 0,
          .pieces_done = 0,
          .up_to_date = resumed.up_to_date,
          .under_way = {},
          .widest_name = 0});
  state.withheld = false;
}

Phase::~Phase() {
  State& state = TheState();
  const std::scoped_lock lock(state.lock);
  PhaseState& phase = state.phases.back();
  const std::chrono::milliseconds elapsed =
      phase.taken_before + Elapsed(phase.began, Clock::now());
  if (elapsed >= kShortestPhaseListed) {
    state.done.push_back(
        PhaseDone{
            .phase = std::move(phase.label),
            .elapsed = elapsed,
            .up_to_date = phase.up_to_date});
  }
  state.phases.pop_back();
}

void Pieces(std::size_t count) {
  State& state = TheState();
  const std::scoped_lock lock(state.lock);
  if (!state.phases.empty()) {
    state.phases.back().pieces += count;
  }
}

Piece::Piece(std::string name) : id_(0) {
  State& state = TheState();
  const std::scoped_lock lock(state.lock);
  id_ = ++state.pieces_begun;
  if (!state.phases.empty()) {
    PhaseState& phase = state.phases.back();
    phase.widest_name = std::max(phase.widest_name, name.size());
    phase.under_way.push_back(
        PieceState{.id = id_, .name = std::move(name), .began = Clock::now()});
  }
}

Piece::~Piece() {
  State& state = TheState();
  const std::scoped_lock lock(state.lock);
  for (PhaseState& phase : state.phases) {
    if (std::erase_if(phase.under_way, [&](const PieceState& piece) {
          return piece.id == id_;
        }) > 0) {
      ++phase.pieces_done;
    }
  }
}

void UpToDate() {
  State& state = TheState();
  const std::scoped_lock lock(state.lock);
  if (!state.phases.empty()) {
    ++state.phases.back().up_to_date;
  }
}

void Errors(std::size_t count) {
  State& state = TheState();
  const std::scoped_lock lock(state.lock);
  state.errors += count;
}

void Clear() {
  State& state = TheState();
  const std::scoped_lock lock(state.lock);
  TakeOff(state);
  state.withheld = true;
}

void Finished() {
  State& state = TheState();
  const std::scoped_lock lock(state.lock);
  TakeOff(state);
  state.withheld = true;
  if (!state.ever_shown) {
    return;
  }
  const std::chrono::milliseconds elapsed = Elapsed(state.began, Clock::now());
  switch (state.look.display) {
    case Display::kNone:
      return;
    case Display::kInPlace: {
      std::string left;
      for (const std::string& line :
           RenderPhasesDone(state.done, state.look.color)) {
        left += line + "\n";
      }
      Write(left + RenderFinishedInPlace(elapsed, state.look.color) + "\n");
      return;
    }
    case Display::kPeriodicLines:
      Write(RenderFinishedLine(elapsed) + "\n");
      return;
  }
}

}  // namespace lyra::status
