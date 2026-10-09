#pragma once

#include <chrono>
#include <cstddef>
#include <span>
#include <string>
#include <vector>

namespace lyra::status {

// A piece being worked on, and for how long it has been.
struct PieceUnderWay {
  std::string name;
  std::chrono::milliseconds elapsed{};
};

// A phase that is over: how long it took, and how many of its pieces were
// taken from what an earlier build kept.
struct PhaseDone {
  std::string phase;
  std::chrono::milliseconds elapsed{};
  std::size_t up_to_date = 0;
};

// What a command is doing at one moment.
struct Snapshot {
  std::string phase;
  // How long the phase has been under way.
  std::chrono::milliseconds phase_elapsed{};
  // How long the command has run.
  std::chrono::milliseconds elapsed{};
  // The pieces the phase is made of, and how many of them are done. A phase
  // that is one indivisible step has none.
  std::size_t pieces = 0;
  std::size_t pieces_done = 0;
  // How many of the pieces done were taken from what an earlier build kept.
  std::size_t up_to_date = 0;
  // How many errors the command has to report when it ends.
  std::size_t errors = 0;
  // The named pieces being worked on, the one begun first at the front.
  std::vector<PieceUnderWay> under_way;
  // The longest name of any piece the phase has had under way. The column of
  // names is this wide, so it does not shift as pieces come and go.
  std::size_t widest_name = 0;
};

// A length of time as a person reads one: tenths of a second below ten
// seconds, whole seconds below a minute, then minutes and seconds, then hours
// and minutes.
auto RenderElapsed(std::chrono::milliseconds elapsed) -> std::string;

// A line for each phase that is over, saying how long it took. A phase too
// short to see while it ran is seen here, and the lines are where a command's
// time is read off once it has ended.
auto RenderPhasesDone(std::span<const PhaseDone> done, bool color)
    -> std::vector<std::string>;

// The lines a terminal shows for `snapshot`: the phase under way with its
// counts and how long it has taken, then the pieces that have been under way
// longest, which is where a piece that is slow ends up, and how many more
// there are.
auto RenderInPlace(const Snapshot& snapshot, bool color)
    -> std::vector<std::string>;

// The same moment as one line for a stream that keeps what it is sent. It
// opens with the elapsed time in brackets, which nothing else the compiler
// prints does.
auto RenderLine(const Snapshot& snapshot) -> std::string;

// The line left where a status was, once the command has done what it was
// asked.
auto RenderFinishedInPlace(std::chrono::milliseconds elapsed, bool color)
    -> std::string;
auto RenderFinishedLine(std::chrono::milliseconds elapsed) -> std::string;

}  // namespace lyra::status
