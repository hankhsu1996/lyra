#pragma once

#include <cstddef>
#include <cstdint>
#include <string>
#include <string_view>

namespace lyra::status {

// What a command is doing at this moment, shown on the error stream to whoever
// is waiting on it, in the words of someone who wrote the design and not the
// compiler: the phase under way, how many of its pieces are done, which pieces
// are being worked on and for how long.
//
// What is shown is the state when the display next looks: a piece that begins
// and ends between two looks was never on the screen, and a command that ends
// before the first look has shown nothing at all. A phase that is over keeps a
// line saying how long it took, so one too short to be seen under way is seen
// there. Where inside a phase a run's cost went is the trace's to say.
//
// On a terminal the status is a few lines redrawn in place several times a
// second, and it is taken off before anything else is printed. Anywhere else
// it is one line at intervals that lengthen to a minute, which tells whoever
// watches a log that the command is alive without filling the log.
enum class Display : std::uint8_t { kNone, kPeriodicLines, kInPlace };

struct Look {
  Display display = Display::kNone;
  bool color = false;
  // Whether the terminal is one that shows how far a command has got on its
  // own window, and is told.
  bool tells_the_terminal = false;
};

// Statuses are shown for as long as this lives. A command makes one as it
// begins, and how long the command has run is counted from then.
class Shown {
 public:
  explicit Shown(Look look);
  ~Shown();

  Shown(const Shown&) = delete;
  auto operator=(const Shown&) -> Shown& = delete;
  Shown(Shown&&) = delete;
  auto operator=(Shown&&) -> Shown& = delete;
};

// A phase of the command's work, under way for as long as this lives. A phase
// begun inside another is the one shown until it ends.
class Phase {
 public:
  explicit Phase(std::string_view label);
  ~Phase();

  Phase(const Phase&) = delete;
  auto operator=(const Phase&) -> Phase& = delete;
  Phase(Phase&&) = delete;
  auto operator=(Phase&&) -> Phase& = delete;
};

// The phase under way is made of this many more pieces.
void Pieces(std::size_t count);

// One piece of the phase under way, being worked on for as long as this lives
// and done when it goes. It may begin and end on any thread. `name` is what the
// design's author calls the piece; a piece they never named is given none, and
// is counted without being listed.
class Piece {
 public:
  explicit Piece(std::string name);
  ~Piece();

  Piece(const Piece&) = delete;
  auto operator=(const Piece&) -> Piece& = delete;
  Piece(Piece&&) = delete;
  auto operator=(Piece&&) -> Piece& = delete;

 private:
  std::uint64_t id_;
};

// One piece of the phase under way was taken from what an earlier build kept,
// and cost this one nothing.
void UpToDate();

// The command has `count` more errors to report when it ends.
void Errors(std::size_t count);

// Takes the status off the terminal, so that what is printed next starts on a
// line of its own. Nothing is shown again until another phase begins.
void Clear();

// The same, for a command that did what it was asked: where a status was ever
// shown, how long the command took is left in its place, and on a terminal how
// long each of its phases took.
void Finished();

}  // namespace lyra::status
