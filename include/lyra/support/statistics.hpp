#pragma once

#include <cstddef>
#include <cstdint>
#include <expected>
#include <filesystem>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/base/diag_message.hpp"

namespace lyra::support {

// The numbers a run records about itself when asked, for whoever compares two
// runs or reads one: each stage's peak memory, what each unit holds and what it
// left behind, how often each kind of work ran, and how long each tool the
// build ran took. Where the compiler's own time went is the trace's to say, so
// none of it is here.
//
// Nothing is recorded unless the run enables it, and every recording call is
// then a single check. Units are recorded from whichever thread worked on them.
void EnableStatistics();

// Whether the run records, for a caller whose record takes working out.
[[nodiscard]] auto RecordsStatistics() -> bool;

// How many units the run lowers and compiles at once, which is what the time a
// stage of several units took has to be read against.
void RecordWidth(std::size_t width);

// A stage that runs alone, held for as long as it runs. Its peak is the most
// memory the process or a tool it ran held from the stage's start to its end:
// one process holds every unit of a design, so the peak of the whole run says
// nothing about which stage to look at, and what a stage that compiles a
// program costs is mostly its compiler's. Stages of different units overlap and
// share one heap, so they are measured together, as the one stage that runs
// them. On a platform that cannot give a stage's own peak, the stage is
// recorded without one.
class StageMemory {
 public:
  explicit StageMemory(std::string_view stage);
  ~StageMemory();

  StageMemory(const StageMemory&) = delete;
  auto operator=(const StageMemory&) -> StageMemory& = delete;
  StageMemory(StageMemory&&) = delete;
  auto operator=(StageMemory&&) -> StageMemory& = delete;

 private:
  struct Recording {
    std::string stage;
    // Whether the process's high-water mark was lowered as the stage began,
    // without which what it reads at the end is not this stage's.
    bool mark_lowered = false;
  };

  // Absent in a run that records nothing.
  std::optional<Recording> recording_;
};

enum class ArtifactKind : std::uint8_t { kObject, kCppSource, kCppHeader };

// A file a unit left behind. A kept object is one this run took from the store
// rather than made, which is why it cost the run nothing.
struct Artifact {
  ArtifactKind kind;
  std::string name;
  std::uint64_t bytes = 0;
  bool made = true;
};

void RecordUnit(std::string_view unit, std::vector<Artifact> artifacts);

// How many of one kind of thing a unit holds.
struct Count {
  std::string kind;
  std::uint64_t held = 0;
};

// What a unit holds in one of the forms it passes through, by kind. A file's
// size says how much a unit left behind and not what of: a unit that holds
// twice the classes and one that holds twice the statements leave the same
// bytes, and only one of them was written twice by the design.
void RecordCounts(
    std::string_view unit, std::string_view form, std::vector<Count> counts);

// How deeply a unit nests in one of its forms, at the deepest. Not a count of
// things held: two units together nest no deeper than the deeper of them.
void RecordNestingDepth(
    std::string_view unit, std::string_view form, std::uint64_t depth);

// One more of a kind of work the run did, counted where the work is done and
// for the run as a whole, whichever unit it was done for, since one unit may
// do it several times. A count says how often the work ran, which one tree
// answers alike on every run, where its time does not.
void RecordWork(std::string_view kind);

// How long one tool the build ran took: on the clock, and of processor time as
// the system charges it once the child has been reaped; and the most memory
// the system charges it, which it starts from what the process held as it
// started the tool, so it is read only beside the process's own, as part of
// the peak of the stage that ran the tool.
struct ChildUsage {
  std::uint64_t wall_us = 0;
  std::uint64_t cpu_us = 0;
  std::uint64_t peak_rss_bytes = 0;
};

void RecordChild(std::string command, ChildUsage usage);

auto WriteStatistics(const std::filesystem::path& path)
    -> std::expected<void, base::DiagMessage>;

}  // namespace lyra::support
