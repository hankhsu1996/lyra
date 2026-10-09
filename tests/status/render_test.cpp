#include "lyra/status/render.hpp"

#include <chrono>
#include <gtest/gtest.h>
#include <string>
#include <vector>

namespace {

using lyra::status::PhaseDone;
using lyra::status::PieceUnderWay;
using lyra::status::RenderElapsed;
using lyra::status::RenderFinishedLine;
using lyra::status::RenderInPlace;
using lyra::status::RenderLine;
using lyra::status::RenderPhasesDone;
using lyra::status::Snapshot;
using namespace std::chrono_literals;

// A phase of six pieces under way at once, three minutes into a command, with
// what an earlier build kept and what went wrong both counted.
auto BusyPhase() -> Snapshot {
  return Snapshot{
      .phase = "Compiling",
      .phase_elapsed = 108s,
      .elapsed = 192s,
      .pieces = 1330,
      .pieces_done = 412,
      .up_to_date = 43,
      .errors = 2,
      .under_way =
          {{.name = "core", .elapsed = 41s},
           {.name = "cs_registers", .elapsed = 12s},
           {.name = "id_stage", .elapsed = 3400ms},
           {.name = "csr", .elapsed = 900ms},
           {.name = "alu", .elapsed = 200ms},
           {.name = "decoder", .elapsed = 100ms}},
      .widest_name = 12};
}

TEST(StatusRender, ATimeIsWrittenAtTheGrainItIsReadAt) {
  EXPECT_EQ(RenderElapsed(0ms), "0.0s");
  EXPECT_EQ(RenderElapsed(1449ms), "1.4s");
  EXPECT_EQ(RenderElapsed(9999ms), "9.9s");
  EXPECT_EQ(RenderElapsed(10s), "10s");
  EXPECT_EQ(RenderElapsed(59s), "59s");
  EXPECT_EQ(RenderElapsed(60s), "1m00s");
  EXPECT_EQ(RenderElapsed(192s), "3m12s");
  EXPECT_EQ(RenderElapsed(3600s), "1h00m");
  EXPECT_EQ(RenderElapsed(3725s), "1h02m");
}

// A terminal shows the phase under way with how long it has taken, and names
// the pieces that have been under way longest, their names in one column and
// their times in the next, counting the rest.
TEST(StatusRender, ATerminalShowsThePhaseThenThePiecesUnderWayLongest) {
  const std::vector<std::string> expected = {
      "Compiling  412/1330  1m48s  43 up to date  2 errors",
      "    core             41s",
      "    cs_registers     12s",
      "    id_stage        3.4s",
      "    csr             0.9s",
      "    2 more"};
  EXPECT_EQ(RenderInPlace(BusyPhase(), false), expected);
}

// A phase that is one step has no pieces to count or name, and nothing is said
// of what there is none of.
TEST(StatusRender, APhaseThatIsOneStepIsItsNameAndTheTime) {
  const Snapshot elaborating{
      .phase = "Elaborating",
      .phase_elapsed = 12s,
      .elapsed = 12s,
      .pieces = 0,
      .pieces_done = 0,
      .up_to_date = 0,
      .errors = 0,
      .under_way = {},
      .widest_name = 0};
  EXPECT_EQ(
      RenderInPlace(elaborating, false),
      std::vector<std::string>{"Elaborating  12s"});
  EXPECT_EQ(RenderLine(elaborating), "[12s] Elaborating");
  EXPECT_EQ(
      RenderInPlace(elaborating, true),
      std::vector<std::string>{"\x1b[1mElaborating\x1b[0m  \x1b[2m12s\x1b[0m"});
}

// A stream that keeps what it is sent gets the same moment as one line, which
// opens with a bracket so a reader that wants only diagnostics can drop it.
TEST(StatusRender, ALogGetsTheSameMomentAsOneLine) {
  EXPECT_EQ(
      RenderLine(BusyPhase()),
      "[3m12s] Compiling 412/1330, 43 up to date, 2 errors: core 41s, "
      "cs_registers 12s, 4 more");

  Snapshot one_error = BusyPhase();
  one_error.up_to_date = 0;
  one_error.errors = 1;
  one_error.under_way.resize(1);
  EXPECT_EQ(
      RenderLine(one_error), "[3m12s] Compiling 412/1330, 1 error: core 41s");
  EXPECT_EQ(RenderFinishedLine(221s), "[3m41s] Finished");
}

// What a command that has ended leaves on a terminal is a line per phase with
// the times in one column, and what a phase took from an earlier build.
TEST(StatusRender, EachPhaseThatIsOverKeepsALineWithItsTime) {
  const std::vector<PhaseDone> done = {
      {.phase = "Elaborating", .elapsed = 600ms, .up_to_date = 0},
      {.phase = "Compiling", .elapsed = 2600ms, .up_to_date = 45},
      {.phase = "Linking", .elapsed = 300ms, .up_to_date = 0}};
  const std::vector<std::string> expected = {
      "Elaborating    0.6s", "Compiling      2.6s  45 up to date",
      "Linking        0.3s"};
  EXPECT_EQ(RenderPhasesDone(done, false), expected);
}

}  // namespace
