#include "lyra/status/render.hpp"

#include <algorithm>
#include <chrono>
#include <cstddef>
#include <format>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace lyra::status {

namespace {

using std::chrono_literals::operator""s;

// How many pieces under way are named before the rest are only counted. A
// terminal has the lines to name more than one line of a log does.
constexpr std::size_t kNamedInPlace = 4;
constexpr std::size_t kNamedInALine = 2;

constexpr std::string_view kBold = "\x1b[1m";
constexpr std::string_view kDim = "\x1b[2m";
constexpr std::string_view kPlain = "\x1b[0m";

auto Styled(std::string_view style, std::string_view text, bool color)
    -> std::string {
  return color ? std::format("{}{}{}", style, text, kPlain) : std::string(text);
}

// How far through its pieces the phase is, after `lead`. A phase that is one
// indivisible step has no such thing to say.
auto Progress(const Snapshot& snapshot, std::string_view lead) -> std::string {
  if (snapshot.pieces == 0) {
    return {};
  }
  return std::format("{}{}/{}", lead, snapshot.pieces_done, snapshot.pieces);
}

// What else the command has counted and is worth a word only once there is
// any: the pieces that cost nothing, and what went wrong.
auto Remarks(const Snapshot& snapshot) -> std::vector<std::string> {
  std::vector<std::string> remarks;
  if (snapshot.up_to_date > 0) {
    remarks.push_back(std::format("{} up to date", snapshot.up_to_date));
  }
  if (snapshot.errors > 0) {
    remarks.push_back(
        std::format(
            "{} error{}", snapshot.errors, snapshot.errors == 1 ? "" : "s"));
  }
  return remarks;
}

}  // namespace

auto RenderElapsed(std::chrono::milliseconds elapsed) -> std::string {
  const auto tenths = elapsed.count() / 100;
  const auto seconds = elapsed.count() / 1000;
  if (elapsed < 10s) {
    return std::format("{}.{}s", tenths / 10, tenths % 10);
  }
  if (seconds < 60) {
    return std::format("{}s", seconds);
  }
  if (seconds < 3600) {
    return std::format("{}m{:02}s", seconds / 60, seconds % 60);
  }
  return std::format("{}h{:02}m", seconds / 3600, seconds / 60 % 60);
}

auto RenderPhasesDone(std::span<const PhaseDone> done, bool color)
    -> std::vector<std::string> {
  std::size_t widest = 0;
  for (const PhaseDone& phase : done) {
    widest = std::max(widest, phase.phase.size());
  }
  std::vector<std::string> lines;
  lines.reserve(done.size());
  for (const PhaseDone& phase : done) {
    std::string line = std::format(
        "{:<{}}  {}", phase.phase, widest,
        Styled(
            kDim, std::format("{:>6}", RenderElapsed(phase.elapsed)), color));
    if (phase.up_to_date > 0) {
      line += std::format("  {} up to date", phase.up_to_date);
    }
    lines.push_back(std::move(line));
  }
  return lines;
}

auto RenderInPlace(const Snapshot& snapshot, bool color)
    -> std::vector<std::string> {
  std::string header = std::format(
      "{}{}  {}", Styled(kBold, snapshot.phase, color),
      Progress(snapshot, "  "),
      Styled(kDim, RenderElapsed(snapshot.phase_elapsed), color));
  for (const std::string& remark : Remarks(snapshot)) {
    header += std::format("  {}", remark);
  }
  std::vector<std::string> lines = {std::move(header)};

  const std::size_t named = std::min(kNamedInPlace, snapshot.under_way.size());
  for (std::size_t i = 0; i < named; ++i) {
    const PieceUnderWay& piece = snapshot.under_way[i];
    lines.push_back(
        std::format(
            "    {:<{}}  {}", piece.name, snapshot.widest_name,
            Styled(
                kDim, std::format("{:>6}", RenderElapsed(piece.elapsed)),
                color)));
  }
  if (snapshot.under_way.size() > named) {
    lines.push_back(
        std::format("    {} more", snapshot.under_way.size() - named));
  }
  return lines;
}

auto RenderLine(const Snapshot& snapshot) -> std::string {
  std::string line = std::format(
      "[{}] {}{}", RenderElapsed(snapshot.elapsed), snapshot.phase,
      Progress(snapshot, " "));
  for (const std::string& remark : Remarks(snapshot)) {
    line += std::format(", {}", remark);
  }
  std::string_view before = ": ";
  const std::size_t named = std::min(kNamedInALine, snapshot.under_way.size());
  for (std::size_t i = 0; i < named; ++i) {
    const PieceUnderWay& piece = snapshot.under_way[i];
    line += std::format(
        "{}{} {}", before, piece.name, RenderElapsed(piece.elapsed));
    before = ", ";
  }
  if (snapshot.under_way.size() > named) {
    line += std::format("{}{} more", before, snapshot.under_way.size() - named);
  }
  return line;
}

auto RenderFinishedInPlace(std::chrono::milliseconds elapsed, bool color)
    -> std::string {
  return std::format(
      "{} in {}", Styled(kBold, "Finished", color), RenderElapsed(elapsed));
}

auto RenderFinishedLine(std::chrono::milliseconds elapsed) -> std::string {
  return std::format("[{}] Finished", RenderElapsed(elapsed));
}

}  // namespace lyra::status
