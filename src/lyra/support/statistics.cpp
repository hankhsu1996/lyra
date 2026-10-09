#include "lyra/support/statistics.hpp"

#include <algorithm>
#include <atomic>
#include <charconv>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <mutex>
#include <nlohmann/json.hpp>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"

namespace lyra::support {

namespace {

struct StagePeak {
  std::string name;
  // Absent where the platform offers no reading of it.
  std::optional<std::uint64_t> peak_rss_bytes;
};

struct UnitRecord {
  std::string name;
  std::vector<Artifact> artifacts;
};

struct ChildRecord {
  std::string command;
  ChildUsage usage;
};

struct Statistics {
  std::atomic<bool> enabled = false;
  std::mutex lock;
  std::size_t width = 0;
  std::vector<StagePeak> stages;
  std::vector<UnitRecord> units;
  std::vector<ChildRecord> children;
};

auto TheStatistics() -> Statistics& {
  static Statistics statistics;
  return statistics;
}

// Linux keeps the process's high-water mark of resident memory and lets the
// process lower it to what it holds now, which is what makes a peak per stage
// readable. Where the mark cannot be lowered or read, a stage is recorded
// without one: a number standing in for it would be a measurement nobody made.
auto ResetPeakResidentSize() -> bool {
  std::ofstream clear("/proc/self/clear_refs");
  clear << "5";
  clear.flush();
  return clear.good();
}

auto PeakResidentSize() -> std::optional<std::uint64_t> {
  std::ifstream status("/proc/self/status");
  std::string line;
  while (std::getline(status, line)) {
    constexpr std::string_view kField = "VmHWM:";
    if (!line.starts_with(kField)) {
      continue;
    }
    const std::string_view value = std::string_view(line).substr(kField.size());
    const std::size_t start = value.find_first_not_of(" \t");
    std::uint64_t kib = 0;
    if (start == std::string_view::npos ||
        std::from_chars(value.data() + start, value.data() + value.size(), kib)
                .ec != std::errc{}) {
      return std::nullopt;
    }
    return kib * 1024;
  }
  return std::nullopt;
}

auto KindName(ArtifactKind kind) -> std::string_view {
  switch (kind) {
    case ArtifactKind::kObject:
      return "object";
    case ArtifactKind::kCppSource:
      return "C++ source";
    case ArtifactKind::kCppHeader:
      return "C++ header";
  }
  throw InternalError("an artifact kind has no name");
}

auto Enabled() -> bool {
  return TheStatistics().enabled;
}

}  // namespace

void EnableStatistics() {
  TheStatistics().enabled = true;
}

void RecordWidth(std::size_t width) {
  if (!Enabled()) {
    return;
  }
  Statistics& statistics = TheStatistics();
  const std::scoped_lock lock(statistics.lock);
  statistics.width = width;
}

StageMemory::StageMemory(std::string_view stage) {
  if (!Enabled()) {
    return;
  }
  recording_.emplace(
      Recording{
          .stage = std::string(stage),
          .mark_lowered = ResetPeakResidentSize()});
}

StageMemory::~StageMemory() {
  if (!recording_.has_value()) {
    return;
  }
  // A mark that was not lowered still holds an earlier stage's peak.
  const std::optional<std::uint64_t> peak =
      recording_->mark_lowered ? PeakResidentSize() : std::nullopt;
  Statistics& statistics = TheStatistics();
  const std::scoped_lock lock(statistics.lock);
  statistics.stages.push_back(
      StagePeak{.name = std::move(recording_->stage), .peak_rss_bytes = peak});
}

void RecordUnit(std::string_view unit, std::vector<Artifact> artifacts) {
  if (!Enabled()) {
    return;
  }
  Statistics& statistics = TheStatistics();
  const std::scoped_lock lock(statistics.lock);
  statistics.units.push_back(
      UnitRecord{.name = std::string(unit), .artifacts = std::move(artifacts)});
}

void RecordChild(std::string command, ChildUsage usage) {
  if (!Enabled()) {
    return;
  }
  Statistics& statistics = TheStatistics();
  const std::scoped_lock lock(statistics.lock);
  statistics.children.push_back(
      ChildRecord{.command = std::move(command), .usage = usage});
}

auto WriteStatistics(const std::filesystem::path& path)
    -> std::expected<void, std::string> {
  Statistics& statistics = TheStatistics();
  const std::scoped_lock lock(statistics.lock);

  // Units finish in whatever order the threads took them, so the file lists
  // them by name and two runs of one design read the same.
  std::ranges::sort(statistics.units, {}, &UnitRecord::name);

  nlohmann::ordered_json stages = nlohmann::ordered_json::array();
  for (const StagePeak& stage : statistics.stages) {
    nlohmann::ordered_json entry = {{"name", stage.name}};
    if (stage.peak_rss_bytes.has_value()) {
      entry["peak_rss_bytes"] = *stage.peak_rss_bytes;
    }
    stages.push_back(std::move(entry));
  }
  nlohmann::ordered_json units = nlohmann::ordered_json::array();
  for (const UnitRecord& unit : statistics.units) {
    nlohmann::ordered_json artifacts = nlohmann::ordered_json::array();
    for (const Artifact& artifact : unit.artifacts) {
      artifacts.push_back(
          {{"kind", KindName(artifact.kind)},
           {"name", artifact.name},
           {"bytes", artifact.bytes},
           {"made", artifact.made}});
    }
    units.push_back({{"name", unit.name}, {"artifacts", std::move(artifacts)}});
  }
  nlohmann::ordered_json children = nlohmann::ordered_json::array();
  for (const ChildRecord& child : statistics.children) {
    children.push_back(
        {{"command", child.command},
         {"wall_us", child.usage.wall_us},
         {"cpu_us", child.usage.cpu_us}});
  }
  const nlohmann::ordered_json document = {
      {"width", statistics.width},
      {"stages", std::move(stages)},
      {"units", std::move(units)},
      {"children", std::move(children)}};

  std::ofstream out(path);
  out << document.dump(2) << '\n';
  out.flush();
  if (!out) {
    return std::unexpected(
        std::format("failed to write statistics to '{}'", path.string()));
  }
  return {};
}

}  // namespace lyra::support
