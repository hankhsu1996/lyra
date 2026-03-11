#include "tests/framework/timing_collector.hpp"

#include <algorithm>
#include <cstdio>
#include <format>
#include <string>
#include <utility>
#include <vector>

namespace lyra::test {

namespace {

bool g_timing_enabled =
    false;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

}  // namespace

void TimingCollector::Record(const std::string& name, const TestTimings& t) {
  entries_.emplace_back(name, t);
}

void TimingCollector::PrintSummary() const {
  if (entries_.empty()) return;

  double total_parse = 0.0;
  double total_hir = 0.0;
  double total_mir = 0.0;
  double total_llvm = 0.0;
  double total_backend = 0.0;
  double total_execute = 0.0;
  double total_wall = 0.0;

  for (const auto& [name, t] : entries_) {
    total_parse += t.parse;
    total_hir += t.hir_lower;
    total_mir += t.mir_lower;
    total_llvm += t.llvm_lower;
    total_backend += t.backend;
    total_execute += t.execute;
    total_wall += t.total;
  }

  double total_accounted = total_parse + total_hir + total_mir + total_llvm +
                           total_backend + total_execute;

  auto pct = [&](double v) -> double {
    return total_accounted > 0.0 ? 100.0 * v / total_accounted : 0.0;
  };

  fputs(
      std::format(
          "[timing] {} tests, total wall: {:.1f}s\n", entries_.size(),
          total_wall)
          .c_str(),
      stderr);
  fputs(
      std::format(
          "[timing]   parse:      {:.1f}s ({:.1f}%)\n", total_parse,
          pct(total_parse))
          .c_str(),
      stderr);
  fputs(
      std::format(
          "[timing]   hir_lower:  {:.1f}s ({:.1f}%)\n", total_hir,
          pct(total_hir))
          .c_str(),
      stderr);
  fputs(
      std::format(
          "[timing]   mir_lower:  {:.1f}s ({:.1f}%)\n", total_mir,
          pct(total_mir))
          .c_str(),
      stderr);
  fputs(
      std::format(
          "[timing]   llvm_lower: {:.1f}s ({:.1f}%)\n", total_llvm,
          pct(total_llvm))
          .c_str(),
      stderr);
  fputs(
      std::format(
          "[timing]   backend:    {:.1f}s ({:.1f}%)\n", total_backend,
          pct(total_backend))
          .c_str(),
      stderr);
  fputs(
      std::format(
          "[timing]   execute:    {:.1f}s ({:.1f}%)\n", total_execute,
          pct(total_execute))
          .c_str(),
      stderr);

  // Top 5 slowest tests
  auto sorted = entries_;
  std::sort(sorted.begin(), sorted.end(), [](const auto& a, const auto& b) {
    return a.second.total > b.second.total;
  });
  size_t top_n = std::min(sorted.size(), size_t{5});
  fputs(std::format("[timing] Top {} slowest:\n", top_n).c_str(), stderr);
  for (size_t i = 0; i < top_n; ++i) {
    const auto& [name, t] = sorted[i];
    fputs(
        std::format(
            "[timing]   {}. {}: {:.3f}s (backend={:.3f}s)\n", i + 1, name,
            t.total, t.backend)
            .c_str(),
        stderr);
  }
  std::fflush(stderr);
}

auto GetTimingCollector() -> TimingCollector& {
  static TimingCollector
      instance;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
  return instance;
}

auto IsTimingEnabled() -> bool {
  return g_timing_enabled;
}

void SetTimingEnabled(bool enabled) {
  g_timing_enabled = enabled;
}

}  // namespace lyra::test
