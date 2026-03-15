#include "stats_report.hpp"

#include <array>
#include <cstdio>
#include <filesystem>
#include <format>
#include <fstream>
#include <string>
#include <string_view>

#include <llvm/Config/llvm-config.h>

#include "verbose_logger.hpp"

namespace lyra::driver {

auto ResolveGitSha() -> std::string {
#ifdef _WIN32
  return "";
#else
  std::array<char, 128> buf{};
  FILE* pipe = popen("git rev-parse --short HEAD 2>/dev/null", "r");
  if (pipe == nullptr) return "";
  std::string result;
  while (fgets(buf.data(), static_cast<int>(buf.size()), pipe) != nullptr) {
    result += buf.data();
  }
  pclose(pipe);
  while (!result.empty() && (result.back() == '\n' || result.back() == '\r')) {
    result.pop_back();
  }
  return result;
#endif
}

void WriteStatsJson(
    const StatsReport& report, const std::filesystem::path& path) {
  if (path.has_parent_path()) {
    std::filesystem::create_directories(path.parent_path());
  }

  std::ofstream out(path);
  if (!out) return;

  auto dur = [](double v) { return std::format("{:.6f}", v); };

  out << "{\n";
  out << "  \"version\": " << StatsReport::kVersion << ",\n";
  out << "  \"backend\": \""
      << (report.backend == StatsBackend::kAot ? "aot" : "jit") << "\",\n";
  out << "  \"git\": \"" << report.git_sha << "\",\n";
  out << "  \"llvm_version\": \"" << LLVM_VERSION_STRING << "\",\n";

  // Phases -- iterate kPhaseOrder, only include recorded phases
  out << "  \"phases\": {";
  bool first = true;
  for (std::string_view phase : VerboseLogger::kPhaseOrder) {
    auto d = report.vlog->PhaseDuration(phase);
    if (!d) continue;
    if (!first) out << ",";
    first = false;
    out << "\n    \"" << phase << "\": " << dur(*d);
  }
  out << "\n  },\n";

  // LLVM stats
  out << "  \"llvm\": {\n";
  out << "    \"functions\": " << report.llvm.defined_functions << ",\n";
  out << "    \"globals\": " << report.llvm.global_count << ",\n";
  out << "    \"basic_blocks\": " << report.llvm.total_bbs << ",\n";
  out << "    \"instructions\": " << report.llvm.total_insts << "\n";
  out << "  },\n";

  // MIR stats
  out << "  \"mir\": {\n";
  out << "    \"place_temps\": " << report.mir.place_temps << ",\n";
  out << "    \"value_temps\": " << report.mir.value_temps << ",\n";
  out << "    \"materialize_to_place\": " << report.mir.materialize_to_place
      << ",\n";
  out << "    \"mir_stmts\": " << report.mir.mir_stmts << "\n";
  out << "  }";

  // JIT timings (only for JIT backend)
  if (report.jit) {
    const auto& jt = *report.jit;
    out << ",\n  \"jit\": {\n";
    out << "    \"create_jit\": " << dur(jt.create_jit) << ",\n";
    out << "    \"load_runtime\": " << dur(jt.load_runtime) << ",\n";
    out << "    \"optimize_ir\": " << dur(jt.optimize_ir) << ",\n";
    out << "    \"add_ir\": " << dur(jt.add_ir) << ",\n";
    out << "    \"lookup_main\": " << dur(jt.lookup_main) << ",\n";
    out << "    \"codegen\": " << dur(jt.codegen) << ",\n";
    out << "    \"linking\": " << dur(jt.linking) << "\n";
    out << "  }";
  }

  out << "\n}\n";
}

}  // namespace lyra::driver
