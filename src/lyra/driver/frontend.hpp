#pragma once

#include <filesystem>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/text/SourceManager.h>

#include "lyra/common/opt_level.hpp"
#include "lyra/frontend/parse_unit.hpp"
#include "lyra/runtime/iteration_limit.hpp"

namespace lyra::driver {

using frontend::CompilationUnitMode;

struct CompilationInput {
  std::vector<std::string> files;
  std::string top;
  std::vector<std::string> incdir;
  std::vector<std::string> defines;
  std::vector<std::string> warnings;
  std::vector<std::string> param_overrides;
  // Driver-selected filesystem root for relative runtime file operations
  // under driver-controlled execution (`lyra run --backend=jit|aot|lli`).
  // Project mode: config root. --no-project mode: effective CWD after -C.
  // For JIT, this value is embedded in the IR. For AOT/LLI children, the
  // driver transports it to the child via the internal `--lyra-fs-root=...`
  // argv token. Direct-run emitted binaries do not inherit this value;
  // they anchor to launch-time CWD.
  std::filesystem::path fs_root;
  std::vector<std::string> plusargs;
  OptLevel opt_level = OptLevel::kO2;
  bool pedantic = false;
  int verbose = 0;
  int stats_top_n = -1;
  bool enable_system = false;
  bool enable_trace_summary = false;
  std::optional<std::string> trace_signals_output;
  bool trace_activations = false;
  bool dump_suspended = false;
  bool time_trace = false;
  bool two_state = false;
  uint32_t iteration_limit = kDefaultIterationLimit;
  std::optional<std::filesystem::path> stats_out_path;
  std::vector<std::filesystem::path> dpi_link_inputs;
  bool disable_assertions = false;
  CompilationUnitMode compilation_unit_mode = CompilationUnitMode::kPerFile;
};

struct ParseResult {
  std::shared_ptr<slang::SourceManager> source_manager;
  std::unique_ptr<slang::ast::Compilation> compilation;
};

auto ParseFiles(const CompilationInput& input) -> std::optional<ParseResult>;
auto Elaborate(ParseResult& result, const CompilationInput& input) -> bool;
auto LoadFiles(const CompilationInput& input) -> std::optional<ParseResult>;

}  // namespace lyra::driver
