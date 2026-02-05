#pragma once

#include <filesystem>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/text/SourceManager.h>

#include "lyra/common/opt_level.hpp"

namespace lyra::driver {

struct CompilationInput {
  std::vector<std::string> files;
  std::string top;
  std::vector<std::string> incdir;
  std::vector<std::string> defines;
  std::vector<std::string> warnings;
  std::vector<std::string> param_overrides;  // Top-level param overrides (-G)
  std::filesystem::path fs_base_dir;   // Base directory for runtime file I/O
  std::vector<std::string> plusargs;   // Runtime plusargs for $plusargs
  OptLevel opt_level = OptLevel::kO2;  // Default O2 for CLI
  bool pedantic = false;               // Strict LRM compliance mode
  int verbose = 0;                     // Verbosity level (0-3)
  int stats_top_n = -1;        // LLVM stats: -1=off, 0=summary, >0=top N
  bool enable_system = false;  // Security: $system disabled by default
  bool enable_trace = false;   // Enable simulation tracing
  bool time_trace = false;     // LLVM time-trace profiling
};

struct ParseResult {
  std::shared_ptr<slang::SourceManager> source_manager;
  std::unique_ptr<slang::ast::Compilation> compilation;
};

// Parse source files into a compilation object (no elaboration yet).
auto ParseFiles(const CompilationInput& input) -> std::optional<ParseResult>;

// Force elaboration and check for errors. Returns false if errors occurred.
auto Elaborate(ParseResult& result, const CompilationInput& input) -> bool;

// Legacy combined parse + elaborate function.
auto LoadFiles(const CompilationInput& input) -> std::optional<ParseResult>;

}  // namespace lyra::driver
