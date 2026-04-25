#pragma once

#include <memory>
#include <optional>
#include <string>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/text/SourceManager.h>

#include "lyra/diag/sink.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/frontend/slang_source_mapper.hpp"

namespace lyra::frontend {

struct CompilationInput {
  std::vector<std::string> files;
  std::string top;
  std::vector<std::string> incdirs;
  std::vector<std::string> defines;
  std::vector<std::string> param_overrides;
  bool single_unit = false;
};

struct ParseResult {
  std::shared_ptr<slang::SourceManager> source_manager;
  std::unique_ptr<slang::ast::Compilation> compilation;
  diag::SourceManager diag_sources;
  SlangSourceMapper source_mapper;
};

// Build the parse input. Reports file-read failures to `sink`; returns
// nullopt on host failure. Slang parser/elaboration diagnostics are not
// rendered here -- callers invoke RenderSlangDiagnostics with their own
// rendering policy.
auto LoadFiles(const CompilationInput& input, diag::DiagnosticSink& sink)
    -> std::optional<ParseResult>;

// Returns true when slang reports no errors. `parse` is non-const because
// slang's getAllDiagnostics is non-const.
auto RenderSlangDiagnostics(
    ParseResult& parse, bool use_color, std::string& out_text) -> bool;

}  // namespace lyra::frontend
