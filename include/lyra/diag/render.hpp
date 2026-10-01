#pragma once

#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/diag/source_manager.hpp"

namespace lyra::diag {

struct RenderOptions {
  bool use_color = true;
  bool show_source_snippet = true;
  bool show_warnings = true;
  bool show_remarks = false;
};

auto RenderDiagnostic(
    const Diagnostic& diag, const SourceManager* source_manager,
    const RenderOptions& opts = {}) -> std::string;

auto RenderDiagnostics(
    const DiagnosticSink& sink, const SourceManager* source_manager,
    const RenderOptions& opts = {}) -> std::string;

}  // namespace lyra::diag
