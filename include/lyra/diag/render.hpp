#pragma once

#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/diag/source_manager.hpp"

namespace lyra::diag {

struct RenderOptions {
  bool use_color = true;
  bool show_warnings = true;
  bool show_remarks = false;
};

// Every message is shown by `source_manager`, about a place in the source or
// about none, so one run's messages all take one form.
auto RenderDiagnostic(
    const Diagnostic& diag, const SourceManager& source_manager,
    const RenderOptions& opts = {}) -> std::string;

auto RenderDiagnostics(
    const DiagnosticSink& sink, const SourceManager& source_manager,
    const RenderOptions& opts = {}) -> std::string;

}  // namespace lyra::diag
