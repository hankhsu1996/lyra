#pragma once

#include <string_view>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/source_manager.hpp"

namespace lyra::driver {

class TextSink;

void RenderDiagnostic(
    TextSink& sink, const Diagnostic& diag,
    const SourceManager* source_manager);
void RenderDiagnostics(
    TextSink& sink, const DiagnosticSink& diag_sink,
    const SourceManager* source_manager);
void RenderError(TextSink& sink, std::string_view message);
void RenderWarning(TextSink& sink, std::string_view message);
void RenderInternalError(
    TextSink& sink, std::string_view context_fn, std::string_view message);

}  // namespace lyra::driver
