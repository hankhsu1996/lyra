#pragma once

#include <string>

#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/source_manager.hpp"

namespace lyra::driver {

void PrintError(const std::string& message);
void PrintWarning(const std::string& message);
void PrintDiagnostics(
    const DiagnosticSink& sink, const SourceManager* source_manager = nullptr);

}  // namespace lyra::driver
