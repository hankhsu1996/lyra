#pragma once

#include <string>

#include "lyra/common/diagnostic/diagnostic_sink.hpp"

namespace lyra::driver {

void PrintError(const std::string& message);
void PrintWarning(const std::string& message);
void PrintDiagnostics(const DiagnosticSink& sink);
auto FormatDiagnostics(const DiagnosticSink& sink) -> std::string;

}  // namespace lyra::driver
