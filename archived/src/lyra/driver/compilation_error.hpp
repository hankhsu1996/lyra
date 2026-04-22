#pragma once

#include <memory>
#include <string>

#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/source_manager.hpp"

namespace lyra::driver {

class CompilationOutput;

struct CompilationError {
  DiagnosticSink diagnostics;
  std::unique_ptr<SourceManager> source_manager;
  std::string message;

  static auto FromDiagnostics(
      DiagnosticSink sink, std::unique_ptr<SourceManager> mgr)
      -> CompilationError;
  static auto Simple(std::string msg) -> CompilationError;

  void Render(CompilationOutput& output) const;
};

}  // namespace lyra::driver
