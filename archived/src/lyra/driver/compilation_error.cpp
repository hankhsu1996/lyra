#include "compilation_error.hpp"

#include <string>
#include <utility>

#include "compilation_output.hpp"
#include "lyra/common/internal_error.hpp"

namespace lyra::driver {

auto CompilationError::FromDiagnostics(
    DiagnosticSink sink, std::unique_ptr<SourceManager> mgr)
    -> CompilationError {
  return CompilationError{
      .diagnostics = std::move(sink),
      .source_manager = std::move(mgr),
      .message = {},
  };
}

auto CompilationError::Simple(std::string msg) -> CompilationError {
  return CompilationError{
      .diagnostics = {},
      .source_manager = nullptr,
      .message = std::move(msg),
  };
}

void CompilationError::Render(CompilationOutput& output) const {
  if (diagnostics.HasErrors()) {
    output.PrintDiagnostics(diagnostics, source_manager.get());
    return;
  }
  if (!message.empty()) {
    output.PrintError(message);
    return;
  }
  throw common::InternalError(
      "CompilationError::Render",
      "compilation error has neither diagnostics nor message");
}

}  // namespace lyra::driver
