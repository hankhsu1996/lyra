#pragma once

#include <expected>
#include <memory>
#include <string>
#include <vector>

#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/source_manager.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"

namespace lyra::driver {

struct CompilationResult {
  lowering::ast_to_hir::LoweringResult hir;
  lowering::hir_to_mir::LoweringResult mir;
};

struct CompilationError {
  DiagnosticSink diagnostics;
  std::unique_ptr<SourceManager> source_manager;
  std::string message;

  static auto FromDiagnostics(
      DiagnosticSink sink, std::unique_ptr<SourceManager> mgr)
      -> CompilationError;
  static auto Simple(std::string msg) -> CompilationError;

  void Print() const;
};

auto CompileToMir(const std::vector<std::string>& files)
    -> std::expected<CompilationResult, CompilationError>;

}  // namespace lyra::driver
