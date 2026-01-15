#include "dump.hpp"

#include <iostream>

#include <fmt/color.h>
#include <fmt/core.h>

#include "frontend.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/hir/dumper.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/mir/dumper.hpp"

namespace lyra::driver {

namespace {

void PrintDiagnostics(const DiagnosticSink& sink) {
  for (const auto& diag : sink.GetDiagnostics()) {
    const char* severity_str = nullptr;
    fmt::text_style severity_style;

    switch (diag.severity) {
      case DiagnosticSeverity::kError:
        severity_str = "error";
        severity_style = fmt::fg(fmt::terminal_color::bright_red);
        break;
      case DiagnosticSeverity::kWarning:
        severity_str = "warning";
        severity_style = fmt::fg(fmt::terminal_color::bright_yellow);
        break;
      case DiagnosticSeverity::kNote:
        severity_str = "note";
        severity_style = fmt::fg(fmt::terminal_color::bright_cyan);
        break;
    }

    fmt::print(
        stderr, "{}: {}: {}\n", fmt::styled("lyra", fmt::emphasis::bold),
        fmt::styled(severity_str, severity_style), diag.message);
  }
}

}  // namespace

auto DumpHir(const std::string& path) -> int {
  auto parse_result = LoadFile(path);
  if (!parse_result) {
    return 1;
  }

  DiagnosticSink sink;
  auto result =
      lowering::ast_to_hir::LowerAstToHir(*parse_result->compilation, sink);

  if (sink.HasErrors()) {
    PrintDiagnostics(sink);
    return 1;
  }

  hir::Dumper dumper(
      result.hir_arena.get(), result.type_arena.get(),
      result.constant_arena.get(), result.symbol_table.get(), &std::cout);
  dumper.Dump(result.design);

  return 0;
}

auto DumpMir(const std::string& path) -> int {
  auto parse_result = LoadFile(path);
  if (!parse_result) {
    return 1;
  }

  DiagnosticSink sink;
  auto hir_result =
      lowering::ast_to_hir::LowerAstToHir(*parse_result->compilation, sink);

  if (sink.HasErrors()) {
    PrintDiagnostics(sink);
    return 1;
  }

  lowering::hir_to_mir::LoweringInput mir_input{
      .design = hir_result.design,
      .hir_arena = *hir_result.hir_arena,
      .type_arena = *hir_result.type_arena,
      .constant_arena = *hir_result.constant_arena,
      .symbol_table = *hir_result.symbol_table,
  };
  auto mir_result = lowering::hir_to_mir::LowerHirToMir(mir_input);

  mir::Dumper dumper(
      mir_result.mir_arena.get(), hir_result.type_arena.get(), &std::cout);
  dumper.Dump(mir_result.design);

  return 0;
}

}  // namespace lyra::driver
