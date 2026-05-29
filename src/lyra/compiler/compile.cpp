#include "lyra/compiler/compile.hpp"

#include <format>
#include <utility>
#include <vector>

#include <slang/analysis/AnalysisManager.h>
#include <slang/analysis/AnalyzedProcedure.h>
#include <slang/ast/Statement.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>

#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/frontend/load.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/hir_to_mir/lower_module_unit.hpp"

namespace lyra::compiler {

namespace {

// Deduplicated `ReadRange`-shaped list from slang's DFA maps 1:1. Disjoint
// bit ranges for the same symbol are preserved so the runtime can subscribe
// at bit-range granularity once Observable gains that resolution.
template <typename SlangReads>
auto TranslateReads(const SlangReads& reads)
    -> std::vector<lowering::ast_to_hir::SensitivityRead> {
  std::vector<lowering::ast_to_hir::SensitivityRead> entries;
  entries.reserve(reads.size());
  for (const auto& read : reads) {
    entries.push_back({.symbol = read.symbol, .bit_range = read.bitRange});
  }
  return entries;
}

// Harvests slang's procedure-level and per-`@*` read sets via the
// AnalysisManager listener. Other forms (wait, future wait_order, etc.)
// run their own walkers locally at the lowering site and do not pass
// through this listener.
auto ComputeImplicitSensitivityReads(slang::ast::Compilation& compilation)
    -> lowering::ast_to_hir::ImplicitSensitivityReads {
  lowering::ast_to_hir::ImplicitSensitivityReads out;
  slang::analysis::AnalysisManager manager;
  manager.addListener([&](const slang::analysis::AnalyzedProcedure& ap) {
    const auto* proc =
        ap.analyzedSymbol->as_if<slang::ast::ProceduralBlockSymbol>();
    if (proc == nullptr) return;
    // LRM 9.2.2.2.1: for always_comb / always_latch the procedure-level
    // sensitivity attaches to the body statement of the procedure.
    if (proc->procedureKind == slang::ast::ProceduralBlockKind::AlwaysComb ||
        proc->procedureKind == slang::ast::ProceduralBlockKind::AlwaysLatch) {
      const auto& sens = ap.getSensitivityList();
      if (sens.kind == slang::analysis::SensitivityList::Kind::Implicit) {
        out.emplace(&proc->getBody(), TranslateReads(sens.reads));
      }
    }
    // LRM 9.4.2.2: each `@*` TimedStatement carries its own sensitivity.
    for (const auto& region : ap.getImplicitEventReadSets()) {
      out.emplace(region.statement, TranslateReads(region.reads));
    }
  });
  manager.analyze(compilation);
  return out;
}

}  // namespace

auto Compile(
    const frontend::CompilationInput& input, diag::DiagnosticSink& sink,
    StopAfter stop_after) -> CompileResult {
  CompileResult result;

  auto parse = frontend::LoadFiles(input, sink);
  if (!parse) {
    return result;
  }
  result.artifacts.parse = std::move(*parse);

  // Stop early if slang reports parse/elaboration errors; running
  // AST->HIR over a broken AST would emit confusing follow-on errors.
  result.slang_ok = !frontend::HasSlangErrors(*result.artifacts.parse);
  if (!result.slang_ok) {
    return result;
  }

  if (stop_after == StopAfter::kParse) {
    return result;
  }

  const auto sensitivity_reads =
      ComputeImplicitSensitivityReads(*result.artifacts.parse->compilation);

  auto hir = lowering::ast_to_hir::LowerCompilation(
      lowering::ast_to_hir::LowerCompilationFacts(
          *result.artifacts.parse->compilation,
          result.artifacts.parse->source_mapper, sensitivity_reads));
  if (!hir) {
    sink.Report(std::move(hir.error()));
    return result;
  }
  result.artifacts.hir_units = std::move(*hir);

  if (stop_after == StopAfter::kHir) {
    return result;
  }

  if (result.artifacts.hir_units->size() != 1) {
    sink.Report(
        diag::Diagnostic::HostError(
            diag::DiagCode::kHostExpectedSingleTopModule,
            std::format(
                "expected exactly one top module, got {}",
                result.artifacts.hir_units->size())));
    return result;
  }

  auto mir = lowering::hir_to_mir::LowerModuleUnit(
      result.artifacts.hir_units->front());
  if (!mir) {
    sink.Report(std::move(mir.error()));
    return result;
  }
  result.artifacts.mir_unit = std::move(*mir);

  return result;
}

}  // namespace lyra::compiler
