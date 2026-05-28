#include "lyra/compiler/compile.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <functional>
#include <tuple>
#include <unordered_set>
#include <utility>
#include <vector>

#include <slang/analysis/AnalysisManager.h>
#include <slang/analysis/AnalyzedProcedure.h>
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

auto ComputeImplicitSensitivityReads(slang::ast::Compilation& compilation)
    -> lowering::ast_to_hir::ImplicitSensitivityReads {
  lowering::ast_to_hir::ImplicitSensitivityReads out;
  slang::analysis::AnalysisManager manager;
  manager.addListener([&](const slang::analysis::AnalyzedProcedure& ap) {
    const auto* proc =
        ap.analyzedSymbol->as_if<slang::ast::ProceduralBlockSymbol>();
    if (proc == nullptr) return;
    if (proc->procedureKind != slang::ast::ProceduralBlockKind::AlwaysComb &&
        proc->procedureKind != slang::ast::ProceduralBlockKind::AlwaysLatch) {
      return;
    }
    const auto& sens = ap.getSensitivityList();
    if (sens.kind != slang::analysis::SensitivityList::Kind::Implicit) return;
    // Preserve every (symbol, bit_range) slang produced; the same symbol may
    // appear several times with disjoint ranges (e.g. `bus[3] | bus[5]`),
    // and that precision is needed once the runtime supports bit-level
    // subscription. Dedup only by exact (symbol, bit_range) so a defensive
    // caller is shielded from accidental slang-side duplicates.
    using DedupKey = std::tuple<
        const slang::ast::ValueSymbol*, std::uint64_t, std::uint64_t>;
    struct DedupHash {
      auto operator()(const DedupKey& k) const noexcept -> std::size_t {
        const auto h1 =
            std::hash<const slang::ast::ValueSymbol*>{}(std::get<0>(k));
        const auto h2 = std::hash<std::uint64_t>{}(std::get<1>(k));
        const auto h3 = std::hash<std::uint64_t>{}(std::get<2>(k));
        return h1 ^ (h2 * 0x9E3779B97F4A7C15ULL) ^ (h3 << 1);
      }
    };
    std::vector<lowering::ast_to_hir::SensitivityRead> entries;
    std::unordered_set<DedupKey, DedupHash> seen;
    for (const auto& read : sens.reads) {
      const slang::ast::ValueSymbol* symbol = read.symbol;
      if (!seen.insert({symbol, read.bitRange.first, read.bitRange.second})
               .second) {
        continue;
      }
      entries.push_back({.symbol = symbol, .bit_range = read.bitRange});
    }
    out.emplace(proc, std::move(entries));
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
