#pragma once

#include <cstdint>
#include <unordered_map>
#include <utility>
#include <vector>

#include <slang/ast/Compilation.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/frontend/slang_source_mapper.hpp"
#include "lyra/hir/module_unit.hpp"

namespace slang::ast {
class ProceduralBlockSymbol;
class ValueSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// One element of the precomputed implicit sensitivity list for a single
// always_comb / always_latch procedural block. Mirrors slang's ReadRange:
// `symbol` is the structural variable being read; `bit_range` is the flat
// bit range within its selectable-width packed encoding. Multiple entries
// for the same symbol with disjoint bit ranges are kept as-is so downstream
// can preserve precision when the runtime supports bit-level subscription.
struct SensitivityRead {
  const slang::ast::ValueSymbol* symbol;
  std::pair<std::uint64_t, std::uint64_t> bit_range;
};

// Per-procedure implicit sensitivity list (LRM 9.2.2.2.1), computed once by
// slang::analysis::AnalysisManager. Slang excludes block-local declarations,
// also-written variables, and timing-control-only identifiers per the LRM.
using ImplicitSensitivityReads = std::unordered_map<
    const slang::ast::ProceduralBlockSymbol*, std::vector<SensitivityRead>>;

class LowerCompilationFacts {
 public:
  LowerCompilationFacts(
      slang::ast::Compilation& compilation,
      const frontend::SlangSourceMapper& source_mapper,
      const ImplicitSensitivityReads& sensitivity_reads)
      : compilation_(&compilation),
        source_mapper_(&source_mapper),
        sensitivity_reads_(&sensitivity_reads) {
  }

  [[nodiscard]] auto Compilation() const -> slang::ast::Compilation& {
    return *compilation_;
  }
  [[nodiscard]] auto SourceMapper() const
      -> const frontend::SlangSourceMapper& {
    return *source_mapper_;
  }
  [[nodiscard]] auto SensitivityReads() const
      -> const ImplicitSensitivityReads& {
    return *sensitivity_reads_;
  }

 private:
  slang::ast::Compilation* compilation_;
  const frontend::SlangSourceMapper* source_mapper_;
  const ImplicitSensitivityReads* sensitivity_reads_;
};

auto LowerCompilation(const LowerCompilationFacts& facts)
    -> diag::Result<std::vector<hir::ModuleUnit>>;

}  // namespace lyra::lowering::ast_to_hir
