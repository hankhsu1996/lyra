#pragma once

#include <string>
#include <vector>

#include <slang/ast/Compilation.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/frontend/slang_source_mapper.hpp"
#include "lyra/hir/module_unit.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"

namespace lyra::lowering::ast_to_hir {

class LowerCompilationFacts {
 public:
  LowerCompilationFacts(
      slang::ast::Compilation& compilation,
      const frontend::SlangSourceMapper& source_mapper,
      SensitivityAnalyzer& sensitivity_analyzer)
      : compilation_(&compilation),
        source_mapper_(&source_mapper),
        sensitivity_analyzer_(&sensitivity_analyzer) {
  }

  [[nodiscard]] auto Compilation() const -> slang::ast::Compilation& {
    return *compilation_;
  }
  [[nodiscard]] auto SourceMapper() const
      -> const frontend::SlangSourceMapper& {
    return *source_mapper_;
  }
  [[nodiscard]] auto Sensitivity() const -> SensitivityAnalyzer& {
    return *sensitivity_analyzer_;
  }

 private:
  slang::ast::Compilation* compilation_;
  const frontend::SlangSourceMapper* source_mapper_;
  SensitivityAnalyzer* sensitivity_analyzer_;
};

// Lowers the top-level blocks and, transitively, every unit they instantiate.
// The result is a superset of the tops: a unit reached only through
// instantiation is compiled but is not itself a top.
auto LowerCompilation(const LowerCompilationFacts& facts)
    -> diag::Result<std::vector<hir::ModuleUnit>>;

// A top-level block is an auto-promoted, uninstantiated module. These names are
// a subset of the compiled units: a unit reached only through instantiation is
// compiled but is not a top.
auto TopLevelUnitNames(slang::ast::Compilation& compilation)
    -> std::vector<std::string>;

}  // namespace lyra::lowering::ast_to_hir
