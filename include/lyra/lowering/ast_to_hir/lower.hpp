#pragma once

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
      const SensitivityReadStore& sensitivity_reads)
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
  [[nodiscard]] auto SensitivityReads() const -> const SensitivityReadStore& {
    return *sensitivity_reads_;
  }

 private:
  slang::ast::Compilation* compilation_;
  const frontend::SlangSourceMapper* source_mapper_;
  const SensitivityReadStore* sensitivity_reads_;
};

auto LowerCompilation(const LowerCompilationFacts& facts)
    -> diag::Result<std::vector<hir::ModuleUnit>>;

}  // namespace lyra::lowering::ast_to_hir
