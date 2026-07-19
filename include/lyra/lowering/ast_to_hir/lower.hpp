#pragma once

#include <string>
#include <vector>

#include <slang/ast/Compilation.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/frontend/slang_source_mapper.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"

namespace lyra::lowering::ast_to_hir {

// Driver-supplied facts threaded into AST-to-HIR lowering. `Compilation&` is
// the slang elaboration root; `SourceMapper&` translates slang source
// locations; `SensitivityAnalyzer&` is reused across modules so its read
// cache survives. `disable_assertions` is the lowering policy that elides
// assertion constructs instead of rejecting them.
class LowerCompilationFacts {
 public:
  LowerCompilationFacts(
      slang::ast::Compilation& compilation,
      const frontend::SlangSourceMapper& source_mapper,
      SensitivityAnalyzer& sensitivity_analyzer, bool disable_assertions)
      : compilation_(&compilation),
        source_mapper_(&source_mapper),
        sensitivity_analyzer_(&sensitivity_analyzer),
        disable_assertions_(disable_assertions) {
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
  [[nodiscard]] auto DisableAssertions() const -> bool {
    return disable_assertions_;
  }

 private:
  slang::ast::Compilation* compilation_;
  const frontend::SlangSourceMapper* source_mapper_;
  SensitivityAnalyzer* sensitivity_analyzer_;
  bool disable_assertions_;
};

// Lowers the whole compilation to its HIR units: every package the design
// declares, then every distinct module body reachable from the tops, each
// tagged with its `UnitKind`. Each unit is lowered independently -- it reads
// only its own scope and the shared frontend -- so the result is a flat set of
// self-contained units with no cross-unit HIR references.
auto LowerCompilationToHir(const LowerCompilationFacts& facts)
    -> diag::Result<std::vector<hir::CompilationUnit>>;

// A top-level block is an auto-promoted, uninstantiated module. These names
// are a subset of the compiled units: a unit reached only through
// instantiation is compiled but is not a top.
auto TopLevelUnitNames(slang::ast::Compilation& compilation)
    -> std::vector<std::string>;

}  // namespace lyra::lowering::ast_to_hir
