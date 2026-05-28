#pragma once

#include <slang/ast/Scope.h>
#include <slang/ast/symbols/BlockSymbols.h>

#include "lyra/frontend/slang_source_mapper.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"

namespace lyra::lowering::ast_to_hir {

class UnitLoweringFacts {
 public:
  UnitLoweringFacts(
      const frontend::SlangSourceMapper& source_mapper,
      const ImplicitSensitivityReads& sensitivity_reads)
      : source_mapper_(&source_mapper), sensitivity_reads_(&sensitivity_reads) {
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
  const frontend::SlangSourceMapper* source_mapper_;
  const ImplicitSensitivityReads* sensitivity_reads_;
};

class ScopeLoweringFacts {
 public:
  explicit ScopeLoweringFacts(const slang::ast::Scope& slang_scope)
      : slang_scope_(&slang_scope) {
  }

  [[nodiscard]] auto SlangScope() const -> const slang::ast::Scope& {
    return *slang_scope_;
  }

 private:
  const slang::ast::Scope* slang_scope_;
};

class ProcessLoweringFacts {
 public:
  ProcessLoweringFacts(
      const ScopeLoweringFacts& scope_facts,
      const slang::ast::ProceduralBlockSymbol& proc)
      : scope_facts_(&scope_facts), proc_(&proc) {
  }

  [[nodiscard]] auto Scope() const -> const ScopeLoweringFacts& {
    return *scope_facts_;
  }

  [[nodiscard]] auto Proc() const -> const slang::ast::ProceduralBlockSymbol& {
    return *proc_;
  }

 private:
  const ScopeLoweringFacts* scope_facts_;
  const slang::ast::ProceduralBlockSymbol* proc_;
};

}  // namespace lyra::lowering::ast_to_hir
