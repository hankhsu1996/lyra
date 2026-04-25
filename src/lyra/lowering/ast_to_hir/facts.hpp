#pragma once

#include <slang/ast/Scope.h>
#include <slang/ast/symbols/BlockSymbols.h>

#include "lyra/diag/slang_source_mapper.hpp"

// Lower* shape: pure translators return data; arena appenders take the
// mutable owner and return an id; output mutators take an explicit output
// (named "...Into...") and return Result<void>. Facts are read-only;
// state must not expose facts.

namespace lyra::lowering::ast_to_hir {

class UnitLoweringFacts {
 public:
  explicit UnitLoweringFacts(const diag::SlangSourceMapper& source_mapper)
      : source_mapper_(&source_mapper) {
  }

  [[nodiscard]] auto SourceMapper() const -> const diag::SlangSourceMapper& {
    return *source_mapper_;
  }

 private:
  const diag::SlangSourceMapper* source_mapper_;
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
