#pragma once

#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>

namespace lyra::lowering::ast_to_hir {

class ModuleLoweringFacts {
 public:
  explicit ModuleLoweringFacts(const slang::ast::InstanceBodySymbol& body)
      : body_(&body) {
  }

  [[nodiscard]] auto Body() const -> const slang::ast::InstanceBodySymbol& {
    return *body_;
  }

 private:
  const slang::ast::InstanceBodySymbol* body_;
};

class ProcessLoweringFacts {
 public:
  ProcessLoweringFacts(
      const ModuleLoweringFacts& module_facts,
      const slang::ast::ProceduralBlockSymbol& proc)
      : module_facts_(&module_facts), proc_(&proc) {
  }

  [[nodiscard]] auto Module() const -> const ModuleLoweringFacts& {
    return *module_facts_;
  }

  [[nodiscard]] auto Proc() const -> const slang::ast::ProceduralBlockSymbol& {
    return *proc_;
  }

 private:
  const ModuleLoweringFacts* module_facts_;
  const slang::ast::ProceduralBlockSymbol* proc_;
};

}  // namespace lyra::lowering::ast_to_hir
