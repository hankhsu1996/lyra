#include "process.hpp"

#include <slang/ast/symbols/BlockSymbols.h>

#include "facts.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "state.hpp"
#include "statement/lower.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerProcess(
    const ScopeLoweringFacts& scope_facts, ScopeLoweringState& scope_state,
    ScopeStack& stack, const slang::ast::ProceduralBlockSymbol& proc)
    -> hir::Process {
  const ProcessLoweringFacts facts(scope_facts, proc);
  ProcessLoweringState state;

  const hir::StmtId body_id =
      LowerStatement(facts, state, scope_state, stack, proc.getBody());

  return state.Finalize(hir::Initial{.body = body_id});
}

}  // namespace lyra::lowering::ast_to_hir
