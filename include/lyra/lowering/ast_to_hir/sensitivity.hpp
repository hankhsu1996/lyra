#pragma once

#include <vector>

#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"

namespace lyra::lowering::ast_to_hir {

class ScopeStack;
class UnitLoweringState;

// Translates the slang-side read vector (ValueSymbol* + bit_range) into
// HIR-side identity entries (StructuralVarRef + bit_range), resolving each
// symbol through the unit's structural binding table and the active scope
// stack. Reads that don't resolve to a visible structural variable are
// silently skipped -- slang may surface reads whose home scope was not
// elaborated into HIR (e.g. ports of an instance the caller is not in).
auto TranslateSensitivityReads(
    const std::vector<SensitivityRead>& reads,
    const UnitLoweringState& unit_state, const ScopeStack& stack)
    -> std::vector<hir::SensitivityEntry>;

}  // namespace lyra::lowering::ast_to_hir
