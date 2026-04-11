#pragma once

#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/common/type_utils.hpp"

namespace lyra::lowering::mir_to_llvm {

// Layout-phase effective four-state queries.
//
// These compose the intrinsic type fact with the force_two_state override
// for code that runs before Context exists (layout construction).
// Session/codegen code must use IsFourState / IsPackedFourState from
// cu_facts.hpp.

inline auto IsLayoutPackedFourState(
    const Type& type, const TypeArena& types, bool force_two_state) -> bool {
  return !force_two_state && lyra::IsIntrinsicallyPackedFourState(type, types);
}

inline auto IsLayoutFourState(
    TypeId type_id, const TypeArena& types, bool force_two_state) -> bool {
  return !force_two_state && lyra::IsIntrinsicallyFourState(type_id, types);
}

}  // namespace lyra::lowering::mir_to_llvm
