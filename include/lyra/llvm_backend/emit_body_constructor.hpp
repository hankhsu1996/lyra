#pragma once

#include <cstdint>
#include <span>
#include <vector>

#include <llvm/IR/Constants.h>

#include "lyra/llvm_backend/codegen_session.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/cu_facts.hpp"
#include "lyra/llvm_backend/emit_descriptor_utils.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"

namespace lyra::lowering::mir_to_llvm {

// Emit body-constructor LLVM function bodies for bodies that opted into
// the backend-emitted construction path (mir::ModuleBody::
// uses_mir_constructor). The forward declarations of
// `__lyra_body_construct_<body_index>` are created by
// EmitBodyRealizationDescs so body descriptor globals already embed
// their addresses; this function fills in the definitions.
//
// Each emitted function has signature
//     void __lyra_body_construct_<i>(
//         void* ctor, RuntimeInstance* this_inst, <typed formals...>);
// and performs three things in order:
//   1. Entry stores copying each typed formal argument into this body's
//      corresponding kParamConst module slot.
//   2. For every NewObject rvalue in the body constructor, a three-step
//      pattern: AllocateObject + direct call into the child's own
//      __lyra_body_construct_<child_i> with typed transmitted values +
//      store of the returned handle into the parent's kChildHandle slot.
//   3. A void return.
//
// Invariant: each flagged body has a single compile-time instance (the
// direct-constructor path computes per-instance constants; multi-
// instance flagged bodies would need per-instance resolution). Cases
// outside this subset are rejected loudly.
void EmitBodyConstructorFunctions(
    Context& context, const CuFacts& facts, const Layout& layout,
    const std::vector<BodyDescriptorPackageEmission>& body_descs,
    llvm::Constant* body_ref_table_ptr, const ConstructionProgramData& prog,
    std::span<const uint8_t> effective_uses_mir_ctor);

}  // namespace lyra::lowering::mir_to_llvm
