#pragma once

namespace lyra::mir {
struct ModuleBody;
}

namespace lyra::lowering::hir_to_mir {

// Finalize expression connection process artifacts for a body.
// Synthesizes looping processes from kFunction-sourced connection recipes,
// appends them to body.processes as a contiguous suffix, and populates
// body.expr_connection_templates.
//
// This is the single owning function for expression-connection execution
// artifact creation. After this call, body.processes and
// body.expr_connection_templates are finalized. No later phase may append
// expression processes, mutate their MIR, change their trigger set, or
// change their child destination representation.
void MaterializeExprConnectionProcessSuffix(mir::ModuleBody& body);

}  // namespace lyra::lowering::hir_to_mir
