#pragma once

#include <cstdint>
#include <string_view>

#include "lyra/common/type_arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/external_ref.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::mir {

struct CompiledModuleBody;

// Phase-aware verification context. Controls what operand/statement
// forms are legal and how external refs are resolved.
struct VerifyContext {
  enum class Phase : uint8_t {
    // Pre-backend: ExternalRefId is legal. Resolved through
    // body->external_refs.
    kPreBackend,
    // Backend-ready: ExternalRefId is illegal. body may be null.
    kBackendReady,
  };

  const CompiledModuleBody* body = nullptr;
  const TypeArena* types = nullptr;
  Phase phase = Phase::kBackendReady;
};

// Canonical external-ref recipe resolver. Validates ref_id is in range,
// table position matches, and type is valid. Throws InternalError on failure.
auto RequireExternalRefRecipe(
    const CompiledModuleBody& body, ExternalRefId id, const char* where)
    -> const ExternalAccessRecipe&;

// Verify MIR function invariants. Throws InternalError on failure.
// label: descriptive name for error messages (e.g., "top.u_alu: func foo").
//
// Invariants checked:
// - param_local_slots.size() == signature.params.size()
// - Each param slot < local_types.size()
// - Param slots are unique (no aliasing)
// - Non-void function: all Return terminators must have a value
// - Void function: all Return terminators must NOT have a value
// - BlockParam temp_ids are unique across all blocks
// - For Jump/Branch: edge arg count == target block param count
// - For Jump/Branch: edge arg types match target block param types
// - All UseTemp operands in edge args reference defined temp_ids
void VerifyFunction(
    const Function& func, const Arena& arena, const VerifyContext& cx,
    std::string_view label = "function");

// Verify MIR process invariants. Throws InternalError on failure.
// label: descriptive name for error messages (e.g., "top.u_alu: always[3]").
//
// Invariants checked:
// - All Return terminators must NOT have a value (processes don't return)
// - BlockParam temp_ids are unique across all blocks
// - For Jump/Branch: edge arg count == target block param count
// - For Jump/Branch: edge arg types match target block param types
// - All UseTemp operands in edge args reference defined temp_ids
void VerifyProcess(
    const Process& proc, const Arena& arena, const VerifyContext& cx,
    std::string_view label = "process");

// Phase-aware MIR verification.
//
// Pre-backend MIR: ExternalRefId is legal. Operand::kExternalRef and
// WriteTarget containing ExternalRefId are valid. The verifier resolves
// ExternalRefId types through body.external_refs.
//
// Backend-ready MIR: ExternalRefId must not appear anywhere. All external
// refs must have been lowered/bound before this point.

// Verify pre-backend MIR body. ExternalRefId is legal and must resolve
// through body.external_refs. Throws InternalError on failure.
void VerifyPreBackendBody(
    const CompiledModuleBody& body, const TypeArena& types,
    std::string_view label = "body");

// Verify backend-ready MIR body. No ExternalRefId may appear in any
// operand or write target. Throws InternalError if any are found.
void VerifyBackendReadyBody(
    const CompiledModuleBody& body, std::string_view label = "body");

}  // namespace lyra::mir
