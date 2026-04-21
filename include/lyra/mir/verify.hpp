#pragma once

#include <cstdint>
#include <span>
#include <string_view>

#include "lyra/common/type_arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/constructor.hpp"
#include "lyra/mir/external_ref.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::mir {

struct CompiledModuleBody;

// Phase-aware verification context. Controls what statement forms are
// legal and provides external-ref recipe lookup.
struct VerifyContext {
  enum class Phase : uint8_t {
    // Pre-backend: ExternalRefId write targets are legal.
    kPreBackend,
    // Backend-ready: ExternalRefId write targets are illegal.
    kBackendReady,
  };

  const TypeArena* types = nullptr;
  Phase phase = Phase::kBackendReady;

  // External-ref recipe table for validating ExternalReadRvalueInfo
  // and WriteTarget ExternalRefId references. Populated from either
  // ModuleBody::external_refs or CompiledModuleBody::external_refs.
  std::span<const ExternalAccessRecipe> external_refs;

  // Body-local named event count. Used to validate EventId operands
  // in WaitEvent/TriggerEvent. 0 when event info is unavailable.
  uint32_t num_events = 0;
};

// Validate an ExternalRefId against a recipe table. Throws InternalError
// if ref_id is out of range, mismatched, or has invalid type.
auto RequireExternalRefRecipe(
    std::span<const ExternalAccessRecipe> recipes, ExternalRefId id,
    const char* where) -> const ExternalAccessRecipe&;

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

// Verify MIR constructor body invariants. Reuses the same block/temp
// validators as VerifyProcess (constructors are void-return and carry no
// parameters).
void VerifyConstructor(
    const Constructor& ctor, const Arena& arena, const VerifyContext& cx,
    std::string_view label = "constructor");

// Phase-aware MIR verification for CompiledModuleBody.
//
// Pre-backend: ExternalRefId write targets are legal. ExternalReadRvalueInfo
// is validated against external_refs recipes.
//
// Backend-ready: ExternalRefId write targets are illegal.

// Verify pre-backend MIR body. Validates external_refs table integrity,
// ExternalRefId write targets, and ExternalReadRvalueInfo recipes.
void VerifyPreBackendBody(
    const CompiledModuleBody& body, const TypeArena& types,
    std::string_view label = "body");

// Verify backend-ready MIR body. No ExternalRefId write targets allowed.
void VerifyBackendReadyBody(
    const CompiledModuleBody& body, std::string_view label = "body");

}  // namespace lyra::mir
