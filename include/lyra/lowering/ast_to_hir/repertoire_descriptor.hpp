#pragma once

#include <cstdint>
#include <string>
#include <variant>
#include <vector>

#include "lyra/lowering/ast_to_hir/compile_owned_type_desc.hpp"

namespace slang::ast {
class InstanceBodySymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

enum class RepertoireArtifactKind : uint8_t {
  kDecl,
  kProcess,
  kChildInstance,
  kContinuousAssign,
};

enum class SelectionStepKind : uint8_t {
  kBranch,
  kArrayEntry,
};

// One constructor-selection step inside the definition.
// construct_index is local to the parent coordinate prefix.
struct SelectionStepDesc {
  SelectionStepKind kind;
  uint32_t construct_index;
  uint32_t alt_index;

  auto operator==(const SelectionStepDesc& other) const -> bool = default;
};

using RepertoireCoord = std::vector<SelectionStepDesc>;

enum class ProcessKindDesc : uint8_t {
  kInitial,
  kFinal,
  kAlways,
  kAlwaysComb,
  kAlwaysLatch,
  kAlwaysFF,
};

// Semantic payload for declarations.
// Identity is the coordinate-local ordinal (encounter order within the
// coordinate bucket) plus a compile-owned type ID referencing the definition's
// type store. Debug names are not part of semantic identity.
struct DeclArtifactDesc {
  uint32_t local_ordinal;
  TypeDescId type_id;
};

// Provisional inspection payload for processes.
// local_ordinal is deterministic encounter order within the
// (coord, process_kind) bucket. This is placeholder identity for inspection
// and dump, not body/semantic identity. M2c-2c will define stronger process
// identity if required.
struct ProcessArtifactDesc {
  ProcessKindDesc process_kind;
  uint32_t local_ordinal;
};

// Provisional inspection payload for child instances.
// inst_name and target_def are human-readable labels for inspection and dump.
// This is NOT semantic identity and cannot support M2c-3 discriminator
// integration without a strengthening step (M2c-2c).
struct ChildInstanceArtifactDesc {
  std::string inst_name;
  std::string target_def;
};

// Provisional inspection payload for continuous assigns.
// local_ordinal is deterministic encounter order within the coord bucket.
// Placeholder identity for inspection only.
struct ContinuousAssignArtifactDesc {
  uint32_t local_ordinal;
};

using RepertoirePayload = std::variant<
    DeclArtifactDesc, ProcessArtifactDesc, ChildInstanceArtifactDesc,
    ContinuousAssignArtifactDesc>;

struct RepertoireArtifactDesc {
  RepertoireArtifactKind kind;
  RepertoireCoord coord;
  RepertoirePayload payload;
};

// Definition-owned repertoire descriptor for M2c.
//
// Pointer-free, deterministically ordered. Captures all compile-relevant
// artifacts and the constructor-selection coordinates under which each exists.
//
// The type_store is definition-scoped: it contains types from ALL generate
// branches (active and inactive), not just the instantiated path. This is
// because BuildArtifactInventory walks all branches without filtering by
// isUninstantiated. The specialization fingerprint (HashCompileOwnedTypeStore)
// hashes the load-bearing subset of this store, so two instances of the same
// definition on different active branches produce the same fingerprint.
//
// Declaration payloads use coordinate-local ordinals and compile-owned type
// IDs referencing the shared type_store. Debug labels are not part of semantic
// identity. Process/child-instance/continuous-assign payloads remain
// provisional inspection labels pending future strengthening.
struct DefinitionRepertoireDesc {
  CompileOwnedTypeStore type_store;
  std::vector<RepertoireArtifactDesc> artifacts;
};

// Builds a pointer-free, definition-owned repertoire descriptor from one
// definition body. The descriptor captures all compile-relevant artifacts
// and the constructor-selection coordinates under which each exists.
//
// The resulting type_store is definition-scoped because the underlying
// BuildArtifactInventory walks all generate branches (including
// uninstantiated ones). See BuildArtifactInventory contract.
auto BuildDefinitionRepertoireDesc(const slang::ast::InstanceBodySymbol& body)
    -> DefinitionRepertoireDesc;

}  // namespace lyra::lowering::ast_to_hir
