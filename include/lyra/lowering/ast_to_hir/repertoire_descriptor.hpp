#pragma once

#include <cstdint>
#include <string>
#include <variant>
#include <vector>

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

// Provisional inspection payload for declarations.
// Identity is the declaration name. This is NOT semantic identity and cannot
// support M2c-3 discriminator integration without a strengthening step
// (M2c-2c: structured compile-owned payloads).
struct DeclArtifactDesc {
  std::string name;
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

// Inspection/modeling infrastructure for M2c.
//
// This descriptor is pointer-free, definition-owned, and deterministically
// ordered. It captures all compile-relevant artifacts and the constructor-
// selection coordinates under which each exists.
//
// This is NOT yet a hash input for specialization grouping. Payload identities
// (decl names, instance names, process ordinals) are provisional inspection
// labels. Discriminator integration (M2c-3) requires a payload strengthening
// step (M2c-2c) first.
struct DefinitionRepertoireDesc {
  std::vector<RepertoireArtifactDesc> artifacts;
};

// Builds a pointer-free, definition-owned repertoire descriptor from one
// definition body. The descriptor captures all compile-relevant artifacts
// and the constructor-selection coordinates under which each exists.
auto BuildDefinitionRepertoireDesc(const slang::ast::InstanceBodySymbol& body)
    -> DefinitionRepertoireDesc;

// Text dump for inspection and testing.
// Format per artifact:
//   <kind> <identity> coord=[<step>, ...]
// where <step> is:
//   branch(ci=N,alt=M)     for kBranch
//   entry(ci=N,idx=M)      for kArrayEntry
auto DumpDefinitionRepertoireDesc(const DefinitionRepertoireDesc& desc)
    -> std::string;

}  // namespace lyra::lowering::ast_to_hir
