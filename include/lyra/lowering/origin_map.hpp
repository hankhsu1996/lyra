#pragma once

#include <cstdint>
#include <optional>
#include <variant>
#include <vector>

#include "lyra/common/origin_id.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering {

// Reference to a MIR statement (block + index within block).
// Uses BasicBlockId for structural typing rather than raw indices.
struct StatementRef {
  mir::BasicBlockId block;
  uint32_t statement_index;  // Index within block (unavoidable, no
                             // StatementId exists)

  auto operator==(const StatementRef&) const -> bool = default;
};

// Reference to a MIR terminator (one per block).
struct TerminatorRef {
  mir::BasicBlockId block;

  auto operator==(const TerminatorRef&) const -> bool = default;
};

// Reference to a function prologue parameter operation.
// Used for errors during parameter allocation/initialization in LLVM backend.
struct PrologueParamRef {
  mir::FunctionId func;
  uint32_t param_index;

  auto operator==(const PrologueParamRef&) const -> bool = default;
};

// The MIR node that an origin entry refers to.
// std::monostate is used for source-only entries (projections) where no MIR
// construct exists to reference - these origins are purely for error reporting.
// Extensible: can add BasicBlockId later for block-level origins.
using MirNode = std::variant<
    std::monostate, mir::FunctionId, mir::ProcessId, StatementRef,
    TerminatorRef, PrologueParamRef>;

// Reference to a function parameter in HIR (for prologue errors).
// Resolves to the parameter's declaration span via function.parameters[index].
struct FunctionParamRef {
  hir::FunctionId func;
  uint32_t param_index;

  auto operator==(const FunctionParamRef&) const -> bool = default;
};

// The HIR source that generated a MIR node.
using HirSource = std::variant<
    hir::StatementId, hir::ExpressionId, hir::FunctionId, hir::ProcessId,
    FunctionParamRef>;

// Entry recording the mapping from a MIR node to its HIR source.
struct OriginEntry {
  MirNode mir_node;
  HirSource hir_source;
};

// Maps MIR constructs back to their HIR sources for error reporting.
// Monotonic allocator: once allocated, OriginIds are stable.
class OriginMap {
 public:
  OriginMap() = default;

  // Core API (variant-based).
  auto Record(MirNode mir_node, HirSource hir_source) -> common::OriginId;

  // Lookup an origin entry by ID. Returns nullopt for invalid IDs.
  [[nodiscard]] auto Resolve(common::OriginId id) const
      -> std::optional<OriginEntry>;

  // Get the current size (for debugging/stats).
  [[nodiscard]] auto Size() const -> size_t {
    return entries_.size();
  }

  // Typed overloads - prevent cross-kind misuse at call sites.
  auto Record(mir::FunctionId mir, hir::FunctionId hir) -> common::OriginId {
    return Record(MirNode{mir}, HirSource{hir});
  }
  auto Record(mir::ProcessId mir, hir::ProcessId hir) -> common::OriginId {
    return Record(MirNode{mir}, HirSource{hir});
  }
  auto Record(StatementRef mir, hir::StatementId hir) -> common::OriginId {
    return Record(MirNode{mir}, HirSource{hir});
  }
  auto Record(StatementRef mir, hir::ExpressionId hir) -> common::OriginId {
    return Record(MirNode{mir}, HirSource{hir});
  }
  auto Record(TerminatorRef mir, hir::StatementId hir) -> common::OriginId {
    return Record(MirNode{mir}, HirSource{hir});
  }
  auto Record(TerminatorRef mir, hir::ExpressionId hir) -> common::OriginId {
    return Record(MirNode{mir}, HirSource{hir});
  }
  auto Record(PrologueParamRef mir, FunctionParamRef hir) -> common::OriginId {
    return Record(MirNode{mir}, HirSource{hir});
  }

  // Source-only entry for projections (no MIR construct to reference).
  auto Record(std::monostate mir, hir::ExpressionId hir) -> common::OriginId {
    return Record(MirNode{mir}, HirSource{hir});
  }

 private:
  std::vector<OriginEntry> entries_;
};

}  // namespace lyra::lowering
