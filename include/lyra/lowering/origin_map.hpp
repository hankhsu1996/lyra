#pragma once

#include <cstdint>
#include <optional>
#include <variant>
#include <vector>

#include "lyra/common/unsupported_error.hpp"
#include "lyra/hir/fwd.hpp"

namespace lyra::lowering {

// What kind of MIR node this origin entry refers to.
enum class MirNodeKind {
  kInstruction,
  kTerminator,
  kBlock,
};

// The HIR source that generated a MIR node.
using HirSource = std::variant<hir::StatementId, hir::ExpressionId>;

// Entry recording the mapping from a MIR node to its HIR source.
struct OriginEntry {
  MirNodeKind kind;
  uint32_t mir_index;  // Meaning depends on kind (block-local for instructions)
  HirSource hir_source;
};

// Maps MIR constructs back to their HIR sources for error reporting.
// Monotonic allocator: once allocated, OriginIds are stable.
class OriginMap {
 public:
  OriginMap() = default;

  // Record a new origin entry and return its ID.
  auto Record(MirNodeKind kind, uint32_t mir_index, HirSource hir_source)
      -> common::OriginId;

  // Lookup an origin entry by ID. Returns nullopt for invalid IDs.
  [[nodiscard]] auto Resolve(common::OriginId id) const
      -> std::optional<OriginEntry>;

  // Get the current size (for debugging/stats).
  [[nodiscard]] auto Size() const -> size_t {
    return entries_.size();
  }

 private:
  std::vector<OriginEntry> entries_;
};

}  // namespace lyra::lowering
