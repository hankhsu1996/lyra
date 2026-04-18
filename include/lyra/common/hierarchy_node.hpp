#pragma once

#include <cstdint>
#include <span>
#include <string>

namespace lyra::common {

// One node in the runtime scope hierarchy. Covers both module instances
// (kInstance) and generate scopes (kGenerate). Built at AST->HIR time from
// the slang instance tree, threaded through to the construction program
// emitter. Strict construction order: parents appear before children.
// Child ordering within each parent follows structural (declaration) order.
struct HierarchyNode {
  enum Kind : uint8_t { kInstance, kGenerate };
  Kind kind = kInstance;
  // Index of parent node in hierarchy_nodes. UINT32_MAX for root.
  uint32_t parent_node_index = UINT32_MAX;
  // Display label relative to parent (e.g., "u", "blk[0]", "gen_true").
  // Derived from authoritative frontend path, not synthesized.
  std::string label;
  // Full hierarchical path from authoritative frontend source.
  std::string full_path;
  // For kInstance: index into instance payload tables (objects,
  // instance_table). NOT a child position. UINT32_MAX for generate scopes.
  uint32_t instance_index = UINT32_MAX;
};

// Canonical helper: count earlier siblings under the same parent to get
// the child's ordinal in hierarchy_nodes tree order. This is the ONLY
// valid child-position concept for runtime scope tree addressing.
inline auto ComputeOrdinalInParent(
    std::span<const HierarchyNode> nodes, uint32_t child_idx) -> uint32_t {
  auto parent = nodes[child_idx].parent_node_index;
  uint32_t ord = 0;
  for (uint32_t i = 0; i < child_idx; ++i) {
    if (nodes[i].parent_node_index == parent) ++ord;
  }
  return ord;
}

// Find the nearest enclosing instance node for a given node. Walks up
// parent_node_index chain until an instance is found.
inline auto FindEnclosingInstanceNode(
    std::span<const HierarchyNode> nodes, uint32_t node_idx) -> uint32_t {
  uint32_t current = nodes[node_idx].parent_node_index;
  while (current != UINT32_MAX) {
    if (nodes[current].kind == HierarchyNode::kInstance) return current;
    current = nodes[current].parent_node_index;
  }
  return UINT32_MAX;
}

}  // namespace lyra::common
