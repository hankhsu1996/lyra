#pragma once

#include <cstdint>
#include <vector>

#include "lyra/common/type_arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/basic_block.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::mir::passes {

// A canonical induction loop recognized from the LowerForLoop shape:
//
//   preheader: init IV with constant, Jump(header)
//   header:    Branch(cmp(iv, const_bound), header+1, exit)
//   body:      one or more blocks (IV not modified)
//   latch:     iv = iv + 1, Jump(header)
//
// First-version scope: only recognizes canonical increasing `for` loops
// with constant init, constant bound, and +1 step. Does not recognize
// `repeat(N)`, `while`, decrementing loops, or non-constant bounds.
struct CanonicalInductionLoop {
  uint32_t header;
  uint32_t latch;
  PlaceId induction_var;
  int64_t init;
  int64_t max_value;
  std::vector<uint32_t> body_blocks;
};

// Find all canonical induction loops in a routine's block graph.
// Returns one entry per recognized loop. Unrecognized back-edges are
// silently skipped.
//
// Each recognized loop is independent: nested loops with distinct IVs are
// recognized separately. Body block ranges may overlap for nested loops
// (an outer loop's body range includes inner loop blocks). Consumers that
// use body_blocks for rewriting must handle this; consumers that only use
// header/latch (like bounded-back-edge detection) are not affected.
auto FindCanonicalInductionLoops(
    const std::vector<BasicBlock>& blocks, const Arena& arena)
    -> std::vector<CanonicalInductionLoop>;

// A back-edge proven safe to skip the iteration-limit guard.
//
// This is a stronger qualification than canonical-loop recognition: in
// addition to proving canonical induction structure, it also proves that
// the post-latch IV value (max_value + 1) is representable in the IV type,
// so removing the back-edge guard cannot create wraparound-driven
// nontermination.
//
// Only accepts loops whose IV is a plain kIntegral scalar with bit width
// in [2, 63]. Packed arrays, structs, enums, and non-integral types are
// conservatively rejected. If the post-latch IV value would overflow the
// type, the loop is also rejected. Only loops where termination is
// provable at the type level are accepted.
//
// First-version scope: canonical `for` loops only. Does not cover
// `repeat(N)` or other bounded loop shapes.
struct BoundedBackEdge {
  uint32_t latch_block;
  uint32_t header_block;
  // Proven upper bound on iteration count (max_value - init + 1).
  // Retained as part of the semantic result for diagnostics, logging,
  // and future use (e.g., cost estimation). Not consumed by codegen today.
  int64_t max_iterations;
};

// Find all back-edges that are provably bounded in a routine.
// Uses FindCanonicalInductionLoops for structural recognition, then
// applies the IV overflow safety qualification.
auto FindBoundedBackEdges(
    const std::vector<BasicBlock>& blocks, const Arena& arena,
    const TypeArena& types) -> std::vector<BoundedBackEdge>;

}  // namespace lyra::mir::passes
