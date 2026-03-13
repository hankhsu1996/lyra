#pragma once

#include <cstdint>
#include <string>
#include <vector>

namespace slang::ast {
class InstanceBodySymbol;
class Symbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Distinguishes the two kinds of generate selection points.
//
// kBranch: one alternative of a generate-if or generate-case.
//   Sibling branches share the same construct_index within the parent scope.
//   The chosen alternative is identified by the specific GenerateBlockSymbol
//   pointer (slang assigns no ordinal to if/case alternatives).
//
// kArrayEntry: one iteration of a generate-for.
//   All entries belong to the same GenerateBlockArraySymbol.
//   The chosen alternative is identified by the entry_index ordinal.
enum class SelectionKind : uint8_t {
  kBranch,
  kArrayEntry,
};

// One step in a generate availability path.
//
// Represents a single generate selection required for an artifact to exist.
// A sequence of these (the availability path) describes all the generate
// selections that must be made for an artifact to be active.
//
// A selection point is identified by its parent availability path plus
// construct_index (slang's constructIndex, which is local to the parent
// scope). construct_index alone is NOT a global construct identity -- it
// must be interpreted relative to the path position.
//
// The block pointer provides observation/extraction identity within the
// borrowed slang compilation lifetime. It is not a stable cross-compilation
// semantic key. Later repertoire descriptor design (M2c-2b) will define
// what survives beyond this extraction boundary.
struct GenerateSelection {
  SelectionKind kind;

  // The GenerateBlockSymbol at this selection point.
  // kBranch: the specific branch's GenerateBlockSymbol. For branch
  //   selections, pointer identity IS the chosen-alternative identity
  //   (valid within the borrowed slang compilation lifetime).
  // kArrayEntry: the entry's GenerateBlockSymbol (array.entries[i]).
  const slang::ast::Symbol* block;

  // Construct-level identity within the parent scope (slang's constructIndex).
  // Shared by all alternatives of the same generate construct.
  // Local to the parent scope -- only meaningful at a given path position.
  uint32_t construct_index;

  // Entry ordinal (meaningful only for kArrayEntry; 0 for kBranch).
  uint32_t entry_index;
};

// Classifies the compile-relevant role of an inventoried artifact.
enum class ArtifactKind : uint8_t {
  kDecl,
  kProcess,
  kInstance,
  kContinuousAssign,
};

// An artifact that Lyra may need to compile, together with the generate
// selection path under which it exists.
//
// Artifact identity is the slang symbol pointer itself -- Lyra does not
// copy or re-model the symbol. The availability path records the sequence
// of generate selections required for this artifact to be active.
//
// An empty availability path means the artifact is unconditionally present
// (not inside any generate construct).
struct ArtifactHandle {
  ArtifactKind kind;
  const slang::ast::Symbol* symbol;
  std::vector<GenerateSelection> availability;
};

// Flat inventory of compile-relevant artifacts within an instance body.
// No tree structure -- just the artifacts Lyra cares about, each annotated
// with its generate availability path.
//
// Included artifact kinds (these produce compiled code):
//   Variable, Net          -> decls (storage declarations)
//   ProceduralBlock        -> processes (always/initial/final blocks)
//   Instance               -> child_instances (sub-module instantiations)
//   ContinuousAssign       -> continuous_assigns (combinational logic)
//
// Intentionally excluded (not compile-relevant artifacts):
//   Parameter, TypeParameter  -- parameterize, not compiled output
//   Genvar                    -- generate loop iterator, not runtime
//   Subroutine                -- functions/tasks, future scope
//   TypeAlias, ForwardingTypedef -- type decls, not runtime
//   Specparam                 -- specify block parameters
//   Modport                   -- interface modports
//
// Traversal transparency rule: scopes that are not constructor-time selection
// points (e.g., named StatementBlocks) are traversed but not modeled. Their
// artifacts inherit the enclosing scope's availability path.
//
// Ordering: artifacts appear in slang member iteration order (source order
// within each scope, depth-first through generate scopes). This ordering is
// deterministic for a given source input.
struct ArtifactInventory {
  std::vector<ArtifactHandle> decls;
  std::vector<ArtifactHandle> processes;
  std::vector<ArtifactHandle> child_instances;
  std::vector<ArtifactHandle> continuous_assigns;
};

// Walks the full body including uninstantiated generate branches,
// collecting every compile-relevant artifact with its availability path.
auto BuildArtifactInventory(const slang::ast::InstanceBodySymbol& body)
    -> ArtifactInventory;

// Text dump for inspection and testing.
// Format per artifact:
//   <kind> <symbol_name> avail=[<selection>, ...]
// where <selection> is:
//   branch(<block_name>@<construct_index>)    for kBranch
//   entry(<block_name>[<entry_index>])        for kArrayEntry
auto DumpArtifactInventory(const ArtifactInventory& inventory) -> std::string;

}  // namespace lyra::lowering::ast_to_hir
