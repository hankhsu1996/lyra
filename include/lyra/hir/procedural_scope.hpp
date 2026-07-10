#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

#include "lyra/hir/procedural_var.hpp"

namespace lyra::hir {

struct ProceduralScopeId {
  std::uint32_t value;

  auto operator<=>(const ProceduralScopeId&) const
      -> std::strong_ordering = default;
};

// A first-class lexical declaration scope in a procedural body. Holds the
// declarations it directly owns and the immediate child scopes nested
// inside it -- a downward ownership tree that mirrors SV lexical semantics
// (LRM 9.3.4 visibility, LRM 23.9 hierarchical-reference addressability).
// The HIR statement tree carries execution semantics; this scope tree
// carries declaration semantics. The two views share identity through
// `BlockStmt.scope` / `ForkStmt.scope` / etc., but neither view duplicates
// what the other says: a downstream consumer that needs ownership reads it
// here, not from the statement tree.
//
// SV gives several constructs their own declaration scope: the body root of a
// process or subroutine; every `begin ... end` block (LRM 9.3.4 / 9.3.5); a
// `fork ... join` block carrying its own locals (LRM 9.3.2); the implicit
// scope a `foreach` loop introduces for its loop variables (LRM 12.7.3). They
// differ only in whether the scope carries an SV block identifier, which is
// also what makes it addressable as a hierarchical-reference head (LRM 23.9).
struct ProceduralScopeDecl {
  // SV `block_identifier` (LRM 9.3.5). Present for a named begin/end and a
  // named fork/join; absent for a process root, an unnamed block, and a
  // foreach loop scope. Distinct from `Stmt.label` (LRM 6.21 statement
  // label); the two never share storage.
  std::optional<std::string> label;
  std::vector<ProceduralVarId> direct_declarations;
  std::vector<ProceduralScopeId> direct_child_scopes;
};

}  // namespace lyra::hir
