#pragma once

#include <compare>
#include <cstdint>
#include <format>
#include <optional>
#include <string>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/procedural_var.hpp"

namespace lyra::hir {

struct ProceduralScopeId {
  std::uint32_t value;

  auto operator<=>(const ProceduralScopeId&) const
      -> std::strong_ordering = default;
};

// A first-class lexical declaration scope in a procedural body, mirroring SV
// lexical semantics (LRM 9.3.4 visibility, LRM 23.9 hierarchical-reference
// addressability). The HIR statement tree carries execution semantics; this
// scope tree carries declaration semantics. The two views share identity
// through `BlockStmt.scope` / `ForkStmt.scope` / etc., but neither view
// duplicates what the other says: a downstream consumer that needs ownership
// reads it here, not from the statement tree.
//
// Lexical containment runs downward: a scope holds the declarations it owns and
// the scopes nested directly inside it, and both are reached by descending from
// a body's root. Nothing holds a link back up -- the direction the enclosing
// structural scope already runs the same two relations in. A declaration's
// identity lives in its body's arena rather than inside the scope, because an
// identity a hierarchical path can name is minted before any body lowers; what
// the scope holds is that id.
//
// Scopes nest: a `begin ... end` inside a `begin ... end` is two of them, and
// the inner one is a child of the outer. The kinds below say which construct
// introduced each.
//
// Of the constructs LRM 23.9 defines a scope for, these are the ones that live
// in procedural code: a task or function, a `begin ... end` block (LRM 9.3.4 /
// 9.3.5), and a `fork ... join` block carrying its own locals (LRM 9.3.2). A
// loop that declares its own control variables gets a scope too, but not a
// kind of its own: LRM 12.7.1 and 12.7.3 define it as an implicit begin-end
// block around the loop statement, unnamed unless a statement label names it.
//
// `kProcessRoot` is the one kind SV does not define -- LRM 23.9 lists the
// `begin ... end`, not the `always` or `initial` around it. It exists so every
// body has exactly one root even when the source wrote no block at all
// (`initial #7 f();`), which is what keeps the root a plain id rather than an
// optional every consumer has to branch on. It is never a child of anything.
//
// Whether the source named the scope is not a kind: a `begin ... end` is one
// construct whether or not a `block_identifier` follows it. The name it was
// given is recorded as a name, so the kinds stay one per construct and gaining
// named forks adds no value here.
enum class ProceduralScopeKind : std::uint8_t {
  kProcessRoot,
  kSubroutineRoot,
  kBlock,
  kFork,
};

struct ProceduralScopeDecl {
  ProceduralScopeKind kind = ProceduralScopeKind::kBlock;
  // The identifier the source gave this scope -- a `block_identifier` (LRM
  // 9.3.5) or a subroutine name -- absent when it gave none. Its presence is
  // what a hierarchical path can reach (LRM 23.9), so everything that follows
  // from reachability reads it here. Distinct from `Stmt.label` (LRM 6.21
  // statement label); the two never share storage.
  std::optional<std::string> source_name;
  std::vector<ProceduralVarId> declarations;
  std::vector<ProceduralScopeId> child_scopes;
};

// The scope's own component of a generated name: the identifier the source gave
// it, or a stand-in built from the kind and the scope's own identity for one it
// did not name. Every scope has one, so a generated class or member that needs
// to name a scope never asks whether the source named it -- what a hierarchical
// path can reach is the source name, a separate question.
[[nodiscard]] inline auto SegmentName(
    const ProceduralScopeDecl& scope, ProceduralScopeId id) -> std::string {
  if (scope.source_name.has_value()) {
    return *scope.source_name;
  }
  switch (scope.kind) {
    case ProceduralScopeKind::kProcessRoot:
      return std::format("body_{}", id.value);
    case ProceduralScopeKind::kBlock:
      return std::format("block_{}", id.value);
    case ProceduralScopeKind::kFork:
      return std::format("fork_{}", id.value);
    case ProceduralScopeKind::kSubroutineRoot:
      break;
  }
  throw InternalError(
      "hir: a subroutine scope was created without the name the source gave "
      "it");
}

}  // namespace lyra::hir
