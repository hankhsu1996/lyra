#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

#include "lyra/base/pool_id.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/support/assertion_control.hpp"

namespace lyra::hir {

struct ControlledAssertionId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const ControlledAssertionId&) const
      -> std::strong_ordering = default;
};

// A statement the assertion control tasks select (LRM 20.11): an assertion,
// an expect statement, or a `unique` / `unique0` / `priority` statement, whose
// violation reports they control. What a task selects it by is its type
// (Table 20-6) and, for one of the four assertion types, the directive it
// states (Table 20-7). A task naming a scope selects every one the scope holds,
// so each is declared by the scope it stands in, as a variable is; its identity
// is the statement's and not an execution's, so a task's or a loop's many runs
// of one statement are one of these.
struct ControlledAssertionDecl {
  support::AssertionType type;
  std::optional<support::AssertionDirectiveType> directive;

  auto operator==(const ControlledAssertionDecl&) const -> bool = default;
};

struct ProceduralScopeId {
  std::uint32_t value = base::kUnassignedId;

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
// Lexical containment runs downward: a scope holds the automatic variables it
// declares and the scopes nested directly inside it, and both are reached by
// descending from a body's root -- the direction the enclosing structural
// scope already runs the same two relations in. An automatic variable's
// identity lives in its body's arena, and the scope holds that id. A static
// variable written in the scope is no declaration of the body at all: it is a
// variable of the structural scope or class around the body, which records the
// procedural scope it is written in (LRM 6.21).
//
// Scopes nest: a `begin ... end` inside a `begin ... end` is two of them, and
// the inner one is a child of the outer. The kinds below say which construct
// introduced each.
//
// Of the constructs LRM 23.9 defines a scope for, these are the ones that live
// in procedural code: a task or a function, a `begin ... end` block (LRM
// 9.3.4), which a statement label on any other statement opens too (LRM 9.3.5),
// and a `fork ... join` block, by how it joins (LRM 9.3.2). A block is one of
// these where the front end made it a scope: where the source named it or it
// directly declares an item (LRM 37.12 detail 1). Beside them stand a
// `for` or `foreach` loop declaring its own variables, which LRM 12.7.1 and
// 12.7.3 make an implicit block around the loop, and the body a constraint
// block (LRM 18.5) or a covergroup (LRM 19) is lowered as. VPI reads the kind
// as the scope's type (LRM 37.12).
//
// `kProcessRoot` is the one kind SV does not define -- LRM 23.9 lists the
// `begin ... end`, not the `always` or `initial` around it. It exists so every
// body has exactly one root even when the source wrote no block at all
// (`initial #7 f();`), which is what keeps the root a plain id rather than an
// optional every consumer has to branch on. It is never a child of anything.
// `kImplicit` is a scope no construct of the source wrote: one the compiler
// opens for declarations of its own -- the temporary an intra-assignment timing
// control holds, a body made to evaluate an expression -- the statements a
// body lists without a `begin ... end` around them, and a block the source
// neither named nor declared anything in, all of which declare nothing a name
// can reach.
//
// Whether the source named the scope is not a kind: a `begin ... end` is one
// construct whether or not a `block_identifier` follows it. The name it was
// given is recorded as a name.
enum class ProceduralScopeKind : std::uint8_t {
  kProcessRoot,
  kTask,
  kFunction,
  kConstraintBlock,
  kCovergroup,
  kBegin,
  kForkJoin,
  kForkJoinAny,
  kForkJoinNone,
  kFor,
  kForeach,
  kImplicit,
};

struct ProceduralScopeDecl {
  ProceduralScopeKind kind = ProceduralScopeKind::kBegin;
  // The identifier the source gave this scope -- a `block_identifier` (LRM
  // 9.3.5) or a subroutine name -- as a hierarchical name writes it (LRM 23.6),
  // which is escaped where such a name has to escape it; absent when the
  // source gave none. Its presence is what a hierarchical path can reach (LRM
  // 23.9), so everything that follows from reachability reads it here.
  // Distinct from `Stmt.label` (LRM 6.21 statement label); the two never share
  // storage.
  std::optional<std::string> source_name;
  std::vector<ProceduralVarId> declarations;
  std::vector<ControlledAssertionId> controlled_assertions;
  std::vector<ProceduralScopeId> child_scopes;

  auto operator==(const ProceduralScopeDecl&) const -> bool = default;
};

// The scopes of `scopes` no other one of them holds, each the root of one
// body, in the order they were declared. The tree states only the children of
// each scope, so a walk of all of them starts here.
[[nodiscard]] auto RootProceduralScopes(
    const base::Registry<ProceduralScopeDecl, ProceduralScopeId>& scopes)
    -> std::vector<ProceduralScopeId>;

}  // namespace lyra::hir
