#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>

#include "lyra/base/pool_id.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

struct ProceduralVarId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const ProceduralVarId&) const
      -> std::strong_ordering = default;
};

// LRM 6.21 / 13.3.1 / 13.4.2 variable lifetime. A static-lifetime variable has
// one storage location that retains its value between calls, per thing that
// replicates the declaration -- a module instance, and nothing at all for a
// class method or a package subroutine; an automatic-lifetime variable is
// allocated fresh for each activation. slang resolves the source keyword and
// the enclosing module / subroutine default into a per-variable choice, which
// HIR records verbatim.
enum class VariableLifetime : std::uint8_t {
  kStatic,
  kAutomatic,
};

struct ProceduralVarDecl {
  // The identifier the source declared this variable under, absent for one the
  // lowering introduced to carry a construct the language states without a
  // variable -- a `foreach` bound, the right-hand side an intra-assignment
  // delay holds (LRM 9.4.5). The absence is the fact worth having: this layer
  // answers what the design wrote, and there is no spelling the compiler could
  // put here that a design may not also write (LRM 5.6.1), so the layers below
  // read the absence rather than a word chosen to look unlikely.
  std::optional<std::string> name;
  TypeId type;
  VariableLifetime lifetime = VariableLifetime::kAutomatic;
  // An automatic local a detached (join_none / join_any) fork branch borrows
  // and can outlive (LRM 6.21). Its storage is lifted into a shared object so
  // the branch keeps it alive after the declaring frame returns. Decided by a
  // pre-pass over the body before the decl is interned, so it is known at
  // creation rather than discovered (and back-patched) at a later reference.
  bool lifetime_extended = false;
  // The declaration assignment the source wrote (`int x = 3`). It belongs to
  // the declaration, not to any statement: the lifetime above decides when it
  // runs against storage of that lifetime, never whose it is.
  std::optional<ExprId> init = std::nullopt;
};

}  // namespace lyra::hir
