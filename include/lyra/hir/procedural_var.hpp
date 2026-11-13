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

// A variable a body declares, allocated fresh for each activation (LRM 6.21,
// 13.3.1). A static one is a declaration of the scope or class around the body
// wherever the body writes it, so none is a body's.
struct ProceduralVarDecl {
  // The identifier the source declared this variable under, absent for one the
  // lowering introduced to carry a construct the language states without a
  // variable -- the right-hand side an intra-assignment delay holds (LRM
  // 9.4.5). The absence is the fact worth having: this layer answers what the
  // design wrote, and there is no spelling the compiler could put here that a
  // design may not also write (LRM 5.6.1), so the layers below read the
  // absence rather than a word chosen to look unlikely.
  std::optional<std::string> name;
  TypeId type;
  // An automatic local a detached (join_none / join_any) fork branch borrows
  // and can outlive (LRM 6.21). Its storage is lifted into a shared object so
  // the branch keeps it alive after the declaring frame returns. Decided by a
  // pre-pass over the body before the decl is interned, so it is known at
  // creation rather than discovered (and back-patched) at a later reference.
  bool lifetime_extended = false;
  // The declaration assignment the source wrote (`int x = 3`), made on each
  // entry to the declaring scope (LRM 6.21).
  std::optional<ExprId> init = std::nullopt;

  auto operator==(const ProceduralVarDecl&) const -> bool = default;
};

}  // namespace lyra::hir
