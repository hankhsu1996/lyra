#pragma once

#include <compare>
#include <cstdint>
#include <string>

#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

struct ProceduralVarId {
  std::uint32_t value;

  auto operator<=>(const ProceduralVarId&) const
      -> std::strong_ordering = default;
};

// LRM 13.3.1 / 13.4.2 variable lifetime. A static-lifetime variable has one
// storage location per module instance that retains its value between calls;
// an automatic-lifetime variable is allocated fresh for each activation. slang
// resolves the source keyword and the enclosing module / subroutine default
// into a per-variable choice, which HIR records verbatim.
enum class VariableLifetime : std::uint8_t {
  kStatic,
  kAutomatic,
};

struct ProceduralVarDecl {
  std::string name;
  TypeId type;
  VariableLifetime lifetime = VariableLifetime::kAutomatic;
  // An automatic local a detached (join_none / join_any) fork branch borrows
  // and can outlive (LRM 6.21). Its storage is lifted into a shared object so
  // the branch keeps it alive after the declaring frame returns. Decided by a
  // pre-pass over the body before the decl is interned, so it is known at
  // creation rather than discovered (and back-patched) at a later reference.
  bool lifetime_extended = false;
};

}  // namespace lyra::hir
