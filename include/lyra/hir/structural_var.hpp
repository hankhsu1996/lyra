#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>

#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

struct StructuralVarId {
  std::uint32_t value;

  auto operator<=>(const StructuralVarId&) const
      -> std::strong_ordering = default;
};

// A structural member that aliases storage owned elsewhere rather than holding
// its own cell: the internal variable of a `ref` / `const ref` port (LRM
// 23.3.3.2), bound by the parent during elaboration to alias the connected
// variable. Absent for an ordinary member, which owns its storage. HIR records
// the port direction; HIR-to-MIR turns it into the member's reference type.
enum class ReferenceBinding : std::uint8_t { kRef, kConstRef };

struct StructuralVarDecl {
  std::string name;
  TypeId type;
  std::optional<ExprId> initializer;
  std::optional<ReferenceBinding> reference;
};

}  // namespace lyra::hir
