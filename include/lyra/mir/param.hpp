#pragma once

#include <compare>
#include <cstdint>
#include <string>

#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

struct ParamId {
  std::uint32_t value;

  auto operator<=>(const ParamId&) const -> std::strong_ordering = default;
};

struct ParamDecl {
  std::string name;
  TypeId type;
};

// A read of a constructor-bound structural param (a generate `genvar` per LRM
// 27.4), addressed like a member field: `receiver` is the scope object that
// owns the param -- `self` for a same-scope read, an enclosing-scope object
// reached by climbing the object tree for a cross-scope read -- and `param`
// names the field on that object. The climb is baked into `receiver`, parallel
// to `MemberAccessExpr`, so the read needs no enclosing-hop count of its own.
struct ParamRef {
  ExprId receiver = {};
  ParamId param = {};
};

}  // namespace lyra::mir
