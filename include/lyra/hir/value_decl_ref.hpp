#pragma once

#include <variant>

#include "lyra/hir/net_decl.hpp"
#include "lyra/hir/param_decl.hpp"
#include "lyra/hir/var_decl.hpp"

namespace lyra::hir {

using ValueDeclRef = std::variant<VarDeclId, NetDeclId, ParamDeclId>;

struct LocalValueRef {
  ValueDeclRef target;
};

}  // namespace lyra::hir
