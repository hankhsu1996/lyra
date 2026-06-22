#pragma once

#include <string>

#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

// Records a SystemVerilog `typedef <target> <name>;` declaration. Multiple
// aliases may share the same target TypeId (alias-of-alias collapses to the
// canonical type during lowering). The first alias encountered for a given
// target supplies the type's emitted-class name; all remaining aliases are
// emitted as plain `using` declarations.
struct TypeAliasDecl {
  std::string name;
  TypeId target;
};

}  // namespace lyra::hir
