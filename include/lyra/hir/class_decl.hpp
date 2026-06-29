#pragma once

#include <optional>
#include <string>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

// A class property (LRM 8.4): a named member variable of a class, with its own
// per-object storage. `initializer`, when present, is the declared `= value`
// expression in the class's own `exprs` arena; absent means the property takes
// its type's Table 7-1 default at construction.
struct ClassField {
  std::string name;
  TypeId type;
  std::optional<ExprId> initializer;
};

// A SystemVerilog class declaration (LRM 8). The class's properties, its
// instance methods, and the expressions their initializers reference;
// references to a class name resolve to this declaration's id. A class is
// reached through a handle, so it carries no structural position of its own.
// Each method (LRM 8.6) is a subroutine reached through the instance, so its
// body reads the receiver and the class's properties through the receiver.
struct ClassDecl {
  std::string name;
  std::vector<ClassField> fields;
  std::vector<SubroutineDecl> methods;
  base::Arena<Expr, ExprId> exprs;
};

}  // namespace lyra::hir
