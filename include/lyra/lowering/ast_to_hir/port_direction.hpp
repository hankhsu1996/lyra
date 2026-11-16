#pragma once

#include <slang/ast/symbols/PortSymbols.h>

#include "lyra/support/port_direction.hpp"

namespace lyra::lowering::ast_to_hir {

// The direction `port` is declared with (LRM 23.2.2.3), a `ref` port reaching
// a declaration that forbids writing through it being a `const ref` (LRM
// 23.3.3.2).
auto PortDirectionOf(const slang::ast::PortSymbol& port)
    -> support::PortDirection;

}  // namespace lyra::lowering::ast_to_hir
