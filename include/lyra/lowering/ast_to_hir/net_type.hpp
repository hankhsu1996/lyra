#pragma once

#include <optional>

#include <slang/ast/types/NetType.h>

#include "lyra/hir/structural_data_object.hpp"

namespace lyra::lowering::ast_to_hir {

// The fold a net's declared net type names (LRM 6.6), or nothing when the
// compiler does not model that fold. `wire` and `tri` differ only in source
// spelling; both resolve under the tri-state truth table.
auto TranslateNetType(const slang::ast::NetType& net_type)
    -> std::optional<hir::NetType>;

}  // namespace lyra::lowering::ast_to_hir
