#pragma once

#include <optional>

#include <slang/ast/types/NetType.h>

#include "lyra/hir/structural_data_object.hpp"

namespace lyra::lowering::ast_to_hir {

// The net type a declaration carries (LRM 6.6), or nothing when the compiler
// does not model that net type. The source spelling is kept as declared, so
// `wire` and `tri` stay distinct here even though they name one fold.
auto TranslateNetType(const slang::ast::NetType& net_type)
    -> std::optional<hir::NetType>;

}  // namespace lyra::lowering::ast_to_hir
