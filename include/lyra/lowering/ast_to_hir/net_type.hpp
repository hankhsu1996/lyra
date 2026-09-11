#pragma once

#include <slang/ast/types/NetType.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/structural_data_object.hpp"

namespace lyra::lowering::ast_to_hir {

// The net type a declaration carries (LRM 6.6). The source spelling is kept as
// declared, so `wire` and `tri` stay distinct here even though they are one net
// type. A net type the compiler does not model is refused by name.
auto TranslateNetType(
    const slang::ast::NetType& net_type, diag::SourceSpan span)
    -> diag::Result<hir::NetType>;

}  // namespace lyra::lowering::ast_to_hir
