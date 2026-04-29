#pragma once

#include <slang/ast/types/Type.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerTypeData(const slang::ast::Type& type, diag::SourceSpan decl_span)
    -> diag::Result<hir::TypeData>;

}  // namespace lyra::lowering::ast_to_hir
