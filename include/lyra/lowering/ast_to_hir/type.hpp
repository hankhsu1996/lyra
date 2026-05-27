#pragma once

#include <slang/ast/types/Type.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/ast_to_hir/state.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerType(
    const slang::ast::Type& type, diag::SourceSpan decl_span,
    UnitLoweringState& state) -> diag::Result<hir::TypeData>;

}  // namespace lyra::lowering::ast_to_hir
