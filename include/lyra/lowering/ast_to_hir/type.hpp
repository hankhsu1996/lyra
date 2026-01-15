#pragma once

#include <slang/ast/types/Type.h>

#include "lyra/common/source_span.hpp"
#include "lyra/common/type.hpp"

namespace lyra::lowering::ast_to_hir {

struct Context;

auto LowerType(const slang::ast::Type& type, SourceSpan source, Context* ctx)
    -> TypeId;

}  // namespace lyra::lowering::ast_to_hir
