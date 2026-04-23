#pragma once

#include <slang/ast/types/Type.h>

#include "lyra/hir/type.hpp"
#include "state.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerType(ModuleLoweringState& state, const slang::ast::Type& type)
    -> hir::TypeId;

}  // namespace lyra::lowering::ast_to_hir
