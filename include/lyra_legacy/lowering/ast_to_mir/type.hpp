#pragma once

#include <slang/ast/types/Type.h>
#include <slang/text/SourceLocation.h>

#include "lyra/common/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"

namespace lyra::lowering::ast_to_mir {

auto LowerType(
    const slang::ast::Type& type, slang::SourceRange source_range,
    common::TypeArena& arena) -> Result<common::Type>;

}  // namespace lyra::lowering::ast_to_mir
