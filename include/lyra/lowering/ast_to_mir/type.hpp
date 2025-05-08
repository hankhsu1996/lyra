#pragma once

#include <slang/ast/types/Type.h>

#include "lyra/common/type.hpp"

namespace lyra::lowering {

auto LowerType(const slang::ast::Type& type) -> common::Type;

}  // namespace lyra::lowering
