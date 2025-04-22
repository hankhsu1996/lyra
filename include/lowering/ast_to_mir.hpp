// ast_to_mir.hpp
#pragma once

#include <slang/ast/Compilation.h>

#include "mir/module.hpp"

namespace volans::lowering {

auto AstToMir(slang::ast::Compilation& comp) -> mir::Module;

}
