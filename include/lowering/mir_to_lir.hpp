#pragma once

#include "lir/module.hpp"
#include "mir/module.hpp"

namespace volans::lowering {

auto MirToLir(const mir::Module& mod) -> lir::Module;

}  // namespace volans::lowering
