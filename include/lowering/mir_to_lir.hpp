#pragma once

#include "lir/module.hpp"
#include "mir/module.hpp"

namespace lyra::lowering {

auto MirToLir(const mir::Module& mod) -> lir::Module;

}  // namespace lyra::lowering
