#include "lyra/lowering/mir_to_lir/mir_to_lir.hpp"

#include "lyra/lir/module.hpp"
#include "lyra/lowering/mir_to_lir/module.hpp"
#include "lyra/mir/module.hpp"

namespace lyra::lowering::mir_to_lir {

auto MirToLir(const mir::Module& module) -> std::unique_ptr<lir::Module> {
  return LowerModule(module);
}

}  // namespace lyra::lowering::mir_to_lir
