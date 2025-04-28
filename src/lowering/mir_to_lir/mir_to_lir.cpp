#include "lowering/mir_to_lir/mir_to_lir.hpp"

#include "lir/module.hpp"
#include "lowering/mir_to_lir/module.hpp"
#include "mir/module.hpp"

namespace lyra::lowering {

auto MirToLir(const mir::Module& module) -> std::unique_ptr<lir::Module> {
  return LowerModule(module);
}

}  // namespace lyra::lowering
