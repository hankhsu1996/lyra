#include "lyra/lowering/mir_to_lir/mir_to_lir.hpp"

#include <memory>

#include "lyra/common/symbol.hpp"
#include "lyra/lir/module.hpp"
#include "lyra/lowering/mir_to_lir/module.hpp"
#include "lyra/mir/module.hpp"

namespace lyra::lowering::mir_to_lir {

auto MirToLir(
    const mir::Module& module, const common::SymbolTable& symbol_table)
    -> std::unique_ptr<lir::Module> {
  return LowerModule(module, symbol_table);
}

}  // namespace lyra::lowering::mir_to_lir
