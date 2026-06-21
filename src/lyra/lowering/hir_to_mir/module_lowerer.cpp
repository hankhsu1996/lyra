#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <utility>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/lowering/hir_to_mir/class_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::lowering::hir_to_mir {

auto ModuleLowerer::Run() -> diag::Result<mir::CompilationUnit> {
  WalkFrame root_frame;

  for (std::size_t i = 0; i < hir_->types.size(); ++i) {
    const hir::TypeId hir_id{static_cast<std::uint32_t>(i)};
    auto mir_data = TranslateTypeData(hir_->types[i].data);
    const mir::TypeId mir_id = unit_.AddType(std::move(mir_data));
    MapType(hir_id, mir_id);
  }

  ClassLowerer root(*this, nullptr, hir_->name, hir_->root_scope);
  auto top_r = root.Run(root_frame);
  if (!top_r) return std::unexpected(std::move(top_r.error()));

  unit_.top_class = *std::move(top_r);
  return std::move(unit_);
}

}  // namespace lyra::lowering::hir_to_mir
