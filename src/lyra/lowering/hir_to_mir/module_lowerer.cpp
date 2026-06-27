#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <string>
#include <string_view>
#include <utility>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/lowering/hir_to_mir/class_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::lowering::hir_to_mir {

auto ModuleLowerer::Run() -> diag::Result<mir::CompilationUnit> {
  WalkFrame root_frame;

  // Every HIR type is MIR-representable: AST-to-HIR rejects the forms MIR has
  // no shape for, so this projection never fails.
  for (std::size_t i = 0; i < hir_->types.size(); ++i) {
    const hir::TypeId hir_id{static_cast<std::uint32_t>(i)};
    const hir::Type& hir_type = hir_->types.Get(hir_id);
    const mir::TypeId mir_id =
        unit_.types.Intern(TranslateTypeData(hir_type.data));
    MapType(hir_id, mir_id);
  }

  ClassLowerer root(*this, nullptr, hir_->name, hir_->root_scope);
  auto top_r = root.Run(root_frame);
  if (!top_r) return std::unexpected(std::move(top_r.error()));

  unit_.root = *top_r;
  return std::move(unit_);
}

auto ModuleLowerer::NextGenerateScopeName(std::string_view arm_tag)
    -> std::string {
  return std::format("gen{}_{}", next_generate_scope_name_++, arm_tag);
}

}  // namespace lyra::lowering::hir_to_mir
