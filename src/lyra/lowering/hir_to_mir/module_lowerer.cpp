#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <string>
#include <string_view>
#include <utility>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/lowering/hir_to_mir/class_decl_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

auto ModuleLowerer::Run() -> diag::Result<mir::CompilationUnit> {
  WalkFrame root_frame;

  // Every class identity is minted before any type is translated, so a class
  // handle type resolves to the managed-reference pointee that names it while
  // the class bodies are still being built.
  for (std::size_t i = 0; i < hir_->classes.size(); ++i) {
    const hir::ClassId hir_id{static_cast<std::uint32_t>(i)};
    const mir::ClassId mir_id = unit_.DeclareClass();
    const mir::TypeId object_type =
        unit_.types.Intern(mir::ObjectType{.class_id = mir_id});
    MapClass(hir_id, mir_id, object_type);
  }

  // Every HIR type is MIR-representable: AST-to-HIR rejects the forms MIR has
  // no shape for, so this projection never fails.
  for (std::size_t i = 0; i < hir_->types.size(); ++i) {
    const hir::TypeId hir_id{static_cast<std::uint32_t>(i)};
    const hir::Type& hir_type = hir_->types.Get(hir_id);
    const mir::TypeId mir_id =
        unit_.types.Intern(TranslateTypeData(hir_type.data));
    MapType(hir_id, mir_id);
  }

  for (std::size_t i = 0; i < hir_->classes.size(); ++i) {
    const hir::ClassId hir_id{static_cast<std::uint32_t>(i)};
    ClassDeclLowerer class_lowerer(
        *this, TranslateClass(hir_id), ClassObjectType(hir_id),
        hir_->classes.Get(hir_id));
    auto class_or = class_lowerer.Run();
    if (!class_or) return std::unexpected(std::move(class_or.error()));
    unit_.DefineClass(TranslateClass(hir_id), *std::move(class_or));
  }

  // Two-sweep structural lowering: the first sweep mints every class identity
  // and publishes its shape; the second lowers every body and commits the
  // composed class to the unit.
  StructuralScopeLowerer root(*this, nullptr, hir_->name, hir_->root_scope);
  auto top_r = root.DeclareShape();
  if (!top_r) return std::unexpected(std::move(top_r.error()));
  auto body_r = root.PopulateBodies(root_frame);
  if (!body_r) return std::unexpected(std::move(body_r.error()));

  unit_.root = *top_r;
  return std::move(unit_);
}

auto ModuleLowerer::NextGenerateScopeName(std::string_view arm_tag)
    -> std::string {
  return std::format("gen{}_{}", next_generate_scope_name_++, arm_tag);
}

}  // namespace lyra::lowering::hir_to_mir
