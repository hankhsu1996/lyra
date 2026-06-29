#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/hir/module_unit.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

// Per-unit lowerer for the HIR-to-MIR pass.
//
// Fact: a reference to the input `hir::ModuleUnit`.
// Registry: the HIR-TypeId to MIR-TypeId translation map, populated by `Run`
// as it walks the input's type list.
// Root output: the in-progress `mir::CompilationUnit`. Constructed in the
// ctor; populated by `Run`; moved out by `Run`'s return. After `Run` returns
// the class holds no IR pointer. Handlers reach the unit's append-only API
// through the mutable `Unit()` overload; downstream consumers post-`Run` use
// the const overload, which is the same shape they hold post-move.
class ModuleLowerer {
 public:
  ModuleLowerer(
      const hir::ModuleUnit& hir, const diag::SourceManager& source_manager)
      : hir_(&hir), source_manager_(&source_manager) {
  }

  auto Run() -> diag::Result<mir::CompilationUnit>;

  // Access to the in-progress compilation unit. The const overload is the
  // read-only view downstream consumers see once lowering finishes; the mutable
  // overload lets a handler append unit-wide output -- a synthesized type, a
  // deferred-check site -- to the unit, the same discipline by which nested IR
  // is written through the frame's current targets.
  [[nodiscard]] auto Unit() const -> const mir::CompilationUnit& {
    return unit_;
  }
  [[nodiscard]] auto Unit() -> mir::CompilationUnit& {
    return unit_;
  }

  [[nodiscard]] auto Hir() const -> const hir::ModuleUnit& {
    return *hir_;
  }

  // Resolves a `SourceSpan` to a "file:line:col" string for diagnostic
  // origin tagging (LRM 20.10 source identification). Returned by value so
  // the caller can intern it as a MIR `StringLiteral` at the emit site.
  [[nodiscard]] auto SourceManager() const -> const diag::SourceManager& {
    return *source_manager_;
  }

  [[nodiscard]] auto TranslateType(hir::TypeId hir_id) const -> mir::TypeId {
    if (hir_id.value >= type_map_.size()) {
      throw InternalError("ModuleLowerer::TranslateType: unmapped HIR type");
    }
    return type_map_[hir_id.value];
  }

  void MapType(hir::TypeId hir_id, mir::TypeId mir_id) {
    if (hir_id.value != type_map_.size()) {
      throw InternalError(
          "ModuleLowerer::MapType: HIR types must be mapped in HIR id order");
    }
    type_map_.emplace_back(mir_id);
  }

  // A HIR class declaration's MIR identity and the interned `ObjectType` that
  // names it. Both are minted before any type is translated, so a class handle
  // type can resolve to its managed-reference pointee while the class body is
  // still being built.
  void MapClass(
      hir::ClassId hir_id, mir::ClassId mir_id, mir::TypeId object_type) {
    if (hir_id.value != class_map_.size()) {
      throw InternalError(
          "ModuleLowerer::MapClass: HIR classes must be mapped in HIR id "
          "order");
    }
    class_map_.emplace_back(mir_id);
    class_object_type_map_.emplace_back(object_type);
  }

  [[nodiscard]] auto TranslateClass(hir::ClassId hir_id) const -> mir::ClassId {
    if (hir_id.value >= class_map_.size()) {
      throw InternalError("ModuleLowerer::TranslateClass: unmapped HIR class");
    }
    return class_map_[hir_id.value];
  }

  [[nodiscard]] auto ClassObjectType(hir::ClassId hir_id) const -> mir::TypeId {
    if (hir_id.value >= class_object_type_map_.size()) {
      throw InternalError("ModuleLowerer::ClassObjectType: unmapped HIR class");
    }
    return class_object_type_map_[hir_id.value];
  }

  // Mints a collision-free class name for one generate scope, tagged by its
  // arm kind (`loop` / `then` / `else` / ...). The name is only an
  // implementation handle for the emitted type -- a generate scope's runtime
  // identity is its HierarchySegment -- so it need only be unit-unique and
  // deterministic, which a monotonic count over the deterministic lowering walk
  // provides.
  [[nodiscard]] auto NextGenerateScopeName(std::string_view arm_tag)
      -> std::string;

  // Posts a class's structural shape; the shape is committed once and is
  // read back during peer-body lowering.
  void DefineClassShape(mir::ClassId id, mir::ClassShape shape) {
    if (id.value >= class_shapes_.size()) {
      class_shapes_.resize(id.value + 1);
    }
    auto& slot = class_shapes_[id.value];
    if (slot.has_value()) {
      throw InternalError(
          "ModuleLowerer::DefineClassShape: shape for this id is already "
          "defined");
    }
    slot = std::move(shape);
  }

  [[nodiscard]] auto GetClassShape(mir::ClassId id) const
      -> const mir::ClassShape& {
    if (id.value >= class_shapes_.size() ||
        !class_shapes_[id.value].has_value()) {
      throw InternalError(
          "ModuleLowerer::GetClassShape: shape for this id is not defined");
    }
    return *class_shapes_[id.value];
  }

 private:
  [[nodiscard]] auto TranslateTypeData(const hir::TypeData& data) const
      -> mir::TypeData;

  const hir::ModuleUnit* hir_;
  const diag::SourceManager* source_manager_;
  mir::CompilationUnit unit_;
  std::vector<mir::TypeId> type_map_;
  std::vector<mir::ClassId> class_map_;
  std::vector<mir::TypeId> class_object_type_map_;
  std::uint32_t next_generate_scope_name_ = 0;
  // Class shapes published during the declare pass and read during peer-body
  // lowering. Lives only on the lowerer; the finished compilation unit holds
  // the only authoritative class representation.
  std::vector<std::optional<mir::ClassShape>> class_shapes_;
};

}  // namespace lyra::lowering::hir_to_mir
