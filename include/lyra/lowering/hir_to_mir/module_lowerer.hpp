#pragma once

#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/hir/module_unit.hpp"
#include "lyra/hir/type.hpp"
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

  // Access to the in-progress compilation unit. The const overload mirrors the
  // interface downstream consumers see post-`Run`; the mutable overload lets
  // handlers reach the unit's own append-only API (e.g. `AddType`,
  // `AllocateDeferredCheckSiteId`) directly, matching the discipline used for
  // nested-scope writes through `frame.current_*_scope`.
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

 private:
  [[nodiscard]] auto TranslateTypeData(const hir::TypeData& data) const
      -> mir::TypeData;

  const hir::ModuleUnit* hir_;
  const diag::SourceManager* source_manager_;
  mir::CompilationUnit unit_;
  std::vector<mir::TypeId> type_map_;
};

}  // namespace lyra::lowering::hir_to_mir
