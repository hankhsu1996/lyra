#pragma once

#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/loop_var.hpp"
#include "lyra/hir/structural_hops.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/structural_var.hpp"
#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/structural_hops.hpp"
#include "lyra/mir/structural_param.hpp"
#include "lyra/mir/structural_scope.hpp"
#include "lyra/mir/structural_scope_id.hpp"
#include "lyra/mir/structural_var.hpp"

namespace lyra::lowering::hir_to_mir {

struct ChildStructuralScopeBinding {
  mir::StructuralScopeId scope_id;
  mir::StructuralVarId var_id;
};

// Bindings produced by `InstallGenerateOwnedChildScopes` for a single
// `hir::Generate`. Indexed by `hir::StructuralScopeId.value`. Phase-local to
// constructor lowering and not part of `StructuralScopeLowerer`.
struct GenerateBindings {
  std::vector<ChildStructuralScopeBinding> by_scope_id;
};

// One structural parameter the child scope receives at construction. The
// `param` is the MIR decl installed on the child scope; `source_loop_var`
// records the HIR loop variable (if any) that maps to it. Generate-for child
// scopes use this today.
struct ScopeEntryStructuralParamBinding {
  mir::StructuralParamDecl param;
  hir::LoopVarDeclId source_loop_var = {};
};

// Per-scope lowering registries for one `mir::StructuralScope`. Carries
// scope-local HIR-to-MIR translation maps and the cross-unit-ref slot table.
// Holds no pointer to the IR scope being built: handlers reach it through
// `frame.current_structural_scope`.
class StructuralScopeLowerer {
 public:
  StructuralScopeLowerer(
      ModuleLowerer& module, const StructuralScopeLowerer* parent,
      std::string name, const hir::StructuralScope& hir_scope)
      : module_(&module),
        parent_(parent),
        name_(std::move(name)),
        hir_scope_(&hir_scope) {
  }

  // Constructs the `mir::StructuralScope` on the stack, walks the HIR scope
  // body, and returns the result. Handlers reached through dispatch write to
  // `frame.current_structural_scope`. `entry_bindings` are the structural
  // params injected by an enclosing for-generate iteration.
  auto Run(
      WalkFrame parent_frame,
      std::span<const ScopeEntryStructuralParamBinding> entry_bindings = {})
      -> diag::Result<mir::StructuralScope>;

  [[nodiscard]] auto Module() -> ModuleLowerer& {
    return *module_;
  }
  [[nodiscard]] auto Module() const -> const ModuleLowerer& {
    return *module_;
  }

  [[nodiscard]] auto Parent() const -> const StructuralScopeLowerer* {
    return parent_;
  }

  [[nodiscard]] auto Name() const -> const std::string& {
    return name_;
  }

  [[nodiscard]] auto HirScope() const -> const hir::StructuralScope& {
    return *hir_scope_;
  }

  // Resolve a subroutine reference to its HIR declaration by walking `hops`
  // scopes outward. The HIR declaration is complete before any body is lowered,
  // so a call can read a peer's formals even when the peer's MIR declaration is
  // not yet built (forward / mutual reference, LRM 13.7). The desugar reads the
  // formals' directions and types from here.
  [[nodiscard]] auto LookupHirSubroutine(
      hir::StructuralHops hops, hir::StructuralSubroutineId id) const
      -> const hir::StructuralSubroutineDecl& {
    if (hops.value == 0) {
      return hir_scope_->structural_subroutines.at(id.value);
    }
    if (parent_ == nullptr) {
      throw InternalError(
          "StructuralScopeLowerer::LookupHirSubroutine: hops walk ran "
          "past the root scope");
    }
    return parent_->LookupHirSubroutine(
        hir::StructuralHops{.value = hops.value - 1}, id);
  }

  // The MIR target each HIR cross-unit ref slot lowers to, in HIR slot order: a
  // `StructuralVarRef` to the slot member -- a synthesized ExternalRef member
  // for an upward ref, a borrowed-pointer slot for a downward ref -- bundled
  // with the slot's MIR type so a body reader can decide whether the read
  // dereferences without re-touching the structural scope.
  struct CrossUnitRefMeta {
    mir::SensitivityRef target;
    mir::TypeId slot_type;
  };

  void AddCrossUnitRefTarget(
      mir::SensitivityRef target, mir::TypeId slot_type) {
    cross_unit_ref_targets_.push_back(
        CrossUnitRefMeta{.target = std::move(target), .slot_type = slot_type});
  }

  [[nodiscard]] auto CrossUnitRefTarget(hir::CrossUnitRefId hir_id) const
      -> const CrossUnitRefMeta& {
    return cross_unit_ref_targets_.at(hir_id.value);
  }

  void MapStructuralVar(
      hir::StructuralVarId hir_id, mir::StructuralVarId mir_id) {
    if (hir_id.value != structural_var_map_.size()) {
      throw InternalError(
          "StructuralScopeLowerer::MapStructuralVar: HIR structural "
          "vars must be mapped in HIR id order");
    }
    structural_var_map_.push_back(mir_id);
  }

  [[nodiscard]] auto TranslateStructuralVar(
      hir::StructuralHops hops, hir::StructuralVarId hir_id) const
      -> mir::StructuralVarRef {
    const mir::StructuralVarId mir_id = LookupStructuralVarAtHops(hops, hir_id);
    return mir::StructuralVarRef{
        .hops = mir::StructuralHops{.value = hops.value}, .var = mir_id};
  }

  void MapLoopVarAsStructuralParam(
      hir::LoopVarDeclId hir_id, mir::StructuralParamId mir_id) {
    if (hir_id.value >= structural_param_map_.size()) {
      structural_param_map_.resize(hir_id.value + 1);
    }
    if (structural_param_map_[hir_id.value].has_value()) {
      throw InternalError(
          "StructuralScopeLowerer::MapLoopVarAsStructuralParam: HIR "
          "loop var already mapped");
    }
    structural_param_map_[hir_id.value] = mir_id;
  }

  // For-generate header procedural-var registry (LRM 27.5). HIR places the
  // `LoopVarDecl` in the for-generate's parent scope; the constructor body
  // lowering binds it to a procedural-var ref via this table so header
  // expressions (init / stop / iter) resolve to the constructor induction var.
  // The child-owned `StructuralParamDecl` path is separate; see
  // `MapLoopVarAsStructuralParam`.
  void MapLoopVarAsProcedural(
      hir::LoopVarDeclId hir_id, mir::ProceduralVarRef ref) {
    if (hir_id.value >= loop_var_procedural_map_.size()) {
      loop_var_procedural_map_.resize(hir_id.value + 1);
    }
    if (loop_var_procedural_map_[hir_id.value].has_value()) {
      throw InternalError(
          "StructuralScopeLowerer::MapLoopVarAsProcedural: HIR loop var "
          "already mapped");
    }
    loop_var_procedural_map_[hir_id.value] = ref;
  }
  [[nodiscard]] auto TranslateLoopVarAsProcedural(
      hir::LoopVarDeclId hir_id) const -> mir::ProceduralVarRef {
    if (hir_id.value >= loop_var_procedural_map_.size() ||
        !loop_var_procedural_map_[hir_id.value].has_value()) {
      throw InternalError(
          "StructuralScopeLowerer::TranslateLoopVarAsProcedural: unmapped "
          "HIR loop var");
    }
    return *loop_var_procedural_map_[hir_id.value];
  }

  // HIR places `LoopVarDecl` in the for-generate's parent scope; MIR re-homes
  // the same value into the constructed child scope. MIR hops = HIR hops - 1.
  [[nodiscard]] auto TranslateLoopVarAsStructuralParam(
      hir::StructuralHops hir_hops, hir::LoopVarDeclId hir_id) const
      -> mir::StructuralParamRef {
    if (hir_hops.value == 0) {
      throw InternalError(
          "StructuralScopeLowerer::TranslateLoopVarAsStructuralParam: "
          "HIR hops=0 cannot resolve to a structural param (the param is "
          "child-owned in MIR; header expressions must use the procedural "
          "induction var path)");
    }
    const std::uint32_t mir_hops = hir_hops.value - 1;
    const mir::StructuralParamId mir_id = LookupStructuralParamAtMirHops(
        mir::StructuralHops{.value = mir_hops}, hir_id);
    return mir::StructuralParamRef{
        .hops = mir::StructuralHops{.value = mir_hops}, .param = mir_id};
  }

  void MapStructuralSubroutine(
      hir::StructuralSubroutineId hir_id, mir::StructuralSubroutineId mir_id) {
    if (hir_id.value != structural_subroutine_map_.size()) {
      throw InternalError(
          "StructuralScopeLowerer::MapStructuralSubroutine: HIR "
          "structural subroutines must be mapped in HIR id order");
    }
    structural_subroutine_map_.push_back(mir_id);
  }

  [[nodiscard]] auto TranslateStructuralSubroutine(
      hir::StructuralHops hops, hir::StructuralSubroutineId hir_id) const
      -> mir::StructuralSubroutineRef {
    const mir::StructuralSubroutineId mir_id =
        LookupStructuralSubroutineAtHops(hops, hir_id);
    return mir::StructuralSubroutineRef{
        .hops = mir::StructuralHops{.value = hops.value}, .subroutine = mir_id};
  }

 private:
  [[nodiscard]] auto LookupStructuralVarAtHops(
      hir::StructuralHops hops, hir::StructuralVarId hir_id) const
      -> mir::StructuralVarId {
    if (hops.value == 0) {
      if (hir_id.value >= structural_var_map_.size()) {
        throw InternalError(
            "StructuralScopeLowerer::TranslateStructuralVar: unmapped "
            "HIR structural var");
      }
      return structural_var_map_[hir_id.value];
    }
    if (parent_ == nullptr) {
      throw InternalError(
          "StructuralScopeLowerer::TranslateStructuralVar: hops out "
          "of scope chain");
    }
    return parent_->LookupStructuralVarAtHops(
        hir::StructuralHops{hops.value - 1}, hir_id);
  }

  [[nodiscard]] auto LookupStructuralParamAtMirHops(
      mir::StructuralHops mir_hops, hir::LoopVarDeclId hir_id) const
      -> mir::StructuralParamId {
    if (mir_hops.value == 0) {
      if (hir_id.value >= structural_param_map_.size() ||
          !structural_param_map_[hir_id.value].has_value()) {
        throw InternalError(
            "StructuralScopeLowerer::TranslateLoopVarAsStructuralParam: "
            "unmapped HIR loop var");
      }
      return *structural_param_map_[hir_id.value];
    }
    if (parent_ == nullptr) {
      throw InternalError(
          "StructuralScopeLowerer::TranslateLoopVarAsStructuralParam: "
          "hops exceed scope chain depth");
    }
    return parent_->LookupStructuralParamAtMirHops(
        mir::StructuralHops{.value = mir_hops.value - 1}, hir_id);
  }

  [[nodiscard]] auto LookupStructuralSubroutineAtHops(
      hir::StructuralHops hops, hir::StructuralSubroutineId hir_id) const
      -> mir::StructuralSubroutineId {
    if (hops.value == 0) {
      if (hir_id.value >= structural_subroutine_map_.size()) {
        throw InternalError(
            "StructuralScopeLowerer::TranslateStructuralSubroutine: "
            "unmapped HIR subroutine");
      }
      return structural_subroutine_map_[hir_id.value];
    }
    if (parent_ == nullptr) {
      throw InternalError(
          "StructuralScopeLowerer::TranslateStructuralSubroutine: hops "
          "exceed scope chain depth");
    }
    return parent_->LookupStructuralSubroutineAtHops(
        hir::StructuralHops{hops.value - 1}, hir_id);
  }

  ModuleLowerer* module_;
  const StructuralScopeLowerer* parent_;
  std::string name_;
  const hir::StructuralScope* hir_scope_;
  std::vector<mir::StructuralVarId> structural_var_map_;
  std::vector<std::optional<mir::StructuralParamId>> structural_param_map_;
  std::vector<mir::StructuralSubroutineId> structural_subroutine_map_;
  std::vector<CrossUnitRefMeta> cross_unit_ref_targets_;
  std::vector<std::optional<mir::ProceduralVarRef>> loop_var_procedural_map_;
};

}  // namespace lyra::lowering::hir_to_mir
