#pragma once

#include <memory>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/loop_var.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/structural_hops.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/enclosing_hops.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/member.hpp"
#include "lyra/mir/param.hpp"

namespace lyra::lowering::hir_to_mir {

struct ChildStructuralScopeBinding {
  mir::ClassId scope_id;
  mir::MemberId var_id;
};

// The owned-child-scope bindings for a single `hir::Generate`, indexed by
// `hir::StructuralScopeId.value`. A transient table threaded through generate
// installation during constructor lowering, not stored on the pass object.
struct GenerateBindings {
  std::vector<ChildStructuralScopeBinding> by_scope_id;
};

// One structural parameter the child scope receives at construction. The
// `param` is the MIR decl installed on the child scope; `source_loop_var`
// records the HIR loop variable (if any) that maps to it. Generate-for child
// scopes use this today.
struct ScopeEntryStructuralParamBinding {
  mir::ParamDecl param;
  hir::LoopVarDeclId source_loop_var = {};
};

// A located structural param: the enclosing-scope distance to the scope that
// owns it and the param's id on that scope. The reading site turns it into a
// receiver-addressed `mir::ParamRef`.
struct StructuralParamLocation {
  mir::EnclosingHops hops = {};
  mir::ParamId param = {};
};

// The MIR slot a HIR cross-unit ref resolves to: a member ref to the slot
// bundled with the slot's MIR type, so a body reader decides whether the read
// dereferences without re-touching the owning scope.
struct CrossUnitRefMeta {
  mir::MemberRef target = {};
  mir::TypeId slot_type = {};
};

// Lowers one HIR structural scope into one MIR class across two ordered
// phases. Carries the scope-local HIR-to-MIR identity mappings populated in
// `DeclareShape` and consulted in `PopulateBodies`; borrows the enclosing
// scope's lowerer through `parent_` for hops-walked cross-scope lookups; and
// owns descendant scope lowerers in `children_` so the structural-scope tree
// stays alive across the phase boundary. The lowerer itself holds no
// pointer to the in-progress `mir::Class`: body handlers reach it through
// `frame.current_class`.
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

  // Mints this class's identity, builds its structural shape, publishes the
  // shape so peer body lowering can query it, and recurses to declare every
  // descendant scope's shape. `entry_bindings` are the structural params
  // injected by an enclosing for-generate iteration.
  auto DeclareShape(
      std::span<const ScopeEntryStructuralParamBinding> entry_bindings = {})
      -> diag::Result<mir::ClassId>;

  // Lowers every body and every install statement against the already-
  // published shape, recurses into descendants, and commits the composed
  // class to the compilation unit. `parent_frame` carries the
  // enclosing-class chain this scope's bodies thread through; the root call
  // receives a default `WalkFrame`.
  auto PopulateBodies(WalkFrame parent_frame) -> diag::Result<void>;

  // Central scope-level expression dispatcher. One switch over `hir::Expr::
  // data` routing each kind to the per-family handler in `expression/*.cpp`.
  [[nodiscard]] auto LowerExpr(const hir::Expr& expr, WalkFrame frame) const
      -> diag::Result<mir::Expr>;

  // LHS-context expression dispatcher: addressable kinds only, no auto-Get
  // wrap.
  [[nodiscard]] auto LowerLhsExpr(const hir::Expr& expr, WalkFrame frame) const
      -> diag::Result<mir::Expr>;

  [[nodiscard]] auto Module() const -> ModuleLowerer& {
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

  // The expression arena of the scope being lowered. The uniform sub-expression
  // accessor the context-free expression handler templates reach through; both
  // lowering pass classes expose it with the same shape so those templates bind
  // to either.
  [[nodiscard]] auto HirExprs() const
      -> const base::Arena<hir::Expr, hir::ExprId>& {
    return hir_scope_->exprs;
  }

  // Resolve a subroutine reference to its HIR declaration by walking `hops`
  // scopes outward. The HIR declaration is complete before any body is lowered,
  // so a call can read a peer's formals even when the peer's MIR declaration is
  // not yet built (forward / mutual reference, LRM 13.7). The desugar reads the
  // formals' directions and types from here.
  [[nodiscard]] auto LookupHirSubroutine(
      hir::StructuralHops hops, hir::StructuralSubroutineId id) const
      -> const hir::SubroutineDecl& {
    if (hops.value == 0) {
      return hir_scope_->structural_subroutines.Get(id);
    }
    if (parent_ == nullptr) {
      throw InternalError(
          "StructuralScopeLowerer::LookupHirSubroutine: hops walk ran "
          "past the root scope");
    }
    return parent_->LookupHirSubroutine(
        hir::StructuralHops{.value = hops.value - 1}, id);
  }

  void AddCrossUnitRefTarget(mir::MemberRef target, mir::TypeId slot_type) {
    cross_unit_ref_targets_.push_back(
        CrossUnitRefMeta{.target = std::move(target), .slot_type = slot_type});
  }

  [[nodiscard]] auto CrossUnitRefTarget(hir::CrossUnitRefId hir_id) const
      -> const CrossUnitRefMeta& {
    return cross_unit_ref_targets_.at(hir_id.value);
  }

  void MapStructuralDataObject(
      hir::StructuralDataObjectId hir_id, mir::MemberId mir_id) {
    if (hir_id.value != structural_data_object_map_.size()) {
      throw InternalError(
          "StructuralScopeLowerer::MapStructuralDataObject: HIR structural "
          "data objects must be mapped in HIR id order");
    }
    structural_data_object_map_.push_back(mir_id);
  }

  [[nodiscard]] auto TranslateStructuralDataObject(
      hir::StructuralHops hops, hir::StructuralDataObjectId hir_id) const
      -> mir::MemberId {
    return LookupStructuralDataObjectAtHops(hops, hir_id);
  }

  void MapLoopVarAsStructuralParam(
      hir::LoopVarDeclId hir_id, mir::ParamId mir_id) {
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
  void MapLoopVarAsProcedural(hir::LoopVarDeclId hir_id, mir::LocalRef ref) {
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
      hir::LoopVarDeclId hir_id) const -> mir::LocalRef {
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
  // The caller turns the located (hops, param) into a receiver-addressed read
  // via `BuildStructuralParamAccessExpr`.
  [[nodiscard]] auto TranslateLoopVarAsStructuralParam(
      hir::StructuralHops hir_hops, hir::LoopVarDeclId hir_id) const
      -> StructuralParamLocation {
    if (hir_hops.value == 0) {
      throw InternalError(
          "StructuralScopeLowerer::TranslateLoopVarAsStructuralParam: "
          "HIR hops=0 cannot resolve to a structural param (the param is "
          "child-owned in MIR; header expressions must use the procedural "
          "induction var path)");
    }
    const mir::EnclosingHops mir_hops{.value = hir_hops.value - 1};
    return StructuralParamLocation{
        .hops = mir_hops,
        .param = LookupStructuralParamAtMirHops(mir_hops, hir_id)};
  }

  // Records the MIR slot a HIR owned-child reference resolves to. An instance
  // member maps to one `mir::MemberId`; a generate maps to a per-arm table
  // (`GenerateBindings`) so that a `GenerateChildRef` picks the arm's member.
  // Callers register in HIR-id order during scope materialization; a nested
  // scope reads them back through the parent chain when it needs to resolve
  // a head that lives in an enclosing scope.
  void MapInstanceMember(hir::InstanceMemberId hir_id, mir::MemberId mir_id) {
    if (hir_id.value != instance_member_map_.size()) {
      throw InternalError(
          "StructuralScopeLowerer::MapInstanceMember: HIR instance members "
          "must be "
          "mapped in HIR id order");
    }
    instance_member_map_.push_back(mir_id);
  }

  [[nodiscard]] auto TranslateInstanceMember(hir::InstanceMemberId hir_id) const
      -> mir::MemberId {
    if (hir_id.value >= instance_member_map_.size()) {
      throw InternalError(
          "StructuralScopeLowerer::TranslateInstanceMember: unmapped HIR "
          "instance member");
    }
    return instance_member_map_[hir_id.value];
  }

  void MapGenerate(hir::GenerateId hir_id, GenerateBindings bindings) {
    if (hir_id.value != generate_map_.size()) {
      throw InternalError(
          "StructuralScopeLowerer::MapGenerate: HIR generates must be mapped "
          "in HIR id "
          "order");
    }
    generate_map_.push_back(std::move(bindings));
  }

  [[nodiscard]] auto LookupGenerateBindings(hir::GenerateId hir_id) const
      -> const GenerateBindings& {
    if (hir_id.value >= generate_map_.size()) {
      throw InternalError(
          "StructuralScopeLowerer::LookupGenerateBindings: unmapped HIR "
          "generate");
    }
    return generate_map_[hir_id.value];
  }

  // Resolves an owned-child reference (the `child` field of a
  // `hir::DownwardHead`) to the MIR member that owns the child. `hops == 0`
  // reads this scope's own table; `hops > 0` walks the parent chain to an
  // enclosing scope, used by the sibling-of-ancestor install when the head
  // lives outside the referrer's frame.
  [[nodiscard]] auto TranslateOwnedChild(
      hir::StructuralHops hops,
      const std::variant<hir::InstanceMemberId, hir::GenerateChildRef>& child)
      const -> mir::MemberId {
    return LookupOwnedChildAtHops(hops, child);
  }

  void MapStructuralSubroutine(
      hir::StructuralSubroutineId hir_id, mir::MethodId mir_id) {
    if (hir_id.value != structural_subroutine_map_.size()) {
      throw InternalError(
          "StructuralScopeLowerer::MapStructuralSubroutine: HIR "
          "structural subroutines must be mapped in HIR id order");
    }
    structural_subroutine_map_.push_back(mir_id);
  }

  [[nodiscard]] auto TranslateStructuralSubroutine(
      hir::StructuralHops hops, hir::StructuralSubroutineId hir_id) const
      -> mir::Direct {
    const mir::MethodId mir_id = LookupStructuralSubroutineAtHops(hops, hir_id);
    return mir::Direct{.target = mir_id};
  }

 private:
  [[nodiscard]] auto LookupOwnedChildAtHops(
      hir::StructuralHops hops,
      const std::variant<hir::InstanceMemberId, hir::GenerateChildRef>& child)
      const -> mir::MemberId {
    if (hops.value == 0) {
      return std::visit(
          Overloaded{
              [&](const hir::InstanceMemberId& id) -> mir::MemberId {
                if (id.value >= instance_member_map_.size()) {
                  throw InternalError(
                      "StructuralScopeLowerer::TranslateOwnedChild: unmapped "
                      "HIR "
                      "instance member");
                }
                return instance_member_map_[id.value];
              },
              [&](const hir::GenerateChildRef& g) -> mir::MemberId {
                if (g.generate.value >= generate_map_.size()) {
                  throw InternalError(
                      "StructuralScopeLowerer::TranslateOwnedChild: unmapped "
                      "HIR "
                      "generate");
                }
                return generate_map_[g.generate.value]
                    .by_scope_id.at(g.scope.value)
                    .var_id;
              },
          },
          child);
    }
    if (parent_ == nullptr) {
      throw InternalError(
          "StructuralScopeLowerer::TranslateOwnedChild: hops exceed scope "
          "chain depth");
    }
    return parent_->LookupOwnedChildAtHops(
        hir::StructuralHops{hops.value - 1}, child);
  }

  [[nodiscard]] auto LookupStructuralDataObjectAtHops(
      hir::StructuralHops hops, hir::StructuralDataObjectId hir_id) const
      -> mir::MemberId {
    if (hops.value == 0) {
      if (hir_id.value >= structural_data_object_map_.size()) {
        throw InternalError(
            "StructuralScopeLowerer::LookupStructuralDataObjectAtHops: "
            "unmapped "
            "HIR structural data object");
      }
      return structural_data_object_map_[hir_id.value];
    }
    if (parent_ == nullptr) {
      throw InternalError(
          "StructuralScopeLowerer::TranslateStructuralDataObject: hops out "
          "of scope chain");
    }
    return parent_->LookupStructuralDataObjectAtHops(
        hir::StructuralHops{hops.value - 1}, hir_id);
  }

  [[nodiscard]] auto LookupStructuralParamAtMirHops(
      mir::EnclosingHops mir_hops, hir::LoopVarDeclId hir_id) const
      -> mir::ParamId {
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
        mir::EnclosingHops{.value = mir_hops.value - 1}, hir_id);
  }

  [[nodiscard]] auto LookupStructuralSubroutineAtHops(
      hir::StructuralHops hops, hir::StructuralSubroutineId hir_id) const
      -> mir::MethodId {
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
  std::vector<mir::MemberId> structural_data_object_map_;
  std::vector<std::optional<mir::ParamId>> structural_param_map_;
  std::vector<mir::MethodId> structural_subroutine_map_;
  std::vector<CrossUnitRefMeta> cross_unit_ref_targets_;
  std::vector<std::optional<mir::LocalRef>> loop_var_procedural_map_;
  std::vector<mir::MemberId> instance_member_map_;
  std::vector<GenerateBindings> generate_map_;
  mir::ClassId class_id_{};
  std::vector<std::unique_ptr<StructuralScopeLowerer>> children_;
};

}  // namespace lyra::lowering::hir_to_mir
