#pragma once

#include <cstdint>
#include <cstdlib>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/time.hpp"
#include "lyra/hir/loop_var.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/structural_hops.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/structural_var.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/procedural_var.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/structural_hops.hpp"
#include "lyra/mir/structural_param.hpp"
#include "lyra/mir/structural_scope.hpp"
#include "lyra/mir/structural_scope_id.hpp"
#include "lyra/mir/structural_var.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

struct BuiltinMirTypes {
  mir::TypeId int32;
  mir::TypeId bit1;
  mir::TypeId string;
  mir::TypeId void_type;
  mir::TypeId realtime;
};

class UnitLoweringState {
 public:
  explicit UnitLoweringState(mir::CompilationUnit& unit) : unit_(&unit) {
    builtins_ = BuiltinMirTypes{
        .int32 = AddType(
            mir::TypeData{mir::PackedArrayType{
                .atom = mir::BitAtom::kBit,
                .signedness = mir::Signedness::kSigned,
                .dims = {mir::PackedRange{.left = 31, .right = 0}},
                .form = mir::PackedArrayForm::kInt}}),
        .bit1 = AddType(
            mir::TypeData{mir::PackedArrayType{
                .atom = mir::BitAtom::kBit,
                .signedness = mir::Signedness::kUnsigned,
                .dims = {mir::PackedRange{.left = 0, .right = 0}},
                .form = mir::PackedArrayForm::kExplicit}}),
        .string = AddType(mir::TypeData{mir::StringType{}}),
        .void_type = AddType(mir::TypeData{mir::VoidType{}}),
        .realtime = AddType(mir::TypeData{mir::RealTimeType{}})};
  }

  [[nodiscard]] auto Unit() const -> const mir::CompilationUnit& {
    return *unit_;
  }

  auto AddType(mir::TypeData data) -> mir::TypeId {
    const mir::TypeId id{static_cast<std::uint32_t>(unit_->types.size())};
    unit_->types.push_back(mir::Type{.data = std::move(data)});
    return id;
  }

  [[nodiscard]] auto GetType(mir::TypeId id) const -> const mir::Type& {
    return unit_->GetType(id);
  }

  [[nodiscard]] auto RootStructuralScope() -> mir::StructuralScope& {
    return unit_->structural_scope;
  }

  [[nodiscard]] auto TranslateType(hir::TypeId hir_id) const -> mir::TypeId {
    if (hir_id.value >= type_map_.size()) {
      throw InternalError(
          "UnitLoweringState::TranslateType: unmapped HIR type");
    }
    return type_map_[hir_id.value];
  }

  void MapType(hir::TypeId hir_id, mir::TypeId mir_id) {
    if (hir_id.value >= type_map_.size()) {
      type_map_.resize(hir_id.value + 1);
    }
    type_map_[hir_id.value] = mir_id;
  }

  [[nodiscard]] auto Builtins() const -> const BuiltinMirTypes& {
    return builtins_;
  }

 private:
  mir::CompilationUnit* unit_;
  std::vector<mir::TypeId> type_map_;
  BuiltinMirTypes builtins_{};
};

struct ChildStructuralScopeBinding {
  mir::StructuralScopeId scope_id;
  mir::StructuralVarId var_id;
};

// Bindings produced by `InstallGenerateOwnedChildScopes` for a single
// `hir::Generate`. Indexed by `hir::StructuralScopeId.value`. Phase-local to
// constructor lowering: `LowerConstructorBody` consumes them and they then
// go away. They are not part of `StructuralScopeLoweringState`.
struct GenerateBindings {
  std::vector<ChildStructuralScopeBinding> by_scope_id;
};

// Per-scope lowering state for one mir::StructuralScope. Owns scope-level
// mutation during lowering AND owns scope-local HIR-to-MIR translation maps.
// Scope declaration and scope-local name translation form one lowering unit.
class StructuralScopeLoweringState {
 public:
  StructuralScopeLoweringState(
      const StructuralScopeLoweringState* parent, mir::StructuralScope& scope)
      : parent_(parent), scope_(&scope) {
  }

  [[nodiscard]] auto Parent() const -> const StructuralScopeLoweringState* {
    return parent_;
  }

  [[nodiscard]] auto Scope() const -> const mir::StructuralScope& {
    return *scope_;
  }

  auto AddStructuralVar(mir::StructuralVarDecl var) -> mir::StructuralVarId {
    const mir::StructuralVarId id{
        static_cast<std::uint32_t>(scope_->structural_vars.size())};
    scope_->structural_vars.push_back(std::move(var));
    return id;
  }

  auto AddStructuralParam(mir::StructuralParamDecl param)
      -> mir::StructuralParamId {
    const mir::StructuralParamId id{
        static_cast<std::uint32_t>(scope_->structural_params.size())};
    scope_->structural_params.push_back(std::move(param));
    return id;
  }

  auto AddChildStructuralScope(mir::StructuralScope child)
      -> mir::StructuralScopeId {
    const mir::StructuralScopeId id{
        static_cast<std::uint32_t>(scope_->child_structural_scopes.size())};
    scope_->child_structural_scopes.push_back(std::move(child));
    return id;
  }

  auto AddProcess(mir::Process process) -> mir::ProcessId {
    const mir::ProcessId id{
        static_cast<std::uint32_t>(scope_->processes.size())};
    scope_->processes.push_back(std::move(process));
    return id;
  }

  auto AddStructuralSubroutine(mir::StructuralSubroutineDecl decl)
      -> mir::StructuralSubroutineId {
    const mir::StructuralSubroutineId id{
        static_cast<std::uint32_t>(scope_->structural_subroutines.size())};
    scope_->structural_subroutines.push_back(std::move(decl));
    return id;
  }

  void SetConstructorScope(mir::ProceduralScope ctor) {
    scope_->constructor_scope = std::move(ctor);
  }

  void MapStructuralVar(
      hir::StructuralVarId hir_id, mir::StructuralVarId mir_id) {
    if (hir_id.value >= structural_var_map_.size()) {
      structural_var_map_.resize(hir_id.value + 1);
    }
    if (structural_var_map_[hir_id.value].has_value()) {
      throw InternalError(
          "StructuralScopeLoweringState::MapStructuralVar: HIR structural var "
          "already mapped");
    }
    structural_var_map_[hir_id.value] = mir_id;
  }

  // Walk the scope chain `hops` levels outward and look up `hir_id` in the
  // owning scope's structural-var map. Returns a MIR ref that preserves the
  // hops, so the renderer/dumper can traverse the chain at emit/dump time.
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
          "StructuralScopeLoweringState::MapLoopVarAsStructuralParam: HIR "
          "loop var already mapped");
    }
    structural_param_map_[hir_id.value] = mir_id;
  }

  // HIR places `LoopVarDecl` in the for-generate's parent scope; MIR re-homes
  // the same value into the constructed child scope (every constructed object
  // owns its own copy as a `StructuralParamDecl`). MIR hops = HIR hops - 1.
  // HIR hops = 0 would mean the decl lives in the current HIR scope, but in
  // MIR that param is owned by a child scope and is not reachable by walking
  // up. Header expressions (init/stop/iter) hit that case and must use the
  // ConstructorLoweringState (ProceduralVarRef) path instead.
  [[nodiscard]] auto TranslateLoopVarAsStructuralParam(
      hir::StructuralHops hir_hops, hir::LoopVarDeclId hir_id) const
      -> mir::StructuralParamRef {
    if (hir_hops.value == 0) {
      throw InternalError(
          "StructuralScopeLoweringState::TranslateLoopVarAsStructuralParam: "
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
    if (hir_id.value >= structural_subroutine_map_.size()) {
      structural_subroutine_map_.resize(hir_id.value + 1);
    }
    if (structural_subroutine_map_[hir_id.value].has_value()) {
      throw InternalError(
          "StructuralScopeLoweringState::MapStructuralSubroutine: HIR "
          "structural subroutine already mapped");
    }
    structural_subroutine_map_[hir_id.value] = mir_id;
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
      if (hir_id.value >= structural_var_map_.size() ||
          !structural_var_map_[hir_id.value].has_value()) {
        throw InternalError(
            "StructuralScopeLoweringState::TranslateStructuralVar: unmapped "
            "HIR structural var");
      }
      return *structural_var_map_[hir_id.value];
    }
    if (parent_ == nullptr) {
      throw InternalError(
          "StructuralScopeLoweringState::TranslateStructuralVar: hops out "
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
            "StructuralScopeLoweringState::TranslateLoopVarAsStructuralParam: "
            "unmapped HIR loop var");
      }
      return *structural_param_map_[hir_id.value];
    }
    if (parent_ == nullptr) {
      throw InternalError(
          "StructuralScopeLoweringState::TranslateLoopVarAsStructuralParam: "
          "hops exceed scope chain depth");
    }
    return parent_->LookupStructuralParamAtMirHops(
        mir::StructuralHops{.value = mir_hops.value - 1}, hir_id);
  }

  [[nodiscard]] auto LookupStructuralSubroutineAtHops(
      hir::StructuralHops hops, hir::StructuralSubroutineId hir_id) const
      -> mir::StructuralSubroutineId {
    if (hops.value == 0) {
      if (hir_id.value >= structural_subroutine_map_.size() ||
          !structural_subroutine_map_[hir_id.value].has_value()) {
        throw InternalError(
            "StructuralScopeLoweringState::TranslateStructuralSubroutine: "
            "unmapped HIR subroutine");
      }
      return *structural_subroutine_map_[hir_id.value];
    }
    if (parent_ == nullptr) {
      throw InternalError(
          "StructuralScopeLoweringState::TranslateStructuralSubroutine: hops "
          "exceed scope chain depth");
    }
    return parent_->LookupStructuralSubroutineAtHops(
        hir::StructuralHops{hops.value - 1}, hir_id);
  }

  const StructuralScopeLoweringState* parent_;
  mir::StructuralScope* scope_;
  std::vector<std::optional<mir::StructuralVarId>> structural_var_map_;
  std::vector<std::optional<mir::StructuralParamId>> structural_param_map_;
  std::vector<std::optional<mir::StructuralSubroutineId>>
      structural_subroutine_map_;
};

struct ProceduralVarBinding {
  std::uint32_t declaration_procedural_depth;
  mir::ProceduralVarId var;
};

class ProcessLoweringState {
 public:
  explicit ProcessLoweringState(TimeResolution time_resolution)
      : time_resolution_(time_resolution) {
  }

  [[nodiscard]] auto Resolution() const -> TimeResolution {
    return time_resolution_;
  }

  void EnterProceduralScope() {
    ++procedural_depth_;
  }
  void LeaveProceduralScope() {
    if (procedural_depth_ == 0) {
      throw InternalError(
          "ProcessLoweringState::LeaveProceduralScope: depth underflow");
    }
    --procedural_depth_;
  }
  [[nodiscard]] auto CurrentProceduralDepth() const -> std::uint32_t {
    return procedural_depth_;
  }

  void MapProceduralVar(
      hir::ProceduralVarId hir_id, ProceduralVarBinding binding) {
    if (hir_id.value >= bindings_.size()) {
      bindings_.resize(hir_id.value + 1);
    }
    if (bindings_[hir_id.value].has_value()) {
      throw InternalError(
          "ProcessLoweringState::MapProceduralVar: HIR ProceduralVarId "
          "already mapped (duplicate VarDeclStmt for the same declaration)");
    }
    bindings_[hir_id.value] = binding;
  }

  [[nodiscard]] auto LookupProceduralVar(hir::ProceduralVarId hir_id) const
      -> const ProceduralVarBinding& {
    if (hir_id.value >= bindings_.size() ||
        !bindings_[hir_id.value].has_value()) {
      throw InternalError(
          "ProcessLoweringState::LookupProceduralVar: unmapped HIR "
          "procedural var");
    }
    return *bindings_[hir_id.value];
  }

  [[nodiscard]] auto TranslateProceduralVar(hir::ProceduralVarId hir_id) const
      -> mir::ProceduralVarRef {
    const auto& binding = LookupProceduralVar(hir_id);
    if (binding.declaration_procedural_depth > procedural_depth_) {
      throw InternalError(
          "ProcessLoweringState::TranslateProceduralVar: declaration depth "
          "exceeds current depth (forward reference into a child scope)");
    }
    return mir::ProceduralVarRef{
        .hops =
            mir::ProceduralHops{
                .value =
                    procedural_depth_ - binding.declaration_procedural_depth},
        .var = binding.var,
    };
  }

 private:
  TimeResolution time_resolution_;
  std::uint32_t procedural_depth_ = 0;
  std::vector<std::optional<ProceduralVarBinding>> bindings_;
};

class ProceduralDepthGuard {
 public:
  explicit ProceduralDepthGuard(ProcessLoweringState& state) : state_(&state) {
    state_->EnterProceduralScope();
  }

  ~ProceduralDepthGuard() {
    state_->LeaveProceduralScope();
  }

  ProceduralDepthGuard(const ProceduralDepthGuard&) = delete;
  auto operator=(const ProceduralDepthGuard&) -> ProceduralDepthGuard& = delete;
  ProceduralDepthGuard(ProceduralDepthGuard&&) = delete;
  auto operator=(ProceduralDepthGuard&&) -> ProceduralDepthGuard& = delete;

 private:
  ProcessLoweringState* state_;
};

class ConstructorLoweringState {
 public:
  void MapLoopVar(hir::LoopVarDeclId hir_id, mir::ProceduralVarRef ref) {
    if (hir_id.value >= map_.size()) {
      map_.resize(hir_id.value + 1);
    }
    map_[hir_id.value] = ref;
  }

  [[nodiscard]] auto TranslateLoopVar(hir::LoopVarDeclId hir_id) const
      -> mir::ProceduralVarRef {
    if (hir_id.value >= map_.size()) {
      throw InternalError("TranslateLoopVar: unmapped HIR loop var");
    }
    return map_[hir_id.value];
  }

 private:
  std::vector<mir::ProceduralVarRef> map_;
};

class ProceduralScopeLoweringState {
 public:
  ProceduralScopeLoweringState() = default;

  auto AddProceduralVar(mir::ProceduralVarDecl var) -> mir::ProceduralVarId {
    const mir::ProceduralVarId id{
        static_cast<std::uint32_t>(scope_.vars.size())};
    scope_.vars.push_back(std::move(var));
    return id;
  }

  [[nodiscard]] auto GetProceduralVar(mir::ProceduralVarId id) const
      -> const mir::ProceduralVarDecl& {
    return scope_.vars.at(id.value);
  }

  auto AddExpr(mir::Expr expr) -> mir::ExprId {
    const mir::ExprId id{static_cast<std::uint32_t>(scope_.exprs.size())};
    scope_.exprs.push_back(std::move(expr));
    return id;
  }

  [[nodiscard]] auto GetExpr(mir::ExprId id) const -> const mir::Expr& {
    return scope_.exprs.at(id.value);
  }

  [[nodiscard]] auto Scope() const -> const mir::ProceduralScope& {
    return scope_;
  }

  auto AddStmt(mir::Stmt stmt) -> mir::StmtId {
    const mir::StmtId id{static_cast<std::uint32_t>(scope_.stmts.size())};
    scope_.stmts.push_back(std::move(stmt));
    return id;
  }

  void AddRootStmt(mir::StmtId id) {
    scope_.root_stmts.push_back(id);
  }

  auto Finish() -> mir::ProceduralScope {
    return std::move(scope_);
  }

 private:
  mir::ProceduralScope scope_;
};

class ScopeStack {
 public:
  auto Push(const hir::StructuralScope& scope) -> void {
    frames_.push_back(&scope);
  }

  auto Pop(const hir::StructuralScope& expected) noexcept -> void {
    if (frames_.empty() || frames_.back() != &expected) {
      std::abort();
    }
    frames_.pop_back();
  }

  [[nodiscard]] auto Resolve(hir::StructuralHops hops) const
      -> const hir::StructuralScope& {
    if (hops.value >= frames_.size()) {
      throw InternalError("ScopeStack::Resolve: hops out of range");
    }
    return *frames_[frames_.size() - 1 - hops.value];
  }

  [[nodiscard]] auto Depth() const -> std::uint32_t {
    return static_cast<std::uint32_t>(frames_.size());
  }

 private:
  std::vector<const hir::StructuralScope*> frames_;
};

class ScopeStackGuard {
 public:
  ScopeStackGuard(ScopeStack& stack, const hir::StructuralScope& scope)
      : stack_(&stack), scope_(&scope) {
    stack_->Push(scope);
  }

  ~ScopeStackGuard() noexcept {
    stack_->Pop(*scope_);
  }

  ScopeStackGuard(const ScopeStackGuard&) = delete;
  auto operator=(const ScopeStackGuard&) -> ScopeStackGuard& = delete;
  ScopeStackGuard(ScopeStackGuard&&) = delete;
  auto operator=(ScopeStackGuard&&) -> ScopeStackGuard& = delete;

 private:
  ScopeStack* stack_;
  const hir::StructuralScope* scope_;
};

}  // namespace lyra::lowering::hir_to_mir
