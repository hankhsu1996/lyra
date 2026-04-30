#pragma once

#include <cstdint>
#include <cstdlib>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/time.hpp"
#include "lyra/hir/local_var.hpp"
#include "lyra/hir/loop_var.hpp"
#include "lyra/hir/member_var.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/mir/class_decl.hpp"
#include "lyra/mir/class_decl_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local_var.hpp"
#include "lyra/mir/member_var.hpp"
#include "lyra/mir/stmt.hpp"
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

  auto AddClass(mir::ClassDecl cls) -> mir::ClassDeclId {
    const mir::ClassDeclId id{
        static_cast<std::uint32_t>(unit_->classes.size())};
    unit_->classes.push_back(std::move(cls));
    return id;
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

struct ChildClassBinding {
  mir::ClassDeclId class_id;
  mir::MemberVarId member_id;
};

// Bindings produced by `InstallGenerateOwnedChildClasses` for a single
// `hir::Generate`. Indexed by `hir::StructuralScopeId.value`. Phase-local to
// constructor lowering: `LowerConstructorBody` consumes them and they then
// go away. They are not part of `ClassLoweringState`.
struct GenerateBindings {
  std::vector<ChildClassBinding> by_scope_id;
};

// Class-level lowering state for one mir::ClassDecl. Owns class-level
// mutation during lowering AND owns class-scope HIR-to-MIR translation maps.
// Class declaration and class-scope name translation form one lowering unit.
class ClassLoweringState {
 public:
  ClassLoweringState(const ClassLoweringState* parent, mir::ClassDecl& cls)
      : parent_(parent), class_decl_(&cls) {
  }

  [[nodiscard]] auto Parent() const -> const ClassLoweringState* {
    return parent_;
  }

  [[nodiscard]] auto Class() const -> const mir::ClassDecl& {
    return *class_decl_;
  }

  auto AddMemberVar(mir::MemberVar member) -> mir::MemberVarId {
    const mir::MemberVarId id{
        static_cast<std::uint32_t>(class_decl_->member_vars.size())};
    class_decl_->member_vars.push_back(std::move(member));
    return id;
  }

  auto AddClass(mir::ClassDecl child) -> mir::ClassDeclId {
    const mir::ClassDeclId id{
        static_cast<std::uint32_t>(class_decl_->classes.size())};
    class_decl_->classes.push_back(std::move(child));
    return id;
  }

  auto AddProcess(mir::Process process) -> mir::ProcessId {
    const mir::ProcessId id{
        static_cast<std::uint32_t>(class_decl_->processes.size())};
    class_decl_->processes.push_back(std::move(process));
    return id;
  }

  auto AddUserSubroutineTarget(mir::UserSubroutineTarget target)
      -> mir::UserSubroutineTargetId {
    const mir::UserSubroutineTargetId id{static_cast<std::uint32_t>(
        class_decl_->user_subroutine_targets.size())};
    class_decl_->user_subroutine_targets.push_back(std::move(target));
    return id;
  }

  void SetConstructor(mir::Body body) {
    class_decl_->constructor = std::move(body);
  }

  void MapMemberVar(hir::MemberVarId hir_id, mir::MemberVarId mir_id) {
    if (hir_id.value >= member_var_map_.size()) {
      member_var_map_.resize(hir_id.value + 1);
    }
    member_var_map_[hir_id.value] = mir_id;
  }

  // Walk the class chain `hops` levels outward and look up `hir_id` in that
  // class's member-var map. The class chain mirrors the lexical scope chain
  // exactly, so `parent_scope_hops` from a HIR ref maps directly to chain
  // depth.
  [[nodiscard]] auto TranslateMemberVar(
      hir::ParentScopeHops hops, hir::MemberVarId hir_id) const
      -> mir::MemberVarId {
    if (hops.value == 0) {
      if (hir_id.value >= member_var_map_.size()) {
        throw InternalError(
            "ClassLoweringState::TranslateMemberVar: unmapped HIR member var");
      }
      return member_var_map_[hir_id.value];
    }
    if (parent_ == nullptr) {
      throw InternalError(
          "ClassLoweringState::TranslateMemberVar: hops out of class chain");
    }
    return parent_->TranslateMemberVar(
        hir::ParentScopeHops{hops.value - 1}, hir_id);
  }

  // Look up a member var by id, walking outward `hops` levels first to reach
  // the owning class state.
  [[nodiscard]] auto GetMemberVar(
      hir::ParentScopeHops hops, mir::MemberVarId mir_id) const
      -> const mir::MemberVar& {
    if (hops.value == 0) {
      return class_decl_->GetMemberVar(mir_id);
    }
    if (parent_ == nullptr) {
      throw InternalError(
          "ClassLoweringState::GetMemberVar: hops out of class chain");
    }
    return parent_->GetMemberVar(hir::ParentScopeHops{hops.value - 1}, mir_id);
  }

  void MapUserSubroutine(
      hir::SubroutineId hir_id, mir::UserSubroutineTargetId mir_id) {
    if (hir_id.value >= user_subroutine_map_.size()) {
      user_subroutine_map_.resize(hir_id.value + 1);
    }
    user_subroutine_map_[hir_id.value] = mir_id;
  }

  [[nodiscard]] auto TranslateUserSubroutine(
      hir::ParentScopeHops hops, hir::SubroutineId hir_id) const
      -> mir::UserSubroutineTargetId {
    if (hops.value == 0) {
      if (hir_id.value >= user_subroutine_map_.size()) {
        throw InternalError(
            "ClassLoweringState::TranslateUserSubroutine: unmapped HIR "
            "subroutine");
      }
      return user_subroutine_map_[hir_id.value];
    }
    if (parent_ == nullptr) {
      throw InternalError(
          "ClassLoweringState::TranslateUserSubroutine: hops out of class "
          "chain");
    }
    return parent_->TranslateUserSubroutine(
        hir::ParentScopeHops{hops.value - 1}, hir_id);
  }

 private:
  const ClassLoweringState* parent_;
  mir::ClassDecl* class_decl_;
  std::vector<mir::MemberVarId> member_var_map_;
  std::vector<mir::UserSubroutineTargetId> user_subroutine_map_;
};

struct LocalBinding {
  std::uint32_t declaration_body_depth;
  mir::LocalVarId local;
};

class ProcessLoweringState {
 public:
  explicit ProcessLoweringState(TimeResolution time_resolution)
      : time_resolution_(time_resolution) {
  }

  [[nodiscard]] auto Resolution() const -> TimeResolution {
    return time_resolution_;
  }

  void EnterBody() {
    ++body_depth_;
  }
  void LeaveBody() {
    if (body_depth_ == 0) {
      throw InternalError(
          "ProcessLoweringState::LeaveBody: body-depth underflow");
    }
    --body_depth_;
  }
  [[nodiscard]] auto CurrentBodyDepth() const -> std::uint32_t {
    return body_depth_;
  }

  void MapLocalVar(hir::LocalVarId hir_id, LocalBinding binding) {
    if (hir_id.value >= bindings_.size()) {
      bindings_.resize(hir_id.value + 1);
    }
    if (bindings_[hir_id.value].has_value()) {
      throw InternalError(
          "ProcessLoweringState::MapLocalVar: HIR LocalVarId already mapped "
          "(duplicate VarDeclStmt for the same declaration)");
    }
    bindings_[hir_id.value] = binding;
  }

  [[nodiscard]] auto LookupLocalVar(hir::LocalVarId hir_id) const
      -> const LocalBinding& {
    if (hir_id.value >= bindings_.size() ||
        !bindings_[hir_id.value].has_value()) {
      throw InternalError(
          "ProcessLoweringState::LookupLocalVar: unmapped HIR local var");
    }
    return *bindings_[hir_id.value];
  }

  [[nodiscard]] auto TranslateLocalVar(hir::LocalVarId hir_id) const
      -> mir::LocalVarRef {
    const auto& binding = LookupLocalVar(hir_id);
    if (binding.declaration_body_depth > body_depth_) {
      throw InternalError(
          "ProcessLoweringState::TranslateLocalVar: declaration body depth "
          "exceeds current depth (forward reference into a child body)");
    }
    return mir::LocalVarRef{
        .body_hops =
            mir::BodyHops{
                .value = body_depth_ - binding.declaration_body_depth},
        .local = binding.local,
    };
  }

 private:
  TimeResolution time_resolution_;
  std::uint32_t body_depth_ = 0;
  std::vector<std::optional<LocalBinding>> bindings_;
};

class BodyDepthGuard {
 public:
  explicit BodyDepthGuard(ProcessLoweringState& state) : state_(&state) {
    state_->EnterBody();
  }

  ~BodyDepthGuard() {
    state_->LeaveBody();
  }

  BodyDepthGuard(const BodyDepthGuard&) = delete;
  auto operator=(const BodyDepthGuard&) -> BodyDepthGuard& = delete;
  BodyDepthGuard(BodyDepthGuard&&) = delete;
  auto operator=(BodyDepthGuard&&) -> BodyDepthGuard& = delete;

 private:
  ProcessLoweringState* state_;
};

class ConstructorLoweringState {
 public:
  void MapLoopVar(hir::LoopVarDeclId hir_id, mir::LocalVarRef ref) {
    if (hir_id.value >= map_.size()) {
      map_.resize(hir_id.value + 1);
    }
    map_[hir_id.value] = ref;
  }

  [[nodiscard]] auto TranslateLoopVar(hir::LoopVarDeclId hir_id) const
      -> mir::LocalVarRef {
    if (hir_id.value >= map_.size()) {
      throw InternalError("TranslateLoopVar: unmapped HIR loop var");
    }
    return map_[hir_id.value];
  }

 private:
  std::vector<mir::LocalVarRef> map_;
};

class BodyLoweringState {
 public:
  BodyLoweringState() = default;

  auto AddLocal(mir::LocalVar local) -> mir::LocalVarId {
    const mir::LocalVarId id{static_cast<std::uint32_t>(body_.locals.size())};
    body_.locals.push_back(std::move(local));
    return id;
  }

  [[nodiscard]] auto GetLocal(mir::LocalVarId id) const
      -> const mir::LocalVar& {
    return body_.locals.at(id.value);
  }

  auto AddExpr(mir::Expr expr) -> mir::ExprId {
    const mir::ExprId id{static_cast<std::uint32_t>(body_.exprs.size())};
    body_.exprs.push_back(std::move(expr));
    return id;
  }

  [[nodiscard]] auto Body() const -> const mir::Body& {
    return body_;
  }

  auto AddStmt(mir::Stmt stmt) -> mir::StmtId {
    const mir::StmtId id{static_cast<std::uint32_t>(body_.stmts.size())};
    body_.stmts.push_back(std::move(stmt));
    return id;
  }

  void AddRootStmt(mir::StmtId id) {
    body_.root_stmts.push_back(id);
  }

  auto Finish() -> mir::Body {
    return std::move(body_);
  }

 private:
  mir::Body body_;
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

  [[nodiscard]] auto Resolve(hir::ParentScopeHops hops) const
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
