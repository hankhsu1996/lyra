#pragma once

#include <compare>
#include <cstdint>
#include <map>
#include <optional>
#include <ranges>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/Type.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/module_unit.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/structural_hops.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/structural_var.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

// Lowering-only identity for a scope on the stack. Monotonically assigned
// by ScopeStack; never reused; never stored in HIR.
struct ScopeFrameId {
  std::uint32_t value;

  auto operator<=>(const ScopeFrameId&) const -> std::strong_ordering = default;
};

struct StructuralVarBinding {
  ScopeFrameId home_frame;
  hir::StructuralVarId var_id;
  hir::TypeId type;
};

using StructuralVarBindings =
    std::unordered_map<const slang::ast::VariableSymbol*, StructuralVarBinding>;

struct SubroutineBinding {
  ScopeFrameId owner_frame;
  hir::StructuralSubroutineId subroutine_id;
};

using SubroutineBindings =
    std::unordered_map<const slang::ast::SubroutineSymbol*, SubroutineBinding>;

struct LoopVarBinding {
  ScopeFrameId home_frame;
  hir::LoopVarDeclId loop_var_id;
  hir::TypeId type;
};

using LoopVarBindings =
    std::unordered_map<const slang::ast::ValueSymbol*, LoopVarBinding>;

struct InstanceMemberBinding {
  ScopeFrameId home_frame;
  hir::InstanceMemberId member_id;
};

using InstanceMemberBindings =
    std::unordered_map<const slang::ast::Symbol*, InstanceMemberBinding>;

class UnitLoweringState {
 public:
  explicit UnitLoweringState(std::string name)
      : hir_unit_{.name = std::move(name), .types = {}, .root_scope = {}} {
    // Intrinsic types are pre-registered so callers reference them by a
    // stable id rather than re-adding entries on demand. Extend this section
    // when more synthesized types accumulate (1-bit logic, etc.).
    void_type_id_ = AddType(hir::TypeData{hir::VoidType{}});
    int32_type_id_ = AddType(
        hir::TypeData{hir::PackedArrayType{
            .atom = hir::BitAtom::kBit,
            .signedness = hir::Signedness::kSigned,
            .dims = {hir::PackedRange{.left = 31, .right = 0}},
            .form = hir::PackedArrayForm::kInt}});
    string_type_id_ = AddType(hir::TypeData{hir::StringType{}});
    time_type_id_ = AddType(
        hir::TypeData{hir::PackedArrayType{
            .atom = hir::BitAtom::kLogic,
            .signedness = hir::Signedness::kUnsigned,
            .dims = {hir::PackedRange{.left = 63, .right = 0}},
            .form = hir::PackedArrayForm::kTime}});
    realtime_type_id_ = AddType(hir::TypeData{hir::RealTimeType{}});
  }

  // Canonical id for the synthesized `void` type (system-call sinks, lvalue
  // refs used in non-value context, etc.). Single source of truth so the
  // unit never holds more than one VoidType entry.
  [[nodiscard]] auto VoidTypeId() const -> hir::TypeId {
    return void_type_id_;
  }

  // Canonical id for the synthesized 32-bit signed packed `int`. Used by
  // system functions whose return is fixed at int32 by the language
  // (`$fopen` per LRM 21.3.1, etc.) rather than by a slang-derived type.
  [[nodiscard]] auto Int32TypeId() const -> hir::TypeId {
    return int32_type_id_;
  }

  // Canonical id for the synthesized `string` type. Used by system functions
  // whose return is fixed at `string` by the language (`$sformatf` per LRM
  // 21.3.3) rather than by a slang-derived type.
  [[nodiscard]] auto StringTypeId() const -> hir::TypeId {
    return string_type_id_;
  }

  // Canonical id for the synthesized 64-bit unsigned `time` (LRM 6.11). Used
  // by $time, whose return is fixed at `time` by the language (LRM 20.3.1).
  [[nodiscard]] auto TimeTypeId() const -> hir::TypeId {
    return time_type_id_;
  }

  // Canonical id for the synthesized `realtime` (LRM 6.12.1). Used by
  // $realtime, whose return is fixed at `realtime` by the language (LRM
  // 20.3.3).
  [[nodiscard]] auto RealTimeTypeId() const -> hir::TypeId {
    return realtime_type_id_;
  }

  [[nodiscard]] auto HirUnit() const -> const hir::ModuleUnit& {
    return hir_unit_;
  }

  auto HirUnit() -> hir::ModuleUnit& {
    return hir_unit_;
  }

  // Canonical entry point for slang-driven types. Same slang canonical type
  // always returns the same hir::TypeId by going through a canonical-pointer
  // cache; callers never raw-add slang-backed types. Defined in type.cpp so
  // the body lives next to LowerType.
  auto GetTypeId(const slang::ast::Type& type, diag::SourceSpan span)
      -> diag::Result<hir::TypeId>;

  [[nodiscard]] auto GetType(hir::TypeId id) const -> const hir::Type& {
    return hir_unit_.GetType(id);
  }

  void MapStructuralVarBinding(
      const slang::ast::VariableSymbol& var, ScopeFrameId home_frame,
      hir::StructuralVarId local, hir::TypeId type) {
    const auto [_, inserted] = structural_var_bindings_.emplace(
        &var, StructuralVarBinding{
                  .home_frame = home_frame, .var_id = local, .type = type});
    if (!inserted) {
      throw InternalError(
          "UnitLoweringState::MapStructuralVarBinding: structural variable "
          "symbol already mapped");
    }
  }

  [[nodiscard]] auto LookupStructuralVarBinding(
      const slang::ast::VariableSymbol& var) const
      -> std::optional<StructuralVarBinding> {
    const auto it = structural_var_bindings_.find(&var);
    if (it == structural_var_bindings_.end()) {
      return std::nullopt;
    }
    return it->second;
  }

  void MapSubroutineBinding(
      const slang::ast::SubroutineSymbol& sym, ScopeFrameId owner_frame,
      hir::StructuralSubroutineId local) {
    const auto [_, inserted] = subroutine_bindings_.emplace(
        &sym,
        SubroutineBinding{.owner_frame = owner_frame, .subroutine_id = local});
    if (!inserted) {
      throw InternalError(
          "UnitLoweringState::MapSubroutineBinding: subroutine symbol already "
          "mapped");
    }
  }

  [[nodiscard]] auto LookupSubroutineBinding(
      const slang::ast::SubroutineSymbol& sym) const
      -> std::optional<SubroutineBinding> {
    const auto it = subroutine_bindings_.find(&sym);
    if (it == subroutine_bindings_.end()) {
      return std::nullopt;
    }
    return it->second;
  }

  void MapLoopVarBinding(
      const slang::ast::ValueSymbol& sym, ScopeFrameId home_frame,
      hir::LoopVarDeclId id, hir::TypeId type) {
    const auto [_, inserted] = loop_var_bindings_.emplace(
        &sym, LoopVarBinding{
                  .home_frame = home_frame, .loop_var_id = id, .type = type});
    if (!inserted) {
      throw InternalError(
          "UnitLoweringState::MapLoopVarBinding: loop variable symbol already "
          "mapped");
    }
  }

  [[nodiscard]] auto LookupLoopVarBinding(const slang::ast::ValueSymbol& sym)
      const -> std::optional<LoopVarBinding> {
    const auto it = loop_var_bindings_.find(&sym);
    if (it == loop_var_bindings_.end()) {
      return std::nullopt;
    }
    return it->second;
  }

  void MapInstanceMemberBinding(
      const slang::ast::Symbol& member, ScopeFrameId home_frame,
      hir::InstanceMemberId member_id) {
    const auto [_, inserted] = instance_member_bindings_.emplace(
        &member, InstanceMemberBinding{
                     .home_frame = home_frame, .member_id = member_id});
    if (!inserted) {
      throw InternalError(
          "UnitLoweringState::MapInstanceMemberBinding: instance member "
          "already mapped");
    }
  }

  [[nodiscard]] auto LookupInstanceMemberBinding(
      const slang::ast::Symbol& member) const
      -> std::optional<InstanceMemberBinding> {
    const auto it = instance_member_bindings_.find(&member);
    if (it == instance_member_bindings_.end()) {
      return std::nullopt;
    }
    return it->second;
  }

  // Dedups by target symbol (one slot per referenced cross-unit member) and
  // accumulates the slot decl under its owning scope's frame. The returned id
  // is the slot's index within that scope's `cross_unit_refs`, flushed onto the
  // HIR scope by TakeCrossUnitRefsForFrame when the scope finishes lowering.
  auto MapOrGetCrossUnitRef(
      const slang::ast::ValueSymbol& target, ScopeFrameId home_frame,
      hir::InstanceMemberId instance, std::vector<hir::PathStep> path,
      hir::TypeId type) -> hir::CrossUnitRefId {
    if (const auto it = cross_unit_ref_dedup_.find(&target);
        it != cross_unit_ref_dedup_.end()) {
      return it->second;
    }
    auto& slots = cross_unit_refs_by_frame_[home_frame];
    const hir::CrossUnitRefId id{static_cast<std::uint32_t>(slots.size())};
    slots.push_back(
        hir::CrossUnitRefDecl{
            .instance = instance, .path = std::move(path), .type = type});
    cross_unit_ref_dedup_.emplace(&target, id);
    return id;
  }

  [[nodiscard]] auto LookupCrossUnitRef(const slang::ast::ValueSymbol& target)
      const -> std::optional<hir::CrossUnitRefId> {
    const auto it = cross_unit_ref_dedup_.find(&target);
    if (it == cross_unit_ref_dedup_.end()) {
      return std::nullopt;
    }
    return it->second;
  }

  auto TakeCrossUnitRefsForFrame(ScopeFrameId frame)
      -> std::vector<hir::CrossUnitRefDecl> {
    const auto it = cross_unit_refs_by_frame_.find(frame);
    if (it == cross_unit_refs_by_frame_.end()) {
      return {};
    }
    auto out = std::move(it->second);
    cross_unit_refs_by_frame_.erase(it);
    return out;
  }

  auto MoveHirUnit() -> hir::ModuleUnit {
    return std::move(hir_unit_);
  }

 private:
  // Adds a freshly-built TypeData entry and returns its id. Used internally
  // by the ctor (intrinsic types) and GetTypeId (after a cache miss); no
  // outside caller raw-adds types, so dedup invariants stay enforced.
  auto AddType(hir::TypeData data) -> hir::TypeId {
    const hir::TypeId id{static_cast<std::uint32_t>(hir_unit_.types.size())};
    hir_unit_.types.push_back(hir::Type{.data = std::move(data)});
    return id;
  }

  hir::ModuleUnit hir_unit_;
  StructuralVarBindings structural_var_bindings_;
  SubroutineBindings subroutine_bindings_;
  LoopVarBindings loop_var_bindings_;
  InstanceMemberBindings instance_member_bindings_;
  std::unordered_map<const slang::ast::ValueSymbol*, hir::CrossUnitRefId>
      cross_unit_ref_dedup_;
  std::map<ScopeFrameId, std::vector<hir::CrossUnitRefDecl>>
      cross_unit_refs_by_frame_;
  std::unordered_map<const slang::ast::Type*, hir::TypeId> type_cache_;
  hir::TypeId void_type_id_{};
  hir::TypeId int32_type_id_{};
  hir::TypeId string_type_id_{};
  hir::TypeId time_type_id_{};
  hir::TypeId realtime_type_id_{};
};

class ScopeStack {
 public:
  auto Push() -> ScopeFrameId {
    const ScopeFrameId id{.value = next_id_++};
    frames_.push_back(id);
    return id;
  }

  void Pop(ScopeFrameId expected) {
    if (frames_.empty() || frames_.back() != expected) {
      throw InternalError("ScopeStack::Pop: unbalanced pop");
    }
    frames_.pop_back();
  }

  [[nodiscard]] auto HopsTo(ScopeFrameId target) const
      -> std::optional<hir::StructuralHops> {
    std::uint32_t hops = 0;
    for (const auto frame : frames_ | std::views::reverse) {
      if (frame == target) {
        return hir::StructuralHops{.value = hops};
      }
      ++hops;
    }
    return std::nullopt;
  }

 private:
  std::vector<ScopeFrameId> frames_;
  std::uint32_t next_id_ = 0;
};

class ScopeStackGuard {
 public:
  explicit ScopeStackGuard(ScopeStack& stack)
      : stack_(&stack), frame_(stack.Push()) {
  }

  ~ScopeStackGuard() {
    stack_->Pop(frame_);
  }

  ScopeStackGuard(const ScopeStackGuard&) = delete;
  auto operator=(const ScopeStackGuard&) -> ScopeStackGuard& = delete;
  ScopeStackGuard(ScopeStackGuard&&) = delete;
  auto operator=(ScopeStackGuard&&) -> ScopeStackGuard& = delete;

  [[nodiscard]] auto Frame() const -> ScopeFrameId {
    return frame_;
  }

 private:
  ScopeStack* stack_;
  ScopeFrameId frame_;
};

class ScopeLoweringState {
 public:
  ScopeLoweringState(
      UnitLoweringState& unit_state, hir::StructuralScope& scope,
      ScopeFrameId frame)
      : unit_state_(&unit_state), scope_(&scope), frame_(frame) {
  }

  auto AddStructuralVar(
      const slang::ast::VariableSymbol& var, hir::TypeId type,
      std::optional<hir::ExprId> initializer = std::nullopt)
      -> hir::StructuralVarId {
    const hir::StructuralVarId local{
        static_cast<std::uint32_t>(scope_->structural_vars.size())};
    scope_->structural_vars.push_back(
        hir::StructuralVarDecl{
            .name = std::string{var.name},
            .type = type,
            .initializer = initializer});
    unit_state_->MapStructuralVarBinding(var, frame_, local, type);
    return local;
  }

  void AddTypeAlias(hir::TypeAliasDecl decl) {
    scope_->type_aliases.push_back(std::move(decl));
  }

  auto AddLoopVarDecl(const slang::ast::ValueSymbol& sym, hir::TypeId type)
      -> hir::LoopVarDeclId {
    const hir::LoopVarDeclId id{
        static_cast<std::uint32_t>(scope_->loop_var_decls.size())};
    scope_->loop_var_decls.push_back(
        hir::LoopVarDecl{.name = std::string{sym.name}, .type = type});
    unit_state_->MapLoopVarBinding(sym, frame_, id, type);
    return id;
  }

  [[nodiscard]] auto GetLoopVarDeclType(hir::LoopVarDeclId id) const
      -> hir::TypeId {
    return scope_->loop_var_decls.at(id.value).type;
  }

  auto AddExpr(hir::Expr expr) -> hir::ExprId {
    const hir::ExprId id{static_cast<std::uint32_t>(scope_->exprs.size())};
    scope_->exprs.push_back(std::move(expr));
    return id;
  }

  auto AddProcess(hir::Process process) -> hir::ProcessId {
    const hir::ProcessId id{
        static_cast<std::uint32_t>(scope_->processes.size())};
    scope_->processes.push_back(std::move(process));
    return id;
  }

  auto AddContinuousAssign(hir::ContinuousAssign ca)
      -> hir::ContinuousAssignId {
    const hir::ContinuousAssignId id{
        static_cast<std::uint32_t>(scope_->continuous_assigns.size())};
    scope_->continuous_assigns.push_back(std::move(ca));
    return id;
  }

  auto AddGenerate(hir::Generate generate) -> hir::GenerateId {
    const hir::GenerateId id{
        static_cast<std::uint32_t>(scope_->generates.size())};
    scope_->generates.push_back(std::move(generate));
    return id;
  }

  auto AddInstanceMember(hir::InstanceMemberDecl decl)
      -> hir::InstanceMemberId {
    const hir::InstanceMemberId id{
        static_cast<std::uint32_t>(scope_->instance_members.size())};
    scope_->instance_members.push_back(std::move(decl));
    return id;
  }

  // Forward-declares a subroutine: registers its binding to a stable
  // source-order id before any body is lowered, so a call resolves regardless
  // of source order -- direct and mutual recursion and forward references (LRM
  // 13.4.2). Bodies are added afterwards in the same order by
  // AddStructuralSubroutine.
  void ReserveSubroutineBinding(const slang::ast::SubroutineSymbol& sym) {
    const hir::StructuralSubroutineId local{reserved_subroutine_count_++};
    unit_state_->MapSubroutineBinding(sym, frame_, local);
  }

  void AddStructuralSubroutine(
      const slang::ast::SubroutineSymbol& sym,
      hir::StructuralSubroutineDecl decl) {
    const auto binding = unit_state_->LookupSubroutineBinding(sym);
    if (!binding.has_value() ||
        binding->subroutine_id.value !=
            static_cast<std::uint32_t>(scope_->structural_subroutines.size())) {
      throw InternalError(
          "ScopeLoweringState::AddStructuralSubroutine: subroutine added out "
          "of "
          "reserved order; ReserveSubroutineBinding must run first in the same "
          "order");
    }
    scope_->structural_subroutines.push_back(std::move(decl));
  }

  [[nodiscard]] auto UnitState() -> UnitLoweringState& {
    return *unit_state_;
  }
  [[nodiscard]] auto UnitState() const -> const UnitLoweringState& {
    return *unit_state_;
  }
  [[nodiscard]] auto Scope() -> hir::StructuralScope& {
    return *scope_;
  }
  [[nodiscard]] auto Scope() const -> const hir::StructuralScope& {
    return *scope_;
  }
  [[nodiscard]] auto Frame() const -> ScopeFrameId {
    return frame_;
  }

 private:
  UnitLoweringState* unit_state_;
  hir::StructuralScope* scope_;
  ScopeFrameId frame_;
  std::uint32_t reserved_subroutine_count_ = 0;
};

class ProcessLoweringState {
 public:
  // `containing_symbol` is the slang symbol whose AST subtree this process
  // belongs to (a `ProceduralBlockSymbol` for `initial` / `always*`, the
  // `ContinuousAssignSymbol` when reused for continuous-assignment lowering,
  // etc.). Threaded into `SensitivityAnalyzer` as its name-lookup anchor.
  explicit ProcessLoweringState(const slang::ast::Symbol& containing_symbol)
      : containing_symbol_(&containing_symbol) {
  }

  [[nodiscard]] auto ContainingSymbol() const -> const slang::ast::Symbol& {
    return *containing_symbol_;
  }

  auto AddExpr(hir::Expr expr) -> hir::ExprId {
    const hir::ExprId id{static_cast<std::uint32_t>(body_.exprs.size())};
    body_.exprs.push_back(std::move(expr));
    return id;
  }

  auto AddStmt(hir::Stmt stmt) -> hir::StmtId {
    const hir::StmtId id{static_cast<std::uint32_t>(body_.stmts.size())};
    body_.stmts.push_back(std::move(stmt));
    return id;
  }

  auto AddProceduralVar(const slang::ast::VariableSymbol& var, hir::TypeId type)
      -> hir::ProceduralVarId {
    const hir::ProceduralVarId id{
        static_cast<std::uint32_t>(body_.procedural_vars.size())};
    body_.procedural_vars.push_back(
        hir::ProceduralVarDecl{
            .name = std::string{var.name},
            .type = type,
            .lifetime = var.lifetime == slang::ast::VariableLifetime::Automatic
                            ? hir::VariableLifetime::kAutomatic
                            : hir::VariableLifetime::kStatic});
    const auto [_, inserted] = procedural_var_bindings_.emplace(&var, id);
    if (!inserted) {
      throw InternalError(
          "ProcessLoweringState::AddProceduralVar: procedural variable symbol "
          "already mapped");
    }
    return id;
  }

  // Procedural variable with no slang symbol behind it. Name resolution
  // keys on slang symbol pointers, so a synthetic var cannot shadow a user
  // identifier; the `__lyra_` prefix is convention so the var is visually
  // distinct in dumps and emit. Lifetime is automatic because no SV
  // declaration governs it.
  auto AddSyntheticProceduralVar(std::string_view name, hir::TypeId type)
      -> hir::ProceduralVarId {
    const hir::ProceduralVarId id{
        static_cast<std::uint32_t>(body_.procedural_vars.size())};
    body_.procedural_vars.push_back(
        hir::ProceduralVarDecl{
            .name = std::string{name},
            .type = type,
            .lifetime = hir::VariableLifetime::kAutomatic});
    return id;
  }

  [[nodiscard]] auto LookupProceduralVar(const slang::ast::VariableSymbol& var)
      const -> std::optional<hir::ProceduralVarId> {
    const auto it = procedural_var_bindings_.find(&var);
    if (it == procedural_var_bindings_.end()) {
      return std::nullopt;
    }
    return it->second;
  }

  [[nodiscard]] auto GetProceduralVarType(hir::ProceduralVarId id) const
      -> hir::TypeId {
    return body_.procedural_vars.at(id.value).type;
  }

  auto FinalizeBody(hir::StmtId root_stmt) -> hir::ProceduralBody {
    body_.root_stmt = root_stmt;
    return std::move(body_);
  }

  // FJ1 lowers each fork-join branch as a separate process whose body may touch
  // only structural (module-scope) state. While a branch is being lowered this
  // depth is nonzero, and a reference to a procedural variable is rejected --
  // per-branch local storage and capture of the parent's locals ride on a later
  // cut. A counter (not a flag) so a nested fork stays rejected correctly.
  void EnterForkBranch() {
    ++fork_branch_depth_;
  }
  void ExitForkBranch() {
    --fork_branch_depth_;
  }
  [[nodiscard]] auto InForkBranch() const -> bool {
    return fork_branch_depth_ > 0;
  }

 private:
  hir::ProceduralBody body_;
  std::unordered_map<const slang::ast::VariableSymbol*, hir::ProceduralVarId>
      procedural_var_bindings_;
  const slang::ast::Symbol* containing_symbol_;
  int fork_branch_depth_ = 0;
};

}  // namespace lyra::lowering::ast_to_hir
