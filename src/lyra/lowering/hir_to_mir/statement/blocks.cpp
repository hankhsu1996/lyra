#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"

#include <cstddef>
#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/member.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// LRM 6.21: a block declaring automatic locals a detached fork branch borrows
// and can outlive lifts the whole borrowed set into one shared activation
// object. Synthesize a plain box class holding those locals as members,
// allocate it at block entry through a handle (a shared pointer), and record
// each promoted var's slot so its declaration and references reach
// `handle->member`. The branch keeps the activation alive by holding a by-value
// copy of the handle.
void OpenActivationScope(
    ProcessLowerer& process, const WalkFrame& frame, const hir::BlockStmt& b) {
  const hir::ProceduralBody& body = process.HirBody();
  std::vector<hir::ProceduralVarId> promoted;
  for (const hir::StmtId sid : b.statements) {
    const auto* vd = std::get_if<hir::VarDeclStmt>(&body.stmts.Get(sid).data);
    if (vd != nullptr && body.procedural_vars.Get(vd->var).lifetime_extended) {
      promoted.push_back(vd->var);
    }
  }
  if (promoted.empty()) {
    return;
  }

  ModuleLowerer& module = process.Module();
  mir::CompilationUnit& unit = module.Unit();
  mir::Class& owner = *process.OwnerCtorFrame().current_class;

  // Build the box fully, then append it (the class arena is append-only).
  const std::string box_name = std::string(process.CallableName()) + "__act" +
                               std::to_string(owner.nested_classes.size());
  mir::Class box;
  box.name = box_name;
  box.base = std::nullopt;
  box.time_resolution = owner.time_resolution;
  std::vector<mir::MemberId> members;
  members.reserve(promoted.size());
  for (const hir::ProceduralVarId v : promoted) {
    const hir::ProceduralVarDecl& decl = body.procedural_vars.Get(v);
    members.push_back(box.members.Add(
        mir::MemberDecl{
            .name = decl.name, .type = module.TranslateType(decl.type)}));
  }
  const mir::TypeId box_object =
      unit.AddType(mir::ObjectType{.name = box_name});
  box.self_pointer_type = unit.AddType(
      mir::PointerType{
          .pointee = box_object,
          .ownership = mir::PointerOwnership::kBorrowed});
  owner.nested_classes.Add(std::move(box));

  // The handle: a shared pointer to the box, allocated by make_shared. Declared
  // first in the scope, before the promoted locals it stands in for.
  const mir::TypeId handle_type = unit.AddType(
      mir::PointerType{
          .pointee = box_object, .ownership = mir::PointerOwnership::kShared});
  mir::Block& block = *frame.current_block;
  const mir::ExprId init = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::ConstructorCallee{}, .arguments = {}},
          .type = handle_type});
  const mir::LocalRef handle = block.AppendLocal(
      mir::LocalDecl{.name = box_name + "_h", .type = handle_type}, init);

  for (std::size_t i = 0; i < promoted.size(); ++i) {
    process.RecordPendingActivation(
        promoted[i], PromotedVarBinding{
                         .handle_depth = frame.block_depth,
                         .handle = handle.var,
                         .handle_type = handle_type,
                         .member = members[i]});
  }
}

}  // namespace

auto LowerEmptyStmt(std::optional<std::string> label)
    -> diag::Result<mir::Stmt> {
  return mir::Stmt{.label = std::move(label), .data = mir::EmptyStmt{}};
}

auto LowerBlockStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::BlockStmt& b) -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  mir::Block child_block;
  const WalkFrame child_frame = frame.WithBlock(&child_block).Deeper();
  OpenActivationScope(process, child_frame, b);
  for (const hir::StmtId child_hir_id : b.statements) {
    const hir::Stmt& child = hir_proc.stmts.Get(child_hir_id);
    auto lowered = process.LowerStmt(child, child_frame);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    child_block.AppendStmt(*std::move(lowered));
  }
  const mir::BlockId scope_id =
      frame.current_block->child_scopes.Add(std::move(child_block));
  return mir::Stmt{
      .label = std::move(label), .data = mir::BlockStmt{.scope = scope_id}};
}

auto LowerStmtIntoChildScope(
    ProcessLowerer& process, WalkFrame frame, hir::StmtId hir_stmt_id)
    -> diag::Result<mir::Block> {
  mir::Block child_block;
  const WalkFrame child_frame = frame.WithBlock(&child_block).Deeper();
  const hir::Stmt& hir_stmt = process.HirBody().stmts.Get(hir_stmt_id);
  auto lowered = process.LowerStmt(hir_stmt, child_frame);
  if (!lowered) {
    return std::unexpected(std::move(lowered.error()));
  }
  child_block.AppendStmt(*std::move(lowered));
  return std::move(child_block);
}

}  // namespace lyra::lowering::hir_to_mir
