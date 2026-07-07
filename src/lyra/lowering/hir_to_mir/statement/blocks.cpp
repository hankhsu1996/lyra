#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"

#include <cstddef>
#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/struct_decl.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// LRM 6.21: a block declaring automatic locals a detached fork branch borrows
// and can outlive lifts the whole borrowed set into one shared promoted scope
// (the "activation frame" role). Synthesize a struct holding those locals as
// fields, allocate it at block entry through a shared handle, and record each
// promoted var's field so its declaration and references reach `handle->field`.
// The branch keeps the scope alive by holding a by-value copy of the handle.
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

  // The escaping scope's locals are promoted into a compiler-generated struct
  // whose identity lives in the unit's struct registry; its emission nesting is
  // recorded separately on the enclosing class below.
  const std::string struct_name = std::string(process.CallableName()) +
                                  "__scope" +
                                  std::to_string(unit.structs.size());
  mir::StructDecl struct_decl;
  struct_decl.name = struct_name;
  std::vector<mir::FieldId> fields;
  fields.reserve(promoted.size());
  for (const hir::ProceduralVarId v : promoted) {
    const hir::ProceduralVarDecl& decl = body.procedural_vars.Get(v);
    fields.push_back(struct_decl.fields.Add(
        mir::FieldDecl{
            .name = decl.name, .type = module.TranslateType(decl.type)}));
  }
  const mir::StructId struct_id = unit.AddStruct(std::move(struct_decl));
  // The struct is nested in the class whose body opens this scope -- its
  // emission host. Record the nesting on the class explicitly, parallel to the
  // child-class `contained` list, so a backend emits it by iteration, never by
  // walking the body tree.
  if (frame.current_class == nullptr) {
    throw InternalError(
        "OpenActivationScope: promoted scope opened outside any class");
  }
  frame.current_class->structs.push_back(struct_id);
  const mir::TypeId struct_type =
      unit.types.Intern(mir::StructType{.struct_id = struct_id});

  // The handle: a shared pointer to the generated struct, allocated by
  // make_shared. Declared first in the scope, before the promoted locals it
  // stands in for.
  const mir::TypeId handle_type =
      unit.types.PointerTo(struct_type, mir::PointerOwnership::kShared);
  mir::Block& block = *frame.current_block;
  const mir::ExprId init = block.exprs.Add(
      mir::Expr{
          .data = mir::CallExpr{.callee = mir::Construct{}, .arguments = {}},
          .type = handle_type});
  // The handle is a synthesized carrier declared in this body and captured (by
  // value, owning) by any branch that borrows a promoted field. Its origin
  // comes from the unit's synthesized-site allocator, the one collision-free id
  // space every synthesized carrier shares.
  const BindingOriginId handle_origin =
      BindingOriginId::Synthesized(module.NextSynthesizedSite(), 0);
  const mir::LocalId handle = frame.bindings->Declare(
      handle_origin,
      mir::LocalDecl{.name = struct_name + "_h", .type = handle_type});
  block.AppendStmt(mir::LocalDeclStmt{.target = handle, .init = init});

  for (std::size_t i = 0; i < promoted.size(); ++i) {
    process.RecordPendingActivation(
        promoted[i], PromotedVarBinding{
                         .handle_origin = handle_origin,
                         .handle_type = handle_type,
                         .field = fields[i]});
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
  const WalkFrame child_frame = frame.WithBlock(&child_block);
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
  const WalkFrame child_frame = frame.WithBlock(&child_block);
  const hir::Stmt& hir_stmt = process.HirBody().stmts.Get(hir_stmt_id);
  auto lowered = process.LowerStmt(hir_stmt, child_frame);
  if (!lowered) {
    return std::unexpected(std::move(lowered.error()));
  }
  child_block.AppendStmt(*std::move(lowered));
  return std::move(child_block);
}

}  // namespace lyra::lowering::hir_to_mir
