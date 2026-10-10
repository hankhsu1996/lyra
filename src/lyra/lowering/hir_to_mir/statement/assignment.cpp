#include "lyra/lowering/hir_to_mir/statement/assignment.hpp"

#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/expression/assignment.hpp"
#include "lyra/lowering/hir_to_mir/expression/dpi_call.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/mem_file.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/sformat.hpp"
#include "lyra/lowering/hir_to_mir/lvalue.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/subroutine_call.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The statement-position lowering a system subroutine needs when its effect
// cannot be expressed as a bare value: a file write whose formatted output is
// bound to an output argument, and the `$sformat` / `$swrite` family whose
// result lands in an output variable. Nullopt for every family that lowers as
// an ordinary expression. Exhaustive over the semantic families, so a new one
// forces a decision about whether it has a statement form.
//
// The label is borrowed rather than taken: this may decline, and a caller that
// had handed its label over would carry on with an emptied one and silently
// drop the name a `disable` resolves against (LRM 9.6.2). Only the paths that
// commit to a statement copy it.
auto LowerSystemSubroutineCallStmtForm(
    ProcessLowerer& process, WalkFrame frame,
    const std::optional<std::string>& label, const hir::CallExpr& call,
    const hir::SystemSubroutineRef& ref,
    std::optional<hir::ExprId> assign_target)
    -> std::optional<diag::Result<mir::Stmt>> {
  const auto& desc = support::LookupSystemSubroutine(ref.id);
  return std::visit(
      Overloaded{
          [](const support::FileIOSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            return std::nullopt;
          },
          [&](const support::SFormatSystemSubroutineInfo& sformat)
              -> std::optional<diag::Result<mir::Stmt>> {
            // The statement form writes into the call's own output variable
            // (`$sformat` / `$swrite`) or discards the text (`$sformatf`). A
            // call feeding an assignment target is necessarily the valued
            // `$sformatf`, whose result the assignment consumes, so it stays an
            // ordinary expression there.
            if (assign_target.has_value()) return std::nullopt;
            return LowerSFormatSystemSubroutineCallStmt(
                process, frame, label, call, sformat);
          },
          [](const support::PrintSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            return std::nullopt;
          },
          [](const support::TerminationSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            return std::nullopt;
          },
          [](const support::DiagnosticSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            return std::nullopt;
          },
          [](const support::ScanSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            return std::nullopt;
          },
          [](const support::TimeSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            return std::nullopt;
          },
          [](const support::TimeFormatSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            return std::nullopt;
          },
          [](const support::PrintTimescaleSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            return std::nullopt;
          },
          [](const support::PlusargsSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            return std::nullopt;
          },
          [](const support::BitVectorSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            return std::nullopt;
          },
          [](const support::HostCommandSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            return std::nullopt;
          },
          [](const support::RandomSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            return std::nullopt;
          },
          [](const support::DistributionSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            // The seed writeback is sequenced inside the call's own expression
            // lowering, so a statement-position call needs nothing extra.
            return std::nullopt;
          },
          [](const support::SampledValueSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            // Reading a sampled value settles nothing outside the value it
            // answers with, so a statement-position call has no form of its own
            // and lowers as the expression it is.
            return std::nullopt;
          },
          [](const support::ValueChangeSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            return std::nullopt;
          },
          [](const support::PastValueSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            return std::nullopt;
          },
          [&](const support::MemFileSystemSubroutineInfo& mem_file)
              -> std::optional<diag::Result<mir::Stmt>> {
            // A void task (LRM 21.4 / 21.5): its only form is a statement, so
            // it never feeds an assignment target. The lowering branches on
            // load vs dump.
            return LowerMemFileSystemSubroutineCallStmt(
                process, frame, label, call, mem_file);
          },
      },
      desc.semantic);
}

}  // namespace

auto LowerExprStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ExprStmt& e) -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  auto& block = *frame.current_block;

  // An assignment statement to a join of lvalues (LRM A.8.5) is a block of its
  // own rather than an expression whose value nobody reads.
  const hir::Expr& inner = hir_proc.exprs.Get(e.expr);
  if (const auto* assign = std::get_if<hir::AssignExpr>(&inner.data)) {
    const hir::Expr& lhs = hir_proc.exprs.Get(assign->lhs);
    if (IsJoin(lhs)) {
      BlockBuilder steps(frame);
      auto stored =
          AssignToJoin(process, steps.Frame(), *assign, lhs, inner.span);
      if (!stored) return std::unexpected(std::move(stored.error()));
      mir::Stmt stmt = steps.BuildStatement();
      stmt.label = std::move(label);
      return stmt;
    }
  }

  // A call statement. Some callees need a shape of their own here, because
  // what they do cannot be written as a bare value; the rest are the call
  // itself, awaited where the callee suspends, and the statement is the
  // discard either way.
  if (const auto* call = std::get_if<hir::CallExpr>(&inner.data)) {
    const mir::TypeId inner_type = process.Owner().TranslateType(inner.type);
    if (const auto* sys_ref =
            std::get_if<hir::SystemSubroutineRef>(&call->callee)) {
      if (auto stmt = LowerSystemSubroutineCallStmtForm(
              process, frame, label, *call, *sys_ref, std::nullopt)) {
        return *std::move(stmt);
      }
    }
    if (const auto* import_ref =
            std::get_if<hir::ForeignImportRef>(&call->callee)) {
      if (auto stmt = LowerForeignImportCallStmtForm(
              process, frame, label, *call, *import_ref)) {
        return *std::move(stmt);
      }
    }
    if (auto stmt = LowerSubroutineCallStmtForm(
            process, frame, label, *call, inner_type)) {
      return *std::move(stmt);
    }
    auto call_or = process.LowerExpr(inner, frame);
    if (!call_or) return std::unexpected(std::move(call_or.error()));
    const mir::ExprId call_id = block.exprs.Add(*std::move(call_or));
    const mir::TypeId call_type = block.exprs.Get(call_id).type;
    // What the call answers says what the statement is: a task enable's
    // execution, awaited (LRM 13.3); a wait, stopped at (LRM 9.7 `await`); or
    // a value nothing reads.
    const mir::CompilationUnit& unit = process.Owner().Unit();
    mir::Stmt stmt = [&] {
      if (unit.types.Get(call_type).Is<mir::CoroutineType>()) {
        return BuildAwaitStmt(process.Owner(), block, call_id);
      }
      if (call_type == unit.builtins.wait) {
        return BuildStopStmt(process.Owner(), frame, call_id);
      }
      return mir::Stmt{
          .label = std::nullopt, .data = mir::ExprStmt{.expr = call_id}};
    }();
    stmt.label = std::move(label);
    return stmt;
  }
  if (const auto* assign = std::get_if<hir::AssignExpr>(&inner.data)) {
    if (!assign->compound.has_value() &&
        std::holds_alternative<hir::ImmediateEffect>(assign->timing)) {
      // Peek through an implicit conversion wrapper that slang inserts when
      // the call's return type does not match the LHS type bit-for-bit.
      const hir::Expr* call_carrier = &hir_proc.exprs.Get(assign->rhs);
      if (const auto* conv =
              std::get_if<hir::ConversionExpr>(&call_carrier->data)) {
        call_carrier = &hir_proc.exprs.Get(conv->operand);
      }
      if (const auto* call = std::get_if<hir::CallExpr>(&call_carrier->data)) {
        if (const auto* sys_ref =
                std::get_if<hir::SystemSubroutineRef>(&call->callee)) {
          if (auto stmt = LowerSystemSubroutineCallStmtForm(
                  process, frame, label, *call, *sys_ref, assign->lhs)) {
            return *std::move(stmt);
          }
        }
      }
    }
  }

  auto expr_or = process.LowerIgnoredExpr(inner, frame);
  if (!expr_or) {
    return std::unexpected(std::move(expr_or.error()));
  }
  return mir::Stmt{
      .label = std::move(label),
      .data = mir::ExprStmt{.expr = block.exprs.Add(*std::move(expr_or))}};
}

}  // namespace lyra::lowering::hir_to_mir
