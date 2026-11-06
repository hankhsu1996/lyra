#pragma once

#include <utility>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class Expression;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Lowers a synchronous drive's target (LRM 14.16): the operand that names the
// clockvar stands for where the output's drive writes, and every select over it
// and every index in it is lowered as the procedure lowers it.
class DriveTargetLowerer {
 public:
  DriveTargetLowerer(
      ProcessLowerer& proc, const slang::ast::Expression& clockvar,
      hir::Expr written)
      : proc_(&proc), clockvar_(&clockvar), written_(std::move(written)) {
  }

  auto LowerExpr(const slang::ast::Expression& expr, WalkFrame frame)
      -> diag::Result<hir::Expr>;
  [[nodiscard]] auto Owner() -> UnitLowerer& {
    return proc_->Owner();
  }

 private:
  ProcessLowerer* proc_;
  const slang::ast::Expression* clockvar_;
  hir::Expr written_;
};

}  // namespace lyra::lowering::ast_to_hir
