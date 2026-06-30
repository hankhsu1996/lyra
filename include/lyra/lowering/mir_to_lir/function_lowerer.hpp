#pragma once

#include <optional>
#include <string>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/class_id.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/lowering/mir_to_lir/unit_lowerer.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {
struct Block;
struct Stmt;
}  // namespace lyra::mir

namespace lyra::lowering::mir_to_lir {

// Lowers one MIR callable body (a method's code or a class constructor) into a
// LIR function. The body is flattened: each MIR expression node becomes one LIR
// instruction defining a temporary, and the parent reads the child temporaries
// as operands.
class FunctionLowerer {
 public:
  FunctionLowerer(
      UnitLowerer& unit, const mir::Block& body,
      std::vector<mir::LocalId> params, std::string name,
      mir::TypeId result_type, lir::ClassId current_class);

  auto Run() -> diag::Result<lir::Function>;

 private:
  auto LowerStmtInto(const mir::Stmt& stmt) -> diag::Result<void>;
  auto LowerExpr(mir::ExprId id) -> diag::Result<lir::Operand>;
  auto Emit(lir::TypeId type, lir::InstrData data) -> lir::Operand;

  UnitLowerer* unit_;
  const mir::Block* body_;
  std::vector<mir::LocalId> params_;
  std::string name_;
  mir::TypeId result_type_;
  lir::ClassId current_class_;
  lir::Function fn_;
  lir::BasicBlock block_;
  std::vector<std::optional<lir::ValueId>> local_to_value_;
  bool terminated_ = false;
};

}  // namespace lyra::lowering::mir_to_lir
