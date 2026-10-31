#pragma once

#include <optional>
#include <vector>

#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/class_shape.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {
class CompilationUnit;
}  // namespace lyra::mir

namespace lyra::lowering::hir_to_mir {

class UnitLowerer;

// A function of a class the lowering writes itself rather than lowers from a
// subroutine the source declared, in the convention every function of the
// program keeps: what its form takes ahead of its formals, its formals, where
// to report what it reads (LRM 9.4.2), and a completion carrying what it
// answers (LRM 13.4). A caller reaching it reaches it as it reaches any.
class SynthesizedFunction {
 public:
  SynthesizedFunction(
      UnitLowerer& unit_lowerer, const WalkFrame& owner,
      const ClassShape& shape, CallableForm form);

  SynthesizedFunction(const SynthesizedFunction&) = delete;
  auto operator=(const SynthesizedFunction&) -> SynthesizedFunction& = delete;
  SynthesizedFunction(SynthesizedFunction&&) = delete;
  auto operator=(SynthesizedFunction&&) -> SynthesizedFunction& = delete;
  ~SynthesizedFunction() = default;

  // A formal of `type`, after every one added before it.
  auto AddFormal(mir::TypeId type) -> mir::LocalId;

  // The same for a formal already declared among the body's bindings, under
  // the identity of the variable it stands for.
  void AddFormalLocal(mir::LocalId local);

  [[nodiscard]] auto Frame() const -> const WalkFrame& {
    return frame_;
  }

  [[nodiscard]] auto Bindings() -> CallableBindings& {
    return bindings_;
  }

  [[nodiscard]] auto Body() const -> mir::Builder {
    return frame_.Builder();
  }

  // The function, its body ending in the return of `result`, a value in the
  // body's block, or of nothing where it answers nothing.
  [[nodiscard]] auto Finish(std::optional<mir::ExprId> result)
      -> mir::CallableCode;

 private:
  UnitLowerer* unit_lowerer_;
  mir::CallableCode code_ = mir::CallableCode::Defined(diag::SourceSpan{});
  CallableBindings bindings_;
  WalkFrame frame_;
  std::vector<mir::LocalId> params_;
};

// `callee` called as a function in that convention, in the block `frame` is
// writing: `arguments`, each already bound as its formal takes it, then the
// report the frame hands on. It answers what the function answers, read out of
// its completion, or the call where it answers nothing.
[[nodiscard]] auto BuildFunctionCall(
    mir::CompilationUnit& unit, const WalkFrame& frame, mir::Callee callee,
    std::vector<mir::ExprId> arguments, std::optional<mir::TypeId> result)
    -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
