#include "lyra/interpreter/system_call/math.hpp"

#include <cassert>
#include <cmath>
#include <string_view>
#include <utility>

#include "lyra/common/system_function.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/interpreter/system_call/context.hpp"
#include "lyra/interpreter/temp_table.hpp"
#include "lyra/lir/instruction.hpp"

namespace lyra::interpreter {

namespace {

/// Execute unary math function (real -> real)
auto ExecuteMathUnary(std::string_view name, double arg) -> double {
  if (name == "$ln") {
    return std::log(arg);
  }
  if (name == "$log10") {
    return std::log10(arg);
  }
  if (name == "$exp") {
    return std::exp(arg);
  }
  if (name == "$sqrt") {
    return std::sqrt(arg);
  }
  if (name == "$floor") {
    return std::floor(arg);
  }
  if (name == "$ceil") {
    return std::ceil(arg);
  }
  if (name == "$sin") {
    return std::sin(arg);
  }
  if (name == "$cos") {
    return std::cos(arg);
  }
  if (name == "$tan") {
    return std::tan(arg);
  }
  if (name == "$asin") {
    return std::asin(arg);
  }
  if (name == "$acos") {
    return std::acos(arg);
  }
  if (name == "$atan") {
    return std::atan(arg);
  }
  if (name == "$sinh") {
    return std::sinh(arg);
  }
  if (name == "$cosh") {
    return std::cosh(arg);
  }
  if (name == "$tanh") {
    return std::tanh(arg);
  }
  if (name == "$asinh") {
    return std::asinh(arg);
  }
  if (name == "$acosh") {
    return std::acosh(arg);
  }
  if (name == "$atanh") {
    return std::atanh(arg);
  }
  std::unreachable();
}

/// Execute binary math function (real, real -> real)
auto ExecuteMathBinary(std::string_view name, double a, double b) -> double {
  if (name == "$pow") {
    return std::pow(a, b);
  }
  if (name == "$atan2") {
    return std::atan2(a, b);
  }
  if (name == "$hypot") {
    return std::hypot(a, b);
  }
  std::unreachable();
}

}  // namespace

auto HandleMathCalls(const lir::Instruction& instr, SystemCallContext& ctx)
    -> InstructionResult {
  using Category = common::SystemFunctionCategory;
  const auto* func_info = common::FindSystemFunction(instr.system_call_name);
  assert(func_info != nullptr);

  if (func_info->category == Category::kMathUnary) {
    assert(instr.result.has_value());
    assert(instr.operands.size() == 1);
    double arg = ctx.GetOperandValue(instr.operands[0]).AsDouble();
    double result = ExecuteMathUnary(instr.system_call_name, arg);
    ctx.GetTempTable().Write(instr.result.value(), RuntimeValue::Real(result));
    return InstructionResult::Continue();
  }

  if (func_info->category == Category::kMathBinary) {
    assert(instr.result.has_value());
    assert(instr.operands.size() == 2);
    double a = ctx.GetOperandValue(instr.operands[0]).AsDouble();
    double b = ctx.GetOperandValue(instr.operands[1]).AsDouble();
    double result = ExecuteMathBinary(instr.system_call_name, a, b);
    ctx.GetTempTable().Write(instr.result.value(), RuntimeValue::Real(result));
    return InstructionResult::Continue();
  }

  std::unreachable();
}

}  // namespace lyra::interpreter
