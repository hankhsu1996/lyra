#pragma once

#include <cstdint>
#include <optional>
#include <sstream>
#include <string>

#include "lyra/common/timescale.hpp"
#include "lyra/mir/expression.hpp"
#include "lyra/mir/module.hpp"

namespace lyra::compiler {

class Codegen {
 public:
  auto Generate(const mir::Module& module) -> std::string;

 private:
  void EmitHeader();
  void EmitClass(const mir::Module& module);
  void EmitVariables(const std::vector<mir::ModuleVariable>& variables);
  void EmitProcess(const mir::Process& process);
  void EmitStatement(const mir::Statement& stmt);
  void EmitConditional(const mir::ConditionalStatement& cond, bool is_else_if);
  void EmitExpression(const mir::Expression& expr, int parent_prec = 0);
  void EmitAssignmentTarget(const mir::AssignmentTarget& target);
  void EmitPackedBitPosition(
      const mir::Expression& index_expr, int32_t lower_bound,
      size_t element_width);
  void EmitSliceShift(
      const mir::Expression& start_expr, int32_t lower_bound,
      int32_t width_offset);

  std::ostringstream out_;
  int indent_ = 0;

  // Timescale info for delay scaling
  std::optional<common::TimeScale> timescale_;
  int8_t global_precision_power_ = common::TimeScale::kDefaultPrecisionPower;

  [[nodiscard]] auto DelayMultiplier() const -> uint64_t;

  void Indent();
  void Line(const std::string& text);
};

}  // namespace lyra::compiler
