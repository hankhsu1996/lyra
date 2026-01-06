#pragma once

#include <sstream>
#include <string>

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

  std::ostringstream out_;
  int indent_ = 0;

  void Indent();
  void Line(const std::string& text);
};

}  // namespace lyra::compiler
