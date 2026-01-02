#pragma once

#include <sstream>
#include <string>

#include "lyra/mir/module.hpp"

namespace lyra::codegen {

class CppCodegen {
 public:
  auto Generate(const mir::Module& module) -> std::string;

 private:
  void EmitHeader();
  void EmitClass(const mir::Module& module);
  void EmitVariables(const std::vector<common::Variable>& variables);
  void EmitProcess(const mir::Process& process);
  void EmitStatement(const mir::Statement& stmt);
  void EmitExpression(const mir::Expression& expr);

  std::ostringstream out_;
  int indent_ = 0;

  void Indent();
  void Line(const std::string& text);
};

}  // namespace lyra::codegen
