#include <cstddef>

#include "lyra/compiler/codegen/codegen.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::compiler {

void Codegen::EmitWhileLoop(const mir::WhileStatement& while_stmt) {
  Indent();
  out_ << "while (";
  EmitExpression(*while_stmt.condition);
  out_ << ") {\n";
  indent_++;
  EmitStatement(*while_stmt.body);
  indent_--;
  Indent();
  out_ << "}\n";
}

void Codegen::EmitDoWhileLoop(const mir::DoWhileStatement& do_while) {
  Indent();
  out_ << "do {\n";
  indent_++;
  EmitStatement(*do_while.body);
  indent_--;
  Indent();
  out_ << "} while (";
  EmitExpression(*do_while.condition);
  out_ << ");\n";
}

void Codegen::EmitForLoop(const mir::ForStatement& for_stmt) {
  // Emit initializers (variable declarations must be outside for loop in C++)
  for (const auto& init : for_stmt.initializers) {
    EmitStatement(*init);
  }
  Indent();
  out_ << "for (; ";
  if (for_stmt.condition) {
    EmitExpression(*for_stmt.condition);
  }
  out_ << "; ";
  for (size_t i = 0; i < for_stmt.steps.size(); ++i) {
    if (i > 0) {
      out_ << ", ";
    }
    EmitExpression(*for_stmt.steps[i]);
  }
  out_ << ") {\n";
  indent_++;
  EmitStatement(*for_stmt.body);
  indent_--;
  Indent();
  out_ << "}\n";
}

void Codegen::EmitRepeatLoop(const mir::RepeatStatement& repeat_stmt) {
  Indent();
  out_ << "for (int _repeat_i = static_cast<int>(";
  EmitExpression(*repeat_stmt.count);
  out_ << "); _repeat_i > 0; --_repeat_i) {\n";
  indent_++;
  EmitStatement(*repeat_stmt.body);
  indent_--;
  Indent();
  out_ << "}\n";
}

}  // namespace lyra::compiler
