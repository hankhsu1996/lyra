#pragma once

#include <memory>
#include <string>
#include <vector>

#include "lyra/common/indent.hpp"
#include "lyra/common/variable.hpp"
#include "lyra/mir/expression.hpp"
#include "lyra/mir/process.hpp"

namespace lyra::mir {

// Module-level variable with optional initializer
struct ModuleVariable {
  common::Variable variable;
  std::unique_ptr<Expression> initializer;  // nullptr if no initializer
};

class Module {
 public:
  std::string name;
  std::vector<ModuleVariable> variables;
  std::vector<std::shared_ptr<Process>> processes;

  [[nodiscard]] auto ToString(int indent = 0) const -> std::string {
    std::string result = common::Indent(indent) + "module " + name + "\n";

    if (!variables.empty()) {
      for (const auto& mod_var : variables) {
        result += common::Indent(indent + 1) + "var " +
                  std::string(mod_var.variable.symbol->name);
        if (mod_var.initializer) {
          result += " = " + mod_var.initializer->ToString();
        }
        result += "\n";
      }
      result += "\n";
    }

    for (const auto& process : processes) {
      result += process->ToString(indent + 1) + "\n";
    }

    return result;
  }
};

}  // namespace lyra::mir
