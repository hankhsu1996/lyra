#pragma once

#include <memory>

#include "lyra/common/indent.hpp"
#include "lyra/common/variable.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::mir {

class Process {
 public:
  std::string name;
  std::vector<common::Variable> variables;
  std::unique_ptr<Statement> body;

  [[nodiscard]] auto ToString(int indent = 0) const -> std::string {
    std::string result = common::Indent(indent) + "process " + name + "\n";

    for (const auto& var : variables) {
      result += common::Indent(indent + 1) + "var " +
                std::string(var.symbol->name) + "\n";
    }

    if (body) {
      result += body->ToString(indent + 1);
    }

    return result;
  }
};

}  // namespace lyra::mir
