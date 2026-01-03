#pragma once

#include <memory>
#include <string>
#include <vector>

#include "lyra/common/formatting.hpp"
#include "lyra/common/variable.hpp"
#include "lyra/mir/process.hpp"

namespace lyra::mir {

class Module {
 public:
  std::string name;
  std::vector<common::Variable> variables;
  std::vector<std::shared_ptr<Process>> processes;

  [[nodiscard]] auto ToString(int indent = 0) const -> std::string {
    std::string result = common::Indent(indent) + "module " + name + "\n";

    if (!variables.empty()) {
      for (const auto& var : variables) {
        result += common::Indent(indent + 1) + "var " +
                  std::string(var.symbol->name) + "\n";
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
