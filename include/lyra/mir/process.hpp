#pragma once

#include <memory>

#include "lyra/common/formatting.hpp"
#include "lyra/common/variable.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::mir {

enum class ProcessKind { kInitial, kAlways };

inline auto ToString(ProcessKind kind) -> std::string {
  switch (kind) {
    case ProcessKind::kInitial:
      return "initial";
    case ProcessKind::kAlways:
      return "always";
  }
}

class Process {
 public:
  std::string name;
  ProcessKind process_kind;
  std::vector<common::Variable> variables;
  std::unique_ptr<Statement> body;

  [[nodiscard]] auto ToString(int indent = 0) const -> std::string {
    std::string result = common::Indent(indent) + "process " + name + " " +
                         mir::ToString(process_kind) + "\n";

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
