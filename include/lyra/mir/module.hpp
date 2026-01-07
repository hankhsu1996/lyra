#pragma once

#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "lyra/common/indent.hpp"
#include "lyra/common/timescale.hpp"
#include "lyra/common/variable.hpp"
#include "lyra/mir/expression.hpp"
#include "lyra/mir/process.hpp"

namespace lyra::mir {

// Port direction for module interfaces
enum class PortDirection { kInput, kOutput, kInout };

// Module port declaration
struct Port {
  common::Variable variable;
  PortDirection direction = PortDirection::kInput;
};

// Output port binding: maps child's output port to parent's signal storage.
// Child writes to output port → actually writes to parent's signal.
// (Input ports use driver processes instead, no binding needed.)
struct OutputBinding {
  std::string port_name;               // Formal port name in child module
  std::unique_ptr<Expression> signal;  // Parent signal that child writes to
};

// Submodule instantiation
struct SubmoduleInstance {
  std::string instance_name;                   // e.g., "counter1"
  std::string module_type;                     // e.g., "Counter"
  std::vector<OutputBinding> output_bindings;  // Output port → parent signal
};

// Module-level variable with optional initializer
struct ModuleVariable {
  common::Variable variable;
  std::unique_ptr<Expression> initializer;  // nullptr if no initializer
};

class Module {
 public:
  std::string name;
  std::optional<common::TimeScale> timescale;
  std::vector<Port> ports;
  std::vector<ModuleVariable> variables;
  std::vector<SubmoduleInstance> submodules;
  std::vector<std::shared_ptr<Process>> processes;

  [[nodiscard]] auto ToString(int indent = 0) const -> std::string {
    std::string result = common::Indent(indent) + "module " + name;
    if (timescale) {
      result += " [timescale: " + timescale->ToString() + "]";
    }
    result += "\n";

    // Print ports
    if (!ports.empty()) {
      for (const auto& port : ports) {
        std::string dir_str;
        switch (port.direction) {
          case PortDirection::kInput:
            dir_str = "input";
            break;
          case PortDirection::kOutput:
            dir_str = "output";
            break;
          case PortDirection::kInout:
            dir_str = "inout";
            break;
        }
        result += common::Indent(indent + 1) + dir_str + " " +
                  std::string(port.variable.symbol->name) + "\n";
      }
      result += "\n";
    }

    // Print variables
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

    // Print submodule instances
    if (!submodules.empty()) {
      for (const auto& submod : submodules) {
        result += common::Indent(indent + 1) + submod.module_type + " " +
                  submod.instance_name + "(";
        bool first = true;
        for (const auto& binding : submod.output_bindings) {
          if (!first) {
            result += ", ";
          }
          first = false;
          result +=
              "." + binding.port_name + "(" + binding.signal->ToString() + ")";
        }
        result += ")\n";
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
