#pragma once

#include <cstdint>
#include <memory>
#include <optional>
#include <ostream>
#include <stdexcept>
#include <string>
#include <vector>

#include <fmt/core.h>

#include "lyra/common/indent.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/timescale.hpp"
#include "lyra/common/variable.hpp"
#include "lyra/lir/process.hpp"

namespace lyra::lir {

/// A sequence of instructions that evaluates an expression and produces a
/// value. Used by $monitor to re-evaluate arguments at each time slot.
///
/// Note: This is $monitor-specific. The interpreter re-executes these LIR
/// instructions at each time slot to detect value changes. The codegen backend
/// uses a different approach (C++ lambdas) and ignores these blocks.
struct MonitorExpressionBlock {
  std::vector<Instruction> instructions;
  TempRef result;  // Temp holding final value
};

// Port direction for module interfaces
enum class PortDirection { kInput, kOutput, kInout };

// Module port declaration
struct Port {
  common::Variable variable;
  PortDirection direction;
};

// Output port binding: maps child's output port to parent's signal storage.
// Child writes to output port → actually writes to parent's signal.
struct OutputBinding {
  std::string port_name;     // Formal port name in child module
  common::SymbolRef signal;  // Parent signal symbol reference
};

// Submodule instantiation
struct SubmoduleInstance {
  common::SymbolRef instance_symbol;  // Instance symbol (for interpreter)
  std::string instance_name;          // e.g., "counter1" (for codegen)
  std::string module_type;            // e.g., "Counter"
  std::vector<OutputBinding> output_bindings;  // Output port → parent signal
};

struct Module {
  std::string name;
  std::optional<common::TimeScale> timescale;
  int8_t global_precision_power = common::TimeScale::kDefaultPrecisionPower;
  std::vector<Port> ports;
  std::vector<common::Variable> variables;
  std::vector<SubmoduleInstance> submodules;
  std::vector<std::shared_ptr<Process>> processes;
  std::shared_ptr<LirContext> context;

  // Expression blocks for $monitor re-evaluation
  std::vector<MonitorExpressionBlock> monitor_expression_blocks;

  /// Get a monitor expression block by index with bounds checking.
  [[nodiscard]] auto GetMonitorExpressionBlock(size_t index) const
      -> const MonitorExpressionBlock& {
    if (index >= monitor_expression_blocks.size()) {
      throw std::out_of_range(
          fmt::format(
              "monitor expression block index {} out of range (size: {})",
              index, monitor_expression_blocks.size()));
    }
    return monitor_expression_blocks[index];
  }

  [[nodiscard]] auto ToString(
      common::FormatMode mode = common::FormatMode::kPlain,
      int indentation_level = 0) const -> std::string {
    std::string out;

    // Module header
    if (mode == common::FormatMode::kContextual) {
      out +=
          fmt::format("{}Module {}\n", common::Indent(indentation_level), name);
    } else {
      out += fmt::format("Module {}\n", name);
    }

    // Ports list
    if (!ports.empty()) {
      if (mode == common::FormatMode::kContextual) {
        out += fmt::format("{}Ports:\n", common::Indent(indentation_level + 1));
      } else {
        out += "Ports:\n";
      }
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
        if (mode == common::FormatMode::kContextual) {
          out += fmt::format(
              "{}{} {}\n", common::Indent(indentation_level + 2), dir_str,
              port.variable.symbol->name);
        } else {
          out += fmt::format("  {} {}\n", dir_str, port.variable.symbol->name);
        }
      }
    }

    // Variables list
    if (mode == common::FormatMode::kContextual) {
      out +=
          fmt::format("{}Variables: ", common::Indent(indentation_level + 1));
    } else {
      out += fmt::format("Variables: ");
    }

    for (const auto& var : variables) {
      out += fmt::format("{} ", var.symbol->name);
    }

    out += "\n";

    // Submodules list
    if (!submodules.empty()) {
      if (mode == common::FormatMode::kContextual) {
        out += fmt::format(
            "{}Submodules:\n", common::Indent(indentation_level + 1));
      } else {
        out += "Submodules:\n";
      }
      for (const auto& submod : submodules) {
        if (mode == common::FormatMode::kContextual) {
          out += fmt::format(
              "{}{} {} (", common::Indent(indentation_level + 2),
              submod.module_type, submod.instance_name);
        } else {
          out += fmt::format(
              "  {} {} (", submod.module_type, submod.instance_name);
        }
        bool first = true;
        for (const auto& binding : submod.output_bindings) {
          if (!first) {
            out += ", ";
          }
          first = false;
          out += fmt::format(
              ".{}(0x{:x})", binding.port_name,
              reinterpret_cast<std::uintptr_t>(binding.signal));
        }
        out += ")\n";
      }
    }

    // Processes
    for (const auto& process : processes) {
      if (process) {
        out += process->ToString(mode, indentation_level + 1);
      }
    }

    return out;
  }
};

inline auto operator<<(std::ostream& os, const Module& module)
    -> std::ostream& {
  return os << module.ToString(common::FormatMode::kContextual);
}

}  // namespace lyra::lir

template <>
struct fmt::formatter<lyra::lir::Module> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(const lyra::lir::Module& module, FormatContext& ctx) const {
    return fmt::format_to(
        ctx.out(), "{}",
        module.ToString(lyra::common::FormatMode::kContextual));
  }
};
