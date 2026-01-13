#pragma once

#include <algorithm>
#include <cstdint>
#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <vector>

#include <fmt/core.h>

#include "lyra/common/indent.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/timescale.hpp"
#include "lyra/common/variable.hpp"
#include "lyra/lir/process.hpp"

namespace lyra::lir {

struct Module;  // Forward declaration for SubmoduleInstance::child_module

// Port direction for module interfaces
enum class PortDirection { kInput, kOutput, kInout };

// Module port declaration
struct Port {
  common::Variable variable;
  PortDirection direction{};
};

// Output port binding: maps child's output port to parent's signal storage.
// Child writes to output port -> actually writes to parent's signal.
struct OutputBinding {
  std::string port_name;  // Formal port name in child
  common::SymbolId signal{common::kInvalidSymbolId};  // Parent signal symbol
};

// Submodule instantiation
struct SubmoduleInstance {
  common::SymbolId instance_symbol{
      common::kInvalidSymbolId};         // For interpreter
  std::string instance_name;             // e.g., "counter1" (for codegen)
  std::string module_type;               // e.g., "Counter"
  std::string module_signature;          // e.g., "Counter<8>" (for linking)
  const Module* child_module = nullptr;  // Resolved during linking
  std::vector<OutputBinding> output_bindings;  // Output port -> parent signal
};

/// Function parameter for user-defined functions.
/// Wrapped in a struct (rather than using Variable directly) for extensibility:
/// future support for output/inout/ref arguments will require additional fields
/// like direction, pass-by-reference semantics, etc.
struct FunctionParameter {
  common::Variable variable;
};

// Function definition (user-defined functions)
struct Function {
  std::string name;
  common::Type return_type;
  std::vector<FunctionParameter> parameters;
  std::vector<common::Variable> local_variables;
  std::vector<std::unique_ptr<BasicBlock>> blocks;
  std::optional<LabelRef> entry_label;  // First block's label

  [[nodiscard]] auto FindBlockIndexByLabel(LabelRef label) const
      -> std::optional<size_t> {
    for (size_t i = 0; i < blocks.size(); ++i) {
      if (blocks[i]->label == label) {
        return i;
      }
    }
    return std::nullopt;
  }

  [[nodiscard]] auto ToString(
      common::FormatMode mode = common::FormatMode::kPlain,
      int indentation_level = 0) const -> std::string {
    std::string out;
    std::string indent = (mode == common::FormatMode::kContextual)
                             ? common::Indent(indentation_level)
                             : "";

    // Function header
    out += fmt::format("{}function {}(", indent, name);
    for (size_t i = 0; i < parameters.size(); ++i) {
      if (i > 0) {
        out += ", ";
      }
      out += fmt::format("sym#{}", parameters[i].variable.symbol);
    }
    out += ")\n";

    // Local variables
    if (!local_variables.empty()) {
      out += fmt::format("{}  locals:", indent);
      for (const auto& local : local_variables) {
        out += fmt::format(" sym#{}", local.symbol);
      }
      out += "\n";
    }

    // Basic blocks
    for (const auto& block : blocks) {
      if (block) {
        out += block->ToString(mode, indentation_level + 1);
      }
    }

    return out;
  }
};

struct Module {
  std::string name;
  std::string signature;  // e.g., "Counter<8>" (for linking/deduplication)
  common::SymbolId instance_symbol{
      common::kInvalidSymbolId};  // Instance symbol
  std::optional<common::TimeScale> timescale;
  int8_t global_precision_power = common::TimeScale::kDefaultPrecisionPower;
  std::vector<Port> ports;
  std::vector<common::Variable> variables;
  std::vector<Function> functions;
  std::vector<SubmoduleInstance> submodules;
  std::vector<std::shared_ptr<Process>> processes;
  std::shared_ptr<LirContext> context;

  /// Find a function by name. Returns nullptr if not found.
  [[nodiscard]] auto FindFunction(std::string_view name_view) const
      -> const Function* {
    auto it = std::ranges::find_if(
        functions, [name_view](const auto& f) { return f.name == name_view; });
    return it != functions.end() ? &*it : nullptr;
  }

  /// Find a process by name. Returns nullptr if not found.
  [[nodiscard]] auto FindProcess(std::string_view name_view) const
      -> std::shared_ptr<Process> {
    auto it = std::ranges::find_if(
        processes, [name_view](const auto& p) { return p->name == name_view; });
    return it != processes.end() ? *it : nullptr;
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
              "{}{} sym#{}\n", common::Indent(indentation_level + 2), dir_str,
              port.variable.symbol);
        } else {
          out += fmt::format("  {} sym#{}\n", dir_str, port.variable.symbol);
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
      out += fmt::format("sym#{} ", var.symbol);
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
          out += fmt::format(".{}(sym#{})", binding.port_name, binding.signal);
        }
        out += ")\n";
      }
    }

    // Functions
    for (const auto& func : functions) {
      out += func.ToString(mode, indentation_level + 1);
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
