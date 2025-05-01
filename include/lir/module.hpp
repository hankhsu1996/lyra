#pragma once

#include <memory>
#include <ostream>
#include <string>
#include <vector>

#include <fmt/core.h>

#include "common/formatting.hpp"
#include "lir/process.hpp"

namespace lyra::lir {

struct Module {
  std::string name;
  std::vector<std::string> signals;
  std::vector<std::shared_ptr<Process>> processes;

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

    // Signals list
    if (mode == common::FormatMode::kContextual) {
      out += fmt::format("{}Signals: ", common::Indent(indentation_level + 1));
    } else {
      out += fmt::format("Signals: ");
    }

    for (const auto& signal : signals) {
      out += fmt::format("{} ", signal);
    }

    out += "\n";

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
