#pragma once

#include <memory>
#include <ostream>
#include <string>
#include <vector>

#include <fmt/core.h>

#include "lir/process.hpp"

namespace lyra::lir {

struct Module {
  std::string name;
  std::vector<std::string> signals;
  std::vector<std::shared_ptr<Process>> processes;

  [[nodiscard]] auto ToString() const -> std::string {
    std::string out;
    out += fmt::format("Module {}\n", name);

    out += "  Signals: ";
    for (const auto& signal : signals) {
      out += fmt::format("{} ", signal);
    }

    out += "\n";
    for (const auto& process : processes) {
      if (process) {
        out += process->ToString();
      }
    }

    return out;
  }
};

inline auto operator<<(std::ostream& os, const Module& module)
    -> std::ostream& {
  return os << module.ToString();
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
    return fmt::format_to(ctx.out(), "{}", module.ToString());
  }
};
