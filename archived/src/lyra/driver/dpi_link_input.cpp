#include "dpi_link_input.hpp"

#include <filesystem>
#include <format>
#include <set>
#include <span>
#include <utility>
#include <vector>

#include "lyra/common/diagnostic/diagnostic.hpp"

namespace lyra::driver {

auto ValidateDpiLinkInputs(std::span<const std::filesystem::path> raw_inputs)
    -> lyra::Result<std::vector<std::filesystem::path>> {
  std::vector<std::filesystem::path> out;
  std::set<std::filesystem::path> seen;

  for (const auto& raw : raw_inputs) {
    auto abs = std::filesystem::absolute(raw);

    if (!std::filesystem::exists(abs)) {
      return std::unexpected(
          Diagnostic::HostError(
              std::format("DPI link input not found: '{}'", abs.string())));
    }
    if (!std::filesystem::is_regular_file(abs)) {
      return std::unexpected(
          Diagnostic::HostError(
              std::format("DPI link input is not a file: '{}'", abs.string())));
    }
    if (seen.insert(abs).second) {
      out.push_back(std::move(abs));
    }
  }

  return out;
}

}  // namespace lyra::driver
