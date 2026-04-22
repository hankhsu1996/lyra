#include "lyra/frontend/parse_unit.hpp"

#include <filesystem>
#include <string>
#include <variant>
#include <vector>

#include <fmt/color.h>
#include <fmt/core.h>
#include <slang/syntax/SyntaxTree.h>

#include "lyra/common/internal_error.hpp"

namespace lyra::frontend {

namespace {
constexpr auto kToolColor = fmt::terminal_color::white;
constexpr auto kToolStyle = fmt::fg(kToolColor) | fmt::emphasis::bold;

void ReportError(std::string_view message) {
  fmt::print(
      stderr, "{}: {}: {}\n", fmt::styled("lyra", kToolStyle),
      fmt::styled("error", fmt::fg(fmt::terminal_color::bright_red)),
      fmt::styled(message, fmt::emphasis::bold));
}
}  // namespace

auto BuildParsePlan(
    const std::vector<std::string>& files, CompilationUnitMode mode)
    -> ParsePlan {
  ParsePlan plan;

  switch (mode) {
    case CompilationUnitMode::kPerFile:
      plan.units.reserve(files.size());
      for (const auto& file : files) {
        plan.units.emplace_back(PerFileUnit{.file = file});
      }
      break;

    case CompilationUnitMode::kSingleUnit:
      plan.units.emplace_back(SingleUnit{.files = files});
      break;
  }

  return plan;
}

auto ExecuteParseUnit(
    const ParseUnit& unit, slang::SourceManager& source_manager,
    slang::ast::Compilation& compilation, const slang::Bag& options) -> bool {
  return std::visit(
      [&](const auto& u) -> bool {
        using T = std::decay_t<decltype(u)>;

        if constexpr (std::is_same_v<T, PerFileUnit>) {
          auto result = slang::syntax::SyntaxTree::fromFile(
              u.file, source_manager, options);
          if (!result) {
            ReportError(fmt::format("cannot read '{}'", u.file));
            return false;
          }
          compilation.addSyntaxTree(result.value());
          return true;

        } else {
          static_assert(std::is_same_v<T, SingleUnit>);
          if (u.files.empty()) {
            throw common::InternalError(
                "ExecuteParseUnit", "single-unit parse unit contains no files");
          }

          std::vector<slang::SourceBuffer> buffers;
          buffers.reserve(u.files.size());

          for (const auto& file : u.files) {
            auto buffer =
                source_manager.readSource(std::filesystem::path(file), nullptr);
            if (!buffer) {
              ReportError(fmt::format("cannot read '{}'", file));
              return false;
            }
            buffers.push_back(*buffer);
          }

          auto tree = slang::syntax::SyntaxTree::fromBuffers(
              buffers, source_manager, options);
          compilation.addSyntaxTree(tree);
          return true;
        }
      },
      unit);
}

}  // namespace lyra::frontend
