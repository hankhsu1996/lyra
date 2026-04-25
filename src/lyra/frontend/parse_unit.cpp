#include "parse_unit.hpp"

#include <filesystem>
#include <string>
#include <type_traits>
#include <variant>
#include <vector>

#include <fmt/core.h>
#include <slang/syntax/SyntaxTree.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/diag/slang_source_mapper.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/support/internal_error.hpp"

namespace lyra::frontend {

namespace {

void RegisterBuffer(
    const slang::SourceBuffer& buffer, const std::filesystem::path& path,
    diag::SourceManager& diag_sources, diag::SlangSourceMapper& source_mapper) {
  if (source_mapper.Contains(buffer.id)) {
    return;
  }
  diag::FileId fid =
      diag_sources.AddFile(path.string(), std::string(buffer.data));
  source_mapper.Register(buffer.id, fid);
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
    slang::ast::Compilation& compilation, const slang::Bag& options,
    diag::SourceManager& diag_sources, diag::SlangSourceMapper& source_mapper,
    diag::DiagnosticSink& sink) -> bool {
  return std::visit(
      [&](const auto& u) -> bool {
        using T = std::decay_t<decltype(u)>;

        if constexpr (std::is_same_v<T, PerFileUnit>) {
          const std::filesystem::path path(u.file);
          auto buffer_or = source_manager.readSource(path, nullptr);
          if (!buffer_or) {
            sink.Report(
                diag::Diagnostic::HostError(
                    fmt::format("cannot read '{}'", u.file)));
            return false;
          }
          RegisterBuffer(*buffer_or, path, diag_sources, source_mapper);

          auto tree = slang::syntax::SyntaxTree::fromBuffer(
              *buffer_or, source_manager, options);
          compilation.addSyntaxTree(tree);
          return true;
        } else {
          static_assert(std::is_same_v<T, SingleUnit>);
          if (u.files.empty()) {
            throw support::InternalError(
                "ExecuteParseUnit: single-unit parse unit contains no files");
          }

          std::vector<slang::SourceBuffer> buffers;
          buffers.reserve(u.files.size());

          for (const auto& file : u.files) {
            const std::filesystem::path path(file);
            auto buffer_or = source_manager.readSource(path, nullptr);
            if (!buffer_or) {
              sink.Report(
                  diag::Diagnostic::HostError(
                      fmt::format("cannot read '{}'", file)));
              return false;
            }
            RegisterBuffer(*buffer_or, path, diag_sources, source_mapper);
            buffers.push_back(*buffer_or);
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
