#pragma once

#include <string>
#include <variant>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/text/SourceManager.h>
#include <slang/util/Bag.h>

namespace lyra::frontend {

enum class CompilationUnitMode {
  kPerFile,
  kSingleUnit,
};

struct PerFileUnit {
  std::string file;
};

struct SingleUnit {
  std::vector<std::string> files;
};

using ParseUnit = std::variant<PerFileUnit, SingleUnit>;

struct ParsePlan {
  std::vector<ParseUnit> units;
};

auto BuildParsePlan(
    const std::vector<std::string>& files, CompilationUnitMode mode)
    -> ParsePlan;

auto ExecuteParseUnit(
    const ParseUnit& unit, slang::SourceManager& source_manager,
    slang::ast::Compilation& compilation, const slang::Bag& options) -> bool;

}  // namespace lyra::frontend
