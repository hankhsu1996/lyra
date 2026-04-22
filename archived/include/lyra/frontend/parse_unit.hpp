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

// Build a parse plan from ordered input files and compilation-unit policy.
// The order of input files is semantically significant: in SingleUnit mode,
// preprocessing state flows forward through this exact order.
auto BuildParsePlan(
    const std::vector<std::string>& files, CompilationUnitMode mode)
    -> ParsePlan;

// Execute a single parse unit, adding resulting syntax tree(s) to the
// compilation. Returns false on error.
auto ExecuteParseUnit(
    const ParseUnit& unit, slang::SourceManager& source_manager,
    slang::ast::Compilation& compilation, const slang::Bag& options) -> bool;

}  // namespace lyra::frontend
