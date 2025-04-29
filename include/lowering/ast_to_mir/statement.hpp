#pragma once

#include <memory>
#include <optional>
#include <vector>

#include "common/timing_control.hpp"

namespace slang::ast {
class Statement;
class TimingControl;
}  // namespace slang::ast

namespace lyra::mir {
class Statement;
}  // namespace lyra::mir

namespace lyra::lowering {

// Lowers a slang AST TimingControl into a list of MIR Statements.
auto LowerTimingControl(const slang::ast::TimingControl& timing_control)
    -> common::TimingControl;

// Lowers a slang AST Statement into a list of MIR Statements.
auto LowerStatement(
    const slang::ast::Statement& statement,
    std::optional<common::TimingControl> timing = std::nullopt)
    -> std::vector<std::shared_ptr<mir::Statement>>;

}  // namespace lyra::lowering
