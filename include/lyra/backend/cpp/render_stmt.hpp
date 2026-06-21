#pragma once

#include <cstddef>
#include <string>

#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::backend::cpp {

auto RenderNestedBlock(
    const ScopeView& parent, const mir::Block& block, std::size_t indent)
    -> diag::Result<std::string>;

auto RenderBlockStatements(const ScopeView& view, std::size_t indent)
    -> diag::Result<std::string>;

auto RenderStmt(
    const ScopeView& view, const mir::Stmt& stmt, std::size_t indent)
    -> diag::Result<std::string>;

}  // namespace lyra::backend::cpp
