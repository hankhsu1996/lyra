#pragma once

#include <cstddef>
#include <string>

#include "lyra/backend/cpp/render_context.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::backend::cpp {

auto RenderNestedProceduralScope(
    const RenderContext& parent, const mir::ProceduralScope& scope,
    std::size_t indent) -> diag::Result<std::string>;

auto RenderProceduralScopeStatements(
    const RenderContext& ctx, std::size_t indent) -> diag::Result<std::string>;

auto RenderStmt(
    const RenderContext& ctx, const mir::Stmt& stmt, std::size_t indent)
    -> diag::Result<std::string>;

}  // namespace lyra::backend::cpp
