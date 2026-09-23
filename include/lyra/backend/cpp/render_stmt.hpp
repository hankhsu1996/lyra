#pragma once

#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::backend::cpp {

void RenderNestedBlock(
    const ScopeView& parent, const mir::Block& block, TargetText& out);

void RenderBlockStatements(const ScopeView& view, TargetText& out);

void RenderStmt(const ScopeView& view, const mir::Stmt& stmt, TargetText& out);

}  // namespace lyra::backend::cpp
