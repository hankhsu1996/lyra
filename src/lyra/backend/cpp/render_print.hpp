#pragma once

#include <cstddef>
#include <string>

#include "lyra/mir/stmt.hpp"
#include "render_context.hpp"

namespace lyra::backend::cpp {

auto RenderRuntimePrintSeqStmt(
    const RenderContext& ctx, const mir::RuntimePrintSeqStmt& stmt,
    std::size_t indent) -> std::string;

}  // namespace lyra::backend::cpp
