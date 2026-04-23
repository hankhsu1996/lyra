#pragma once

#include <cstddef>
#include <string>

#include "lyra/mir/stmt.hpp"
#include "render_context.hpp"

namespace lyra::projection::cpp {

auto RenderProcessBody(
    const RenderContext& ctx, mir::StmtId body_id, std::size_t indent)
    -> std::string;

}  // namespace lyra::projection::cpp
