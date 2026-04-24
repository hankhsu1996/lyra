#pragma once

#include <cstddef>
#include <string>

#include "lyra/mir/class_decl.hpp"
#include "lyra/mir/stmt.hpp"
#include "render_context.hpp"

namespace lyra::backend::cpp {

auto RenderBody(
    const mir::ClassDecl& class_decl, const mir::Body& body, std::size_t indent)
    -> std::string;

auto RenderStmt(
    const RenderContext& ctx, const mir::Stmt& stmt, std::size_t indent)
    -> std::string;

}  // namespace lyra::backend::cpp
