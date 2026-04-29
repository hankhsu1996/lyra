#pragma once

#include <cstddef>
#include <string>

#include "lyra/backend/cpp/render_context.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/mir/class_decl.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::backend::cpp {

auto RenderBody(
    const mir::CompilationUnit& unit, const mir::ClassDecl& class_decl,
    const mir::Body& body, std::size_t indent) -> diag::Result<std::string>;

auto RenderStmt(
    const RenderContext& ctx, const mir::Stmt& stmt, std::size_t indent)
    -> diag::Result<std::string>;

}  // namespace lyra::backend::cpp
