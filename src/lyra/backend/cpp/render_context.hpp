#pragma once

#include <cstddef>
#include <format>
#include <string>
#include <string_view>

#include "lyra/mir/class_decl.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::backend::cpp {

class RenderContext {
 public:
  RenderContext(
      const mir::CompilationUnit& unit, const mir::ClassDecl& class_decl,
      const mir::Body& body)
      : unit_(&unit), class_decl_(&class_decl), body_(&body) {
  }

  [[nodiscard]] auto Unit() const -> const mir::CompilationUnit& {
    return *unit_;
  }

  [[nodiscard]] auto Class() const -> const mir::ClassDecl& {
    return *class_decl_;
  }

  [[nodiscard]] auto Body() const -> const mir::Body& {
    return *body_;
  }

  [[nodiscard]] auto Expr(mir::ExprId id) const -> const mir::Expr& {
    return body_->GetExpr(id);
  }

  // Allocate a unique temp-name suffix. Mutable because the counter is a
  // logical-const supply, not part of the rendering context's observable
  // shape. Single-threaded emission today.
  [[nodiscard]] auto AllocateTemp(std::string_view prefix) const
      -> std::string {
    return std::format("{}_{}", prefix, next_temp_++);
  }

 private:
  const mir::CompilationUnit* unit_;
  const mir::ClassDecl* class_decl_;
  const mir::Body* body_;
  mutable std::size_t next_temp_ = 0;
};

}  // namespace lyra::backend::cpp
