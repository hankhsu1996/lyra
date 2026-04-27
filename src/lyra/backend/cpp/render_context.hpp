#pragma once

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

 private:
  const mir::CompilationUnit* unit_;
  const mir::ClassDecl* class_decl_;
  const mir::Body* body_;
};

}  // namespace lyra::backend::cpp
