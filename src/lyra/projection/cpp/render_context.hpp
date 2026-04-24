#pragma once

#include "lyra/mir/class_decl.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::projection::cpp {

class RenderContext {
 public:
  RenderContext(const mir::ClassDecl& class_decl, const mir::Body& body)
      : class_decl_(&class_decl), body_(&body) {
  }

  [[nodiscard]] auto Class() const -> const mir::ClassDecl& {
    return *class_decl_;
  }

  [[nodiscard]] auto Body() const -> const mir::Body& {
    return *body_;
  }

 private:
  const mir::ClassDecl* class_decl_;
  const mir::Body* body_;
};

}  // namespace lyra::projection::cpp
