#pragma once

#include <cstddef>
#include <format>
#include <string>
#include <string_view>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/body_hops.hpp"
#include "lyra/mir/class_decl.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::backend::cpp {

class RenderContext {
 public:
  RenderContext(
      const mir::CompilationUnit& unit, const mir::ClassDecl& class_decl,
      const mir::Body& body)
      : unit_(&unit),
        class_decl_(&class_decl),
        body_(&body),
        parent_(nullptr),
        temp_counter_(&owned_temp_counter_) {
  }

  RenderContext(
      const mir::CompilationUnit& unit, const mir::ClassDecl& class_decl,
      const mir::Body& body, const RenderContext& parent)
      : unit_(&unit),
        class_decl_(&class_decl),
        body_(&body),
        parent_(&parent),
        temp_counter_(parent.temp_counter_) {
  }

  RenderContext(const RenderContext&) = delete;
  auto operator=(const RenderContext&) -> RenderContext& = delete;
  RenderContext(RenderContext&&) = delete;
  auto operator=(RenderContext&&) -> RenderContext& = delete;
  ~RenderContext() = default;

  [[nodiscard]] auto Unit() const -> const mir::CompilationUnit& {
    return *unit_;
  }

  [[nodiscard]] auto Class() const -> const mir::ClassDecl& {
    return *class_decl_;
  }

  [[nodiscard]] auto Body() const -> const mir::Body& {
    return *body_;
  }

  [[nodiscard]] auto BodyAtHops(mir::BodyHops hops) const -> const mir::Body& {
    if (hops.value == 0) {
      return *body_;
    }
    if (parent_ == nullptr) {
      throw InternalError("RenderContext::BodyAtHops: hops out of range");
    }
    return parent_->BodyAtHops(mir::BodyHops{.value = hops.value - 1});
  }

  [[nodiscard]] auto Expr(mir::ExprId id) const -> const mir::Expr& {
    return body_->GetExpr(id);
  }

  [[nodiscard]] auto AllocateTemp(std::string_view prefix) const
      -> std::string {
    return std::format("{}_{}", prefix, (*temp_counter_)++);
  }

 private:
  const mir::CompilationUnit* unit_;
  const mir::ClassDecl* class_decl_;
  const mir::Body* body_;
  const RenderContext* parent_;
  std::size_t* temp_counter_;
  mutable std::size_t owned_temp_counter_ = 0;
};

}  // namespace lyra::backend::cpp
