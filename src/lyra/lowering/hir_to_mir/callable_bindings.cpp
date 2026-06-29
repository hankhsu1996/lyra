#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"

#include <string>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto CallableBindings::Declare(BindingOriginId origin, mir::LocalDecl decl)
    -> mir::LocalId {
  const mir::LocalId id = code_->locals.Add(std::move(decl));
  available_.insert_or_assign(origin, BodyBindingRef{.ref = id});
  return id;
}

auto CallableBindings::DeclareAnonymous(mir::LocalDecl decl) -> mir::LocalId {
  return code_->locals.Add(std::move(decl));
}

auto CallableBindings::EnsureCarrier(BindingOriginId origin) -> BodyBindingRef {
  if (const auto it = available_.find(origin); it != available_.end()) {
    return it->second;
  }
  if (parent_ == nullptr) {
    throw InternalError(
        "CallableBindings::EnsureCarrier: origin has no carrier in the root "
        "body");
  }

  // Forward one boundary: materialize the origin in the parent, read it at the
  // construction site (a block of the parent), then bind a fresh field here.
  const BodyBindingRef parent_ref = parent_->EnsureCarrier(origin);
  const mir::TypeId parent_type = parent_->TypeOf(parent_ref);
  const mir::ExprId read =
      capture_site_->exprs.Add(parent_->MakeReadExpr(parent_ref));

  mir::ExprId source = read;
  mir::TypeId field_type = parent_type;
  if (policy_.ViewFor(origin) == CaptureView::kAlias) {
    source = BuildReferenceArg(*unit_, *capture_site_, read, parent_type);
    field_type = capture_site_->exprs.Get(source).type;
  }

  const std::string name =
      "_lyra_cap_" + std::to_string(code_->captures.size());
  const mir::CaptureId field =
      code_->captures.Add(mir::LocalDecl{.name = name, .type = field_type});
  capture_inits_.push_back(mir::CaptureInit{.target = field, .source = source});
  const BodyBindingRef result{.ref = field};
  available_.insert_or_assign(origin, result);
  return result;
}

auto CallableBindings::Capture(mir::ExprId source, std::string_view name)
    -> mir::CaptureId {
  const mir::TypeId field_type = capture_site_->exprs.Get(source).type;
  const mir::CaptureId field = code_->captures.Add(
      mir::LocalDecl{.name = std::string(name), .type = field_type});
  capture_inits_.push_back(mir::CaptureInit{.target = field, .source = source});
  return field;
}

auto CallableBindings::MakeReadExpr(BodyBindingRef ref) const -> mir::Expr {
  return std::visit(
      Overloaded{
          [&](mir::LocalId id) -> mir::Expr {
            return mir::MakeLocalRefExpr(id, code_->locals.Get(id).type);
          },
          [&](mir::CaptureId id) -> mir::Expr {
            return mir::MakeCaptureRefExpr(id, code_->captures.Get(id).type);
          },
      },
      ref.ref);
}

auto CallableBindings::TypeOf(BodyBindingRef ref) const -> mir::TypeId {
  return std::visit(
      Overloaded{
          [&](mir::LocalId id) { return code_->locals.Get(id).type; },
          [&](mir::CaptureId id) { return code_->captures.Get(id).type; },
      },
      ref.ref);
}

}  // namespace lyra::lowering::hir_to_mir
