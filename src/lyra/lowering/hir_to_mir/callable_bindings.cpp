#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/closure_record.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/member.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

CallableBindings::CallableBindings(
    mir::CompilationUnit& unit, mir::ClosureRecord& record,
    mir::ClosureRecordId record_id, CallableBindings& parent,
    mir::Block& capture_site, CapturePolicy policy)
    : unit_(&unit),
      code_(&record.invoke),
      record_(&record),
      parent_(&parent),
      capture_site_(&capture_site),
      policy_(std::move(policy)) {
  // The closure receiver is the invoke body's `locals[0]`: a read-only borrow
  // of the record. A captured read is a field access over it, resolved through
  // the record id so the pointee names this record's field set.
  const mir::TypeId record_value =
      unit.types.Intern(mir::ClosureRecordType{.record_id = record_id});
  self_ptr_type_ = unit.types.PointerTo(
      record_value, mir::PointerOwnership::kBorrowed,
      mir::Mutability::kReadOnly);
  self_local_ =
      code_->locals.Add(mir::LocalDecl{.name = "self", .type = self_ptr_type_});
}

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
  // construction site (a block of the parent), then add a captured field here.
  const BodyBindingRef parent_ref = parent_->EnsureCarrier(origin);
  const mir::TypeId parent_type = parent_->TypeOf(parent_ref);
  const std::string name = parent_->NameOf(parent_ref);
  const mir::ExprId read = capture_site_->exprs.Add(
      parent_->MakeReadExpr(parent_ref, *capture_site_));

  mir::ExprId source = read;
  mir::TypeId field_type = parent_type;
  if (policy_.ViewFor(origin) == CaptureView::kAlias) {
    source = BuildReferenceArg(*unit_, *capture_site_, read, parent_type);
    field_type = capture_site_->exprs.Get(source).type;
  }

  const mir::MemberId member =
      record_->fields.Add(mir::MemberDecl{.name = name, .type = field_type});
  captures_.push_back(CaptureEntry{.key = origin, .source = source});
  const BodyBindingRef result{.ref = member};
  available_.insert_or_assign(origin, result);
  return result;
}

auto CallableBindings::MakeReadExpr(BodyBindingRef ref, mir::Block& block) const
    -> mir::Expr {
  return std::visit(
      Overloaded{
          [&](mir::LocalId id) -> mir::Expr {
            return mir::MakeLocalRefExpr(id, code_->locals.Get(id).type);
          },
          [&](mir::MemberId member) -> mir::Expr {
            const mir::TypeId field_type = record_->fields.Get(member).type;
            const mir::ExprId receiver = block.exprs.Add(
                mir::MakeLocalRefExpr(self_local_, self_ptr_type_));
            return mir::MakeMemberAccessExpr(
                receiver, mir::MemberRef{.var = member}, field_type);
          },
      },
      ref.ref);
}

auto CallableBindings::TypeOf(BodyBindingRef ref) const -> mir::TypeId {
  return std::visit(
      Overloaded{
          [&](mir::LocalId id) { return code_->locals.Get(id).type; },
          [&](mir::MemberId member) {
            return record_->fields.Get(member).type;
          },
      },
      ref.ref);
}

auto CallableBindings::NameOf(BodyBindingRef ref) const -> const std::string& {
  return std::visit(
      Overloaded{
          [&](mir::LocalId id) -> const std::string& {
            return code_->locals.Get(id).name;
          },
          [&](mir::MemberId member) -> const std::string& {
            return record_->fields.Get(member).name;
          },
      },
      ref.ref);
}

auto CallableBindings::Finalize() -> std::vector<mir::FieldInit> {
  const std::size_t count = captures_.size();

  // Canonical physical layout: field ids ordered by their binding origin. This
  // fixes only the record's field order (dump, generated-source order,
  // destruction); the invoke body reads by stable field id, so it is untouched.
  std::vector<mir::MemberId> layout;
  layout.reserve(count);
  for (std::size_t i = 0; i < count; ++i) {
    layout.push_back(mir::MemberId{static_cast<std::uint32_t>(i)});
  }
  std::ranges::stable_sort(layout, [&](mir::MemberId a, mir::MemberId b) {
    return captures_[a.value].key < captures_[b.value].key;
  });
  record_->layout = std::move(layout);

  // Field initializers keyed by field id (discovery order). Each is a pure read
  // of an already-materialized capture source, so their construction order is
  // independent of both the layout order and the source evaluation order.
  std::vector<mir::FieldInit> inits;
  inits.reserve(count);
  for (std::size_t i = 0; i < count; ++i) {
    inits.push_back(
        mir::FieldInit{
            .target = mir::MemberId{static_cast<std::uint32_t>(i)},
            .value = captures_[i].source});
  }
  return inits;
}

}  // namespace lyra::lowering::hir_to_mir
