#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"

#include <algorithm>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/closure_decl.hpp"
#include "lyra/mir/closure_signature.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/callable_bounds.hpp"

namespace lyra::lowering::hir_to_mir {

CallableBindings::CallableBindings(
    const mir::CompilationUnit& unit, mir::ClosureDecl& decl,
    mir::ClosureId closure_id, support::ClosureKind kind,
    CallableBindings& parent, mir::Block& capture_site, CapturePolicy policy)
    : unit_(&unit),
      code_(&decl.invoke),
      closure_decl_(&decl),
      closure_id_(closure_id),
      parent_(&parent),
      capture_site_(&capture_site),
      policy_(std::move(policy)),
      self_ptr_type_(mir::ClosureReceiverType(unit.types, closure_id, kind)) {
  // The closure receiver is the invoke body's `locals[0]`. A captured read is a
  // field access over it, resolved through the closure id so the pointee names
  // this closure's field set.
  self_local_ = code_->AddLocal(self_ptr_type_);
  code_->receiver = self_local_;
}

auto CallableBindings::DeclareNamed(
    BindingOriginId origin, std::string name, mir::TypeId type)
    -> mir::LocalId {
  const mir::LocalId id = code_->AddNamedLocal(std::move(name), type);
  available_.insert_or_assign(origin, BodyBindingRef{.ref = id});
  return id;
}

auto CallableBindings::DeclareProcedural(
    BindingOriginId origin, const std::optional<std::string>& name,
    mir::TypeId type) -> mir::LocalId {
  return name.has_value() ? DeclareNamed(origin, *name, type)
                          : Declare(origin, type);
}

auto CallableBindings::Declare(BindingOriginId origin, mir::TypeId type)
    -> mir::LocalId {
  const mir::LocalId id = code_->AddLocal(type);
  available_.insert_or_assign(origin, BodyBindingRef{.ref = id});
  // The object a body is entered on is its receiver by what it is, so the body
  // says which binding that is where the binding is made.
  if (std::holds_alternative<ReceiverOrigin>(origin)) {
    code_->receiver = id;
  }
  return id;
}

auto CallableBindings::RootBlock() const -> mir::Block& {
  return code_->Body();
}

auto CallableBindings::DeclareAnonymous(mir::TypeId type) -> mir::LocalId {
  return code_->AddLocal(type);
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

  if (std::holds_alternative<ReceiverOrigin>(origin) &&
      parent_->Provides(ReceiverShareOrigin{})) {
    return ReceiverThroughShare();
  }

  // Forward one boundary: materialize the origin in the parent, read it at the
  // construction site (a block of the parent), then add a captured field here.
  const BodyBindingRef parent_ref = parent_->EnsureCarrier(origin);
  const mir::TypeId parent_type = parent_->TypeOf(parent_ref);
  const mir::ExprId read = capture_site_->exprs.Add(
      parent_->MakeBindingPlace(parent_ref, *capture_site_));

  mir::ExprId source = read;
  mir::TypeId field_type = parent_type;
  if (CapturesByReference(policy_.ViewFor(origin))) {
    source = BuildReferenceArg(*unit_, *capture_site_, read, parent_type);
    field_type = capture_site_->exprs.Get(source).type;
  } else if (
      const auto* cell =
          unit_->types.Get(parent_type).As<mir::ObservableType>()) {
    // A snapshot is a copy of what the variable holds, taken where the closure
    // is built, and belongs to the closure alone; no other process reaches it,
    // so it is the value rather than a cell that reports its writes.
    field_type = cell->value;
    source = capture_site_->Read(capture_site_->exprs.Add(
        capture_site_->WrapperContents(unit_->types, read, field_type)));
  } else {
    source = capture_site_->Read(read);
  }

  const mir::FieldId field =
      closure_decl_->fields.Add(mir::FieldDecl{.type = field_type});
  captures_.Define(field, CaptureEntry{.origin = origin, .source = source});
  const BodyBindingRef result{.ref = field};
  available_.insert_or_assign(origin, result);
  return result;
}

auto CallableBindings::Provides(BindingOriginId origin) const -> bool {
  return available_.contains(origin) ||
         (parent_ != nullptr && parent_->Provides(origin));
}

auto CallableBindings::ReceiverThroughShare() -> BodyBindingRef {
  const BodyBindingRef share = EnsureCarrier(ReceiverShareOrigin{});
  const mir::TypeId object =
      mir::ObjectReachedThrough(unit_->types, TypeOf(share));
  const mir::TypeId pointer = unit_->types.Intern(
      mir::Type{mir::PointerType{
          .pointee = object,
          .ownership = mir::PointerOwnership::kBorrowed,
          .mutability = mir::Mutability::kMutable}});
  mir::Block& entry = code_->Body();
  const mir::ExprId handle = entry.exprs.Add(MakeBindingPlace(share, entry));
  const mir::ExprId object_place = entry.Deref(unit_->types, handle, object);
  const mir::ExprId address =
      entry.exprs.Add(mir::MakeAddressOfExpr(object_place, pointer));
  const mir::LocalId self = code_->AddLocal(pointer);
  // Ahead of every statement already lowered, any of which may be the first to
  // reach the object.
  const mir::StmtId declared = entry.stmts.Add(
      mir::Stmt{
          .label = std::nullopt,
          .data = mir::LocalDeclStmt{.target = self, .init = address}});
  entry.root_stmts.insert(entry.root_stmts.begin(), declared);
  const BodyBindingRef result{.ref = self};
  available_.insert_or_assign(ReceiverOrigin{}, result);
  return result;
}

auto CallableBindings::MakeBindingPlace(
    BodyBindingRef ref, mir::Block& block) const -> mir::Expr {
  return std::visit(
      Overloaded{
          [&](mir::LocalId id) -> mir::Expr {
            return mir::MakeLocalRefExpr(id, code_->locals.Get(id).type);
          },
          [&](mir::FieldId field) -> mir::Expr {
            const mir::TypeId field_type =
                closure_decl_->fields.Get(field).type;
            const mir::ExprId receiver = BuildObjectDeref(
                *unit_, block, block.LocalPlace(self_local_, self_ptr_type_));
            return mir::MakeFieldAccessExpr(
                receiver,
                mir::ClosureFieldTarget{.owner = closure_id_, .slot = field},
                field_type);
          },
      },
      ref.ref);
}

auto CallableBindings::TypeOf(BodyBindingRef ref) const -> mir::TypeId {
  return std::visit(
      Overloaded{
          [&](mir::LocalId id) { return code_->locals.Get(id).type; },
          [&](mir::FieldId field) {
            return closure_decl_->fields.Get(field).type;
          },
      },
      ref.ref);
}

auto CallableBindings::Finalize() -> std::vector<mir::FieldInit> {
  // Canonical field order: field ids ordered by their binding origin. This is
  // the deterministic emission order (capture clause, dump), independent of the
  // order captures were discovered; the invoke body reads by stable field id,
  // so it is untouched. Not a physical layout -- offsets belong to LIR.
  std::vector<mir::FieldId> order;
  order.reserve(closure_decl_->fields.size());
  for (const mir::FieldId id : closure_decl_->fields.Ids()) {
    order.push_back(id);
  }
  std::ranges::stable_sort(order, [&](mir::FieldId a, mir::FieldId b) {
    return captures_.Get(a).origin < captures_.Get(b).origin;
  });
  closure_decl_->field_order = std::move(order);

  // Field initializers keyed by field id (discovery order). Each is a pure read
  // of an already-materialized capture source, so their construction order is
  // independent of both the layout order and the source evaluation order.
  std::vector<mir::FieldInit> inits;
  inits.reserve(closure_decl_->fields.size());
  for (const mir::FieldId id : closure_decl_->fields.Ids()) {
    inits.push_back(
        mir::FieldInit{.target = id, .value = captures_.Get(id).source});
  }
  return inits;
}

}  // namespace lyra::lowering::hir_to_mir
