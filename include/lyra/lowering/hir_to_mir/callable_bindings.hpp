#pragma once

#include <cstdint>
#include <map>
#include <set>
#include <variant>
#include <vector>

#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {
class CompilationUnit;
struct CallableCode;
struct ClosureDecl;
struct Block;
}  // namespace lyra::mir

namespace lyra::lowering::hir_to_mir {

// How a body uses a captured binding (its carrier/view, the snapshot-vs-alias
// axis of the captured field's type). A snapshot owns a copy taken at
// construction; an alias is a `Ref<T>` over a live cell; an owning carrier is a
// copy of a shared handle that keeps its referent alive (LRM 6.21 activation).
enum class CaptureView : std::uint8_t {
  kSnapshot,
  kAlias,
  kOwning,
};

// What a callable body reads directly: an activation local / parameter of this
// callable (a `LocalId` in its `locals` arena), or a captured field of the
// closure this body belongs to (a `FieldId` in the closure's `fields` arena). A
// reference is one or the other, never disguised. The field id is assigned when
// the capture is discovered and is stable: the invoke body reads by it and is
// never rewritten when the closure's physical field layout is canonicalized.
struct BodyBindingRef {
  std::variant<mir::LocalId, mir::FieldId> ref;
};

// The per-body capture policy: how this body views each origin it captures. The
// receiver is a stable pointer (snapshot); a synthesized activation handle is
// owned (refcount copy); an origin in the construction-scope set -- a `fork`
// branch's own block-item declarations -- is snapshotted; any other enclosing
// origin aliases the live cell. A non-fork closure leaves the set empty, so
// every forwarded source binding aliases.
struct CapturePolicy {
  std::set<BindingOriginId> snapshot_set;

  [[nodiscard]] auto ViewFor(BindingOriginId origin) const -> CaptureView {
    switch (origin.kind) {
      case BindingOriginId::Kind::kReceiver:
        return CaptureView::kSnapshot;
      case BindingOriginId::Kind::kSynthesized:
        return CaptureView::kOwning;
      default:
        return snapshot_set.contains(origin) ? CaptureView::kSnapshot
                                             : CaptureView::kAlias;
    }
  }
};

// The binding-resolution context for one callable body. It owns no IR of its
// own; it writes bindings into the callable code being built and, for a
// closure, reads capture sources into the parent's construction-site block and
// captured fields into the closure declaration. A reference resolves within
// this one body (a `LocalRef` against `locals`, or a field access over the
// closure receiver); a binding declared in an enclosing body reaches this one
// only by capture, forwarded one boundary at a time along the lexical parent.
//
// This is not a block scope: a block governs declaration placement, visibility,
// and cleanup ordering, never reference resolution.
class CallableBindings {
 public:
  // A root body (a process / method / subroutine / constructor): it declares
  // every binding it names; it never forwards and captures nothing.
  CallableBindings(mir::CompilationUnit& unit, mir::CallableCode& code)
      : unit_(&unit), code_(&code) {
  }

  // A closure body: bindings it does not declare are captured from `parent`,
  // their sources read into `capture_site` (a block of the parent) and their
  // fields added to the closure declaration `decl`, with the snapshot-vs-alias
  // view chosen by `policy`. The closure receiver -- a read-only borrow of the
  // closure, typed from `closure_id` -- is materialized here as the body's
  // `locals[0]`; a captured read is a field access over it.
  CallableBindings(
      mir::CompilationUnit& unit, mir::ClosureDecl& decl,
      mir::ClosureId closure_id, CallableBindings& parent,
      mir::Block& capture_site, CapturePolicy policy);

  CallableBindings(const CallableBindings&) = delete;
  auto operator=(const CallableBindings&) -> CallableBindings& = delete;
  CallableBindings(CallableBindings&&) = delete;
  auto operator=(CallableBindings&&) -> CallableBindings& = delete;
  ~CallableBindings() = default;

  // Declare a binding with a cross-body identity in this callable's `locals`,
  // recording it as the origin's canonical carrier here. Used to seed the
  // receiver and parameters at body entry and to materialize a declared local
  // at its declaration.
  auto Declare(BindingOriginId origin, mir::LocalDecl decl) -> mir::LocalId;

  // Declare a local with no cross-body identity (a lowering temporary used only
  // within this body). It is allocated in `locals` but never captured.
  auto DeclareAnonymous(mir::LocalDecl decl) -> mir::LocalId;

  // Make `origin` available as a body binding here, forwarding it through the
  // lexical parent one boundary at a time and recording the canonical carrier.
  // A second call for the same origin returns the same binding.
  auto EnsureCarrier(BindingOriginId origin) -> BodyBindingRef;

  // A read of a binding owned by this body, typed from its arena. For a
  // captured field the closure receiver operand is appended to `block` and the
  // returned field access (to be appended to the same block) reads it.
  [[nodiscard]] auto MakeReadExpr(BodyBindingRef ref, mir::Block& block) const
      -> mir::Expr;
  [[nodiscard]] auto TypeOf(BodyBindingRef ref) const -> mir::TypeId;

  // Freeze the capture set: compute the closure's canonical field order (by
  // binding origin), and return the closure's field initializers keyed by field
  // id. The invoke body is untouched -- it already reads each capture by its
  // stable field id. Closure bodies only.
  auto Finalize() -> std::vector<mir::FieldInit>;

 private:
  [[nodiscard]] auto NameOf(BodyBindingRef ref) const -> const std::string&;

  struct CaptureEntry {
    BindingOriginId key;
    mir::ExprId source{};
  };

  mir::CompilationUnit* unit_;
  mir::CallableCode* code_;
  mir::ClosureDecl* closure_decl_ = nullptr;
  CallableBindings* parent_ = nullptr;
  mir::Block* capture_site_ = nullptr;
  CapturePolicy policy_;
  mir::LocalId self_local_{};
  mir::TypeId self_ptr_type_{};
  std::vector<CaptureEntry> captures_;
  std::map<BindingOriginId, BodyBindingRef> available_;
};

}  // namespace lyra::lowering::hir_to_mir
