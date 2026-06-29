#pragma once

#include <map>
#include <set>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/mir/capture_id.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {
class CompilationUnit;
struct CallableCode;
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

// What a callable body reads directly: an activation local / parameter, or a
// captured environment field. A reference is one or the other, never disguised.
struct BodyBindingRef {
  std::variant<mir::LocalId, mir::CaptureId> ref;
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
// closure, reads capture sources into the parent's construction-site block. A
// reference resolves within this one body (a `LocalRef` against `locals`, a
// `CaptureRef` against `captures`); a binding declared in an enclosing body
// reaches this one only by capture, forwarded one boundary at a time along the
// lexical parent.
//
// This is not a block scope: a block governs declaration placement, visibility,
// and cleanup ordering, never reference resolution.
class CallableBindings {
 public:
  // A root body (a process / method / subroutine / constructor): it declares
  // every binding it names; it never forwards.
  CallableBindings(mir::CompilationUnit& unit, mir::CallableCode& code)
      : unit_(&unit), code_(&code) {
  }

  // A closure body: bindings it does not declare are captured from `parent`,
  // their sources read into `capture_site` (a block of the parent), with the
  // snapshot-vs-alias view chosen by `policy`.
  CallableBindings(
      mir::CompilationUnit& unit, mir::CallableCode& code,
      CallableBindings& parent, mir::Block& capture_site, CapturePolicy policy)
      : unit_(&unit),
        code_(&code),
        parent_(&parent),
        capture_site_(&capture_site),
        policy_(std::move(policy)) {
  }

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

  // Capture a computed outer expression (already built in the construction
  // site) by value into a fresh environment field, returning the field id. For
  // a carrier whose value is a `Ref<T>`, the alias is realized in `source`
  // before this call. Used by closures that build their environment by hand
  // (a non-blocking-assignment place / operand snapshot) rather than by
  // forwarding an origin.
  auto Capture(mir::ExprId source, std::string_view name) -> mir::CaptureId;

  // A read of a binding owned by this callable, typed from its arena.
  [[nodiscard]] auto MakeReadExpr(BodyBindingRef ref) const -> mir::Expr;
  [[nodiscard]] auto TypeOf(BodyBindingRef ref) const -> mir::TypeId;

  auto TakeCaptureInits() -> std::vector<mir::CaptureInit> {
    return std::move(capture_inits_);
  }

 private:
  mir::CompilationUnit* unit_;
  mir::CallableCode* code_;
  CallableBindings* parent_ = nullptr;
  mir::Block* capture_site_ = nullptr;
  CapturePolicy policy_;
  std::vector<mir::CaptureInit> capture_inits_;
  std::map<BindingOriginId, BodyBindingRef> available_;
};

}  // namespace lyra::lowering::hir_to_mir
