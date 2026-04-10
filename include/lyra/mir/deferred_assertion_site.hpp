#pragma once

#include <cstdint>
#include <optional>
#include <variant>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::mir {

// DeferredAssertionSiteId is defined in effect.hpp alongside CoverSiteId
// (both are MIR handle types used in effects and site registries).

// Assertion kind at the MIR level (mirrors hir::ImmediateAssertionKind
// without introducing an HIR dependency in MIR headers).
enum class DeferredAssertionKind : uint8_t {
  kAssert,
  kAssume,
  kCover,
};

// ---------------------------------------------------------------------------
// Semantic deferred assertion action types.
// These model the LRM "pending assertion report" concept without ABI details.
// Backend derivation (thunk emission, payload layout, ref-slot numbering)
// is handled by the LLVM backend from these semantic types.
// ---------------------------------------------------------------------------

// Semantic passing mode for a deferred action actual argument.
enum class DeferredActualKind : uint8_t {
  kSnapshotValue,  // by-value, captured at encounter time
  kLiveRef,        // ref formal, address captured, dereferenced at execution
  kConstLiveRef,   // const ref formal, same timing as LiveRef
};

// One semantic actual in a deferred assertion action.
// No payload layout or ref-slot numbering -- those are realization details.
struct DeferredAssertionActual {
  DeferredActualKind kind = DeferredActualKind::kSnapshotValue;
  TypeId type;          // semantic type of the formal parameter
  PlaceId ref_place{};  // valid only for kLiveRef/kConstLiveRef
};

// User subroutine call action: assert #0 (...) else foo(a, b, c);
struct DeferredUserCallAction {
  FunctionId callee;
  std::vector<DeferredAssertionActual> actuals;  // ordered by formal position
  // Callee ABI metadata captured at site creation time (when the body
  // arena is still active). Consumed by thunk compilation, which runs
  // after the body arena scope has ended.
  bool accepts_decision_owner = false;
};

// Built-in cover-hit action: cover #0 (...) with no user pass action.
struct DeferredCoverHitAction {
  CoverSiteId cover_site_id;
};

// Semantic deferred assertion action. Tagged variant makes invalid states
// unrepresentable: user-call actions always carry a callee and actuals,
// cover-hit actions always carry a cover site ID, neither has spare fields.
using DeferredAssertionAction =
    std::variant<DeferredUserCallAction, DeferredCoverHitAction>;

// Per-site metadata for a deferred immediate assertion statement.
// Canonical schema: one entry per deferred assertion site in the design,
// indexed by DeferredAssertionSiteId. Semantic-only: no thunk IDs,
// payload layout, or ref-slot numbering. Backend derives all lowering
// details from the semantic action data.
//
// Invariants (enforced at allocation time):
// - kAssert/kAssume: pass_action must not hold DeferredCoverHitAction
// - kCover: no fail_action, no has_default_fail_report
// - has_default_fail_report and fail_action are mutually exclusive
// - kCover must have pass_action
// - kAssert/kAssume must have exactly one fail path
struct DeferredAssertionSiteInfo {
  SourceSpan span;
  common::OriginId origin;
  DeferredAssertionKind kind = DeferredAssertionKind::kAssert;

  // Default failure report. Present for assert/assume sites that have no
  // user-supplied fail action.
  bool has_default_fail_report = false;

  // Semantic fail action (user call only). Mutually exclusive with
  // has_default_fail_report.
  std::optional<DeferredAssertionAction> fail_action;

  // Semantic pass action. For cover sites, may be DeferredUserCallAction
  // or DeferredCoverHitAction. For assert/assume, DeferredUserCallAction
  // only (if present).
  std::optional<DeferredAssertionAction> pass_action;
};

// Validate site metadata invariants. Throws InternalError on violation.
inline void ValidateDeferredAssertionSiteInfo(
    const DeferredAssertionSiteInfo& info) {
  bool is_cover = info.kind == DeferredAssertionKind::kCover;

  auto has_cover_hit_pass = [&] {
    return info.pass_action.has_value() &&
           std::holds_alternative<DeferredCoverHitAction>(*info.pass_action);
  };

  // kAssert/kAssume must not have cover-hit pass action
  if (!is_cover && has_cover_hit_pass()) {
    throw common::InternalError(
        "ValidateDeferredAssertionSiteInfo",
        "assert/assume site must not have cover_hit pass action");
  }

  // kCover must not have fail_action or default fail report
  if (is_cover && info.fail_action.has_value()) {
    throw common::InternalError(
        "ValidateDeferredAssertionSiteInfo",
        "cover site must not have fail_action");
  }
  if (is_cover && info.has_default_fail_report) {
    throw common::InternalError(
        "ValidateDeferredAssertionSiteInfo",
        "cover site must not have default fail report");
  }

  // has_default_fail_report and fail_action are mutually exclusive
  if (info.has_default_fail_report && info.fail_action.has_value()) {
    throw common::InternalError(
        "ValidateDeferredAssertionSiteInfo",
        "default fail report and user fail action are mutually exclusive");
  }

  // Fail actions must be user-call (cover-hit as fail makes no sense)
  if (info.fail_action.has_value() &&
      !std::holds_alternative<DeferredUserCallAction>(*info.fail_action)) {
    throw common::InternalError(
        "ValidateDeferredAssertionSiteInfo",
        "fail action must be a user call action");
  }

  // Completeness: assert/assume must have exactly one fail path
  if (!is_cover) {
    bool has_fail_path =
        info.has_default_fail_report || info.fail_action.has_value();
    if (!has_fail_path) {
      throw common::InternalError(
          "ValidateDeferredAssertionSiteInfo",
          "assert/assume site must have exactly one fail path");
    }
  }

  // Completeness: cover must have exactly one success path
  if (is_cover) {
    if (!info.pass_action.has_value()) {
      throw common::InternalError(
          "ValidateDeferredAssertionSiteInfo",
          "cover site must have exactly one success path");
    }
  }
}

// Dense allocator for deferred assertion sites during HIR-to-MIR lowering.
// Owned by the design lowering scope; shared across all body/process
// lowering contexts via pointer. Produces design-global site ID values.
// Validates site-shape invariants at allocation time.
class DeferredAssertionSiteRegistry {
 public:
  auto Allocate(DeferredAssertionSiteInfo info) -> DeferredAssertionSiteId {
    ValidateDeferredAssertionSiteInfo(info);
    auto id = DeferredAssertionSiteId{static_cast<uint32_t>(sites_.size())};
    sites_.push_back(std::move(info));
    return id;
  }

  [[nodiscard]] auto Size() const -> uint32_t {
    return static_cast<uint32_t>(sites_.size());
  }

  [[nodiscard]] auto Sites() const
      -> const std::vector<DeferredAssertionSiteInfo>& {
    return sites_;
  }

  auto TakeSites() -> std::vector<DeferredAssertionSiteInfo> {
    return std::move(sites_);
  }

 private:
  std::vector<DeferredAssertionSiteInfo> sites_;
};

}  // namespace lyra::mir
