#pragma once

#include <cstdint>
#include <optional>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/mir/effect.hpp"

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

// User-supplied action thunk descriptor. Describes an outlined deferred
// thunk that executes the user's pass or fail action at drain time.
// Structurally separate from built-in outcomes (default report, cover hit).
struct DeferredThunkAction {
  // Will hold thunk FunctionId + CaptureLayout once Phase 3 adds
  // outlined deferred thunk generation. Stub for now.
};

// Cover-hit built-in action. Bridges deferred cover #0 to the existing
// immediate-cover runtime counter path.
struct DeferredCoverHitAction {
  CoverSiteId cover_site_id;
};

// Per-site metadata for a deferred immediate assertion statement.
// Canonical schema: one entry per deferred assertion site in the design,
// indexed by DeferredAssertionSiteId. This type is the single source of
// truth for deferred-site metadata and lowers directly into the runtime
// site metadata table. Do not create parallel metadata structures.
//
// Disposition-oriented: each disposition path that the site can produce
// has its own typed action descriptor. The presence of a descriptor
// determines whether that disposition is possible for this site.
//
// Invariants (enforced at allocation time):
// - kAssert/kAssume: no cover_hit
// - kCover: no fail_action, no has_default_fail_report
// - cover_hit and pass_action are mutually exclusive
// - has_default_fail_report and fail_action are mutually exclusive
struct DeferredAssertionSiteInfo {
  SourceSpan span;
  common::OriginId origin;
  DeferredAssertionKind kind = DeferredAssertionKind::kAssert;

  // Default failure report. Present for assert/assume sites that have no
  // user-supplied fail action. Report is constructed from origin + kind
  // at drain time via existing report machinery.
  bool has_default_fail_report = false;

  // User-supplied fail action thunk. Present when the user writes
  // `assert #0 (...) else <call>;`. Mutually exclusive with
  // has_default_fail_report.
  std::optional<DeferredThunkAction> fail_action;

  // User-supplied pass action thunk. Present when the user writes
  // `assert #0 (...) <call>;` or `cover #0 (...) <call>;`.
  // Mutually exclusive with cover_hit.
  std::optional<DeferredThunkAction> pass_action;

  // Cover-hit built-in action. Present for `cover #0` sites without a
  // user pass action. Mutually exclusive with pass_action.
  std::optional<DeferredCoverHitAction> cover_hit;
};

// Validate site metadata invariants. Throws InternalError on violation.
inline void ValidateDeferredAssertionSiteInfo(
    const DeferredAssertionSiteInfo& info) {
  bool is_cover = info.kind == DeferredAssertionKind::kCover;

  // kAssert/kAssume must not have cover_hit
  if (!is_cover && info.cover_hit.has_value()) {
    throw common::InternalError(
        "ValidateDeferredAssertionSiteInfo",
        "assert/assume site must not have cover_hit");
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

  // cover_hit and pass_action are mutually exclusive
  if (info.cover_hit.has_value() && info.pass_action.has_value()) {
    throw common::InternalError(
        "ValidateDeferredAssertionSiteInfo",
        "cover_hit and pass_action are mutually exclusive");
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
    bool has_success_path =
        info.cover_hit.has_value() || info.pass_action.has_value();
    if (!has_success_path) {
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
