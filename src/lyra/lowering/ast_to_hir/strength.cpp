#include "lyra/lowering/ast_to_hir/strength.hpp"

#include <optional>
#include <utility>

#include <slang/ast/SemanticFacts.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/support/strength_level.hpp"

namespace lyra::lowering::ast_to_hir {

auto TranslateDriveStrength(
    const std::pair<
        std::optional<slang::ast::DriveStrength>,
        std::optional<slang::ast::DriveStrength>>& strength,
    diag::SourceSpan span) -> diag::Result<support::StrengthLevel> {
  const auto& [zero, one] = strength;
  if (!zero.has_value() && !one.has_value()) {
    return support::StrengthLevel::kStrong;
  }
  if (zero != one) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedContinuousAssignForm,
        "a driver whose 0 and 1 strengths differ (LRM 28.11) is not yet "
        "supported");
  }
  switch (*zero) {
    case slang::ast::DriveStrength::Supply:
      return support::StrengthLevel::kSupply;
    case slang::ast::DriveStrength::Strong:
      return support::StrengthLevel::kStrong;
    case slang::ast::DriveStrength::Pull:
      return support::StrengthLevel::kPull;
    case slang::ast::DriveStrength::Weak:
      return support::StrengthLevel::kWeak;
    case slang::ast::DriveStrength::HighZ:
      break;
  }
  // LRM 28.11 makes a specification of high impedance for both values illegal,
  // so the front end has already refused every way of writing one.
  throw InternalError("TranslateDriveStrength: a driver drives nothing at all");
}

auto TranslateChargeStrength(std::optional<slang::ast::ChargeStrength> charge)
    -> std::optional<support::StrengthLevel> {
  if (!charge.has_value()) {
    return std::nullopt;
  }
  switch (*charge) {
    case slang::ast::ChargeStrength::Small:
      return support::StrengthLevel::kSmall;
    case slang::ast::ChargeStrength::Medium:
      return support::StrengthLevel::kMedium;
    case slang::ast::ChargeStrength::Large:
      return support::StrengthLevel::kLarge;
  }
  throw InternalError("TranslateChargeStrength: unknown charge strength");
}

}  // namespace lyra::lowering::ast_to_hir
