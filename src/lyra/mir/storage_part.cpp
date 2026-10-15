#include "lyra/mir/storage_part.hpp"

#include <optional>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::mir {

auto SelectionOf(const CallExpr& call)
    -> std::optional<support::PartSelection> {
  const std::optional<support::BuiltinFn> fn = DirectBuiltinFn(call);
  if (!fn.has_value()) {
    return std::nullopt;
  }
  return support::RuntimeEntryOf(*fn).selects;
}

auto SelectedFrom(const Block& block, ExprId target) -> std::optional<ExprId> {
  const auto* part = std::get_if<CallExpr>(&block.exprs.Get(target).data);
  if (part == nullptr || !SelectionOf(*part).has_value()) {
    return std::nullopt;
  }
  return CalleeReceiver(part->callee);
}

namespace {

// Whether a call answers a part of what it is taken on, as a view of that
// storage rather than a value of its own: a union's member reached for a
// write.
auto AnswersAPartOfItsReceiver(const CallExpr& call) -> bool {
  const std::optional<support::BuiltinFn> fn = DirectBuiltinFn(call);
  if (!fn.has_value()) {
    return false;
  }
  switch (support::RuntimeEntryOf(*fn).answer) {
    case support::EntryAnswer::kPartOfTheReceiver:
      return true;
    case support::EntryAnswer::kNewValue:
    case support::EntryAnswer::kReadOnlyReferenceIntoTheReceiver:
    case support::EntryAnswer::kMutableReferenceIntoTheReceiver:
    case support::EntryAnswer::kReferenceToContents:
    case support::EntryAnswer::kExecution:
      return false;
  }
  throw InternalError("mir: unknown entry answer");
}

}  // namespace

auto ValueLiesInStorage(const Block& block, ExprId id) -> bool {
  const ExprData& data = block.exprs.Get(id).data;
  if (const auto* call = std::get_if<CallExpr>(&data);
      call != nullptr && AnswersAPartOfItsReceiver(*call)) {
    const std::optional<ExprId> whole = CalleeReceiver(call->callee);
    return whole.has_value() && ValueLiesInStorage(block, *whole);
  }
  return IsPlace(data) || std::holds_alternative<CopyExpr>(data) ||
         std::holds_alternative<AwaitExpr>(data);
}

}  // namespace lyra::mir
