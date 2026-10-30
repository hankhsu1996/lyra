#include "lyra/lowering/hir_to_mir/switch_stmt.hpp"

#include <cstdint>
#include <expected>
#include <optional>
#include <set>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/value_operations.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The integer one label reads as, where it is a value of `type` known before
// the run with no x or z bit. The label is lowered as the run would evaluate
// it, and a value known before the run folds as it is lowered, so asking
// whether it holds an x or z bit and what integer it reads as answers with
// literals.
auto ValueOfLabel(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    const Evaluation& label, mir::TypeId type)
    -> diag::Result<std::optional<std::int64_t>> {
  mir::Block probe;
  auto lowered = label(frame.WithBlock(&probe));
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  const mir::ExprId value = probe.AsValue(unit.types, *lowered);
  if (!probe.root_stmts.empty() || probe.exprs.Get(value).type != type) {
    return std::nullopt;
  }
  const auto* unknown = std::get_if<mir::MachineBoolLiteral>(
      &probe.exprs
           .Get(BuildValueOperation(
               unit, probe, support::BuiltinFn::kHasUnknown, value, {},
               unit.builtins.machine_bool))
           .data);
  const auto* number = std::get_if<mir::MachineIntLiteral>(
      &probe.exprs.Get(BuildToInt64Call(unit, probe, value)).data);
  if (unknown == nullptr || unknown->value || number == nullptr) {
    return std::nullopt;
  }
  return std::optional<std::int64_t>{number->value};
}

}  // namespace

auto ValuesOfItems(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    std::span<const std::vector<Evaluation>> labels, mir::TypeId type)
    -> diag::Result<std::optional<std::vector<std::vector<std::int64_t>>>> {
  const mir::Type& stated = unit.types.Get(type);
  if (!stated.IsIntegral() || stated.Integral().bit_width > 64) {
    return std::nullopt;
  }
  std::vector<std::vector<std::int64_t>> values;
  values.reserve(labels.size());
  for (const std::vector<Evaluation>& item : labels) {
    std::vector<std::int64_t>& taken_at = values.emplace_back();
    taken_at.reserve(item.size());
    for (const Evaluation& label : item) {
      auto value = ValueOfLabel(unit, frame, label, type);
      if (!value) return std::unexpected(std::move(value.error()));
      if (!value->has_value()) {
        return std::nullopt;
      }
      taken_at.push_back(**value);
    }
  }
  return values;
}

auto BuildValueSwitch(
    const WalkFrame& frame, mir::ExprId discriminant,
    std::span<const ValueArm> arms, std::optional<mir::Block> default_body)
    -> diag::Result<mir::SwitchStmt> {
  mir::Block& block = *frame.current_block;
  mir::SwitchStmt built{
      .discriminant = discriminant, .cases = {}, .default_body = std::nullopt};
  built.cases.reserve(arms.size());
  std::set<std::int64_t> taken;
  for (const ValueArm& arm : arms) {
    mir::SwitchCase selected;
    for (const std::int64_t value : arm.values) {
      if (taken.insert(value).second) {
        selected.values.push_back(value);
      }
    }
    auto body = arm.body(frame);
    if (!body) return std::unexpected(std::move(body.error()));
    selected.body = block.child_scopes.Add(*std::move(body));
    built.cases.push_back(std::move(selected));
  }
  if (default_body.has_value()) {
    built.default_body = block.child_scopes.Add(*std::move(default_body));
  }
  return built;
}

auto BuildCaseOverValues(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    mir::LocalId subject, mir::TypeId subject_type,
    std::span<const ValueArm> items, std::optional<mir::Block> default_body)
    -> diag::Result<mir::BlockStmt> {
  BreakTarget leaves(*frame.bindings);
  std::vector<ValueArm> leaving;
  leaving.reserve(items.size());
  for (const ValueArm& item : items) {
    leaving.push_back(
        ValueArm{
            .values = item.values,
            .body = [&item,
                     &leaves](const WalkFrame& at) -> diag::Result<mir::Block> {
              auto body = item.body(at);
              if (!body) return body;
              body->AppendStmt(mir::BreakStmt{.target = leaves.Name()});
              return body;
            }});
  }

  mir::Block chosen;
  const WalkFrame chosen_frame = frame.WithBlock(&chosen);
  const auto dispatch_into = [&](mir::Block& in) -> diag::Result<void> {
    const mir::ExprId discriminant =
        BuildToInt64Call(unit, in, in.ReadLocal(subject, subject_type));
    auto dispatch = BuildValueSwitch(
        chosen_frame.WithBlock(&in), discriminant, leaving, std::nullopt);
    if (!dispatch) return std::unexpected(std::move(dispatch.error()));
    in.AppendStmt(*std::move(dispatch));
    return {};
  };
  if (CarriesUnknowns(unit, subject_type)) {
    mir::Block known;
    if (auto built = dispatch_into(known); !built) {
      return std::unexpected(std::move(built.error()));
    }
    const mir::ExprId all_known = BuildConditionNot(
        unit, chosen,
        BuildValueOperation(
            unit, chosen, support::BuiltinFn::kHasUnknown,
            chosen.ReadLocal(subject, subject_type), {},
            unit.builtins.machine_bool));
    chosen.AppendIfThen(all_known, std::move(known));
  } else if (auto built = dispatch_into(chosen); !built) {
    return std::unexpected(std::move(built.error()));
  }
  if (default_body.has_value()) {
    chosen.AppendStmt(
        mir::BlockStmt{
            .scope = chosen.child_scopes.Add(*std::move(default_body))});
  }
  return mir::BlockStmt{
      .scope = frame.current_block->child_scopes.Add(std::move(chosen)),
      .break_label = leaves.Label()};
}

// Whether an arm is selected is kept in one machine boolean, false wherever an
// arm is reached since an arm that held has left the search:
//
//   leaves: {
//     held = false
//     if (!held) held = condition 0 of arm 0
//     if (!held) held = condition 1 of arm 0
//     if (held) { arm 0; break leaves; }
//     ...
//     default
//   }
auto BuildArmSearch(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    std::span<const SearchedArm> arms, std::optional<mir::Block> default_body)
    -> diag::Result<mir::BlockStmt> {
  const mir::TypeId boolean = unit.builtins.machine_bool;
  BreakTarget leaves(*frame.bindings);
  mir::Block search;
  const WalkFrame at = frame.WithBlock(&search);
  const mir::LocalId held =
      DeclareLocal(at, BuildMachineBool(unit, search, false));

  for (const SearchedArm& arm : arms) {
    if (auto assigned = AssignAnyHoldsInOrder(unit, at, held, arm.conditions);
        !assigned) {
      return std::unexpected(std::move(assigned.error()));
    }
    auto body = arm.body(at);
    if (!body) return std::unexpected(std::move(body.error()));
    body->AppendStmt(mir::BreakStmt{.target = leaves.Name()});
    search.AppendIfThen(search.ReadLocal(held, boolean), *std::move(body));
  }
  if (default_body.has_value()) {
    search.AppendStmt(
        mir::BlockStmt{
            .scope = search.child_scopes.Add(*std::move(default_body))});
  }
  return mir::BlockStmt{
      .scope = frame.current_block->child_scopes.Add(std::move(search)),
      .break_label = leaves.Label()};
}

}  // namespace lyra::lowering::hir_to_mir
