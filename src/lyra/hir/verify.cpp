#include "lyra/hir/verify.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <ranges>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/assertion.hpp"
#include "lyra/hir/class_decl.hpp"
#include "lyra/hir/class_declaration.hpp"
#include "lyra/hir/class_id.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/coverage_library.hpp"
#include "lyra/hir/covergroup.hpp"
#include "lyra/hir/covergroup_ref.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/interface_member_access.hpp"
#include "lyra/hir/method_id.hpp"
#include "lyra/hir/owned_child_step.hpp"
#include "lyra/hir/pattern.hpp"
#include "lyra/hir/pattern_id.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/published_covergroup.hpp"
#include "lyra/hir/range_bounds.hpp"
#include "lyra/hir/sampled_history.hpp"
#include "lyra/hir/scope_construction.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/timing.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/unit_definition.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::hir {

namespace {

// How a holder names an expression. An operand is what the holder evaluates,
// and is a place the expression is reached at. A function's report and a
// sensitivity describe what a body reads or waits on, often in terms of
// expressions the body itself holds: the report evaluates what it names in an
// evaluation of its own, ahead of the body, and a sensitivity evaluates
// nothing, so naming one there is no place the body's evaluation reaches it;
// it is held only to naming an expression the arena has. A shared name is an
// expression another holder evaluates, named for the value that evaluation
// gives: the instance a synchronous drive's target reaches, which its landing
// is made on too.
enum class Slot : std::uint8_t { kOperand, kReport, kSensitivity, kShared };

void ReachSensitivityEntry(const SensitivityEntry& entry, const auto& reach) {
  std::visit(
      Overloaded{
          [](const WatchedWhole&) {},
          [&](const WatchedSelect& part) {
            reach(part.prefix, Slot::kSensitivity);
          }},
      entry.part);
}

void ReachSensitivity(
    std::span<const SensitivityEntry> entries, const auto& reach) {
  for (const SensitivityEntry& entry : entries) {
    ReachSensitivityEntry(entry, reach);
  }
}

void ReachReads(const Reads& reads, const auto& reach) {
  ReachSensitivity(reads.cells, reach);
  ReachSensitivity(reads.writes, reach);
  for (const ExprId call : reads.calls) {
    reach(call, Slot::kReport);
  }
}

void ReachEventControl(const EventControl& control, const auto& reach) {
  for (const EventEntry& entry : control.entries) {
    std::visit(
        Overloaded{
            [&](const ValueChange& c) {
              reach(c.signal, Slot::kOperand);
              ReachSensitivity(c.cells, reach);
            },
            [&](const NamedEventTrigger& t) {
              reach(t.event, Slot::kOperand);
            }},
        entry.occurrence);
    if (entry.condition.has_value()) {
      reach(*entry.condition, Slot::kOperand);
    }
  }
}

void ReachTimingControl(const TimingControl& timing, const auto& reach) {
  std::visit(
      Overloaded{
          [&](const DelayControl& c) { reach(c.duration, Slot::kOperand); },
          [&](const EventControl& c) { ReachEventControl(c, reach); },
          [&](const ImplicitEventControl& c) {
            ReachSensitivity(c.sensitivity_list, reach);
          },
          [&](const CycleDelay& c) {
            reach(c.count, Slot::kOperand);
            reach(c.event, Slot::kOperand);
          }},
      timing);
}

void ReachClockingLanding(const ClockingLanding& landing, const auto& reach) {
  std::visit(
      Overloaded{
          [](const ClockingOutputLanding&) {},
          [&](const PublishedLanding& published) {
            std::visit(
                Overloaded{
                    [](const RoutedObjectRef&) {},
                    [&](const InstanceOfTarget& target) {
                      reach(target.access, Slot::kShared);
                    }},
                published.receiver);
          }},
      landing);
}

void ReachDelayOrEventControl(
    const DelayOrEventControl& control, const auto& reach) {
  std::visit(
      Overloaded{
          [&](const DelayControl& c) { reach(c.duration, Slot::kOperand); },
          [&](const EventControl& c) { ReachEventControl(c, reach); },
          [&](const RepeatedEventControl& c) {
            reach(c.count, Slot::kOperand);
            ReachEventControl(c.event, reach);
          },
          [&](const ClockingDriveControl& c) {
            if (c.cycles.has_value()) reach(*c.cycles, Slot::kOperand);
            ReachClockingLanding(c.landing, reach);
          }},
      control);
}

void ReachEffectTiming(const EffectTiming& timing, const auto& reach) {
  std::visit(
      Overloaded{
          [](const ImmediateEffect&) {},
          [&](const NonBlockingEffect& deferred) {
            if (deferred.control.has_value()) {
              ReachDelayOrEventControl(*deferred.control, reach);
            }
          }},
      timing);
}

void ReachConditions(
    std::span<const ConditionClause> conditions, const auto& reach) {
  for (const ConditionClause& clause : conditions) {
    reach(clause.expr, Slot::kOperand);
    if (clause.pattern.has_value()) {
      reach.ThroughPattern(*clause.pattern);
    }
  }
}

void ReachPropertySpec(const PropertySpec& spec, const auto& reach) {
  ReachEventControl(spec.clock, reach);
  if (spec.disable.has_value()) {
    reach(spec.disable->condition, Slot::kOperand);
    ReachSensitivity(spec.disable->sensitivity, reach);
  }
  reach.ThroughProperty(spec.body);
}

// What an access through a virtual interface evaluates: the handle, and what
// selects each loop's block on the way down.
void ReachInstanceAccess(
    const InterfaceInstanceAccessExpr& access, const auto& reach) {
  reach(access.handle, Slot::kOperand);
  for (const PublishedStep& step : access.steps) {
    if (const auto* loop = std::get_if<PublishedGenerateLoopRef>(&step.names)) {
      reach(loop->position, Slot::kOperand);
    }
  }
}

// The expressions a callee holds: the object a call is made on, where the
// callee names it by an expression, and the expression an array method
// evaluates at each entry (LRM 7.12).
void ReachCallee(const SubroutineRef& callee, const auto& reach) {
  std::visit(
      Overloaded{
          [](const StructuralSubroutineRef&) {},
          [&](const MethodCallRef& ref) {
            std::visit(
                Overloaded{
                    [&](const HandleReceiver& receiver) {
                      reach(receiver.expr, Slot::kOperand);
                    },
                    [](const SelfReceiver&) {}, [](const SuperReceiver&) {}},
                ref.receiver);
          },
          [](const StaticMethodCallRef&) {}, [](const SystemSubroutineRef&) {},
          [&](const BuiltinMethodRef& ref) {
            if (ref.receiver.has_value()) {
              reach(*ref.receiver, Slot::kOperand);
            }
          },
          [](const EnumMethodRef&) {},
          [&](const ArrayIteratorMethodRef& ref) {
            if (ref.with_clause.has_value()) {
              reach(ref.with_clause->expr, Slot::kOperand);
            }
          },
          [](const PastValueRef&) {}, [](const ValueChangeRef&) {},
          [](const ForeignImportRef&) {}, [](const NamespaceSubroutineRef&) {},
          [&](const PublishedScopeMethodRef& ref) {
            std::visit(
                Overloaded{
                    [](const RoutedObjectRef&) {},
                    [&](const InterfaceInstanceAccessExpr& access) {
                      ReachInstanceAccess(access, reach);
                    }},
                ref.receiver);
          }},
      callee);
}

void ReachRangeBounds(const RangeBounds& bounds, const auto& reach) {
  std::visit(
      Overloaded{
          [&](const RangeConstantBounds& b) {
            reach(b.left_bound, Slot::kOperand);
            reach(b.right_bound, Slot::kOperand);
          },
          [&](const RangeIndexedUpBounds& b) {
            reach(b.base_index, Slot::kOperand);
            reach(b.width, Slot::kOperand);
          },
          [&](const RangeIndexedDownBounds& b) {
            reach(b.base_index, Slot::kOperand);
            reach(b.width, Slot::kOperand);
          }},
      bounds);
}

void ReachReceiver(const CovergroupReceiver& receiver, const auto& operand) {
  std::visit(
      Overloaded{
          [&](const CovergroupInstanceReceiver& r) { operand(r.instance); },
          [](const CovergroupTypeReceiver&) {}},
      receiver);
}

// Calls `reach` with each expression `data` holds, all of them in the arena
// that holds `data`.
void ForEachOperand(const ExprData& data, const auto& reach) {
  const auto operand = [&](ExprId id) { reach(id, Slot::kOperand); };
  const auto operands = [&](std::span<const ExprId> ids) {
    for (const ExprId id : ids) {
      operand(id);
    }
  };
  const auto optional_operand = [&](const std::optional<ExprId>& id) {
    if (id.has_value()) {
      operand(*id);
    }
  };
  std::visit(
      Overloaded{
          [](const PrimaryExpr&) {},
          [&](const UnaryExpr& e) { operand(e.operand); },
          [&](const BinaryExpr& e) {
            operand(e.lhs);
            operand(e.rhs);
          },
          [&](const ConditionalExpr& e) {
            ReachConditions(e.conditions, reach);
            operand(e.then_value);
            operand(e.else_value);
          },
          [&](const AssignExpr& e) {
            ReachEffectTiming(e.timing, reach);
            operand(e.lhs);
            operand(e.rhs);
          },
          [&](const IncDecExpr& e) { operand(e.target); },
          [&](const CallExpr& e) {
            ReachCallee(e.callee, reach);
            for (const std::optional<ExprId>& argument : e.arguments) {
              optional_operand(argument);
            }
          },
          [](const CalleeDefaultExpr&) {},
          [&](const ConversionExpr& e) { operand(e.operand); },
          [&](const ConstantExpr& e) { operand(e.value); },
          [&](const ValueRangeExpr& e) {
            optional_operand(e.lo);
            optional_operand(e.hi);
          },
          [&](const InsideExpr& e) {
            operand(e.lhs);
            operands(e.items);
          },
          [&](const ElementSelectExpr& e) {
            operand(e.base_value);
            operand(e.index);
          },
          [&](const RangeSelectExpr& e) {
            operand(e.base_value);
            ReachRangeBounds(e.bounds, reach);
          },
          [&](const MemberAccessExpr& e) { operand(e.base_value); },
          [&](const ClassPropertyAccessExpr& e) { operand(e.base_value); },
          [&](const InterfaceMemberAccessExpr& e) {
            ReachInstanceAccess(e.instance, reach);
          },
          [&](const InterfaceInstanceAccessExpr& e) {
            ReachInstanceAccess(e, reach);
          },
          [&](const ConcatExpr& e) { operands(e.operands); },
          [&](const StreamingConcatExpr& e) {
            for (const StreamOperand& streamed : e.operands) {
              operand(streamed.value);
              if (streamed.with.has_value()) {
                std::visit(
                    Overloaded{
                        [&](ExprId element) { operand(element); },
                        [&](const RangeBounds& bounds) {
                          ReachRangeBounds(bounds, reach);
                        }},
                    *streamed.with);
              }
            }
          },
          [&](const ReplicationExpr& e) {
            operand(e.count);
            operand(e.concat);
          },
          [&](const AssignmentPatternExpr& e) { operands(e.elements); },
          [&](const AssignmentPatternReplicationExpr& e) {
            operand(e.count);
            operands(e.items);
          },
          [&](const DynamicArrayNewExpr& e) {
            operand(e.size);
            optional_operand(e.initializer);
          },
          [&](const ClassNewExpr& e) { operands(e.arguments); },
          [&](const AssociativeAssignmentPatternExpr& e) {
            for (const auto& entry : e.entries) {
              operand(entry.key);
              operand(entry.value);
            }
            optional_operand(e.default_value);
          },
          [&](const AssignmentPatternKeyedExpr& e) {
            for (const auto& entry : e.entries) {
              operand(entry.index);
              operand(entry.value);
            }
            optional_operand(e.default_value);
          },
          [&](const TaggedUnionExpr& e) { optional_operand(e.payload); },
          [&](const DynamicCastExpr& e) {
            operand(e.destination);
            operand(e.source);
          },
          [&](const CovergroupNewExpr& e) {
            operands(e.arguments);
            optional_operand(e.embedding);
          },
          [&](const CovergroupMethodCallExpr& e) {
            ReachReceiver(e.receiver, operand);
            for (const std::optional<ExprId>& argument : e.arguments) {
              optional_operand(argument);
            }
          },
          [&](const CovergroupOptionExpr& e) {
            ReachReceiver(e.receiver, operand);
          },
          [&](const RandomizeExpr& e) {
            std::visit(
                Overloaded{
                    [&](const RandomizedClassObject& o) {
                      operand(o.object);
                      std::visit(
                          Overloaded{
                              [](const DeclaredRandomVariables&) {},
                              [&](const ListedRandomVariables& l) {
                                operands(l.variables);
                              }},
                          o.variables);
                    },
                    [&](const RandomizedScopeVariables& v) {
                      operands(v.variables);
                    }},
                e.target);
            if (e.with.has_value()) {
              reach.ThroughConstraint(*e.with);
            }
          },
          [&](const RandModeSetExpr& e) {
            std::visit(
                Overloaded{
                    [&](const RandomVariableMode& m) { operand(m.variable); },
                    [&](const ObjectModes& m) { operand(m.object); }},
                e.target);
            operand(e.on);
          },
          [&](const RandModeGetExpr& e) { operand(e.target.variable); },
          [&](const ConstraintModeSetExpr& e) {
            std::visit(
                Overloaded{
                    [&](const ConstraintBlockMode& m) { operand(m.object); },
                    [](const StaticConstraintBlockMode&) {},
                    [&](const ObjectModes& m) { operand(m.object); }},
                e.target);
            operand(e.on);
          },
          [&](const ConstraintModeGetExpr& e) {
            std::visit(
                Overloaded{
                    [&](const ConstraintBlockMode& m) { operand(m.object); },
                    [](const StaticConstraintBlockMode&) {}},
                e.target);
          }},
      data);
}

// Calls `reach` with each expression `data` holds itself, as opposed to through
// a statement, a pattern or a variable it names.
void ForEachExpr(const StmtData& data, const auto& reach) {
  const auto operand = [&](ExprId id) { reach(id, Slot::kOperand); };
  std::visit(
      Overloaded{
          [](const EmptyStmt&) {},
          [](const VarDeclStmt&) {},
          [&](const ExprStmt& s) { operand(s.expr); },
          [](const BlockStmt&) {},
          [](const ForkStmt&) {},
          [&](const IfStmt& s) { ReachConditions(s.conditions, reach); },
          [&](const CaseStmt& s) {
            operand(s.condition);
            for (const CaseItem& item : s.items) {
              for (const ExprId label : item.labels) {
                operand(label);
              }
            }
          },
          [&](const PatternCaseStmt& s) {
            operand(s.condition);
            for (const PatternCaseItem& item : s.items) {
              reach.ThroughPattern(item.pattern);
              if (item.filter.has_value()) {
                operand(*item.filter);
              }
            }
          },
          [&](const AssertStmt& s) { operand(s.condition); },
          [&](const CoverStmt& s) { operand(s.condition); },
          [&](const ConcurrentAssertStmt& s) {
            ReachPropertySpec(s.spec, reach);
          },
          [&](const ConcurrentCoverStmt& s) {
            ReachPropertySpec(s.spec, reach);
          },
          [&](const ExpectStmt& s) { ReachPropertySpec(s.spec, reach); },
          [&](const ForStmt& s) {
            for (const ExprId init : s.init) {
              operand(init);
            }
            if (s.condition.has_value()) {
              operand(*s.condition);
            }
            for (const ExprId step : s.step) {
              operand(step);
            }
          },
          [&](const ForeachStmt& s) { operand(s.array); },
          [&](const WhileStmt& s) { operand(s.condition); },
          [&](const RepeatStmt& s) { operand(s.count); },
          [&](const DoWhileStmt& s) { operand(s.condition); },
          [](const ForeverStmt&) {},
          [](const BreakStmt&) {},
          [](const ContinueStmt&) {},
          [&](const ReturnStmt& s) {
            if (s.value.has_value()) {
              operand(*s.value);
            }
          },
          [&](const TimedStmt& s) { ReachTimingControl(s.timing, reach); },
          [&](const EventTriggerStmt& s) {
            operand(s.event);
            ReachEffectTiming(s.timing, reach);
          },
          [&](const WaitStmt& s) {
            operand(s.cond);
            ReachSensitivity(s.cells, reach);
          },
          [](const WaitForkStmt&) {},
          [](const DisableForkStmt&) {},
          [](const DisableStmt&) {},
          [&](const ProceduralContinuousAssignStmt& s) {
            operand(s.target);
            operand(s.source);
            ReachSensitivity(s.sensitivity_list, reach);
          },
          [&](const ProceduralContinuousEndStmt& s) { operand(s.target); },
          [&](const ConstraintStmt& s) {
            reach.ThroughConstraint(s.constraints);
          }},
      data);
}

// Calls `enter` with each statement `data` holds.
void ForEachChildStmt(const StmtData& data, const auto& enter) {
  const auto optional_child = [&](const std::optional<StmtId>& id) {
    if (id.has_value()) {
      enter(*id);
    }
  };
  std::visit(
      Overloaded{
          [](const EmptyStmt&) {},
          [](const VarDeclStmt&) {},
          [](const ExprStmt&) {},
          [&](const BlockStmt& s) {
            for (const StmtId id : s.statements) {
              enter(id);
            }
          },
          [&](const ForkStmt& s) {
            for (const StmtId id : s.locals) {
              enter(id);
            }
            for (const StmtId id : s.branches) {
              enter(id);
            }
          },
          [&](const IfStmt& s) {
            enter(s.then_stmt);
            optional_child(s.else_stmt);
          },
          [&](const CaseStmt& s) {
            for (const CaseItem& item : s.items) {
              enter(item.stmt);
            }
            optional_child(s.default_stmt);
          },
          [&](const PatternCaseStmt& s) {
            for (const PatternCaseItem& item : s.items) {
              enter(item.stmt);
            }
            optional_child(s.default_stmt);
          },
          [&](const AssertStmt& s) {
            optional_child(s.pass_stmt);
            optional_child(s.fail_stmt);
          },
          [&](const CoverStmt& s) { optional_child(s.pass_stmt); },
          [&](const ConcurrentAssertStmt& s) {
            optional_child(s.pass_stmt);
            optional_child(s.fail_stmt);
          },
          [&](const ConcurrentCoverStmt& s) { optional_child(s.pass_stmt); },
          [&](const ExpectStmt& s) {
            optional_child(s.pass_stmt);
            optional_child(s.fail_stmt);
          },
          [&](const ForStmt& s) { enter(s.body); },
          [&](const ForeachStmt& s) { enter(s.body); },
          [&](const WhileStmt& s) { enter(s.body); },
          [&](const RepeatStmt& s) { enter(s.body); },
          [&](const DoWhileStmt& s) { enter(s.body); },
          [&](const ForeverStmt& s) { enter(s.body); },
          [](const BreakStmt&) {},
          [](const ContinueStmt&) {},
          [](const ReturnStmt&) {},
          [&](const TimedStmt& s) { enter(s.stmt); },
          [](const EventTriggerStmt&) {},
          [&](const WaitStmt& s) { enter(s.body); },
          [](const WaitForkStmt&) {},
          [](const DisableForkStmt&) {},
          [](const DisableStmt&) {},
          [](const ProceduralContinuousAssignStmt&) {},
          [](const ProceduralContinuousEndStmt&) {},
          [](const ConstraintStmt&) {}},
      data);
}

auto KindOf(const Primary& primary) -> std::string_view {
  return std::visit(
      Overloaded{
          [](const IntegerLiteral&) -> std::string_view {
            return "an integer literal";
          },
          [](const StringLiteral&) -> std::string_view {
            return "a string literal";
          },
          [](const RealLiteral&) -> std::string_view {
            return "a real literal";
          },
          [](const NullLiteral&) -> std::string_view {
            return "a null literal";
          },
          [](const ThisHandle&) -> std::string_view { return "a this handle"; },
          [](const RandomizedObject&) -> std::string_view {
            return "the object a randomize call randomizes";
          },
          [](const QueueLastIndex&) -> std::string_view {
            return "a queue's last index";
          },
          [](const ProceduralVarRef&) -> std::string_view {
            return "a local variable reference";
          },
          [](const ClassPropertyRef&) -> std::string_view {
            return "a property reference";
          },
          [](const StaticPropertyRef&) -> std::string_view {
            return "a static property reference";
          },
          [](const RoutedValueRef&) -> std::string_view {
            return "a routed value reference";
          },
          [](const RoutedViewPlaceRef&) -> std::string_view {
            return "a routed view place reference";
          },
          [](const RoutedViewValueRef&) -> std::string_view {
            return "a routed view value reference";
          },
          [](const RoutedObjectRef&) -> std::string_view {
            return "a routed object reference";
          },
          [](const ScopeArgumentRef&) -> std::string_view {
            return "a scope argument reference";
          },
          [](const IterationBindingRef&) -> std::string_view {
            return "an iteration binding reference";
          },
          [](const PatternVarRef&) -> std::string_view {
            return "a pattern variable reference";
          },
          [](const NamespaceValueRef&) -> std::string_view {
            return "a namespace variable reference";
          }},
      primary);
}

// A call as a report names it. A builtin method is named by its entry: many
// constructs lower to one, and the entry is what says which construct this
// was.
auto DescribeCall(const SubroutineRef& callee) -> std::string {
  return std::visit(
      Overloaded{
          [](const StructuralSubroutineRef&) -> std::string {
            return "a subroutine call";
          },
          [](const MethodCallRef&) -> std::string { return "a method call"; },
          [](const StaticMethodCallRef&) -> std::string {
            return "a static method call";
          },
          [](const SystemSubroutineRef&) -> std::string {
            return "a system subroutine call";
          },
          [](const BuiltinMethodRef& ref) -> std::string {
            return std::format(
                "a call of the builtin method '{}'",
                support::RuntimeEntryOf(ref.method).name);
          },
          [](const EnumMethodRef&) -> std::string {
            return "an enum method call";
          },
          [](const ArrayIteratorMethodRef&) -> std::string {
            return "an array iterator method call";
          },
          [](const PastValueRef&) -> std::string {
            return "a past value read";
          },
          [](const ValueChangeRef&) -> std::string {
            return "a value change read";
          },
          [](const ForeignImportRef&) -> std::string {
            return "a foreign import call";
          },
          [](const NamespaceSubroutineRef&) -> std::string {
            return "a namespace subroutine call";
          },
          [](const PublishedScopeMethodRef&) -> std::string {
            return "a call on another unit's scope";
          }},
      callee);
}

// What a node is, in the words a report of it uses.
auto Describe(const ExprData& data) -> std::string {
  return std::visit(
      Overloaded{
          [](const PrimaryExpr& e) -> std::string {
            return std::string{KindOf(e.data)};
          },
          [](const UnaryExpr&) -> std::string { return "a unary operation"; },
          [](const BinaryExpr&) -> std::string { return "a binary operation"; },
          [](const ConditionalExpr&) -> std::string { return "a conditional"; },
          [](const AssignExpr&) -> std::string { return "an assignment"; },
          [](const IncDecExpr&) -> std::string {
            return "an increment or decrement";
          },
          [](const CallExpr& e) -> std::string {
            return DescribeCall(e.callee);
          },
          [](const CalleeDefaultExpr&) -> std::string {
            return "an omitted argument's default";
          },
          [](const ConversionExpr&) -> std::string { return "a conversion"; },
          [](const ConstantExpr&) -> std::string { return "a constant"; },
          [](const ValueRangeExpr&) -> std::string { return "a value range"; },
          [](const InsideExpr&) -> std::string { return "a set membership"; },
          [](const ElementSelectExpr&) -> std::string {
            return "an element select";
          },
          [](const RangeSelectExpr&) -> std::string {
            return "a range select";
          },
          [](const MemberAccessExpr&) -> std::string {
            return "a member access";
          },
          [](const ClassPropertyAccessExpr&) -> std::string {
            return "a property access";
          },
          [](const InterfaceMemberAccessExpr&) -> std::string {
            return "an interface member access";
          },
          [](const InterfaceInstanceAccessExpr&) -> std::string {
            return "an interface instance access";
          },
          [](const ConcatExpr&) -> std::string { return "a concatenation"; },
          [](const StreamingConcatExpr&) -> std::string {
            return "a streaming concatenation";
          },
          [](const ReplicationExpr&) -> std::string { return "a replication"; },
          [](const AssignmentPatternExpr&) -> std::string {
            return "an assignment pattern";
          },
          [](const AssignmentPatternReplicationExpr&) -> std::string {
            return "a replicated assignment pattern";
          },
          [](const DynamicArrayNewExpr&) -> std::string {
            return "a dynamic array construction";
          },
          [](const ClassNewExpr&) -> std::string {
            return "an object construction";
          },
          [](const AssociativeAssignmentPatternExpr&) -> std::string {
            return "an associative array pattern";
          },
          [](const AssignmentPatternKeyedExpr&) -> std::string {
            return "a keyed assignment pattern";
          },
          [](const TaggedUnionExpr&) -> std::string {
            return "a tagged union expression";
          },
          [](const DynamicCastExpr&) -> std::string {
            return "a dynamic cast";
          },
          [](const CovergroupNewExpr&) -> std::string {
            return "a covergroup construction";
          },
          [](const CovergroupMethodCallExpr&) -> std::string {
            return "a covergroup method call";
          },
          [](const CovergroupOptionExpr&) -> std::string {
            return "a covergroup option";
          },
          [](const RandomizeExpr&) -> std::string {
            return "a randomize call";
          },
          [](const RandModeSetExpr&) -> std::string {
            return "a rand_mode call";
          },
          [](const RandModeGetExpr&) -> std::string {
            return "a rand_mode query";
          },
          [](const ConstraintModeSetExpr&) -> std::string {
            return "a constraint_mode call";
          },
          [](const ConstraintModeGetExpr&) -> std::string {
            return "a constraint_mode query";
          }},
      data);
}

auto KindOf(const StmtData& data) -> std::string_view {
  return std::visit(
      Overloaded{
          [](const EmptyStmt&) -> std::string_view {
            return "an empty statement";
          },
          [](const VarDeclStmt&) -> std::string_view {
            return "a variable declaration statement";
          },
          [](const ExprStmt&) -> std::string_view {
            return "an expression statement";
          },
          [](const BlockStmt&) -> std::string_view { return "a block"; },
          [](const ForkStmt&) -> std::string_view { return "a fork"; },
          [](const IfStmt&) -> std::string_view { return "an if statement"; },
          [](const CaseStmt&) -> std::string_view {
            return "a case statement";
          },
          [](const PatternCaseStmt&) -> std::string_view {
            return "a pattern case statement";
          },
          [](const AssertStmt&) -> std::string_view {
            return "an immediate assertion";
          },
          [](const CoverStmt&) -> std::string_view {
            return "an immediate cover";
          },
          [](const ConcurrentAssertStmt&) -> std::string_view {
            return "a concurrent assertion";
          },
          [](const ConcurrentCoverStmt&) -> std::string_view {
            return "a concurrent cover";
          },
          [](const ExpectStmt&) -> std::string_view {
            return "an expect statement";
          },
          [](const ForStmt&) -> std::string_view { return "a for statement"; },
          [](const ForeachStmt&) -> std::string_view {
            return "a foreach statement";
          },
          [](const WhileStmt&) -> std::string_view {
            return "a while statement";
          },
          [](const RepeatStmt&) -> std::string_view {
            return "a repeat statement";
          },
          [](const DoWhileStmt&) -> std::string_view {
            return "a do-while statement";
          },
          [](const ForeverStmt&) -> std::string_view {
            return "a forever statement";
          },
          [](const BreakStmt&) -> std::string_view { return "a break"; },
          [](const ContinueStmt&) -> std::string_view { return "a continue"; },
          [](const ReturnStmt&) -> std::string_view {
            return "a return statement";
          },
          [](const TimedStmt&) -> std::string_view {
            return "a timed statement";
          },
          [](const EventTriggerStmt&) -> std::string_view {
            return "an event trigger statement";
          },
          [](const WaitStmt&) -> std::string_view {
            return "a wait statement";
          },
          [](const WaitForkStmt&) -> std::string_view { return "a wait fork"; },
          [](const DisableForkStmt&) -> std::string_view {
            return "a disable fork";
          },
          [](const DisableStmt&) -> std::string_view {
            return "a disable statement";
          },
          [](const ProceduralContinuousAssignStmt&) -> std::string_view {
            return "a procedural continuous assignment";
          },
          [](const ProceduralContinuousEndStmt&) -> std::string_view {
            return "a deassign or release statement";
          },
          [](const ConstraintStmt&) -> std::string_view {
            return "a constraint block's constraints";
          }},
      data);
}

// A place an expression is reached from: the expression holding it, or the
// name of a holder that is none, and how that holder reaches it.
struct Place {
  std::optional<ExprId> from;
  std::string_view holder;
  Slot slot;
};

// The places each expression of one arena is reached from, and the places that
// name an expression the arena does not hold: an id is a position in the
// arena of the body that minted it, so one carried into another body lands
// outside it or on an unrelated node.
struct ArenaReaches {
  const base::Arena<Expr, ExprId>* exprs;
  const base::Arena<Pattern, PatternId>* patterns;
  // The trees a property is written in, which only a procedural body has.
  const base::Arena<SequenceExpr, SequenceExprId>* sequences = nullptr;
  const base::Arena<PropertyExpr, PropertyExprId>* properties = nullptr;
  // The constraints written in it (LRM 18.5, 18.7), which only a procedural
  // body has.
  const base::Arena<Constraint, ConstraintId>* constraints = nullptr;
  std::vector<std::vector<Place>> at;
  std::vector<std::pair<ExprId, Place>> outside;

  explicit ArenaReaches(const ProceduralBody& body)
      : exprs(&body.exprs),
        patterns(&body.patterns),
        sequences(&body.sequence_exprs),
        properties(&body.property_exprs),
        constraints(&body.constraints),
        at(body.exprs.size()) {
  }

  explicit ArenaReaches(const StructuralScope& scope)
      : exprs(&scope.exprs), patterns(&scope.patterns), at(scope.exprs.size()) {
  }
};

// What records the expressions one holder names: the expression `from`, or the
// holder named `holder` where there is none. An expression is evaluated at
// every place that reaches it, and what it holds is evaluated when it is, so
// its own operands are followed on its first reach and not again. An
// expression nothing holds is never reached, and so is no place its operands
// are reached at: nothing evaluates it.
struct Reacher {
  ArenaReaches* reaches;
  std::optional<ExprId> from;
  std::string_view holder;

  void operator()(ExprId id, Slot slot) const {
    const Place place{.from = from, .holder = holder, .slot = slot};
    if (id.value >= reaches->at.size()) {
      reaches->outside.emplace_back(id, place);
      return;
    }
    switch (slot) {
      case Slot::kReport:
      case Slot::kSensitivity:
      case Slot::kShared:
        return;
      case Slot::kOperand:
        break;
    }
    std::vector<Place>& places = reaches->at.at(id.value);
    places.push_back(place);
    if (places.size() > 1) {
      return;
    }
    ForEachOperand(
        reaches->exprs->Get(id).data,
        Reacher{.reaches = reaches, .from = id, .holder = {}});
  }

  // The expressions a pattern holds are the constants it matches against (LRM
  // 12.6), however deep in it they are written.
  void ThroughPattern(PatternId id) const {
    std::visit(
        Overloaded{
            [](const WildcardPattern&) {},
            [&](const ConstantPattern& pattern) {
              const Reacher from_pattern{
                  .reaches = reaches,
                  .from = std::nullopt,
                  .holder = "a constant pattern"};
              from_pattern(pattern.value, Slot::kOperand);
            },
            [](const VariablePattern&) {},
            [&](const TaggedPattern& pattern) {
              if (pattern.value_pattern.has_value()) {
                ThroughPattern(*pattern.value_pattern);
              }
            },
            [&](const StructurePattern& pattern) {
              for (const auto& field : pattern.field_patterns) {
                ThroughPattern(field.second);
              }
            }},
        reaches->patterns->Get(id).data);
  }

  // The expressions a property holds are the Boolean expressions its sequences
  // are built from (LRM 16.7), and what an if-else or a case property selects
  // by (LRM 16.12.6, 16.12.16).
  void ThroughProperty(PropertyExprId id) const {
    if (reaches->properties == nullptr) {
      throw InternalError(
          "hir verify: a property was written where no body holds its tree");
    }
    const Reacher from_property{
        .reaches = reaches, .from = std::nullopt, .holder = "a property"};
    std::visit(
        Overloaded{
            [&](const PropertySequence& property) {
              ThroughSequence(property.sequence);
            },
            [&](const PropertyImplication& property) {
              ThroughSequence(property.antecedent);
              ThroughProperty(property.consequent);
            },
            [&](const PropertyFollowedBy& property) {
              ThroughSequence(property.antecedent);
              ThroughProperty(property.consequent);
            },
            [&](const PropertyNot& property) {
              ThroughProperty(property.operand);
            },
            [&](const PropertyBinary& property) {
              ThroughProperty(property.lhs);
              ThroughProperty(property.rhs);
            },
            [&](const PropertyConditional& property) {
              from_property(property.condition, Slot::kOperand);
              ThroughProperty(property.then_property);
              if (property.else_property.has_value()) {
                ThroughProperty(*property.else_property);
              }
            },
            [&](const PropertyCase& property) {
              from_property(property.subject, Slot::kOperand);
              for (const PropertyCaseItem& item : property.items) {
                for (const ExprId label : item.labels) {
                  from_property(label, Slot::kOperand);
                }
                ThroughProperty(item.property);
              }
              if (property.default_property.has_value()) {
                ThroughProperty(*property.default_property);
              }
            }},
        reaches->properties->Get(id).data);
  }

  // The expressions a constraint holds, however deep in its tree (LRM 18.5).
  void ThroughConstraint(ConstraintId id) const {
    if (reaches->constraints == nullptr) {
      throw InternalError(
          "hir verify: a constraint was written where no body holds its tree");
    }
    const Reacher from_constraint{
        .reaches = reaches, .from = std::nullopt, .holder = "a constraint"};
    const auto operand = [&](ExprId e) { from_constraint(e, Slot::kOperand); };
    std::visit(
        Overloaded{
            [&](const ConstraintList& c) {
              for (const ConstraintId item : c.items) {
                ThroughConstraint(item);
              }
            },
            [&](const ExpressionConstraint& c) { operand(c.condition); },
            [&](const DistConstraint& c) {
              operand(c.subject);
              for (const DistItem& item : c.items) {
                operand(item.low);
                if (item.high.has_value()) operand(*item.high);
                if (item.weight.has_value()) operand(*item.weight);
              }
              if (c.default_weight.has_value()) operand(*c.default_weight);
            },
            [&](const ImplicationConstraint& c) {
              operand(c.predicate);
              ThroughConstraint(c.body);
            },
            [&](const ConditionalConstraint& c) {
              operand(c.predicate);
              ThroughConstraint(c.then_body);
              if (c.else_body.has_value()) ThroughConstraint(*c.else_body);
            },
            [&](const UniquenessConstraint& c) {
              for (const ExprId item : c.items) operand(item);
            },
            [&](const ForeachConstraint& c) {
              operand(c.array);
              ThroughConstraint(c.body);
            },
            [&](const SolveBeforeConstraint& c) {
              for (const ExprId e : c.solve) operand(e);
              for (const ExprId e : c.after) operand(e);
            },
            [&](const DisableSoftConstraint& c) { operand(c.target); }},
        reaches->constraints->Get(id).data);
  }

  void ThroughSequence(SequenceExprId id) const {
    std::visit(
        Overloaded{
            [&](const SequenceBoolean& sequence) {
              const Reacher from_sequence{
                  .reaches = reaches,
                  .from = std::nullopt,
                  .holder = "a sequence"};
              from_sequence(sequence.condition, Slot::kOperand);
            },
            [&](const SequenceDelay& sequence) {
              ThroughSequence(sequence.head);
              ThroughSequence(sequence.tail);
            },
            [&](const SequenceRepetition& sequence) {
              ThroughSequence(sequence.body);
            }},
        reaches->sequences->Get(id).data);
  }
};

auto ReachFrom(
    ArenaReaches& reaches, std::optional<ExprId> from, std::string_view holder)
    -> Reacher {
  return Reacher{.reaches = &reaches, .from = from, .holder = holder};
}

auto DescribePlace(const base::Arena<Expr, ExprId>& exprs, const Place& place)
    -> std::string {
  const std::string holder = place.from.has_value()
                                 ? Describe(exprs.Get(*place.from).data)
                                 : std::string{place.holder};
  switch (place.slot) {
    case Slot::kOperand:
      return holder;
    case Slot::kReport:
      return std::format("the report of {}", holder);
    case Slot::kSensitivity:
      return std::format("the sensitivity of {}", holder);
    case Slot::kShared:
      return std::format("the value {} shares", holder);
  }
  throw InternalError("hir::DescribePlace: unknown Slot");
}

// Reaches what the statement `id` of `body` holds, and what every statement
// under it does.
void ReachFromStmt(
    const ProceduralBody& body, ArenaReaches& reaches, StmtId id) {
  const StmtData& data = body.stmts.Get(id).data;
  ForEachExpr(data, ReachFrom(reaches, std::nullopt, KindOf(data)));
  ForEachChildStmt(
      data, [&](StmtId child) { ReachFromStmt(body, reaches, child); });
}

// Adds one line to `lines` for each expression reached from two or more
// places, and one for each reach that left the arena. `where` names the body or
// scope, and is asked only once there is something to report: this runs over
// every body of every unit, and composing a name for each costs more than the
// count itself.
void Report(
    const ArenaReaches& reaches, const auto& where,
    std::vector<std::string>& lines) {
  for (const auto& [id, place] : reaches.outside) {
    lines.push_back(
        std::format(
            "hir verify: {} in {} names expression {}, which the arena it "
            "indexes does not hold",
            DescribePlace(*reaches.exprs, place), where(), id.value));
  }
  for (const ExprId id : reaches.exprs->Ids()) {
    const std::vector<Place>& places = reaches.at.at(id.value);
    if (places.size() < 2) {
      continue;
    }
    std::string standing;
    for (const Place& place : places | std::views::take(3)) {
      if (!standing.empty()) {
        standing += ", ";
      }
      standing += DescribePlace(*reaches.exprs, place);
    }
    lines.push_back(
        std::format(
            "hir verify: {} (expression {}) in {} is reached from {} places: "
            "{}",
            Describe(reaches.exprs->Get(id).data), id.value, where(),
            places.size(), standing));
  }
}

void ForEachClassRead(const Primary& primary, const auto& read) {
  std::visit(
      Overloaded{
          [](const IntegerLiteral&) {}, [](const StringLiteral&) {},
          [](const RealLiteral&) {}, [](const NullLiteral&) {},
          [](const ThisHandle&) {}, [](const RandomizedObject&) {},
          [](const QueueLastIndex&) {}, [](const ProceduralVarRef&) {},
          [&](const ClassPropertyRef& r) { read(r.target.owner); },
          [&](const StaticPropertyRef& r) {
            std::visit(
                Overloaded{
                    [&](const InstanceStaticPropertyTarget& t) {
                      read(t.property.owner);
                    },
                    [&](const ClassStaticPropertyTarget& t) { read(t.owner); }},
                r.target);
          },
          [](const RoutedValueRef&) {}, [](const RoutedViewPlaceRef&) {},
          [](const RoutedViewValueRef&) {}, [](const RoutedObjectRef&) {},
          [](const ScopeArgumentRef&) {}, [](const IterationBindingRef&) {},
          [](const PatternVarRef&) {}, [](const NamespaceValueRef&) {}},
      primary);
}

void ForEachClassRead(const SubroutineRef& callee, const auto& read) {
  std::visit(
      Overloaded{
          [](const StructuralSubroutineRef&) {},
          [&](const MethodCallRef& ref) {
            std::visit(
                Overloaded{
                    [&](const ClassMethodTarget& target) {
                      read(target.owner);
                    },
                    [&](const BuiltinHookCallee& hook) { read(hook.cls); }},
                ref.callee);
          },
          [&](const StaticMethodCallRef& ref) { read(ref.callee.owner); },
          [](const SystemSubroutineRef&) {}, [](const BuiltinMethodRef&) {},
          [](const EnumMethodRef&) {}, [](const ArrayIteratorMethodRef&) {},
          [](const PastValueRef&) {}, [](const ValueChangeRef&) {},
          [](const ForeignImportRef&) {}, [](const NamespaceSubroutineRef&) {},
          [](const PublishedScopeMethodRef&) {}},
      callee);
}

// Calls `read` with each class `data` reads: the class whose declaration says
// what the node means. A method call reads the class declaring the method, and
// a call of a hook a class has without declaring it reads that class, whose
// lineage says which body runs (LRM 18.6.2); a property reads the class
// declaring it; a construction reads the class it builds; and a cast to a class
// of an instance reads the destination's class, which says whether it is an
// interface class (LRM 8.26.5). A handle's type only names its class.
void ForEachClassRead(
    const CompilationUnit& unit, const base::Arena<Expr, ExprId>& exprs,
    const ExprData& data, const auto& read) {
  std::visit(
      Overloaded{
          [&](const PrimaryExpr& e) { ForEachClassRead(e.data, read); },
          [](const UnaryExpr&) {},
          [](const BinaryExpr&) {},
          [](const ConditionalExpr&) {},
          [](const AssignExpr&) {},
          [](const IncDecExpr&) {},
          [&](const CallExpr& e) { ForEachClassRead(e.callee, read); },
          [](const CalleeDefaultExpr&) {},
          [](const ConversionExpr&) {},
          [](const ConstantExpr&) {},
          [](const ValueRangeExpr&) {},
          [](const InsideExpr&) {},
          [](const ElementSelectExpr&) {},
          [](const RangeSelectExpr&) {},
          [](const MemberAccessExpr&) {},
          [&](const ClassPropertyAccessExpr& e) { read(e.target.owner); },
          [](const InterfaceMemberAccessExpr&) {},
          [](const InterfaceInstanceAccessExpr&) {},
          [](const ConcatExpr&) {},
          [](const StreamingConcatExpr&) {},
          [](const ReplicationExpr&) {},
          [](const AssignmentPatternExpr&) {},
          [](const AssignmentPatternReplicationExpr&) {},
          [](const DynamicArrayNewExpr&) {},
          [&](const ClassNewExpr& e) { read(e.class_id); },
          [](const AssociativeAssignmentPatternExpr&) {},
          [](const AssignmentPatternKeyedExpr&) {},
          [](const TaggedUnionExpr&) {},
          [&](const DynamicCastExpr& e) {
            if (!e.destination_instance.has_value()) return;
            const auto* handle = unit.types.Get(exprs.Get(e.destination).type)
                                     .As<ClassHandleType>();
            if (handle != nullptr) read(handle->class_id);
          },
          [](const CovergroupNewExpr&) {},
          [](const CovergroupMethodCallExpr&) {},
          [](const CovergroupOptionExpr&) {},
          [](const RandomizeExpr&) {},
          [](const RandModeSetExpr&) {},
          [](const RandModeGetExpr&) {},
          [](const ConstraintModeSetExpr&) {},
          [](const ConstraintModeGetExpr&) {}},
      data);
}

// Adds one line to `lines` where `unit` holds no declaration of `cls`, which
// `reader` names something reading. A unit holds what another unit published
// of every class it reads, and nothing below HIR reads a signature.
void RequireDeclaration(
    const CompilationUnit& unit, ClassId cls, const auto& reader,
    std::vector<std::string>& lines) {
  if (unit.classes.HasDeclaration(cls)) return;
  lines.push_back(
      std::format(
          "hir verify: {} reads class '{}' of unit '{}', of which unit '{}' "
          "holds no declaration",
          reader(), support::DisplayOf(unit.classes.PathOf(cls)),
          unit.classes.KeyOf(cls).unit_name, unit.name));
}

// Adds one line to `lines` for each expression `reaches` reached that reads a
// class the unit holds no declaration of.
void ReportClassReads(
    const CompilationUnit& unit, const ArenaReaches& reaches, const auto& where,
    std::vector<std::string>& lines) {
  for (const ExprId id : reaches.exprs->Ids()) {
    if (reaches.at.at(id.value).empty()) continue;
    const ExprData& data = reaches.exprs->Get(id).data;
    ForEachClassRead(unit, *reaches.exprs, data, [&](ClassId cls) {
      RequireDeclaration(
          unit, cls,
          [&] {
            return std::format(
                "{} (expression {}) in {}", Describe(data), id.value, where());
          },
          lines);
    });
  }
}

// What a report says of a reference to the namespace `owner` where the unit
// holds no declaration of it, and nothing where it holds one.
auto UnheldNamespace(const CompilationUnit& unit, NamespaceId owner)
    -> std::optional<std::string> {
  if (owner.value < unit.namespaces.size() &&
      unit.namespaces.HasDeclaration(owner)) {
    return std::nullopt;
  }
  return std::format(
      "namespace {}, which unit '{}' holds no declaration of", owner.value,
      unit.name);
}

// What a report says of a reference to the `kind` at `position` of the
// namespace `owner`, which declares `count` of them, and nothing where it
// declares one there.
auto UndeclaredNamespaceMember(
    const CompilationUnit& unit, NamespaceId owner, std::string_view kind,
    std::uint32_t position, std::size_t count) -> std::optional<std::string> {
  if (position < count) return std::nullopt;
  return std::format(
      "{} {} of namespace '{}', which declares {}", kind, position,
      unit.namespaces.KeyOf(owner).unit_name, count);
}

auto MissingNamespaceVariable(
    const CompilationUnit& unit, const NamespaceValueRef& ref)
    -> std::optional<std::string> {
  if (auto unheld = UnheldNamespace(unit, ref.owner)) return unheld;
  return UndeclaredNamespaceMember(
      unit, ref.owner, "variable", ref.variable.value,
      unit.namespaces.Declaration(ref.owner).variables.size());
}

auto MissingNamespaceSubroutine(
    const CompilationUnit& unit, NamespaceId owner,
    NamespaceSubroutineId subroutine) -> std::optional<std::string> {
  if (auto unheld = UnheldNamespace(unit, owner)) return unheld;
  return UndeclaredNamespaceMember(
      unit, owner, "subroutine", subroutine.value,
      unit.namespaces.Declaration(owner).subroutines.size());
}

// Adds one line to `lines` for each expression `reaches` reached that names a
// member of a namespace the unit holds no declaration of, or one that
// namespace does not declare. What a read set or a sampled cell names is a
// read one of these expressions makes, so it is checked here.
void ReportNamespaceMembers(
    const CompilationUnit& unit, const ArenaReaches& reaches, const auto& where,
    std::vector<std::string>& lines) {
  for (const ExprId id : reaches.exprs->Ids()) {
    if (reaches.at.at(id.value).empty()) continue;
    const ExprData& data = reaches.exprs->Get(id).data;
    std::vector<std::optional<std::string>> missing;
    if (const auto* primary = std::get_if<PrimaryExpr>(&data)) {
      if (const auto* r = std::get_if<NamespaceValueRef>(&primary->data)) {
        missing.push_back(MissingNamespaceVariable(unit, *r));
      }
    }
    if (const auto* call = std::get_if<CallExpr>(&data)) {
      if (const auto* r = std::get_if<NamespaceSubroutineRef>(&call->callee)) {
        missing.push_back(
            MissingNamespaceSubroutine(unit, r->owner, r->subroutine));
      }
      if (const auto* r = std::get_if<ForeignImportRef>(&call->callee)) {
        const auto* space =
            r->declaring_scope.has_value()
                ? std::get_if<NamespaceDeclaringScope>(&*r->declaring_scope)
                : nullptr;
        if (space != nullptr) {
          for (const std::optional<NamespaceSubroutineId>& evaluator :
               space->defaults) {
            if (!evaluator.has_value()) continue;
            missing.push_back(
                MissingNamespaceSubroutine(unit, space->owner, *evaluator));
          }
        }
      }
    }
    for (const std::optional<std::string>& reported : missing) {
      if (!reported.has_value()) continue;
      lines.push_back(
          std::format(
              "hir verify: {} (expression {}) in {} names {}", Describe(data),
              id.value, where(), *reported));
    }
  }
}

// Adds one line to `lines` for each formal of `subroutine` bound to a
// namespace's variable the unit does not hold.
void ReportNamespaceFormals(
    const CompilationUnit& unit, const SubroutineDecl& subroutine,
    const auto& where, std::vector<std::string>& lines) {
  for (std::size_t position = 0; position < subroutine.params.size();
       ++position) {
    const auto* kept =
        std::get_if<NamespaceValueRef>(&subroutine.params[position].variable);
    if (kept == nullptr) continue;
    if (const std::optional<std::string> reported =
            MissingNamespaceVariable(unit, *kept)) {
      lines.push_back(
          std::format(
              "hir verify: formal {} of {} binds {}", position, where(),
              *reported));
    }
  }
}

// Adds one line to `lines` where what a namespace unit defines does not match
// what its entry for itself declares: a variable at every position defined,
// a subroutine of its scope for every subroutine.
void VerifyNamespaceDefinition(
    const CompilationUnit& unit, std::vector<std::string>& lines) {
  const bool is_namespace = !hir::RootsAnObject(unit.definition);
  if (unit.namespace_definition.has_value() != is_namespace) {
    lines.push_back(
        std::format(
            "hir verify: unit '{}' {} what a namespace defines, and {} one",
            unit.name,
            unit.namespace_definition.has_value() ? "holds" : "lacks",
            is_namespace ? "is" : "is not"));
    return;
  }
  if (!unit.namespace_definition.has_value()) return;
  const NamespaceDefinition& defined = *unit.namespace_definition;
  if (const std::optional<std::string> unheld =
          UnheldNamespace(unit, defined.entry)) {
    lines.push_back(
        std::format("hir verify: unit '{}' defines {}", unit.name, *unheld));
    return;
  }
  if (!unit.namespaces.IsOwn(defined.entry)) {
    lines.push_back(
        std::format(
            "hir verify: unit '{}' defines namespace '{}', another unit",
            unit.name, unit.namespaces.KeyOf(defined.entry).unit_name));
    return;
  }
  const NamespaceDeclaration& declared =
      unit.namespaces.Declaration(defined.entry);
  if (defined.variables.size() != declared.variables.size() ||
      defined.subroutines.size() != declared.subroutines.size()) {
    lines.push_back(
        std::format(
            "hir verify: unit '{}' defines {} variables and {} subroutines of "
            "a namespace declaring {} and {}",
            unit.name, defined.variables.size(), defined.subroutines.size(),
            declared.variables.size(), declared.subroutines.size()));
    return;
  }
  for (const NamespaceVariableId id : defined.variables.Ids()) {
    if (defined.variables.IsDefined(id)) continue;
    lines.push_back(
        std::format(
            "hir verify: unit '{}' defines no variable '{}' its namespace "
            "declares",
            unit.name, support::DisplayOf(declared.variables.Get(id).name)));
  }
  // The scope's subroutine a declared subroutine is evaluates its defaults by
  // the subroutines the declaration names for them.
  const auto& subroutines = unit.root_scope.structural_subroutines;
  for (const NamespaceSubroutineId id : defined.subroutines.Ids()) {
    const StructuralSubroutineId body = defined.subroutines.Get(id);
    if (body.value >= subroutines.size()) continue;
    const SubroutineDecl& decl = subroutines.Get(body);
    const NamespaceSubroutine& listed = declared.subroutines.Get(id);
    if (decl.params.size() != listed.defaults.size()) continue;
    for (std::size_t j = 0; j < decl.params.size(); ++j) {
      const std::optional<StructuralSubroutineId> stated =
          listed.defaults[j].transform([&](NamespaceSubroutineId evaluator) {
            return defined.subroutines.Get(evaluator);
          });
      if (stated == decl.params[j].default_evaluator) continue;
      lines.push_back(
          std::format(
              "hir verify: subroutine '{}' of namespace '{}' names another "
              "evaluator for formal {} than its scope's subroutine does",
              listed.prototype.name, unit.name, j));
    }
  }
}

// Adds one line to `lines` where a behavior `declared` names -- one a method
// overrides, or one of an interface class and what answers it -- is no method
// of a class the unit holds that introduces a behavior (LRM 8.20). A behavior
// is named by the introducing method's position, which nothing but this
// checks against what that class declares.
void VerifyBehaviorsOf(
    const CompilationUnit& unit, const ClassDeclaration& declared,
    const auto& by, std::vector<std::string>& lines) {
  const auto require = [&](const ClassMethodTarget& behavior) {
    RequireDeclaration(unit, behavior.owner, by, lines);
    if (!unit.classes.HasDeclaration(behavior.owner)) return;
    const DeclaredMethods& methods =
        unit.classes.Declaration(behavior.owner).methods;
    if (behavior.method.value >= methods.size() ||
        !std::holds_alternative<IntroducesVirtual>(
            methods.Get(behavior.method).dispatch)) {
      lines.push_back(
          std::format(
              "hir verify: {} names method {} of class '{}' as a behavior, "
              "which introduces none",
              by(), behavior.method.value,
              support::DisplayOf(unit.classes.PathOf(behavior.owner))));
    }
  };
  for (const DeclaredMethod& method : declared.methods) {
    if (const auto* overriding =
            std::get_if<OverridesVirtual>(&method.dispatch)) {
      require(overriding->overridden);
    }
  }
  for (const ConformingBehavior& answered : declared.conforming) {
    require(answered.interface_behavior);
    if (answered.answered_by.has_value()) require(*answered.answered_by);
  }
}

// Adds one line to `lines` where `defaults` does not state one evaluator per
// formal of `interface`, or names one past the `count` callables of whatever
// declares it: the two lists are read side by side, and nothing else holds
// them in step.
template <typename Id>
void VerifyDefaults(
    const CalleeInterface& interface,
    const std::vector<std::optional<Id>>& defaults, std::size_t count,
    const auto& what, std::vector<std::string>& lines) {
  if (defaults.size() != interface.params.size()) {
    lines.push_back(
        std::format(
            "hir verify: {} states {} defaults for {} formals", what(),
            defaults.size(), interface.params.size()));
    return;
  }
  for (const std::optional<Id>& evaluator : defaults) {
    if (evaluator.has_value() && evaluator->value >= count) {
      lines.push_back(
          std::format(
              "hir verify: {} names evaluator {} of a declarer of {}", what(),
              evaluator->value, count));
    }
  }
}

// Every list of evaluators the unit holds, of each class, scope class and
// namespace it holds the declaration of.
void VerifyDeclaredDefaults(
    const CompilationUnit& unit, std::vector<std::string>& lines) {
  for (const ClassId id : unit.classes.Ids()) {
    if (!unit.classes.HasDeclaration(id)) continue;
    const ClassDeclaration& declared = unit.classes.Declaration(id);
    const std::size_t count = declared.methods.size();
    for (const MethodId at : declared.methods.Ids()) {
      const DeclaredMethod& method = declared.methods.Get(at);
      VerifyDefaults(
          method.prototype.interface, method.defaults, count,
          [&] {
            return std::format(
                "method '{}' of class '{}'", method.prototype.name,
                support::DisplayOf(unit.classes.PathOf(id)));
          },
          lines);
    }
    if (declared.constructor.has_value()) {
      VerifyDefaults(
          declared.constructor->interface, declared.constructor->defaults,
          count,
          [&] {
            return std::format(
                "the constructor of class '{}'",
                support::DisplayOf(unit.classes.PathOf(id)));
          },
          lines);
    }
  }
  for (const ScopeClassId id : unit.scope_classes.Ids()) {
    if (!unit.scope_classes.IsRecorded(id)) continue;
    const ScopeClassSignature& scope = unit.scope_classes.Get(id);
    for (const ScopeCallable& callable : scope.callables) {
      VerifyDefaults(
          callable.prototype.interface, callable.defaults,
          scope.callables.size(),
          [&] {
            return std::format(
                "subroutine '{}' of scope class '{}'", callable.prototype.name,
                support::DisplayOf(unit.scope_classes.KeyOf(id).class_path));
          },
          lines);
    }
  }
  for (const NamespaceId id : unit.namespaces.Ids()) {
    if (!unit.namespaces.HasDeclaration(id)) continue;
    const NamespaceDeclaration& declared = unit.namespaces.Declaration(id);
    for (const NamespaceSubroutine& subroutine : declared.subroutines) {
      VerifyDefaults(
          subroutine.prototype.interface, subroutine.defaults,
          declared.subroutines.size(),
          [&] {
            return std::format(
                "subroutine '{}' of namespace '{}'", subroutine.prototype.name,
                unit.namespaces.KeyOf(id).unit_name);
          },
          lines);
    }
  }
}

// Whether `ancestor` is `of` or a covergroup `of` extends, along what the unit
// holds of each (LRM 19.4.1).
auto IsOrExtends(
    const CompilationUnit& unit, CovergroupId of, CovergroupId ancestor)
    -> bool {
  for (std::optional<CovergroupId> at = of; at.has_value();) {
    if (*at == ancestor) return true;
    if (at->value >= unit.covergroups.size() ||
        !unit.covergroups.IsPublished(*at)) {
      return false;
    }
    at = unit.covergroups.Published(*at).base;
  }
  return false;
}

// The covergroup declaring the item `item` names.
auto CovergroupOf(const CoverageItemRef& item) -> CovergroupId {
  return std::visit(
      Overloaded{
          [](const CoverpointRef& point) { return point.covergroup; },
          [](const CrossRef& cross) { return cross.covergroup; }},
      item);
}

// Adds one line to `lines` where `item` names a covergroup the unit holds no
// declaration of, or a place that covergroup's declaration lacks.
void RequireCoverageItem(
    const CompilationUnit& unit, const CoverageItemRef& item, const auto& by,
    std::vector<std::string>& lines) {
  const auto [covergroup, place, is_cross] = std::visit(
      Overloaded{
          [](const CoverpointRef& point) {
            return std::tuple{point.covergroup, point.coverpoint.value, false};
          },
          [](const CrossRef& cross) {
            return std::tuple{cross.covergroup, cross.cross.value, true};
          }},
      item);
  const std::string_view kind = is_cross ? "cross" : "coverpoint";
  if (covergroup.value >= unit.covergroups.size() ||
      !unit.covergroups.IsPublished(covergroup)) {
    lines.push_back(
        std::format(
            "hir verify: {} names a {} of covergroup {}, of which the unit "
            "holds no declaration",
            by(), kind, covergroup.value));
    return;
  }
  const PublishedCovergroup& published = unit.covergroups.Published(covergroup);
  if (place >=
      (is_cross ? published.cross_count : published.coverpoints.size())) {
    lines.push_back(
        std::format(
            "hir verify: {} names {} {} of covergroup '{}', which declares no "
            "such {}",
            by(), kind, place,
            support::DisplayOf(unit.covergroups.PathOf(covergroup)), kind));
  }
}

// Adds one line to `lines` where `item` is no item `through` declares or
// extends one declaring it, which is how it reaches the items it names (LRM
// 19.4.1).
void RequireItemThrough(
    const CompilationUnit& unit, CovergroupId through,
    const CoverageItemRef& item, const auto& by,
    std::vector<std::string>& lines) {
  RequireCoverageItem(unit, item, by, lines);
  const CovergroupId owner = CovergroupOf(item);
  if (!IsOrExtends(unit, through, owner)) {
    lines.push_back(
        std::format(
            "hir verify: {} names an item of covergroup {} through covergroup "
            "{}, which neither is nor extends it",
            by(), owner.value, through.value));
  }
}

// Adds one line to `lines` for each item a use of a covergroup in `exprs`
// names that is no item of that covergroup or of one it extends.
void ReportCoverageItems(
    const CompilationUnit& unit, const base::Arena<Expr, ExprId>& exprs,
    const auto& where, std::vector<std::string>& lines) {
  const auto check = [&](CovergroupId covergroup,
                         const std::optional<CoverageItemRef>& item) {
    if (item.has_value()) {
      RequireItemThrough(unit, covergroup, *item, where, lines);
    }
  };
  for (const ExprId id : exprs.Ids()) {
    const ExprData& data = exprs.Get(id).data;
    if (const auto* call = std::get_if<CovergroupMethodCallExpr>(&data)) {
      check(call->covergroup, call->item);
    } else if (const auto* option = std::get_if<CovergroupOptionExpr>(&data)) {
      check(option->covergroup, option->item);
    }
  }
}

// Adds one line to `lines` for each class of the coverage library the unit
// resolved members of that it holds no declaration of; a read of a class
// closes over the classes it extends, which declare the rest of its members.
void VerifyCoverageLibrary(
    const CompilationUnit& unit, const CoverageLibraryMembers& library,
    std::vector<std::string>& lines) {
  const auto by = [] { return std::string{"the coverage library"}; };
  for (const ClassId cls :
       {library.registry.cls, library.group_type.cls,
        library.group_instance.cls, library.item.cls, library.coverpoint.cls,
        library.cross.cls, library.selection.cls}) {
    RequireDeclaration(unit, cls, by, lines);
  }
  for (const auto& entry : library.coverpoint_classes) {
    RequireDeclaration(unit, entry.first, by, lines);
  }
}

// What a class declares is read with what it extends and the interface classes
// it names, since its layout and its tables read theirs. A covergroup is laid
// out from the class embedding it and is counted by its coverpoints' classes,
// and a unit lowering a covergroup makes and calls the coverage library's
// classes.
void VerifyClassReadsOfDeclarations(
    const CompilationUnit& unit, std::vector<std::string>& lines) {
  for (const ClassId id : unit.classes.Ids()) {
    if (!unit.classes.HasDeclaration(id)) continue;
    const ClassDeclaration& declared = unit.classes.Declaration(id);
    const auto by = [&] {
      return std::format(
          "the declaration of class '{}'",
          support::DisplayOf(unit.classes.PathOf(id)));
    };
    if (declared.base.has_value()) {
      RequireDeclaration(unit, *declared.base, by, lines);
    }
    for (const ClassId iface : declared.implements) {
      RequireDeclaration(unit, iface, by, lines);
    }
    VerifyBehaviorsOf(unit, declared, by, lines);
  }
  for (const CovergroupId id : unit.covergroups.Ids()) {
    if (!unit.covergroups.IsPublished(id)) continue;
    const PublishedCovergroup& published = unit.covergroups.Published(id);
    const auto by = [&] {
      return std::format(
          "covergroup '{}'", support::DisplayOf(unit.covergroups.PathOf(id)));
    };
    if (published.embedding.has_value()) {
      RequireDeclaration(unit, *published.embedding, by, lines);
    }
    for (const PublishedCoverpoint& point : published.coverpoints) {
      RequireDeclaration(unit, point.library_class, by, lines);
    }
  }
  if (unit.coverage_library.has_value()) {
    VerifyCoverageLibrary(unit, *unit.coverage_library, lines);
  }
}

// A synchronous drive landed on the instance its target is reached on names the
// member access of the target that reaches it, under the selects the drive
// wrote: the one expression evaluating the instance for both (LRM 14.16).
void ReportDriveReceivers(
    const base::Arena<Expr, ExprId>& exprs, const auto& where,
    std::vector<std::string>& lines) {
  for (const ExprId id : exprs.Ids()) {
    const auto* assign = std::get_if<AssignExpr>(&exprs.Get(id).data);
    if (assign == nullptr) continue;
    const auto* deferred = std::get_if<NonBlockingEffect>(&assign->timing);
    if (deferred == nullptr || !deferred->control.has_value()) continue;
    const auto* drive = std::get_if<ClockingDriveControl>(&*deferred->control);
    if (drive == nullptr) continue;
    const auto* published = std::get_if<PublishedLanding>(&drive->landing);
    if (published == nullptr) continue;
    const auto* target = std::get_if<InstanceOfTarget>(&published->receiver);
    if (target == nullptr) continue;
    ExprId written = assign->lhs;
    for (;;) {
      const ExprData& data = exprs.Get(written).data;
      if (const auto* sel = std::get_if<ElementSelectExpr>(&data)) {
        written = sel->base_value;
      } else if (const auto* range = std::get_if<RangeSelectExpr>(&data)) {
        written = range->base_value;
      } else {
        break;
      }
    }
    if (written != target->access ||
        !std::holds_alternative<InterfaceMemberAccessExpr>(
            exprs.Get(written).data)) {
      lines.push_back(
          std::format(
              "in {}: the drive (expression {}) is landed on the instance "
              "expression {} reaches, which is not the member access of its "
              "target",
              where(), id.value, target->access.value));
    }
  }
}

// Counts the reaches into one body's expressions. `entered_at` is the
// statements execution enters the body at, which whatever owns the body
// states, and `reach_from_owner` adds the reaches that owner makes itself,
// holding expressions of the body outside any statement of it.
void VerifyBody(
    const CompilationUnit& unit, const ProceduralBody& body,
    std::span<const StmtId> entered_at, const auto& reach_from_owner,
    const auto& where, std::vector<std::string>& lines) {
  ArenaReaches reaches{body};
  for (const StmtId id : entered_at) {
    ReachFromStmt(body, reaches, id);
  }
  const auto from_variable =
      ReachFrom(reaches, std::nullopt, "a variable's declaration");
  for (const ProceduralVarId id : body.procedural_vars.Ids()) {
    // A variable declared and never defined holds no expression, and saying so
    // is not this check's to do.
    if (!body.procedural_vars.IsDefined(id)) {
      continue;
    }
    const ProceduralVarDecl& variable = body.procedural_vars.Get(id);
    if (variable.init.has_value()) {
      from_variable(*variable.init, Slot::kOperand);
    }
  }
  reach_from_owner(reaches);
  Report(reaches, where, lines);
  ReportClassReads(unit, reaches, where, lines);
  ReportNamespaceMembers(unit, reaches, where, lines);
  ReportDriveReceivers(body.exprs, where, lines);
  ReportCoverageItems(unit, body.exprs, where, lines);
}

void VerifySubroutine(
    const CompilationUnit& unit, const SubroutineDecl& subroutine,
    const auto& reach_from_owner, const auto& where,
    std::vector<std::string>& lines) {
  // A prototype states a signature and has no statement to enter at (LRM
  // 8.21).
  const std::vector<StmtId> entered_at =
      subroutine.root_stmt.has_value()
          ? std::vector<StmtId>{*subroutine.root_stmt}
          : std::vector<StmtId>{};
  VerifyBody(
      unit, subroutine.body, entered_at,
      [&](ArenaReaches& reaches) {
        ReachReads(
            subroutine.reads, ReachFrom(reaches, std::nullopt, "a subroutine"));
        reach_from_owner(reaches);
      },
      where, lines);
  ReportNamespaceFormals(unit, subroutine, where, lines);
}

void VerifyClass(
    const CompilationUnit& unit, const ReplicatedClass& replicated,
    std::vector<std::string>& lines) {
  const ClassId id = replicated.id;
  const ClassDecl& cls = replicated.code;
  const auto owner = [&] {
    return std::format(
        "class '{}' in unit '{}'", support::DisplayOf(unit.classes.PathOf(id)),
        unit.name);
  };
  for (const MethodId method : cls.methods.Ids()) {
    const SubroutineDecl& decl = cls.methods.Get(method);
    VerifySubroutine(
        unit, decl, [](ArenaReaches&) {},
        [&] { return std::format("method '{}' of {}", decl.name, owner()); },
        lines);
  }
  VerifySubroutine(
      unit, cls.constructor,
      [&](ArenaReaches& reaches) {
        const auto from_base =
            ReachFrom(reaches, std::nullopt, "a base constructor call");
        for (const ExprId argument : cls.base_call.arguments) {
          from_base(argument, Slot::kOperand);
        }
        const auto from_init =
            ReachFrom(reaches, std::nullopt, "a property initializer");
        for (const FieldInit& init : cls.field_inits) {
          from_init(init.value, Slot::kOperand);
        }
      },
      [&] { return std::format("the constructor of {}", owner()); }, lines);
  VerifyBody(
      unit, cls.static_init, {},
      [&](ArenaReaches& reaches) {
        const auto from_init =
            ReachFrom(reaches, std::nullopt, "a static property initializer");
        for (const StaticPropertyInit& init : cls.static_property_inits) {
          from_init(init.value, Slot::kOperand);
        }
      },
      [&] { return std::format("the static initializers of {}", owner()); },
      lines);
}

void ReachBinElements(std::span<const BinElement> elements, const auto& reach) {
  for (const BinElement& element : elements) {
    std::visit(
        Overloaded{
            [&](const BinValue& value) { reach(value.value, Slot::kOperand); },
            [&](const BinRange& range) {
              if (range.low.has_value()) reach(*range.low, Slot::kOperand);
              if (range.high.has_value()) reach(*range.high, Slot::kOperand);
            },
            [&](const BinTolerance& tolerance) {
              reach(tolerance.center, Slot::kOperand);
              reach(tolerance.tolerance, Slot::kOperand);
            }},
        element);
  }
}

void ReachOptions(std::span<const OptionSetting> options, const auto& reach) {
  for (const OptionSetting& option : options) {
    reach(option.value, Slot::kOperand);
  }
}

void ReachMatches(const MatchesRequired& matches, const auto& reach) {
  std::visit(
      Overloaded{
          [](const OneMatch&) {}, [](const EveryMatch&) {},
          [&](ExprId count) { reach(count, Slot::kOperand); }},
      matches);
}

void ReachBin(const BinConstruct& bin, const auto& reach) {
  if (bin.iff.has_value()) reach(*bin.iff, Slot::kOperand);
  std::visit(
      Overloaded{
          [&](const ValueBins& values) {
            std::visit(
                Overloaded{
                    [](const OneBin&) {}, [](const BinPerValue&) {},
                    [&](const FixedBins& fixed) {
                      reach(fixed.count, Slot::kOperand);
                    }},
                values.array);
            ReachBinElements(values.elements, reach);
            if (values.set.has_value()) reach(*values.set, Slot::kOperand);
            if (values.filter.has_value()) {
              reach(values.filter->keeps, Slot::kOperand);
            }
          },
          [&](const TransitionBins& transitions) {
            for (const auto& sequence : transitions.sequences) {
              for (const TransitionStep& step : sequence) {
                ReachBinElements(step.values, reach);
                if (step.least.has_value()) reach(*step.least, Slot::kOperand);
                if (step.most.has_value()) reach(*step.most, Slot::kOperand);
              }
            }
          },
          [](const DefaultBins&) {}, [](const DefaultSequenceBins&) {}},
      bin.form);
}

void ReachCross(const Cross& cross, const auto& reach) {
  if (cross.iff.has_value()) reach(*cross.iff, Slot::kOperand);
  ReachOptions(cross.options, reach);
  for (const CrossBin& bin : cross.bins) {
    if (bin.iff.has_value()) reach(*bin.iff, Slot::kOperand);
  }
  for (const CrossSelectId id : cross.selects.Ids()) {
    std::visit(
        Overloaded{
            [](const SelectAll&) {},
            [&](const SelectBinsOf& select) {
              ReachBinElements(select.intersect, reach);
            },
            [](const SelectNot&) {}, [](const SelectAnd&) {},
            [](const SelectOr&) {},
            [&](const SelectWith& select) {
              reach(select.filter, Slot::kOperand);
              ReachMatches(select.matches, reach);
            },
            [&](const SelectSet& select) {
              reach(select.tuples, Slot::kOperand);
              ReachMatches(select.matches, reach);
            }},
        cross.selects.Get(id));
  }
}

// Adds one line to `lines` for each item `group` names that its declaration
// or a covergroup it extends lacks: a cross's coverpoints, a coverpoint
// `binsof` selects and the construct whose bins it selects, and what an item
// replaces, which is an item of the same kind of a covergroup it extends (LRM
// 19.4.1). Its own declaration publishes as many items as it declares.
void VerifyCovergroupItems(
    const CompilationUnit& unit, CovergroupId id, const CovergroupDecl& group,
    std::vector<std::string>& lines) {
  const auto by = [&] {
    return std::format(
        "covergroup '{}' in unit '{}'",
        support::DisplayOf(unit.covergroups.PathOf(id)), unit.name);
  };
  const PublishedCovergroup& published = unit.covergroups.Published(id);
  if (published.coverpoints.size() != group.coverpoints.size() ||
      published.cross_count != group.crosses.size()) {
    lines.push_back(
        std::format(
            "hir verify: {} publishes {} coverpoints and {} crosses and "
            "declares {} and {}",
            by(), published.coverpoints.size(), published.cross_count,
            group.coverpoints.size(), group.crosses.size()));
  }
  const auto named = [&](const CoverageItemRef& item) {
    RequireItemThrough(unit, id, item, by, lines);
  };
  const auto replaced = [&](const CoverageItemRef& item) {
    named(item);
    if (CovergroupOf(item) == id) {
      lines.push_back(
          std::format(
              "hir verify: {} replaces an item of its own declaration", by()));
    }
  };
  for (const Coverpoint& point : group.coverpoints) {
    if (point.replaces.has_value()) replaced(*point.replaces);
  }
  for (const Cross& cross : group.crosses) {
    if (cross.replaces.has_value()) replaced(*cross.replaces);
    for (const CoverpointRef& target : cross.targets) named(target);
    for (const CrossSelectId select : cross.selects.Ids()) {
      const auto* binsof =
          std::get_if<SelectBinsOf>(&cross.selects.Get(select));
      if (binsof == nullptr) continue;
      named(binsof->point);
      if (!std::ranges::contains(cross.targets, binsof->point)) {
        lines.push_back(
            std::format(
                "hir verify: {} selects the bins of a coverpoint its cross "
                "does not cross",
                by()));
      }
      if (binsof->construct.has_value() && binsof->point.covergroup == id &&
          binsof->point.coverpoint.value < group.coverpoints.size() &&
          binsof->construct->value >=
              group.coverpoints.Get(binsof->point.coverpoint).bins.size()) {
        lines.push_back(
            std::format(
                "hir verify: {} selects the bins of construct {}, which its "
                "coverpoint lacks",
                by(), binsof->construct->value));
      }
    }
  }
}

// A covergroup holds every expression it evaluates in its own body, outside any
// statement: what it samples, its bins, its options and its sampling event.
void VerifyCovergroup(
    const CompilationUnit& unit, const ReplicatedCovergroup& replicated,
    std::vector<std::string>& lines) {
  const CovergroupId id = replicated.id;
  const CovergroupDecl& group = replicated.code;
  VerifyBody(
      unit, group.body, {},
      [&](ArenaReaches& reaches) {
        const auto from_group =
            ReachFrom(reaches, std::nullopt, "a covergroup");
        ReachOptions(group.options, from_group);
        std::visit(
            Overloaded{
                [](const SampledByCall&) {},
                [&](const SampledOnEvent& on) {
                  ReachEventControl(on.event, from_group);
                },
                [](const SampledAtBlocks&) {}},
            group.sampling);
        const auto from_coverpoint =
            ReachFrom(reaches, std::nullopt, "a coverpoint");
        for (const Coverpoint& point : group.coverpoints) {
          from_coverpoint(point.expression, Slot::kOperand);
          if (point.iff.has_value()) {
            from_coverpoint(*point.iff, Slot::kOperand);
          }
          ReachOptions(point.options, from_coverpoint);
          for (const BinConstruct& bin : point.bins) {
            ReachBin(bin, from_coverpoint);
          }
        }
        const auto from_cross = ReachFrom(reaches, std::nullopt, "a cross");
        for (const Cross& cross : group.crosses) {
          ReachCross(cross, from_cross);
        }
      },
      [&] {
        return std::format(
            "covergroup '{}' in unit '{}'",
            support::DisplayOf(unit.covergroups.PathOf(id)), unit.name);
      },
      lines);

  VerifyCovergroupItems(unit, id, group, lines);
}

void ReachFromDataObjects(ArenaReaches& reaches, const StructuralScope& scope) {
  const auto from_variable =
      ReachFrom(reaches, std::nullopt, "a variable's declaration");
  const auto from_parameter =
      ReachFrom(reaches, std::nullopt, "a parameter's declaration");
  for (const StructuralDataObjectDecl& object : scope.structural_data_objects) {
    std::visit(
        Overloaded{
            [&](const StructuralVariableDecl& decl) {
              if (decl.initializer.has_value()) {
                from_variable(*decl.initializer, Slot::kOperand);
              }
            },
            [](const StructuralNetDecl&) {},
            [](const StructuralReferenceDecl&) {},
            [](const StructuralGenvarDecl&) {},
            [](const StructuralConstructionValueDecl&) {},
            [&](const StructuralParameterDecl& decl) {
              if (decl.default_value.has_value()) {
                from_parameter(*decl.default_value, Slot::kOperand);
              }
            },
            [&](const StructuralLocalparamDecl& decl) {
              from_parameter(decl.initializer, Slot::kOperand);
            }},
        object.kind);
  }
}

// The expressions a generate construct holds live in the scope holding the
// construct, whose construction evaluates them.
void ReachFromGenerate(ArenaReaches& reaches, const Generate& generate) {
  const auto from_loop = ReachFrom(reaches, std::nullopt, "a loop generate");
  const auto from_choice =
      ReachFrom(reaches, std::nullopt, "a conditional generate");
  const auto from_block =
      ReachFrom(reaches, std::nullopt, "a generate block's construction");
  std::visit(
      Overloaded{
          [](const SingleBlock&) {},
          [&](const BlocksRepeat& loop) {
            from_loop(loop.initial, Slot::kOperand);
            from_loop(loop.condition, Slot::kOperand);
            from_loop(loop.step, Slot::kOperand);
            from_block(loop.index, Slot::kOperand);
          },
          [&](const BlocksChoose& chosen) {
            for (const SelectionChoice& choice : chosen.choices) {
              std::visit(
                  Overloaded{
                      [&](const ChoiceOnCondition& on) {
                        from_choice(on.condition, Slot::kOperand);
                      },
                      [&](const ChoiceOnLabel& on) {
                        from_choice(on.selector, Slot::kOperand);
                        for (const LabeledItem& item : on.items) {
                          for (const ExprId label : item.labels) {
                            from_choice(label, Slot::kOperand);
                          }
                        }
                      }},
                  choice);
            }
          }},
      generate.counting);
}

void ReachFromConnections(ArenaReaches& reaches, const StructuralScope& scope) {
  const auto from_instance =
      ReachFrom(reaches, std::nullopt, "an instance's construction");
  for (const InstanceMemberDecl& instance : scope.instance_members) {
    for (const InstanceAlternative& alternative : instance.alternatives) {
      for (const ParameterValueAssignment& assigned : alternative.assignments) {
        if (assigned.value.has_value()) {
          from_instance(*assigned.value, Slot::kOperand);
        }
      }
    }
  }
  const auto from_port = ReachFrom(reaches, std::nullopt, "a port connection");
  for (const PortConnection& connection : scope.port_connections) {
    std::visit(
        Overloaded{
            [&](const DataPortConnection& data) {
              std::visit(
                  Overloaded{
                      [&](const PortCellEndpoint& endpoint) {
                        from_port(endpoint.cell, Slot::kOperand);
                      },
                      [](const ValueRoute&) {}},
                  data.endpoint);
              if (data.peer.has_value()) {
                from_port(*data.peer, Slot::kOperand);
              }
              ReachSensitivity(data.sensitivity, from_port);
            },
            [](const InterfacePortConnection&) {}},
        connection.kind);
  }
  const auto from_join = ReachFrom(reaches, std::nullopt, "a net join");
  for (const NetJoin& join : scope.net_joins) {
    for (const NetSide& side : join.sides) {
      for (const NetPositions& operand : side) {
        from_join(operand.part, Slot::kOperand);
      }
    }
  }
}

// Every object an instance declaration stands for is built one of the ways the
// declaration states, so it names one of its alternatives, and there is one
// such entry per object its dimensions count out.
void VerifyInstanceMembers(
    const StructuralScope& scope, const std::string& label,
    std::vector<std::string>& lines) {
  for (const InstanceMemberId id : scope.instance_members.Ids()) {
    const InstanceMemberDecl& instance = scope.instance_members.Get(id);
    std::size_t objects = 1;
    for (const UnpackedRange& dim : instance.array_dims) {
      objects *= dim.ElementCount();
    }
    const bool every_one_built =
        instance.taken.size() == objects &&
        std::ranges::all_of(instance.taken, [&](std::uint32_t alternative) {
          return alternative < instance.alternatives.size();
        });
    if (!every_one_built) {
      lines.push_back(
          std::format(
              "hir verify: instance member '{}' of {} says how {} of its {} "
              "objects are built, from {} alternatives",
              instance.instance_name, label, instance.taken.size(), objects,
              instance.alternatives.size()));
    }
  }
}

// How many values whoever builds `scope` hands it: one per declaration a
// construction fills.
auto ValuesHandedTo(const StructuralScope& scope) -> std::size_t {
  std::size_t handed = 0;
  for (const StructuralDataObjectId id : scope.structural_data_objects.Ids()) {
    std::visit(
        Overloaded{
            [&](const StructuralConstructionValueDecl&) { ++handed; },
            [&](const StructuralParameterDecl&) { ++handed; },
            [](const StructuralVariableDecl&) {},
            [](const StructuralNetDecl&) {},
            [](const StructuralReferenceDecl&) {},
            [](const StructuralGenvarDecl&) {},
            [](const StructuralLocalparamDecl&) {}},
        scope.structural_data_objects.Get(id).kind);
  }
  return handed;
}

// What an object builds of each child says which specialization stands at each
// object of each instance member, and of each construct, place by place,
// which body stands there: a block standing on its own at its one place, a
// conditional at most one of the alternatives the source wrote, each at its
// position (LRM 27.5), and a loop one body per block it counted out (LRM
// 27.4).
void VerifyConstruction(
    const CompilationUnit& unit, const StructuralScope& scope,
    const ScopeConstruction& built, const std::string& label,
    std::vector<std::string>& lines) {
  bool makes_each_object =
      built.instances.size() == scope.instance_members.size();
  for (const InstanceMemberId m : scope.instance_members.Ids()) {
    makes_each_object =
        makes_each_object && built.instances[m.value].size() ==
                                 scope.instance_members.Get(m).taken.size();
  }
  if (!makes_each_object) {
    lines.push_back(
        std::format(
            "hir verify: the construction of {} makes other than one child "
            "per object of each instance member",
            label));
  }
  if (built.generates.size() != scope.generates.size()) {
    lines.push_back(
        std::format(
            "hir verify: the construction of {} builds {} generates of its {}",
            label, built.generates.size(), scope.generates.size()));
    return;
  }
  for (const GenerateId id : scope.generates.Ids()) {
    const std::vector<BlockConstruction>& places = built.generates[id.value];
    const auto says = [&](std::string_view what) {
      lines.push_back(
          std::format(
              "hir verify: the construction of generate {} of {} {}", id.value,
              label, what));
    };
    // A block is handed the index its loop stands at and nothing else (LRM
    // 27.4; a block has no parameter ports, Syntax 27-1), so a loop's bodies
    // each declare that one value and every other block's declare none.
    std::size_t handed = 0;
    std::visit(
        Overloaded{
            [&](const SingleBlock&) {
              if (places.size() != 1 || !places.front().body.has_value()) {
                says("builds other than its one block");
              }
            },
            [&](const BlocksRepeat&) {
              handed = 1;
              if (std::ranges::any_of(places, [](const BlockConstruction& p) {
                    return !p.body.has_value();
                  })) {
                says("counts out a block of no body");
              }
            },
            [&](const BlocksChoose& chosen) {
              if (places.size() != chosen.alternatives.size() ||
                  std::ranges::count_if(places, [](const BlockConstruction& p) {
                    return p.body.has_value();
                  }) > 1) {
                says("builds other than one of the alternatives written");
              }
            }},
        scope.generates.Get(id).counting);
    for (const BlockConstruction& place : places) {
      if (!place.body.has_value()) continue;
      if (place.body->value >= unit.block_bodies.size()) {
        says("builds a body the unit does not hold");
        continue;
      }
      const StructuralScope& body = unit.block_bodies.Get(*place.body).scope;
      if (ValuesHandedTo(body) != handed) {
        says("builds a body handed other than its loop's index");
      }
      VerifyConstruction(
          unit, body, place.built,
          std::format("block body {}", place.body->value), lines);
    }
  }
}

// A route names a scope's own identity for what it reaches -- a data object, a
// static, a disable target -- only where it reaches the reader's own scope:
// climbing nothing and stepping nowhere. Anything another scope holds is
// named by what that scope published, so the reader's code states nothing of
// which body that scope is.
void VerifyRoutesStayOrPublish(
    const StructuralScope& scope, const std::string& label,
    std::vector<std::string>& lines) {
  const auto in_own_scope = [](const auto& route) {
    const auto* base = std::get_if<InUnitBase>(&route.base);
    return base != nullptr && base->hops.value == 0 && route.steps.empty();
  };
  for (const ValueRoute& route : scope.routes.values) {
    const bool own_identity = std::visit(
        Overloaded{
            [](const StructuralDataObjectLeaf&) { return true; },
            [](const ProceduralStaticLeaf&) { return true; },
            [](const PublishedMemberLeaf&) { return false; }},
        route.leaf);
    if (own_identity && !in_own_scope(route)) {
      lines.push_back(
          std::format(
              "hir verify: a route of {} names a declaration by its own "
              "scope's identity, from outside that scope",
              label));
    }
  }
  for (const DisableTargetRoute& route : scope.routes.disable_targets) {
    const bool own_identity = std::visit(
        Overloaded{
            [](const DisableTargetLeaf&) { return true; },
            [](const PublishedDisableTargetLeaf&) { return false; }},
        route.leaf);
    if (own_identity && !in_own_scope(route)) {
      lines.push_back(
          std::format(
              "hir verify: a disable route of {} names a target by its own "
              "scope's identity, from outside that scope",
              label));
    }
  }
}

// `label` names the scope eagerly, unlike a body: a scope's name is part of
// the name of every scope and body under it, and there are few scopes.
void VerifyScope(
    const CompilationUnit& unit, const StructuralScope& scope,
    const std::string& label, std::vector<std::string>& lines) {
  ArenaReaches reaches{scope};
  ReachFromDataObjects(reaches, scope);
  // A namespace keeps its subroutines' statics as its variables, and their
  // initializers are expressions of its own scope, the one scope it has.
  if (&scope == &unit.root_scope) {
    const auto from_kept =
        ReachFrom(reaches, std::nullopt, "a subroutine static's declaration");
    if (unit.namespace_definition.has_value()) {
      const NamespaceDefinition& defined = *unit.namespace_definition;
      for (const NamespaceVariableId id : defined.variables.Ids()) {
        if (!defined.variables.IsDefined(id)) continue;
        const auto* kept =
            std::get_if<NamespaceStatic>(&defined.variables.Get(id));
        if (kept != nullptr && kept->initializer.has_value()) {
          from_kept(*kept->initializer, Slot::kOperand);
        }
      }
    }
  }
  const auto from_assign =
      ReachFrom(reaches, std::nullopt, "a continuous assignment");
  for (const ContinuousAssign& assign : scope.continuous_assigns) {
    from_assign(assign.lhs, Slot::kOperand);
    from_assign(assign.rhs, Slot::kOperand);
    if (assign.delay.has_value()) {
      std::visit(
          Overloaded{
              [&](const SingleDelay& d) {
                from_assign(d.amount, Slot::kOperand);
              },
              [&](const RiseFallDelay& d) {
                from_assign(d.rise, Slot::kOperand);
                from_assign(d.fall, Slot::kOperand);
              },
              [&](const RiseFallTurnOffDelay& d) {
                from_assign(d.rise, Slot::kOperand);
                from_assign(d.fall, Slot::kOperand);
                from_assign(d.turn_off, Slot::kOperand);
              }},
          *assign.delay);
    }
    ReachSensitivity(assign.sensitivity_list, from_assign);
  }
  for (const Generate& generate : scope.generates) {
    ReachFromGenerate(reaches, generate);
  }
  ReachFromConnections(reaches, scope);
  const auto from_history =
      ReachFrom(reaches, std::nullopt, "a sampled history");
  for (const SampledHistoryDecl& history : scope.sampled_histories) {
    from_history(history.subject, Slot::kOperand);
    ReachEventControl(history.clock, from_history);
    from_history(history.depth, Slot::kOperand);
  }
  const auto from_clocking =
      ReachFrom(reaches, std::nullopt, "a clocking block");
  const auto reach_delay = [&](const DelayControl& delay) {
    from_clocking(delay.duration, Slot::kOperand);
  };
  for (const ClockingBlockDecl& clocking : scope.clocking_blocks) {
    ReachEventControl(clocking.event, from_clocking);
    for (const Clockvar& clockvar : clocking.clockvars) {
      from_clocking(clockvar.signal, Slot::kOperand);
      ReachSensitivity(clockvar.signal_reads, from_clocking);
      if (clockvar.input.has_value()) {
        std::visit(
            Overloaded{
                [](const OneStepSkew&) {},
                [&](const DelaySkew& skew) { reach_delay(skew.delay); }},
            clockvar.input->skew);
      }
      if (clockvar.output.has_value()) {
        std::visit(
            Overloaded{
                [](const NoSkew&) {},
                [&](const DelaySkew& skew) { reach_delay(skew.delay); },
                [&](const EdgeSkew& skew) {
                  if (skew.delay.has_value()) reach_delay(*skew.delay);
                }},
            clockvar.output->skew);
        std::visit(
            Overloaded{
                [&](const NetSignal& nets) {
                  from_clocking(nets.driving, Slot::kOperand);
                  ReachSensitivity(nets.reads, from_clocking);
                },
                [](const VariableSignal&) {}},
            clockvar.output->signal);
      }
    }
  }
  Report(reaches, [&] { return label; }, lines);
  ReportClassReads(unit, reaches, [&] { return label; }, lines);
  ReportNamespaceMembers(unit, reaches, [&] { return label; }, lines);
  VerifyInstanceMembers(scope, label, lines);
  VerifyRoutesStayOrPublish(scope, label, lines);

  for (const ProcessId id : scope.processes.Ids()) {
    const Process& process = scope.processes.Get(id);
    const std::vector<StmtId> entered_at{process.root_stmt};
    VerifyBody(
        unit, process.body, entered_at,
        [&](ArenaReaches& body_reaches) {
          const auto from_process =
              ReachFrom(body_reaches, std::nullopt, "a process");
          std::visit(
              Overloaded{
                  [](const InitialProcess&) {}, [](const FinalProcess&) {},
                  [](const AlwaysProcess&) {}, [](const AlwaysFfProcess&) {},
                  [&](const AlwaysCombProcess& comb) {
                    ReachReads(comb.implicit_reads, from_process);
                  },
                  [&](const AlwaysLatchProcess& latch) {
                    ReachReads(latch.implicit_reads, from_process);
                  }},
              process.kind);
        },
        [&] { return std::format("process {} of {}", id.value, label); },
        lines);
  }
  for (const SubroutineDecl& subroutine : scope.structural_subroutines) {
    VerifySubroutine(
        unit, subroutine, [](ArenaReaches&) {},
        [&] {
          return std::format("subroutine '{}' of {}", subroutine.name, label);
        },
        lines);
  }
  for (const ConcurrentAssertionId id : scope.concurrent_assertions.Ids()) {
    const ConcurrentAssertionDecl& declared =
        scope.concurrent_assertions.Get(id);
    // The action is entered at whichever arm an outcome selects (LRM 16.14).
    std::vector<StmtId> entered_at;
    const auto arm = [&](const std::optional<StmtId>& stmt) {
      if (stmt.has_value()) {
        entered_at.push_back(*stmt);
      }
    };
    std::visit(
        Overloaded{
            [&](const ConcurrentAssertStmt& s) {
              arm(s.pass_stmt);
              arm(s.fail_stmt);
            },
            [&](const ConcurrentCoverStmt& s) { arm(s.pass_stmt); }},
        declared.assertion);
    VerifyBody(
        unit, declared.action, entered_at,
        [&](ArenaReaches& body_reaches) {
          const auto from_assertion =
              ReachFrom(body_reaches, std::nullopt, "a concurrent assertion");
          std::visit(
              Overloaded{
                  [&](const ConcurrentAssertStmt& s) {
                    ReachPropertySpec(s.spec, from_assertion);
                  },
                  [&](const ConcurrentCoverStmt& s) {
                    ReachPropertySpec(s.spec, from_assertion);
                  }},
              declared.assertion);
        },
        [&] {
          return std::format("concurrent assertion {} of {}", id.value, label);
        },
        lines);
  }
}

}  // namespace

void Verify(
    const CompilationUnit& unit, const ScopeConstruction& construction) {
  std::vector<std::string> lines;
  VerifyScope(
      unit, unit.root_scope,
      std::format("the root scope of unit '{}'", unit.name), lines);
  for (const BlockBodyId id : unit.block_bodies.Ids()) {
    VerifyScope(
        unit, unit.block_bodies.Get(id).scope,
        std::format("block body {} of unit '{}'", id.value, unit.name), lines);
  }
  VerifyConstruction(
      unit, unit.root_scope, construction,
      std::format("the root scope of unit '{}'", unit.name), lines);
  const auto verify_replicated = [&](const StructuralScope& scope) {
    for (const ReplicatedClass& cls : scope.replicated_classes) {
      VerifyClass(unit, cls, lines);
    }
    for (const ReplicatedCovergroup& covergroup :
         scope.replicated_covergroups) {
      VerifyCovergroup(unit, covergroup, lines);
    }
  };
  verify_replicated(unit.root_scope);
  for (const BlockBodyId id : unit.block_bodies.Ids()) {
    verify_replicated(unit.block_bodies.Get(id).scope);
  }
  VerifyClassReadsOfDeclarations(unit, lines);
  VerifyNamespaceDefinition(unit, lines);
  VerifyDeclaredDefaults(unit, lines);
  std::string report;
  for (const std::string& line : lines) {
    if (!report.empty()) {
      report += '\n';
    }
    report += line;
  }
  if (!lines.empty()) {
    throw InternalError(report);
  }
}

}  // namespace lyra::hir
