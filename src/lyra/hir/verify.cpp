#include "lyra/hir/verify.hpp"

#include <cstdint>
#include <format>
#include <optional>
#include <ranges>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/assertion.hpp"
#include "lyra/hir/class_decl.hpp"
#include "lyra/hir/class_id.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/interface_member_access.hpp"
#include "lyra/hir/method_id.hpp"
#include "lyra/hir/owned_child_ref.hpp"
#include "lyra/hir/pattern.hpp"
#include "lyra/hir/pattern_id.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/range_bounds.hpp"
#include "lyra/hir/sampled_history.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/timing.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::hir {

namespace {

// How a holder names an expression. An operand is what the holder evaluates,
// and is a place the expression is reached at. A read set and a sensitivity
// describe what a body reads or waits on in terms of expressions the body
// already holds, so naming one there is no place it is evaluated at; it is
// held only to naming an expression the arena has.
enum class Slot : std::uint8_t { kOperand, kReadSet, kSensitivity };

void ReachSensitivityEntry(const SensitivityEntry& entry, const auto& reach) {
  std::visit(
      Overloaded{
          [](const WatchedWhole&) {},
          [&](const WatchedSelect& part) {
            reach(part.prefix, Slot::kSensitivity);
          },
          [](const WatchedBits&) {}},
      entry.part);
}

void ReachSensitivity(
    std::span<const SensitivityEntry> entries, const auto& reach) {
  for (const SensitivityEntry& entry : entries) {
    ReachSensitivityEntry(entry, reach);
  }
}

void ReachReads(const Reads& reads, const auto& reach) {
  for (const WaitLeaf& leaf : reads.leaves) {
    std::visit(
        Overloaded{
            [&](const SensitivityEntry& entry) {
              ReachSensitivityEntry(entry, reach);
            },
            [&](const InterfaceMemberAccessExpr& access) {
              reach(access.instance.handle, Slot::kReadSet);
            },
            [&](const ObjectChain& chain) {
              std::visit(
                  Overloaded{
                      [&](const ExprId& root) { reach(root, Slot::kReadSet); },
                      [](const ReceiverObject&) {}},
                  chain.root);
            },
            [](const EveryObject&) {}},
        leaf);
  }
  for (const ReportingCall& call : reads.calls) {
    reach(call.call, Slot::kReadSet);
  }
}

void ReachEventControl(const EventControl& control, const auto& reach) {
  for (const EventTrigger& trigger : control.triggers) {
    reach(trigger.signal, Slot::kOperand);
    ReachReads(trigger.reads, reach);
    if (trigger.condition.has_value()) {
      reach(*trigger.condition, Slot::kOperand);
    }
  }
}

void ReachNamedEventControl(
    const NamedEventControl& control, const auto& reach) {
  ReachSensitivityEntry(control.event, reach);
  if (control.condition.has_value()) {
    reach(*control.condition, Slot::kOperand);
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
          [&](const NamedEventControl& c) {
            ReachNamedEventControl(c, reach);
          }},
      timing);
}

void ReachDelayOrEventControl(
    const DelayOrEventControl& control, const auto& reach) {
  std::visit(
      Overloaded{
          [&](const DelayControl& c) { reach(c.duration, Slot::kOperand); },
          [&](const EventControl& c) { ReachEventControl(c, reach); },
          [&](const NamedEventControl& c) { ReachNamedEventControl(c, reach); },
          [&](const RepeatedEventControl& c) {
            reach(c.count, Slot::kOperand);
            std::visit(
                Overloaded{
                    [&](const EventControl& event) {
                      ReachEventControl(event, reach);
                    },
                    [&](const NamedEventControl& event) {
                      ReachNamedEventControl(event, reach);
                    }},
                c.event);
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

// The object a call is made on, where the callee holds an expression for it.
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
          [](const EnumMethodRef&) {}, [](const PastValueRef&) {},
          [](const ValueChangeRef&) {}, [](const ForeignImportRef&) {},
          [](const ExternalUnitSubroutineRef&) {},
          [&](const ExternalUnitMethodRef& ref) {
            std::visit(
                Overloaded{
                    [](const RoutedObjectRef&) {},
                    [&](const InterfaceInstanceAccessExpr& access) {
                      reach(access.handle, Slot::kOperand);
                    }},
                ref.receiver);
          },
          [](const OpaqueUnitMethodRef&) {}},
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
            if (e.with_clause.has_value()) {
              operand(e.with_clause->expr);
            }
          },
          [&](const ConversionExpr& e) { operand(e.operand); },
          [&](const ValueRangeExpr& e) {
            operand(e.lo);
            operand(e.hi);
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
            operand(e.instance.handle);
          },
          [&](const InterfaceInstanceAccessExpr& e) { operand(e.handle); },
          [&](const ConcatExpr& e) { operands(e.operands); },
          [&](const StreamingConcatExpr& e) { operands(e.operands); },
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
            ReachReads(s.reads, reach);
          },
          [](const WaitForkStmt&) {},
          [](const DisableForkStmt&) {},
          [](const DisableStmt&) {},
          [&](const ProceduralContinuousAssignStmt& s) {
            operand(s.target);
            operand(s.source);
            ReachSensitivity(s.sensitivity_list, reach);
          },
          [&](const ProceduralContinuousEndStmt& s) { operand(s.target); }},
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
          [](const ProceduralContinuousEndStmt&) {}},
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
          [](const RoutedObjectRef&) -> std::string_view {
            return "a routed object reference";
          },
          [](const IterationBindingRef&) -> std::string_view {
            return "an iteration binding reference";
          },
          [](const PatternVarRef&) -> std::string_view {
            return "a pattern variable reference";
          },
          [](const ExternalUnitValueRef&) -> std::string_view {
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
          [](const PastValueRef&) -> std::string {
            return "a past value read";
          },
          [](const ValueChangeRef&) -> std::string {
            return "a value change read";
          },
          [](const ForeignImportRef&) -> std::string {
            return "a foreign import call";
          },
          [](const ExternalUnitSubroutineRef&) -> std::string {
            return "a namespace subroutine call";
          },
          [](const ExternalUnitMethodRef&) -> std::string {
            return "a call on another unit's instance";
          },
          [](const OpaqueUnitMethodRef&) -> std::string {
            return "a call by hierarchical name";
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
          [](const ConversionExpr&) -> std::string { return "a conversion"; },
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
  std::vector<std::vector<Place>> at;
  std::vector<std::pair<ExprId, Place>> outside;

  explicit ArenaReaches(const ProceduralBody& body)
      : exprs(&body.exprs),
        patterns(&body.patterns),
        sequences(&body.sequence_exprs),
        properties(&body.property_exprs),
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
      case Slot::kReadSet:
      case Slot::kSensitivity:
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
  // are built from (LRM 16.7).
  void ThroughProperty(PropertyExprId id) const {
    if (reaches->properties == nullptr) {
      throw InternalError(
          "hir verify: a property was written where no body holds its tree");
    }
    std::visit(
        Overloaded{
            [&](const PropertySequence& property) {
              ThroughSequence(property.sequence);
            },
            [&](const PropertyImplication& property) {
              ThroughSequence(property.antecedent);
              ThroughProperty(property.consequent);
            }},
        reaches->properties->Get(id).data);
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
    case Slot::kReadSet:
      return std::format("the read set of {}", holder);
    case Slot::kSensitivity:
      return std::format("the sensitivity of {}", holder);
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

// Counts the reaches into one body's expressions. `entered_at` is the
// statements execution enters the body at, which whatever owns the body
// states, and `reach_from_owner` adds the reaches that owner makes itself,
// holding expressions of the body outside any statement of it.
void VerifyBody(
    const ProceduralBody& body, std::span<const StmtId> entered_at,
    const auto& reach_from_owner, const auto& where,
    std::vector<std::string>& lines) {
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
}

void VerifySubroutine(
    const SubroutineDecl& subroutine, const auto& reach_from_owner,
    const auto& where, std::vector<std::string>& lines) {
  // A prototype states a signature and has no statement to enter at (LRM
  // 8.21).
  const std::vector<StmtId> entered_at =
      subroutine.is_prototype ? std::vector<StmtId>{}
                              : std::vector<StmtId>{subroutine.root_stmt};
  VerifyBody(
      subroutine.body, entered_at,
      [&](ArenaReaches& reaches) {
        ReachReads(
            subroutine.reads, ReachFrom(reaches, std::nullopt, "a subroutine"));
        reach_from_owner(reaches);
      },
      where, lines);
}

void VerifyClass(
    const CompilationUnit& unit, ClassId id, std::vector<std::string>& lines) {
  const ClassDecl& cls = unit.classes.Get(id);
  const auto owner = [&] {
    return std::format(
        "class '{}' in unit '{}'", unit.classes.NameOf(id), unit.name);
  };
  for (const MethodId method : cls.methods.Ids()) {
    const SubroutineDecl& decl = cls.methods.Get(method);
    VerifySubroutine(
        decl, [](ArenaReaches&) {},
        [&] { return std::format("method '{}' of {}", decl.name, owner()); },
        lines);
  }
  VerifySubroutine(
      cls.constructor,
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
      cls.static_init, {},
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
  std::visit(
      Overloaded{
          [](const BlocksStandAlone&) {},
          [&](const BlocksRepeat& loop) {
            from_loop(loop.initial, Slot::kOperand);
            from_loop(loop.condition, Slot::kOperand);
            from_loop(loop.step, Slot::kOperand);
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
  const auto from_block =
      ReachFrom(reaches, std::nullopt, "a generate block's construction");
  for (const GenerateBlock& block : generate.blocks) {
    for (const ExprId argument : block.arguments) {
      from_block(argument, Slot::kOperand);
    }
  }
}

void ReachFromConnections(ArenaReaches& reaches, const StructuralScope& scope) {
  const auto from_instance =
      ReachFrom(reaches, std::nullopt, "an instance's construction");
  for (const InstanceMemberDecl& instance : scope.instance_members) {
    for (const ExprId argument : instance.arguments) {
      from_instance(argument, Slot::kOperand);
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
              from_port(data.peer, Slot::kOperand);
              ReachSensitivity(data.sensitivity, from_port);
            },
            [](const InterfacePortConnection&) {}},
        connection.kind);
  }
  const auto from_join = ReachFrom(reaches, std::nullopt, "a net join");
  for (const NetJoin& join : scope.net_joins) {
    for (const NetSide& side : join.sides) {
      for (const NetRun& run : side) {
        from_join(run.part, Slot::kOperand);
      }
    }
  }
}

// `label` names the scope eagerly, unlike a body: a scope's name is part of
// the name of every scope and body under it, and there are few scopes.
void VerifyScope(
    const StructuralScope& scope, const std::string& label,
    std::vector<std::string>& lines) {
  ArenaReaches reaches{scope};
  ReachFromDataObjects(reaches, scope);
  const auto from_assign =
      ReachFrom(reaches, std::nullopt, "a continuous assignment");
  for (const ContinuousAssign& assign : scope.continuous_assigns) {
    from_assign(assign.lhs, Slot::kOperand);
    from_assign(assign.rhs, Slot::kOperand);
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
  Report(reaches, [&] { return label; }, lines);

  for (const ProcessId id : scope.processes.Ids()) {
    const Process& process = scope.processes.Get(id);
    const std::vector<StmtId> entered_at{process.root_stmt};
    VerifyBody(
        process.body, entered_at,
        [&](ArenaReaches& body_reaches) {
          ReachSensitivity(
              process.implicit_sensitivity_list,
              ReachFrom(body_reaches, std::nullopt, "a process"));
        },
        [&] { return std::format("process {} of {}", id.value, label); },
        lines);
  }
  for (const SubroutineDecl& subroutine : scope.structural_subroutines) {
    VerifySubroutine(
        subroutine, [](ArenaReaches&) {},
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
        declared.action, entered_at,
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
  for (const GenerateId generate : scope.generates.Ids()) {
    const auto& blocks = scope.generates.Get(generate).blocks;
    for (const StructuralScopeId block : blocks.Ids()) {
      const StructuralScope& child = blocks.Get(block).scope;
      VerifyScope(
          child,
          std::format(
              "block {} '{}' of generate {} in {}", block.value,
              child.source_name, generate.value, label),
          lines);
    }
  }
}

}  // namespace

void Verify(const CompilationUnit& unit) {
  std::vector<std::string> lines;
  VerifyScope(
      unit.root_scope, std::format("the root scope of unit '{}'", unit.name),
      lines);
  for (const ClassId id : unit.classes.Ids()) {
    VerifyClass(unit, id, lines);
  }
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
