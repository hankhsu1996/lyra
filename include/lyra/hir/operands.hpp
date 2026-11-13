#pragma once

#include <cstdint>
#include <optional>
#include <span>
#include <variant>

#include "lyra/base/arena.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/assertion.hpp"
#include "lyra/hir/constraint.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/pattern.hpp"
#include "lyra/hir/pattern_id.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/timing.hpp"

namespace lyra::hir {

// How a holder names an expression. `kOperand` is an expression the holder
// evaluates where it stands. A function's report evaluates what it names in
// an evaluation of its own, ahead of the body; a sensitivity evaluates
// nothing; a shared name is the value another holder's evaluation gives, the
// instance a synchronous drive's target reaches; and a sampled value
// function's clock is evaluated by what keeps its history, at each tick, never
// where the call stands. Only an operand and a clock are places an expression
// is evaluated at; the others are held only to naming one the arena has.
enum class OperandSlot : std::uint8_t {
  kOperand,
  kReport,
  kSensitivity,
  kShared,
  kHistory
};

void ReachSensitivityEntry(const SensitivityEntry& entry, const auto& reach) {
  std::visit(
      Overloaded{
          [](const WatchedWhole&) {},
          [&](const WatchedSelect& part) {
            reach(part.prefix, OperandSlot::kSensitivity);
          }},
      entry.part);
}

void ReachSensitivity(
    std::span<const SensitivityEntry> entries, const auto& reach) {
  for (const SensitivityEntry& entry : entries) {
    ReachSensitivityEntry(entry, reach);
  }
}

// The expressions an event control evaluates, each in `evaluated`.
void ReachEventControl(
    const EventControl& control, OperandSlot evaluated, const auto& reach) {
  for (const EventEntry& entry : control.entries) {
    std::visit(
        Overloaded{
            [&](const ValueChange& c) {
              reach(c.signal, evaluated);
              ReachSensitivity(c.cells, reach);
            },
            [&](const NamedEventTrigger& t) { reach(t.event, evaluated); }},
        entry.occurrence);
    if (entry.condition.has_value()) {
      reach(*entry.condition, evaluated);
    }
  }
}

void ReachTimingControl(const TimingControl& timing, const auto& reach) {
  std::visit(
      Overloaded{
          [&](const DelayControl& c) {
            reach(c.duration, OperandSlot::kOperand);
          },
          [&](const EventControl& c) {
            ReachEventControl(c, OperandSlot::kOperand, reach);
          },
          [&](const ImplicitEventControl& c) {
            ReachSensitivity(c.sensitivity_list, reach);
          },
          [&](const CycleDelay& c) {
            reach(c.count, OperandSlot::kOperand);
            reach(c.event, OperandSlot::kOperand);
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
                      reach(target.access, OperandSlot::kShared);
                    }},
                published.receiver);
          }},
      landing);
}

void ReachDelayOrEventControl(
    const DelayOrEventControl& control, const auto& reach) {
  std::visit(
      Overloaded{
          [&](const DelayControl& c) {
            reach(c.duration, OperandSlot::kOperand);
          },
          [&](const EventControl& c) {
            ReachEventControl(c, OperandSlot::kOperand, reach);
          },
          [&](const RepeatedEventControl& c) {
            reach(c.count, OperandSlot::kOperand);
            ReachEventControl(c.event, OperandSlot::kOperand, reach);
          },
          [&](const ClockingDriveControl& c) {
            if (c.cycles.has_value()) reach(*c.cycles, OperandSlot::kOperand);
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
    reach(clause.expr, OperandSlot::kOperand);
    if (clause.pattern.has_value()) {
      reach.ThroughPattern(*clause.pattern);
    }
  }
}

void ReachPropertySpec(const PropertySpec& spec, const auto& reach) {
  ReachEventControl(spec.clock, OperandSlot::kOperand, reach);
  if (spec.disable.has_value()) {
    reach(spec.disable->condition, OperandSlot::kOperand);
    ReachSensitivity(spec.disable->sensitivity, reach);
  }
  reach.ThroughProperty(spec.body);
}

// What an access through a virtual interface evaluates: the handle, and what
// selects each loop's block on the way down.
void ReachInstanceAccess(
    const InterfaceInstanceAccessExpr& access, const auto& reach) {
  reach(access.handle, OperandSlot::kOperand);
  for (const PublishedStep& step : access.steps) {
    if (const auto* loop = std::get_if<PublishedGenerateLoopRef>(&step.names)) {
      reach(loop->position, OperandSlot::kOperand);
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
            reach(ref.receiver, OperandSlot::kOperand);
          },
          [](const StaticMethodCallRef&) {}, [](const SystemSubroutineRef&) {},
          [&](const BuiltinMethodRef& ref) {
            if (ref.receiver.has_value()) {
              reach(*ref.receiver, OperandSlot::kOperand);
            }
          },
          [](const EnumMethodRef&) {},
          [&](const ArrayIteratorMethodRef& ref) {
            if (ref.with_clause.has_value()) {
              reach(ref.with_clause->expr, OperandSlot::kOperand);
            }
          },
          [&](const PastValueRef& ref) {
            ReachEventControl(ref.clock, OperandSlot::kHistory, reach);
          },
          [&](const ValueChangeRef& ref) {
            ReachEventControl(ref.clock, OperandSlot::kHistory, reach);
          },
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
            reach(b.left_bound, OperandSlot::kOperand);
            reach(b.right_bound, OperandSlot::kOperand);
          },
          [&](const RangeIndexedUpBounds& b) {
            reach(b.base_index, OperandSlot::kOperand);
            reach(b.width, OperandSlot::kOperand);
          },
          [&](const RangeIndexedDownBounds& b) {
            reach(b.base_index, OperandSlot::kOperand);
            reach(b.width, OperandSlot::kOperand);
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
// that holds `data`, and its `ThroughPattern` or `ThroughConstraint` with each
// pattern or constraint tree `data` holds.
void ForEachOperand(const ExprData& data, const auto& reach) {
  const auto operand = [&](ExprId id) { reach(id, OperandSlot::kOperand); };
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

// Calls `reach` with each expression `data` holds itself, and its
// `ThroughPattern`, `ThroughProperty` or `ThroughConstraint` with each tree
// `data` holds. The statements under `data` are `ForEachChildStmt`'s, and a
// declared variable's initializer is the variable's, not the statement's.
void ForEachExpr(const StmtData& data, const auto& reach) {
  const auto operand = [&](ExprId id) { reach(id, OperandSlot::kOperand); };
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

// The expressions a pattern holds are the constants it matches against (LRM
// 12.6), however deep in it they are written.
void ForEachPatternOperand(
    const base::Arena<Pattern, PatternId>& patterns, PatternId id,
    const auto& operand) {
  std::visit(
      Overloaded{
          [](const WildcardPattern&) {},
          [&](const ConstantPattern& pattern) { operand(pattern.value); },
          [](const VariablePattern&) {},
          [&](const TaggedPattern& pattern) {
            if (pattern.value_pattern.has_value()) {
              ForEachPatternOperand(patterns, *pattern.value_pattern, operand);
            }
          },
          [&](const StructurePattern& pattern) {
            for (const auto& field : pattern.field_patterns) {
              ForEachPatternOperand(patterns, field.second, operand);
            }
          }},
      patterns.Get(id).data);
}

// The expressions a sequence holds are the Boolean expressions it is built
// from (LRM 16.7).
void ForEachSequenceOperand(
    const base::Arena<SequenceExpr, SequenceExprId>& sequences,
    SequenceExprId id, const auto& operand) {
  std::visit(
      Overloaded{
          [&](const SequenceBoolean& sequence) { operand(sequence.condition); },
          [&](const SequenceDelay& sequence) {
            ForEachSequenceOperand(sequences, sequence.head, operand);
            ForEachSequenceOperand(sequences, sequence.tail, operand);
          },
          [&](const SequenceRepetition& sequence) {
            ForEachSequenceOperand(sequences, sequence.body, operand);
          }},
      sequences.Get(id).data);
}

// The expressions a property holds are its sequences' and what an if-else or
// a case property selects by (LRM 16.12.6, 16.12.16).
void ForEachPropertyOperand(
    const base::Arena<PropertyExpr, PropertyExprId>& properties,
    const base::Arena<SequenceExpr, SequenceExprId>& sequences,
    PropertyExprId id, const auto& operand) {
  const auto property = [&](PropertyExprId inner) {
    ForEachPropertyOperand(properties, sequences, inner, operand);
  };
  const auto sequence = [&](SequenceExprId inner) {
    ForEachSequenceOperand(sequences, inner, operand);
  };
  std::visit(
      Overloaded{
          [&](const PropertySequence& p) { sequence(p.sequence); },
          [&](const PropertyImplication& p) {
            sequence(p.antecedent);
            property(p.consequent);
          },
          [&](const PropertyFollowedBy& p) {
            sequence(p.antecedent);
            property(p.consequent);
          },
          [&](const PropertyNot& p) { property(p.operand); },
          [&](const PropertyBinary& p) {
            property(p.lhs);
            property(p.rhs);
          },
          [&](const PropertyConditional& p) {
            operand(p.condition);
            property(p.then_property);
            if (p.else_property.has_value()) property(*p.else_property);
          },
          [&](const PropertyCase& p) {
            operand(p.subject);
            for (const PropertyCaseItem& item : p.items) {
              for (const ExprId label : item.labels) operand(label);
              property(item.property);
            }
            if (p.default_property.has_value()) property(*p.default_property);
          }},
      properties.Get(id).data);
}

// The expressions a constraint holds, however deep in its tree (LRM 18.5).
void ForEachConstraintOperand(
    const base::Arena<Constraint, ConstraintId>& constraints, ConstraintId id,
    const auto& operand) {
  const auto inner = [&](ConstraintId c) {
    ForEachConstraintOperand(constraints, c, operand);
  };
  std::visit(
      Overloaded{
          [&](const ConstraintList& c) {
            for (const ConstraintId item : c.items) inner(item);
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
            inner(c.body);
          },
          [&](const ConditionalConstraint& c) {
            operand(c.predicate);
            inner(c.then_body);
            if (c.else_body.has_value()) inner(*c.else_body);
          },
          [&](const UniquenessConstraint& c) {
            for (const ExprId item : c.items) operand(item);
          },
          [&](const ForeachConstraint& c) {
            operand(c.array);
            inner(c.body);
          },
          [&](const SolveBeforeConstraint& c) {
            for (const ExprId e : c.solve) operand(e);
            for (const ExprId e : c.after) operand(e);
          },
          [&](const DisableSoftConstraint& c) { operand(c.target); }},
      constraints.Get(id).data);
}

// The arenas holding the trees one body's expressions name, each null where the
// holder keeps none of that kind.
struct TreeArenas {
  const base::Arena<Pattern, PatternId>* patterns;
  const base::Arena<PropertyExpr, PropertyExprId>* properties;
  const base::Arena<SequenceExpr, SequenceExprId>* sequences;
  const base::Arena<Constraint, ConstraintId>* constraints;
};

// The reach `ForEachOperand` and `ForEachExpr` take, handing `visit` each
// expression the holder evaluates where it stands: an operand, and each
// expression a pattern, a property or a constraint tree it holds is built
// from. What a report, a sensitivity or a shared name names, and a sampled
// value function's clock, are evaluated elsewhere and are not handed over.
template <typename Visit>
struct EvaluatedOperands {
  TreeArenas trees;
  const Visit* visit;

  void operator()(ExprId id, OperandSlot slot) const {
    switch (slot) {
      case OperandSlot::kOperand:
        (*visit)(id);
        return;
      case OperandSlot::kReport:
      case OperandSlot::kSensitivity:
      case OperandSlot::kShared:
      case OperandSlot::kHistory:
        return;
    }
  }
  void ThroughPattern(PatternId id) const {
    if (trees.patterns == nullptr) {
      throw InternalError("hir: a pattern was named where no tree of one is");
    }
    ForEachPatternOperand(*trees.patterns, id, *visit);
  }
  void ThroughProperty(PropertyExprId id) const {
    if (trees.properties == nullptr || trees.sequences == nullptr) {
      throw InternalError("hir: a property was named where no tree of one is");
    }
    ForEachPropertyOperand(*trees.properties, *trees.sequences, id, *visit);
  }
  void ThroughConstraint(ConstraintId id) const {
    if (trees.constraints == nullptr) {
      throw InternalError(
          "hir: a constraint was named where no tree of one is");
    }
    ForEachConstraintOperand(*trees.constraints, id, *visit);
  }
};

}  // namespace lyra::hir
