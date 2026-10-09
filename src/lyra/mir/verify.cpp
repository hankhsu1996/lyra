#include "lyra/mir/verify.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/block_id.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/declared_class.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/external_class.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/struct_decl.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/def_path.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::mir {

namespace {

// A body the source named goes by that name, and one it did not by where it
// sits, so a violation names what was being lowered wherever it can.
auto BodyLabel(std::optional<std::string_view> name, std::uint32_t position)
    -> std::string {
  return name.has_value() ? std::format("'{}'", *name)
                          : std::format("body {}", position);
}

// Whether anything in `block` suspends, checking on the way that each
// suspension waits on what its kind waits on: an await on an execution, a wait
// on the answer a registration gives. The two differ in what ends them, so a
// suspension whose operand is the other kind's is a program neither backend
// could translate as written.
auto Suspends(
    const CompilationUnit& unit, const Block& block, const auto& describe)
    -> bool {
  bool suspends = false;
  for (const Expr& expr : block.exprs) {
    if (const auto* await = std::get_if<AwaitExpr>(&expr.data)) {
      suspends = true;
      if (!unit.types.Get(block.exprs.Get(await->execution).type)
               .Is<CoroutineType>()) {
        throw InternalError(
            std::format(
                "mir verify: {} awaits something that is not an execution",
                describe()));
      }
    } else if (const auto* wait = std::get_if<WaitExpr>(&expr.data)) {
      suspends = true;
      if (!unit.types.Get(block.exprs.Get(wait->park).type)
               .Is<MachineBoolType>()) {
        throw InternalError(
            std::format(
                "mir verify: {} waits on something that does not answer "
                "whether it must park",
                describe()));
      }
    }
  }
  for (const Block& child : block.child_scopes) {
    suspends = Suspends(unit, child, describe) || suspends;
  }
  return suspends;
}

// Calls `reach` with each node `data` takes a value from. Every one is named in
// the block holding `data` but a block expression's value, which is named in
// the block of its steps and is reached from there.
void ForEachOperand(const ExprData& data, const auto& reach) {
  std::visit(
      Overloaded{
          [](const StringLiteral&) {},
          [](const NullLiteral&) {},
          [](const MachineBoolLiteral&) {},
          [](const MachineIntLiteral&) {},
          [](const MachineFloatLiteral&) {},
          [](const ReferenceExpr&) {},
          [&](const UnaryExpr& e) { reach(e.operand); },
          [&](const BinaryExpr& e) {
            reach(e.lhs);
            reach(e.rhs);
          },
          [&](const CastExpr& e) { reach(e.operand); },
          [&](const DynamicCastExpr& e) { reach(e.operand); },
          [&](const ConditionalExpr& e) {
            reach(e.condition);
            reach(e.then_value);
            reach(e.else_value);
          },
          [](const BlockExpr&) {},
          [&](const AssignExpr& e) {
            reach(e.target);
            reach(e.value);
          },
          [&](const CallExpr& e) {
            std::visit(
                Overloaded{
                    [&](const Direct& callee) {
                      if (callee.receiver.has_value()) {
                        reach(*callee.receiver);
                      }
                    },
                    [&](const Indirect& callee) { reach(callee.code); },
                    [](const Construct&) {},
                    [&](const Virtual& callee) { reach(callee.receiver); }},
                e.callee);
            for (const ExprId argument : e.arguments) {
              reach(argument);
            }
          },
          [&](const DerefExpr& e) { reach(e.pointer); },
          [&](const AddressOfExpr& e) { reach(e.operand); },
          [&](const MoveExpr& e) { reach(e.operand); },
          [&](const FieldAccessExpr& e) { reach(e.receiver); },
          [&](const ClosureExpr& e) {
            for (const FieldInit& init : e.field_inits) {
              reach(init.value);
            }
          },
          [&](const CompositeExpr& e) {
            for (const ExprId part : e.parts) {
              reach(part);
            }
          },
          [&](const AwaitExpr& e) { reach(e.execution); },
          [&](const WaitExpr& e) { reach(e.park); },
          [&](const VectorGetExpr& e) {
            reach(e.vector);
            reach(e.index);
          }},
      data);
}

// Calls `reach` with each expression `data` evaluates itself, as opposed to
// through a block it runs.
void ForEachExpr(const StmtData& data, const auto& reach) {
  std::visit(
      Overloaded{
          [](const EmptyStmt&) {},
          [&](const LocalDeclStmt& s) { reach(s.init); },
          [&](const ExprStmt& s) { reach(s.expr); }, [](const BlockStmt&) {},
          [](const TryStmt&) {}, [&](const RaiseStmt& s) { reach(s.effect); },
          [](const FinallyStmt&) {},
          [&](const IfStmt& s) { reach(s.condition); },
          [&](const ForStmt& s) {
            for (const ForInit& init : s.init) {
              std::visit(
                  Overloaded{
                      [&](const ForInitDecl& decl) { reach(decl.init); },
                      [&](const ForInitExpr& expr) { reach(expr.expr); }},
                  init);
            }
            if (s.condition.has_value()) {
              reach(*s.condition);
            }
            for (const ExprId step : s.step) {
              reach(step);
            }
          },
          [&](const WhileStmt& s) { reach(s.condition); },
          [&](const DoWhileStmt& s) { reach(s.condition); },
          [](const BreakStmt&) {}, [](const ContinueStmt&) {},
          [&](const ReturnStmt& s) {
            if (s.value.has_value()) {
              reach(*s.value);
            }
          }},
      data);
}

// Whether a node computes nothing of its own: it names a declared thing, spells
// a constant, or forms a place over what it reaches. What happens where such a
// node stands is that place's, so one of them reached from two places and two
// of them are the same program, and the evaluations it stands for are those of
// what it reaches.
auto ComputesNothingItself(const ExprData& data) -> bool {
  return std::visit(
      Overloaded{
          [](const StringLiteral&) { return true; },
          [](const NullLiteral&) { return true; },
          [](const MachineBoolLiteral&) { return true; },
          [](const MachineIntLiteral&) { return true; },
          [](const MachineFloatLiteral&) { return true; },
          [](const ReferenceExpr&) { return true; },
          [](const DerefExpr&) { return true; },
          [](const AddressOfExpr&) { return true; },
          [](const FieldAccessExpr&) { return true; },
          [](const UnaryExpr&) { return false; },
          [](const BinaryExpr&) { return false; },
          [](const CastExpr&) { return false; },
          [](const DynamicCastExpr&) { return false; },
          [](const ConditionalExpr&) { return false; },
          [](const BlockExpr&) { return false; },
          [](const AssignExpr&) { return false; },
          [](const CallExpr&) { return false; },
          [](const MoveExpr&) { return false; },
          [](const ClosureExpr&) { return false; },
          [](const CompositeExpr&) { return false; },
          [](const AwaitExpr&) { return false; },
          [](const WaitExpr&) { return false; },
          [](const VectorGetExpr&) { return false; }},
      data);
}

// A node as a report names it: its kind, and for a call of a library entry
// which entry, since that is what tells one lowering from another.
auto Describe(const ExprData& data) -> std::string {
  return std::visit(
      Overloaded{
          [](const StringLiteral&) -> std::string {
            return "a string literal";
          },
          [](const NullLiteral&) -> std::string { return "a null literal"; },
          [](const MachineBoolLiteral&) -> std::string {
            return "a boolean literal";
          },
          [](const MachineIntLiteral&) -> std::string {
            return "an integer literal";
          },
          [](const MachineFloatLiteral&) -> std::string {
            return "a float literal";
          },
          [](const ReferenceExpr&) -> std::string { return "a reference"; },
          [](const UnaryExpr&) -> std::string { return "a unary operation"; },
          [](const BinaryExpr&) -> std::string { return "a binary operation"; },
          [](const CastExpr&) -> std::string { return "a cast"; },
          [](const DynamicCastExpr&) -> std::string {
            return "a dynamic cast";
          },
          [](const ConditionalExpr&) -> std::string { return "a conditional"; },
          [](const BlockExpr&) -> std::string { return "a block expression"; },
          [](const AssignExpr&) -> std::string { return "an assignment"; },
          [](const CallExpr& call) -> std::string {
            const auto entry = DirectBuiltinFn(call);
            return entry.has_value() ? std::format(
                                           "a call of the library entry '{}'",
                                           support::RuntimeEntryOf(*entry).name)
                                     : std::string{"a call"};
          },
          [](const DerefExpr&) -> std::string { return "a dereference"; },
          [](const AddressOfExpr&) -> std::string { return "an address-of"; },
          [](const MoveExpr&) -> std::string { return "a move"; },
          [](const FieldAccessExpr&) -> std::string {
            return "a field access";
          },
          [](const ClosureExpr&) -> std::string {
            return "a closure construction";
          },
          [](const CompositeExpr&) -> std::string { return "a composite"; },
          [](const AwaitExpr&) -> std::string { return "an await"; },
          [](const WaitExpr&) -> std::string { return "a wait"; },
          [](const VectorGetExpr&) -> std::string {
            return "an element projection";
          }},
      data);
}

// One arm of a conditional expression: a run that evaluates what stands under
// one arm evaluates nothing under the other.
struct SelectedArm {
  ExprId conditional;
  bool is_then;

  auto operator==(const SelectedArm&) const -> bool = default;
};

// A place a node is reached at: what reaches it -- a node, or a statement where
// there is none -- and the arms of the conditional expressions it stands under,
// outermost first.
struct Place {
  std::optional<ExprId> from;
  std::vector<SelectedArm> under;
};

// Whether no run evaluates both places: they stand under the same conditionals
// down to one whose arms they sit in differ.
auto NeverBothEvaluated(const Place& a, const Place& b) -> bool {
  const auto [in_a, in_b] = std::ranges::mismatch(a.under, b.under);
  return in_a != a.under.end() && in_b != b.under.end() &&
         in_a->conditional == in_b->conditional;
}

// The places each computing node of one block is reached at, and what the
// block's block expressions name as their value in each block of steps.
struct PlacesReached {
  std::vector<std::vector<Place>> at;
  std::vector<std::vector<ExprId>> entries;
};

// Follows what the node `id` reaches from one more place: once for a node that
// computes, whose place is recorded and whose operands are evaluated when it
// is, and every time for one that computes nothing itself, since each place it
// stands at reaches its operands again.
void Reach(
    const Block& block, PlacesReached& reached, ExprId id,
    std::optional<ExprId> from, std::vector<SelectedArm>& under) {
  const ExprData& data = block.exprs.Get(id).data;
  if (!ComputesNothingItself(data)) {
    std::vector<Place>& places = reached.at.at(id.value);
    places.push_back(Place{.from = from, .under = under});
    if (places.size() > 1) {
      return;
    }
  }
  if (const auto* steps = std::get_if<BlockExpr>(&data)) {
    reached.entries.at(steps->scope.value).push_back(steps->value);
  }
  if (const auto* selection = std::get_if<ConditionalExpr>(&data)) {
    Reach(block, reached, selection->condition, id, under);
    under.push_back(SelectedArm{.conditional = id, .is_then = true});
    Reach(block, reached, selection->then_value, id, under);
    under.back().is_then = false;
    Reach(block, reached, selection->else_value, id, under);
    under.pop_back();
    return;
  }
  ForEachOperand(
      data, [&](ExprId operand) { Reach(block, reached, operand, id, under); });
}

// Two of `places` that one run can both evaluate, where there are two.
auto EvaluatedTogether(std::span<const Place> places)
    -> std::optional<std::pair<const Place*, const Place*>> {
  for (std::size_t i = 0; i < places.size(); ++i) {
    for (std::size_t j = i + 1; j < places.size(); ++j) {
      if (!NeverBothEvaluated(places[i], places[j])) {
        return std::pair{&places[i], &places[j]};
      }
    }
  }
  return std::nullopt;
}

auto DescribePlace(const Block& block, const Place& place) -> std::string {
  return place.from.has_value() ? Describe(block.exprs.Get(*place.from).data)
                                : std::string{"a statement"};
}

// Refuses a body in which one run evaluates a computing node twice. A node is
// evaluated at every place that reaches it, so two places a run can both reach
// are two evaluations of what the lowering meant as one: an operand the source
// wrote once would run twice (LRM 11.4.1). A value wanted at several such
// places is bound to a local and named at each. Two places under different arms
// of one conditional expression are not such a pair, since a run takes one arm.
//
// `entered_at` is what the enclosing block's block expressions name as their
// value among this block's nodes.
void VerifyNoRunEvaluatesTwice(
    const Block& block, std::span<const ExprId> entered_at,
    const auto& describe) {
  PlacesReached reached{
      .at = std::vector<std::vector<Place>>(block.exprs.size()),
      .entries = std::vector<std::vector<ExprId>>(block.child_scopes.size())};
  std::vector<SelectedArm> under;
  const auto reach = [&](ExprId id) {
    Reach(block, reached, id, std::nullopt, under);
  };
  for (const StmtId id : block.root_stmts) {
    ForEachExpr(block.stmts.Get(id).data, reach);
  }
  for (const ExprId id : entered_at) {
    reach(id);
  }
  for (const ExprId id : block.exprs.Ids()) {
    const std::vector<Place>& places = reached.at.at(id.value);
    if (const auto together = EvaluatedTogether(places)) {
      throw InternalError(
          std::format(
              "mir verify: {} reaches {} (expression {} of its block) at {} "
              "places, and one run evaluates it at two of them: under {} and "
              "under {}",
              describe(), Describe(block.exprs.Get(id).data), id.value,
              places.size(), DescribePlace(block, *together->first),
              DescribePlace(block, *together->second)));
    }
  }
  for (const BlockId id : block.child_scopes.Ids()) {
    VerifyNoRunEvaluatesTwice(
        block.child_scopes.Get(id), reached.entries.at(id.value), describe);
  }
}

// A member -- a field, or a member function a call enters -- is reached on the
// object as a place, and whatever reaches an object is dereferenced to it
// first. A receiver that still reaches one would leave every consumer to work
// out from its type that it has to be opened. A field is a member of an object
// alone; a call may also be entered on a value of the library's, such as a
// handle or a reference, whose own member function it names, so only a pointer
// is refused there.
void VerifyMemberReceivers(
    const CompilationUnit& unit, const Block& block, const auto& describe) {
  const auto refuse = [&](ExprId id) {
    throw InternalError(
        std::format(
            "mir verify: {} reaches a member through {} (expression {} of its "
            "block) on what reaches an object rather than on the object, so "
            "the dereference it implies is stated nowhere",
            describe(), Describe(block.exprs.Get(id).data), id.value));
  };
  for (const ExprId id : block.exprs.Ids()) {
    const ExprData& data = block.exprs.Get(id).data;
    if (const auto* access = std::get_if<FieldAccessExpr>(&data)) {
      const Type& receiver =
          unit.types.Get(block.exprs.Get(access->receiver).type);
      if (receiver.Is<PointerType>() || receiver.Is<ManagedRefType>() ||
          receiver.Is<ObjectWriteType>() || receiver.Is<RefType>()) {
        refuse(id);
      }
    }
    if (const auto* call = std::get_if<CallExpr>(&data)) {
      const std::optional<ExprId> receiver = CalleeReceiver(call->callee);
      if (receiver.has_value() &&
          unit.types.Get(block.exprs.Get(*receiver).type).Is<PointerType>()) {
        refuse(id);
      }
    }
  }
  for (const BlockId id : block.child_scopes.Ids()) {
    VerifyMemberReceivers(unit, block.child_scopes.Get(id), describe);
  }
}

// `describe` names the body, and is asked only once there is something to
// report: the check runs over every body of every unit, and composing a name
// for each one costs more than the check itself.
void VerifyCode(
    const CompilationUnit& unit, const CallableCode& code,
    const auto& describe) {
  if (!code.body.has_value()) {
    return;
  }
  VerifyNoRunEvaluatesTwice(*code.body, {}, describe);
  VerifyMemberReceivers(unit, *code.body, describe);
  if (!Suspends(unit, *code.body, describe) ||
      unit.types.Get(code.result_type).Is<CoroutineType>()) {
    return;
  }
  throw InternalError(
      std::format(
          "mir verify: {} suspends, but its result type is not a coroutine, "
          "so nothing could resume it",
          describe()));
}

void VerifyClass(const CompilationUnit& unit, const Class& cls) {
  const auto owner = [&] {
    return cls.path.has_value()
               ? std::format(
                     "class '{}' in unit '{}'", support::DisplayOf(*cls.path),
                     unit.name)
               : std::format("a scope of unit '{}'", unit.name);
  };
  if (cls.constructor.has_value()) {
    VerifyCode(unit, cls.constructor->code, [&] {
      return std::format("the constructor of {}", owner());
    });
  }
  for (const CallableId id : cls.callables.Ids()) {
    VerifyCode(unit, cls.callables.Get(id).code, [&] {
      return std::format(
          "{} of {}", BodyLabel(NameOf(cls.named_callables, id), id.value),
          owner());
    });
  }
}

// A class of this unit has one identity here, its id, however the name
// reaching it was written -- through another instance of this unit, or a
// signature naming this unit. Naming it as another unit's class would give one
// class two identities in one unit, which both backends would then have to
// agree are one.
void VerifyOwnClassesNamedAsOwn(const CompilationUnit& unit) {
  const auto refuse = [&](const support::DefPath& class_path,
                          std::string_view where) {
    throw InternalError(
        std::format(
            "mir verify: unit '{}' names its own class '{}' as another unit's, "
            "in {} -- please report this as a bug",
            unit.name, support::DisplayOf(class_path), where));
  };
  for (const Type& type : unit.types) {
    const auto* object = type.As<ObjectType>();
    if (object == nullptr) continue;
    const auto* other = std::get_if<CrossUnitClassRef>(&object->of);
    if (other != nullptr && other->unit_name == unit.name) {
      refuse(other->class_path, "a type");
    }
  }
  for (const ExternalClass& record : unit.external_classes) {
    if (record.unit_name == unit.name) {
      refuse(record.class_path, "its record of what a unit published");
    }
  }
  for (const ConsumedSignature& consumed : unit.consumed_signatures) {
    const auto* cls = std::get_if<ConsumedClass>(&consumed);
    if (cls != nullptr && cls->unit_name == unit.name) {
      refuse(cls->class_path, "what it consumed of a signature");
    }
  }
}

}  // namespace

auto EvaluatesNothing(const Block& block, ExprId id) -> bool {
  const ExprData& data = block.exprs.Get(id).data;
  if (!ComputesNothingItself(data)) {
    return false;
  }
  bool nothing = true;
  ForEachOperand(data, [&](ExprId operand) {
    nothing = nothing && EvaluatesNothing(block, operand);
  });
  return nothing;
}

void Verify(const CompilationUnit& unit) {
  VerifyOwnClassesNamedAsOwn(unit);
  for (const CallableId id : unit.callables.Ids()) {
    VerifyCode(unit, unit.callables.Get(id).code, [&] {
      return std::format(
          "{} of unit '{}'",
          BodyLabel(NameOf(unit.named_callables, id), id.value), unit.name);
    });
  }
  for (const ForeignScopeEntry& entry : unit.foreign_scope_entries) {
    VerifyCode(unit, entry.definition, [&] {
      return std::format("the foreign entry '{}'", entry.linkage.foreign_name);
    });
  }
  for (const ClosureId id : unit.closures.Ids()) {
    VerifyCode(unit, unit.GetClosure(id).invoke, [&] {
      return std::format("closure {} of unit '{}'", id.value, unit.name);
    });
  }
  for (const StructId id : unit.structs.Ids()) {
    const StructDecl& declaration = unit.GetStruct(id);
    for (const StructMethod& method : declaration.methods) {
      VerifyCode(unit, method.code, [&] {
        return std::format(
            "method '{}' of struct '{}' in unit '{}'",
            support::ValueOperationName(method.answers),
            support::DisplayOf(declaration.path), unit.name);
      });
    }
  }
  for (const ClassId id : unit.classes.Ids()) {
    VerifyClass(unit, unit.GetClass(id));
  }
}

}  // namespace lyra::mir
