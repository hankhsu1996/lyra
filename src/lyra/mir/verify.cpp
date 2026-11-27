#include "lyra/mir/verify.hpp"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <format>
#include <iterator>
#include <limits>
#include <map>
#include <optional>
#include <set>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/block_id.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/caller_location.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/closure_decl.hpp"
#include "lyra/mir/closure_signature.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/constant_id.hpp"
#include "lyra/mir/constant_value.hpp"
#include "lyra/mir/dump.hpp"
#include "lyra/mir/evaluation_order.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/frame_size.hpp"
#include "lyra/mir/integral_constant_folding.hpp"
#include "lyra/mir/keeper.hpp"
#include "lyra/mir/linkage.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/object_lifetime.hpp"
#include "lyra/mir/object_operand.hpp"
#include "lyra/mir/operand_use.hpp"
#include "lyra/mir/record_decl.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/storage_part.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/type_interface.hpp"
#include "lyra/mir/wide_integral_operator.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/callable_bounds.hpp"
#include "lyra/support/def_path.hpp"
#include "lyra/support/float_width.hpp"
#include "lyra/support/integral_operation.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/support/linkage.hpp"
#include "lyra/support/value_operation.hpp"
#include "lyra/value/frame_bound.hpp"

namespace lyra::mir {

namespace {

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
          [&](const SwitchStmt& s) { reach(s.discriminant); },
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

// Calls `reach` with each block `data` runs, the other half of what it
// evaluates.
void ForEachBlockRun(const StmtData& data, const auto& reach) {
  std::visit(
      Overloaded{
          [](const EmptyStmt&) {}, [](const LocalDeclStmt&) {},
          [](const ExprStmt&) {}, [](const RaiseStmt&) {},
          [](const BreakStmt&) {}, [](const ContinueStmt&) {},
          [](const ReturnStmt&) {}, [&](const BlockStmt& s) { reach(s.scope); },
          [&](const TryStmt& s) {
            reach(s.body);
            reach(s.handler);
          },
          [&](const FinallyStmt& s) {
            reach(s.body);
            reach(s.cleanup);
          },
          [&](const IfStmt& s) {
            reach(s.then_scope);
            if (s.else_scope.has_value()) {
              reach(*s.else_scope);
            }
          },
          [&](const SwitchStmt& s) {
            for (const SwitchCase& selected : s.cases) {
              reach(selected.body);
            }
            if (s.default_body.has_value()) {
              reach(*s.default_body);
            }
          },
          [&](const ForStmt& s) { reach(s.scope); },
          [&](const WhileStmt& s) { reach(s.scope); },
          [&](const DoWhileStmt& s) { reach(s.scope); }},
      data);
}

// Whether the node `id` of `block` computes nothing of its own: it names a
// declared thing, spells a constant, forms a place over what it reaches, or
// reads a value where it lies in storage that has a base. What happens where
// such a node stands is that place's, so one of them reached from two places
// and two of them are the same program, and the evaluations it stands for are
// those of what it reaches. A read of storage with no base is an evaluation,
// since a write between two of them makes them two values -- a static variable
// or property is storage a call the body makes can write. So is reaching an
// object through a class handle, which opens the handle and can fail the
// design (LRM 8.4), in the unit as it is built as much as once the conversion
// has made the opening a call.
auto ComputesNothingItself(
    const CompilationUnit& unit, const Block& block, ExprId id) -> bool {
  const TypePool& types = unit.types;
  const Expr& expr = block.exprs.Get(id);
  return std::visit(
      Overloaded{
          [](const StringLiteral&) { return true; },
          [](const NullLiteral&) { return true; },
          [](const MachineBoolLiteral&) { return true; },
          [](const MachineIntLiteral&) { return true; },
          [](const FloatingLiteral&) { return true; },
          [](const ReferenceExpr&) { return true; },
          [&](const DerefExpr&) {
            return !HandleReached(types, block, expr).has_value();
          },
          [](const AddressOfExpr&) { return true; },
          [](const BorrowExpr&) { return true; },
          [&](const CopyExpr& copy) {
            return BaseOfPlace(types, block, copy.place).has_value();
          },
          [](const DiscriminantExpr&) { return true; },
          [](const FieldAccessExpr&) { return true; },
          [](const SliceExpr&) { return true; },
          [](const IndexExpr&) { return true; },
          [](const UnaryExpr&) { return false; },
          [](const BinaryExpr&) { return false; },
          [](const CastExpr&) { return false; },
          [](const ConditionalExpr&) { return false; },
          [](const BlockExpr&) { return false; },
          [](const AssignExpr&) { return false; },
          [](const CallExpr&) { return false; },
          [](const NewExpr&) { return false; },
          [](const MoveExpr&) { return false; },
          [](const ClosureExpr&) { return false; },
          [](const CompositeExpr&) { return false; },
          [](const TaggedUnionExpr&) { return false; },
          [](const AwaitExpr&) { return false; },
          [](const WaitExpr&) { return false; },
          [](const VectorGetExpr&) { return false; }},
      expr.data);
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
          [](const FloatingLiteral&) -> std::string {
            return "a float literal";
          },
          [](const ReferenceExpr&) -> std::string { return "a name"; },
          [](const UnaryExpr&) -> std::string { return "a unary operation"; },
          [](const BinaryExpr&) -> std::string { return "a binary operation"; },
          [](const CastExpr&) -> std::string { return "a cast"; },
          [](const ConditionalExpr&) -> std::string { return "a conditional"; },
          [](const BlockExpr&) -> std::string { return "a block expression"; },
          [](const AssignExpr&) -> std::string { return "an assignment"; },
          [](const CallExpr& call) -> std::string {
            const auto entry = DirectBuiltinFn(call);
            if (entry.has_value()) {
              return std::format(
                  "a call of the library entry '{}'",
                  support::RuntimeEntryOf(*entry).name);
            }
            return std::visit(
                Overloaded{
                    [](const Direct&) -> std::string {
                      return "a call of a body";
                    },
                    [](const Indirect&) -> std::string {
                      return "a call through a code address";
                    },
                    [](const Construct&) -> std::string {
                      return "a construction";
                    },
                    [](const Virtual&) -> std::string {
                      return "a dispatched call";
                    }},
                call.callee);
          },
          [](const NewExpr&) -> std::string { return "a new"; },
          [](const DerefExpr&) -> std::string { return "a dereference"; },
          [](const AddressOfExpr&) -> std::string { return "an address-of"; },
          [](const BorrowExpr&) -> std::string { return "a borrow"; },
          [](const MoveExpr&) -> std::string { return "a move"; },
          [](const CopyExpr&) -> std::string { return "a read"; },
          [](const DiscriminantExpr&) -> std::string {
            return "a read of a tag";
          },
          [](const FieldAccessExpr&) -> std::string {
            return "a field access";
          },
          [](const SliceExpr&) -> std::string { return "bits of a place"; },
          [](const IndexExpr&) -> std::string {
            return "an element of a machine array";
          },
          [](const ClosureExpr&) -> std::string {
            return "a closure construction";
          },
          [](const CompositeExpr&) -> std::string { return "a composite"; },
          [](const TaggedUnionExpr&) -> std::string {
            return "a tagged union built holding a member";
          },
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
    const CompilationUnit& unit, const Block& block, PlacesReached& reached,
    ExprId id, std::optional<ExprId> from, std::vector<SelectedArm>& under) {
  const ExprData& data = block.exprs.Get(id).data;
  if (!ComputesNothingItself(unit, block, id)) {
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
    Reach(unit, block, reached, selection->condition, id, under);
    under.push_back(SelectedArm{.conditional = id, .is_then = true});
    Reach(unit, block, reached, selection->then_value, id, under);
    under.back().is_then = false;
    Reach(unit, block, reached, selection->else_value, id, under);
    under.pop_back();
    return;
  }
  ForEachOperand(data, [&](ExprId operand, OperandUse) {
    Reach(unit, block, reached, operand, id, under);
  });
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
    const CompilationUnit& unit, const Block& block,
    std::span<const ExprId> entered_at, const auto& describe) {
  PlacesReached reached{
      .at = std::vector<std::vector<Place>>(block.exprs.size()),
      .entries = std::vector<std::vector<ExprId>>(block.child_scopes.size())};
  std::vector<SelectedArm> under;
  const auto reach = [&](ExprId id) {
    Reach(unit, block, reached, id, std::nullopt, under);
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
        unit, block.child_scopes.Get(id), reached.entries.at(id.value),
        describe);
  }
}

// A place stands as a value only under a read, and a read reads a place that
// holds a value. Storage with no value to read stands as itself wherever it is
// handed on, and what takes the storage itself is never handed a read of
// storage, which would make it act on a copy. A read of bits is taken as the
// value it makes, since bits lie at no address of their own.
//
// `entered_at` is what the enclosing block's block expressions name as their
// value among this block's nodes.
void VerifyPlacesAreRead(
    const CompilationUnit& unit, const Block& block,
    std::span<const ExprId> entered_at, const auto& describe) {
  // `taker` says what takes the operand, and is asked only once there is
  // something to report, since the check runs over every node of every body.
  const auto as_value = [&](ExprId id, const auto& taker) {
    if (IsReadablePlace(unit.types, block.exprs.Get(id))) {
      throw InternalError(
          std::format(
              "mir verify: in {}, {} takes {} (expression {} of its block) as "
              "a value, and a place stands as one only under a read",
              describe(), taker(), Describe(block.exprs.Get(id).data),
              id.value));
    }
  };
  const auto as_storage = [&](ExprId id, const auto& taker) {
    if (std::holds_alternative<CopyExpr>(block.exprs.Get(id).data) &&
        ValueLiesInStorage(block, id)) {
      throw InternalError(
          std::format(
              "mir verify: in {}, {} takes a read (expression {} of its "
              "block) where it takes the storage itself, so it would act on a "
              "copy",
              describe(), taker(), id.value));
    }
  };
  const auto named = [](std::string_view taker) {
    return [taker] { return std::string(taker); };
  };
  for (const StmtId id : block.root_stmts) {
    ForEachExpr(block.stmts.Get(id).data, [&](ExprId operand) {
      as_value(operand, named("a statement"));
    });
  }
  for (const ExprId id : entered_at) {
    as_value(id, named("a block expression"));
  }
  std::vector<std::vector<ExprId>> entries(block.child_scopes.size());
  for (const ExprId id : block.exprs.Ids()) {
    const ExprData& data = block.exprs.Get(id).data;
    const auto taker = [&data] { return Describe(data); };
    ForEachOperandAsTaken(
        unit.types, block.exprs.Get(id), [&](ExprId operand, OperandUse use) {
          switch (use) {
            case OperandUse::kValue:
              as_value(operand, taker);
              return;
            case OperandUse::kStorage:
              as_storage(operand, taker);
              return;
            case OperandUse::kObject:
              return;
            case OperandUse::kRead:
              if (!IsReadablePlace(unit.types, block.exprs.Get(operand))) {
                throw InternalError(
                    std::format(
                        "mir verify: in {}, {} takes {} (expression {} of its "
                        "block), which names no place or names storage with no "
                        "value to read",
                        describe(), taker(),
                        Describe(block.exprs.Get(operand).data),
                        operand.value));
              }
              return;
            // A dereference opens a pointer it reads, as clang reads the
            // operand of builtin `*p`. One that owns what it reaches is entered
            // as an object, as C++ `*unique_ptr` enters `operator*` on it, and
            // so is a place designated within a write, whose dereference is
            // where the write lands and which its own end then reports.
            case OperandUse::kPointer: {
              const Type& pointer =
                  unit.types.Get(block.exprs.Get(operand).type);
              if (pointer.OwnsPointee() || pointer.Is<DesignationType>()) {
                as_storage(operand, taker);
              } else {
                as_value(operand, taker);
              }
              return;
            }
          }
        });
    if (const auto* scope = std::get_if<BlockExpr>(&data)) {
      entries.at(scope->scope.value).push_back(scope->value);
    }
  }
  for (const BlockId id : block.child_scopes.Ids()) {
    VerifyPlacesAreRead(
        unit, block.child_scopes.Get(id), entries.at(id.value), describe);
  }
}

// A member -- a field, or a member function a call enters -- is reached on the
// object as a place, and whatever reaches an object is dereferenced to it
// first. A receiver that still reaches one would leave every consumer to work
// out from its type that it has to be opened. A field is a member of an object
// alone; a call may also be entered on a value of the library's, such as a
// handle or a reference, whose own member function it names, so only a pointer
// is refused there, unless the body takes it over: a closure's body started as
// a coroutine takes the box the closure was made in, and is handed that.
auto TakesItsReceiverOver(
    const CompilationUnit& unit, const CallExpr& call, TypeId handed) -> bool {
  const auto* direct = std::get_if<Direct>(&call.callee);
  if (direct == nullptr) {
    return false;
  }
  const auto* target = std::get_if<CallableTarget>(&direct->target);
  if (target == nullptr || !std::holds_alternative<DeclaredByClosure>(
                               unit.OwnerOf(target->callable))) {
    return false;
  }
  const CallableCode& invoke = unit.callables.Get(target->callable).code;
  switch (ClosureKindOf(unit.types, invoke)) {
    case support::ClosureKind::kFn:
    case support::ClosureKind::kFnMut:
      return false;
    case support::ClosureKind::kFnOnce:
      return handed == invoke.locals.Get(*invoke.receiver).type;
  }
  throw InternalError("mir verify: unknown ClosureKind");
}

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
          receiver.Is<UnmanagedRefType>() || receiver.Is<ObjectWriteType>() ||
          receiver.IsVariableRef()) {
        refuse(id);
      }
    }
    if (const auto* call = std::get_if<CallExpr>(&data)) {
      const std::optional<ExprId> receiver = CalleeReceiver(call->callee);
      if (receiver.has_value() &&
          unit.types.Get(block.exprs.Get(*receiver).type).Is<PointerType>() &&
          !TakesItsReceiverOver(unit, *call, block.exprs.Get(*receiver).type)) {
        refuse(id);
      }
    }
  }
  for (const BlockId id : block.child_scopes.Ids()) {
    VerifyMemberReceivers(unit, block.child_scopes.Get(id), describe);
  }
}

// Whether `type` refers to storage without keeping it alive.
auto RefersWithoutOwning(const TypePool& types, TypeId type) -> bool {
  const Type& t = types.Get(type);
  if (const auto* pointer = t.As<PointerType>()) {
    return pointer->ownership == PointerOwnership::kBorrowed;
  }
  return t.Is<VariableRefType>() || t.Is<ReferenceWrapperType>() ||
         t.Is<ReferenceType>();
}

// A reference is bound once, as it is created, and never re-seated; a
// reference held as a member is bound after its object is built, and again,
// only by the operations that bind one. So no store names a reference of
// either kind, and a binding names only a reference held as a member. A local
// referring to storage it does not own is bound by its declaration alone, so
// what it refers to is what its initializer reached; the one exception is a
// loop's cursor, which a `for`'s step advances, as an iterator is advanced
// over what it walks.
void VerifyReferenceBindings(
    const CompilationUnit& unit, const Block& block, const auto& describe) {
  const auto refuse = [&](ExprId id, std::string_view what) {
    throw InternalError(
        std::format(
            "mir verify: {} {} (expression {} of its block) -- please report "
            "this as a bug",
            describe(), what, id.value));
  };
  std::vector<ExprId> advances;
  for (const Stmt& stmt : block.stmts) {
    const auto* loop = std::get_if<ForStmt>(&stmt.data);
    if (loop == nullptr) continue;
    advances.insert(advances.end(), loop->step.begin(), loop->step.end());
  }
  for (const ExprId id : block.exprs.Ids()) {
    const ExprData& data = block.exprs.Get(id).data;
    if (const auto* assign = std::get_if<AssignExpr>(&data)) {
      const TypeId target = block.exprs.Get(assign->target).type;
      if (unit.types.Get(target).IsVariableRef()) {
        refuse(id, "stores into a reference, which only a binding names");
      }
      if (ReferencedLocal(block.exprs.Get(assign->target).data).has_value() &&
          RefersWithoutOwning(unit.types, target) &&
          std::ranges::find(advances, id) == advances.end()) {
        refuse(
            id,
            "re-seats a local referring to storage it does not own, which "
            "only its declaration binds");
      }
    }
    const auto* call = std::get_if<CallExpr>(&data);
    if (call == nullptr) continue;
    const std::optional<support::BuiltinFn> entry = DirectBuiltinFn(*call);
    if (entry != support::BuiltinFn::kBindMember &&
        entry != support::BuiltinFn::kBindAlias &&
        entry != support::BuiltinFn::kBindReference) {
      continue;
    }
    if (call->arguments.empty() ||
        !unit.types.Get(block.exprs.Get(call->arguments.front()).type)
             .Is<ReferenceWrapperType>()) {
      refuse(id, "binds something other than a reference held as a member");
    }
  }
  for (const BlockId id : block.child_scopes.Ids()) {
    VerifyReferenceBindings(unit, block.child_scopes.Get(id), describe);
  }
}

// Where `local` is declared within `scope` or a block inside it: the block
// declaring it and what initializes it.
struct LocalDeclaration {
  const Block* block;
  ExprId init;
};

auto DeclarationWithin(const Block& scope, LocalId local)
    -> std::optional<LocalDeclaration> {
  for (const Stmt& stmt : scope.stmts) {
    if (const auto* decl = std::get_if<LocalDeclStmt>(&stmt.data);
        decl != nullptr && decl->target == local) {
      return LocalDeclaration{.block = &scope, .init = decl->init};
    }
  }
  for (const Block& inner : scope.child_scopes) {
    if (auto found = DeclarationWithin(inner, local)) {
      return found;
    }
  }
  return std::nullopt;
}

auto RefersToStorageEndingWith(
    const CompilationUnit& unit, const Block& scope, const Block& in,
    ExprId value) -> bool;

// Whether the storage the place `place` of `in` names ends with the block
// `scope`, `in` being `scope` or a block inside it: a local declared there, a
// part of one, or a part of a value made there and held for the place's sake.
// A dereference of an owner held in a place reaches storage that ends with the
// owner, and one of a pointer value reaches what that pointer refers to; a
// class handle keeps its object by itself, so what it reaches is not followed.
auto PlaceEndsWith(
    const CompilationUnit& unit, const Block& scope, const Block& in,
    ExprId place) -> bool {
  const ExprData& data = in.exprs.Get(place).data;
  if (const std::optional<LocalId> local = ReferencedLocal(data)) {
    return DeclarationWithin(scope, *local).has_value();
  }
  if (const auto* field = std::get_if<FieldAccessExpr>(&data)) {
    return !IsPlace(in.exprs.Get(field->receiver).data) ||
           PlaceEndsWith(unit, scope, in, field->receiver);
  }
  if (const auto* slice = std::get_if<SliceExpr>(&data)) {
    return !IsPlace(in.exprs.Get(slice->receiver).data) ||
           PlaceEndsWith(unit, scope, in, slice->receiver);
  }
  if (const auto* element = std::get_if<IndexExpr>(&data)) {
    return PlaceEndsWith(unit, scope, in, element->receiver);
  }
  if (const auto* deref = std::get_if<DerefExpr>(&data)) {
    return IsPlace(in.exprs.Get(deref->pointer).data)
               ? PlaceEndsWith(unit, scope, in, deref->pointer)
               : RefersToStorageEndingWith(unit, scope, in, deref->pointer);
  }
  return false;
}

// Whether the value `value` of `in` refers to storage that ends with `scope`,
// through a reference or a borrowed pointer it is or carries. A call
// answering one is taken to refer into what it is entered on, or into its
// operands where it is entered on nothing.
auto RefersToStorageEndingWith(
    const CompilationUnit& unit, const Block& scope, const Block& in,
    ExprId value) -> bool {
  const auto refers = [&](ExprId operand) {
    return IsPlace(in.exprs.Get(operand).data)
               ? PlaceEndsWith(unit, scope, in, operand)
               : RefersToStorageEndingWith(unit, scope, in, operand);
  };
  const auto any_refers = [&](const auto& operands, const auto& of) {
    return std::ranges::any_of(
        operands, [&](const auto& operand) { return refers(of(operand)); });
  };
  const auto itself = [](ExprId operand) { return operand; };
  const Expr& expr = in.exprs.Get(value);
  // A reference a local holds, read or moved out of it, is what its
  // declaration bound it to.
  const auto held_reference = [&](ExprId place) {
    if (!RefersWithoutOwning(unit.types, expr.type)) {
      return false;
    }
    const std::optional<LocalId> local =
        ReferencedLocal(in.exprs.Get(place).data);
    if (!local.has_value()) {
      return false;
    }
    const std::optional<LocalDeclaration> declared =
        DeclarationWithin(scope, *local);
    return declared.has_value() &&
           RefersToStorageEndingWith(
               unit, scope, *declared->block, declared->init);
  };
  return std::visit(
      Overloaded{
          [&](const BorrowExpr& borrow) {
            return !IsPlace(in.exprs.Get(borrow.operand).data) ||
                   PlaceEndsWith(unit, scope, in, borrow.operand);
          },
          [&](const AddressOfExpr& address) {
            return PlaceEndsWith(unit, scope, in, address.operand);
          },
          [&](const CopyExpr& read) { return held_reference(read.place); },
          [&](const MoveExpr& move) { return held_reference(move.operand); },
          // A reference into what the call is entered on, where that is a
          // value the block makes, ends with it.
          [&](const CallExpr& call) {
            if (!RefersWithoutOwning(unit.types, expr.type)) {
              return false;
            }
            if (const std::optional<ExprId> receiver =
                    CalleeReceiver(call.callee)) {
              return (AnswersReferenceIntoReceiver(call) &&
                      !IsPlace(in.exprs.Get(*receiver).data)) ||
                     refers(*receiver);
            }
            return any_refers(call.arguments, itself);
          },
          [&](const CastExpr& cast) { return refers(cast.operand); },
          [&](const CompositeExpr& composite) {
            return any_refers(composite.parts, itself);
          },
          [&](const TaggedUnionExpr& u) { return refers(u.value); },
          [&](const ClosureExpr& closure) {
            return any_refers(closure.field_inits, [](const FieldInit& init) {
              return init.value;
            });
          },
          [&](const ConditionalExpr& choice) {
            return refers(choice.then_value) || refers(choice.else_value);
          },
          [&](const BlockExpr& steps) {
            const Block& inner = in.child_scopes.Get(steps.scope);
            return RefersToStorageEndingWith(unit, scope, inner, steps.value);
          },
          // What else a value can be refers to nothing a block makes: a
          // constant, a name of storage, an operator's or a new-expression's
          // result, a suspension's completion.
          [](const StringLiteral&) { return false; },
          [](const NullLiteral&) { return false; },
          [](const MachineBoolLiteral&) { return false; },
          [](const MachineIntLiteral&) { return false; },
          [](const FloatingLiteral&) { return false; },
          [](const ReferenceExpr&) { return false; },
          [](const UnaryExpr&) { return false; },
          [](const BinaryExpr&) { return false; },
          [](const AssignExpr&) { return false; },
          [](const NewExpr&) { return false; },
          [](const DerefExpr&) { return false; },
          [](const DiscriminantExpr&) { return false; },
          [](const FieldAccessExpr&) { return false; },
          [](const SliceExpr&) { return false; },
          [](const IndexExpr&) { return false; },
          [](const AwaitExpr&) { return false; },
          [](const WaitExpr&) { return false; },
          [](const VectorGetExpr&) { return false; },
      },
      expr.data);
}

// A block expression's scope ends when it yields, after what it yields is
// taken out, so what it yields refers to nothing that ends there: no local it
// declares and no value made among its nodes. The check follows the forms a
// reference is built from and refuses those it sees reaching into the block;
// it proves no lifetime, since MIR's references carry none.
void VerifyYieldsOutliveTheirBlocks(
    const CompilationUnit& unit, const Block& block, const auto& describe) {
  for (const ExprId id : block.exprs.Ids()) {
    const auto* steps = std::get_if<BlockExpr>(&block.exprs.Get(id).data);
    if (steps == nullptr) continue;
    const Block& scope = block.child_scopes.Get(steps->scope);
    if (RefersToStorageEndingWith(unit, scope, scope, steps->value)) {
      throw InternalError(
          std::format(
              "mir verify: {} yields, from the block expression at expression "
              "{} of its block, {} of type {} referring to storage that ends "
              "with that block -- please report this as a bug",
              describe(), id.value,
              Describe(scope.exprs.Get(steps->value).data),
              DumpType(unit, scope.exprs.Get(steps->value).type)));
    }
  }
  for (const BlockId id : block.child_scopes.Ids()) {
    VerifyYieldsOutliveTheirBlocks(unit, block.child_scopes.Get(id), describe);
  }
}

// A library entry generic over a type states that type, and no other callee
// states one: a target that resolves types where the call is written names the
// type there, so one left out is a call it cannot write, and one a callee does
// not take is a type argument the library never declared.
// A factory is reached on the type it builds, so the type it is called at is
// the type the call answers with.
void VerifyTypeArguments(
    const CompilationUnit& unit, const Block& block, const auto& describe) {
  const auto refuse = [&](ExprId id, std::string_view what) {
    throw InternalError(
        std::format(
            "mir verify: {} holds {} (expression {} of its block), which {} -- "
            "please report this as a bug",
            describe(), Describe(block.exprs.Get(id).data), id.value, what));
  };
  for (const ExprId id : block.exprs.Ids()) {
    const Expr& expr = block.exprs.Get(id);
    const auto* call = std::get_if<CallExpr>(&expr.data);
    if (call == nullptr) {
      continue;
    }
    const auto* direct = std::get_if<Direct>(&call->callee);
    if (direct == nullptr) {
      continue;
    }
    const auto* entry = std::get_if<support::BuiltinFn>(&direct->target);
    if (entry == nullptr) {
      if (!direct->type_arguments.empty()) {
        refuse(id, "states a type argument to a callee generic over none");
      }
      continue;
    }
    const support::RuntimeEntry declared = support::RuntimeEntryOf(*entry);
    if (direct->type_arguments.size() !=
        (declared.takes_a_type_argument ? 1U : 0U)) {
      refuse(
          id, declared.takes_a_type_argument
                  ? "calls an entry generic over one type and states other "
                    "than one"
                  : "states a type argument to an entry generic over none");
    }
    if (!direct->type_arguments.empty() &&
        std::holds_alternative<support::StaticFactory>(declared.declaration) &&
        direct->type_arguments.front() != expr.type) {
      refuse(
          id,
          "calls a factory at one type and answers with a value of "
          "another");
    }
  }
  for (const BlockId id : block.child_scopes.Ids()) {
    VerifyTypeArguments(unit, block.child_scopes.Get(id), describe);
  }
}

// How a call reaches the operation it carries out: by naming it, handing it
// the receiver its declaration takes; or as a run written on a designation,
// which carries out the array type's slice write on the part the designation
// answers.
enum class CallOf : std::uint8_t { kTheOperation, kItsRunOnADesignation };

// A call of an operation MIR declares hands it what the declaration takes:
// the type arguments it is taken at, which the declaration itself checks; a
// receiver exactly where it takes one; then each argument, of each parameter's
// type in order, where it was called last where the declaration reports there
// and the call already states it. A call handing another type's value to a
// type's operation would run that type's body on it.
void VerifyCallOfDeclaredOperation(
    const CompilationUnit& unit, const Block& block, const CallExpr& call,
    const TypeOperationTarget& target, CallOf call_of, const auto& describe) {
  const std::optional<CallableCode> declared = OperationDeclarationOf(
      unit, TypeOperationSource{
                .type = target.type,
                .answers = target.answers,
                .arguments = target.arguments});
  if (!declared.has_value()) {
    return;
  }
  const auto refuse = [&](std::string_view what) {
    throw InternalError(
        std::format(
            "mir verify: {} {} -- please report this as a bug", describe(),
            what));
  };
  std::vector<TypeId> takes;
  for (const LocalId param : declared->params) {
    if (param != declared->caller_location) {
      takes.push_back(declared->locals.Get(param).type);
    }
  }
  std::vector<TypeId> handed;
  const std::optional<ExprId> receiver = CalleeReceiver(call.callee);
  switch (call_of) {
    case CallOf::kTheOperation:
      if (receiver.has_value() != declared->receiver.has_value()) {
        refuse(
            receiver.has_value()
                ? "with a receiver, though its declaration takes none"
                : "with no receiver, though its declaration takes one");
      }
      if (receiver.has_value()) {
        handed.push_back(block.exprs.Get(*receiver).type);
      }
      break;
    // The designation stands for the array the operation writes, so what the
    // call hands is what the operation takes after it.
    case CallOf::kItsRunOnADesignation:
      if (takes.empty()) {
        refuse("as a run's write, though its declaration takes no array");
      }
      takes.erase(takes.begin());
      break;
  }
  for (const ExprId argument : call.arguments) {
    handed.push_back(block.exprs.Get(argument).type);
  }
  if (declared->caller_location.has_value() &&
      handed.size() == takes.size() + 1 &&
      handed.back() == unit.builtins.source_location) {
    handed.pop_back();
  }
  if (handed != takes) {
    refuse("handing it operands of other types than its declaration takes");
  }
}

// A call naming an operation stated per type names a type that can have it;
// another leaves the call bound to nothing. An operation a run-time-sized
// container's type states as its own is called as that type's, never as the
// library's entry asked of the container: the library defines no such entry
// over a container, so the call would reach nothing on a target that resolves
// it where it is written.
void VerifyTypeOperationCalls(
    const CompilationUnit& unit, const Block& block, const auto& describe) {
  for (const ExprId id : block.exprs.Ids()) {
    const auto* call = std::get_if<CallExpr>(&block.exprs.Get(id).data);
    if (call == nullptr) {
      continue;
    }
    const auto* direct = std::get_if<Direct>(&call->callee);
    if (direct == nullptr) {
      continue;
    }
    if (const auto* fn = std::get_if<support::BuiltinFn>(&direct->target);
        fn != nullptr && direct->receiver.has_value()) {
      const TypeId asked = block.exprs.Get(*direct->receiver).type;
      if (IsLaidOutContainer(unit.types, asked) &&
          OperationNamedByEntry(unit, asked, *fn).has_value()) {
        throw InternalError(
            std::format(
                "mir verify: {} asks the library's \"{}\" of a value of type "
                "{} (expression {} of its block), an operation that type "
                "states as its own -- please report this as a bug",
                describe(), support::ValueOperationName(*fn), asked.value,
                id.value));
      }
    }
    const auto* target = std::get_if<TypeOperationTarget>(&direct->target);
    if (target != nullptr &&
        !TypeOperationKindOf(unit, target->type, target->answers).has_value()) {
      throw InternalError(
          std::format(
              "mir verify: {} calls the operation \"{}\" of type {} "
              "(expression {} of its block), which that type cannot have -- "
              "please report this as a bug",
              describe(), support::ValueOperationName(target->answers),
              target->type.value, id.value));
    }
    if (target != nullptr) {
      VerifyCallOfDeclaredOperation(
          unit, block, *call, *target, CallOf::kTheOperation, [&] {
            return std::format(
                "{} calls the operation \"{}\" of type {} (expression {} of "
                "its block)",
                describe(), support::ValueOperationName(target->answers),
                target->type.value, id.value);
          });
    }
    if (DirectBuiltinFn(*call) == support::BuiltinFn::kAssignDesignatedRun) {
      VerifyCallOfDeclaredOperation(
          unit, block, *call, DesignatedRunWrite(unit, block, *call),
          CallOf::kItsRunOnADesignation, [&] {
            return std::format(
                "{} writes a run on a designation (expression {} of its block)",
                describe(), id.value);
          });
    }
  }
  for (const BlockId id : block.child_scopes.Ids()) {
    VerifyTypeOperationCalls(unit, block.child_scopes.Get(id), describe);
  }
}

// A tag is read of a tagged union, and a union value holds one of the members
// its type declares (LRM 7.3, 7.3.2).
void VerifyUnionValues(
    const CompilationUnit& unit, const Block& block, const auto& describe) {
  for (const ExprId id : block.exprs.Ids()) {
    const Expr& expr = block.exprs.Get(id);
    if (const auto* read = std::get_if<DiscriminantExpr>(&expr.data);
        read != nullptr &&
        !IsTaggedUnion(unit, block.exprs.Get(read->tagged).type)) {
      throw InternalError(
          std::format(
              "mir verify: {} reads a tag of something that is no tagged union "
              "(expression {} of its block) -- please report this as a bug",
              describe(), id.value));
    }
    // A tagged union is built holding a member it has; an untagged union is
    // built as its one storage (LRM 7.3, 7.3.2).
    if (const auto* built = std::get_if<TaggedUnionExpr>(&expr.data)) {
      const std::optional<std::span<const TypeId>> members =
          OverlaidMembers(unit, expr.type);
      if (!IsTaggedUnion(unit, expr.type) ||
          built->member.value >= members->size()) {
        throw InternalError(
            std::format(
                "mir verify: {} builds a union holding a member, of a type "
                "that is no tagged union or has no such member (expression {} "
                "of its block) -- please report this as a bug",
                describe(), id.value));
      }
    }
  }
  for (const BlockId id : block.child_scopes.Ids()) {
    VerifyUnionValues(unit, block.child_scopes.Get(id), describe);
  }
}

// Whether an operand read this way is an ordinal, which the lowering states in
// the position type where the source wrote it.
auto ReadsAnOrdinal(support::OperandReading reading) -> bool {
  switch (reading) {
    case support::OperandReading::kPosition:
      return true;
    case support::OperandReading::kHeld:
    case support::OperandReading::kBits:
    case support::OperandReading::kNumber:
    case support::OperandReading::kTyped:
    case support::OperandReading::kKey:
    case support::OperandReading::kMachine:
      return false;
  }
  throw InternalError("mir verify: unknown operand reading");
}

// Whether an entry reaching a part names where the part starts by an ordinal:
// a slice of elements does (LRM 7.4.6), which is what its first operand is.
auto StartsAtAnOrdinal(std::optional<support::PartSelection> selects) -> bool {
  if (!selects.has_value()) {
    return false;
  }
  switch (*selects) {
    case support::PartSelection::kSlice:
      return true;
    case support::PartSelection::kElement:
    case support::PartSelection::kComponent:
      return false;
  }
  throw InternalError("mir verify: unknown part selection");
}

// An ordinal is written in whatever type the source gave it, and the layer
// that still knows which declaration numbers the parts turns it into a
// position (LRM 7.4.5, 11.5.1). One left in its own type would have every
// consumer convert it, each by a rule of its own.
void VerifyOrdinals(
    const CompilationUnit& unit, const Block& block, const auto& describe) {
  for (const ExprId id : block.exprs.Ids()) {
    const Expr& expr = block.exprs.Get(id);
    // A subscript is a machine integer into a machine array, and names an
    // element at the array's element type.
    if (const auto* element = std::get_if<IndexExpr>(&expr.data)) {
      const auto* array =
          unit.types.Get(block.exprs.Get(element->receiver).type)
              .As<MachineArrayType>();
      if (array == nullptr ||
          !unit.types.Get(block.exprs.Get(element->index).type)
               .Is<MachineIntType>() ||
          array->element != expr.type) {
        throw InternalError(
            std::format(
                "mir verify: {} holds {} (expression {} of its block), which "
                "is no element of a machine array at a machine integer -- "
                "please report this as a bug",
                describe(), Describe(expr.data), id.value));
      }
      continue;
    }
    // Where bits start is a position, as every ordinal is (LRM 11.5.1).
    if (const auto* slice = std::get_if<SliceExpr>(&expr.data)) {
      if (!IsPositionType(unit.types, block.exprs.Get(slice->start).type)) {
        throw InternalError(
            std::format(
                "mir verify: {} holds {} (expression {} of its block), whose "
                "start is an ordinal stated in a type other than the position "
                "type -- please report this as a bug",
                describe(), Describe(expr.data), id.value));
      }
      continue;
    }
    const auto* call = std::get_if<CallExpr>(&expr.data);
    if (call == nullptr) {
      continue;
    }
    const std::optional<support::BuiltinFn> entry = DirectBuiltinFn(*call);
    if (!entry.has_value()) {
      continue;
    }
    const support::RuntimeEntry declared = support::RuntimeEntryOf(*entry);
    const std::size_t leading =
        CalleeReceiver(call->callee).has_value() ? 1 : 0;
    for (std::size_t i = 0; i < call->arguments.size(); ++i) {
      const bool ordinal = ReadsAnOrdinal(declared.operands.At(leading + i)) ||
                           (i == 0 && StartsAtAnOrdinal(declared.selects));
      if (ordinal &&
          !IsPositionType(
              unit.types, block.exprs.Get(call->arguments[i]).type)) {
        throw InternalError(
            std::format(
                "mir verify: {} holds {} (expression {} of its block), whose "
                "operand {} is an ordinal stated in a type other than the "
                "position type -- please report this as a bug",
                describe(), Describe(expr.data), id.value, i));
      }
    }
  }
  for (const BlockId id : block.child_scopes.Ids()) {
    VerifyOrdinals(unit, block.child_scopes.Get(id), describe);
  }
}

// The type of the value an operand names: its own, or the one a reference, a
// pointer or a designation within a write reaches.
auto ValueNamedBy(const TypePool& types, TypeId operand) -> const Type& {
  const Type* reached = &types.Get(operand);
  for (;;) {
    if (reached->IsVariableRef()) {
      reached = &types.Get(reached->WrappedValueType());
    } else if (const auto* pointer = reached->As<PointerType>()) {
      reached = &types.Get(pointer->pointee);
    } else if (const auto* designation = reached->As<DesignationType>()) {
      reached = &types.Get(designation->value);
    } else {
      return *reached;
    }
  }
}

// What an entry asks of the object it acts on by how it reads the operand
// that follows it: one read as a position reaches into something that numbers
// its parts, and one read as a key into an associative array, which is all a
// key reaches (LRM 7.8). Every other reading asks nothing.
enum class ReceiverReach : std::uint8_t { kAny, kNumbered, kKeyed };

auto ReceiverReachOf(support::OperandReading following) -> ReceiverReach {
  switch (following) {
    case support::OperandReading::kPosition:
      return ReceiverReach::kNumbered;
    case support::OperandReading::kKey:
      return ReceiverReach::kKeyed;
    case support::OperandReading::kHeld:
    case support::OperandReading::kBits:
    case support::OperandReading::kNumber:
    case support::OperandReading::kTyped:
    case support::OperandReading::kMachine:
      return ReceiverReach::kAny;
  }
  throw InternalError("mir verify: unknown operand reading");
}

// The first operand a call hands its callee: the object it is entered on, or
// the first argument of a call entered on none.
auto FirstOperand(const CallExpr& call) -> std::optional<ExprId> {
  if (const std::optional<ExprId> receiver = CalleeReceiver(call.callee)) {
    return receiver;
  }
  if (call.arguments.empty()) {
    return std::nullopt;
  }
  return call.arguments.front();
}

// Whether what `pointer` reaches may only be read: a read-only reference or
// pointer -- an element's read form among them, which answers the element
// default where its position names no element (LRM 7.4.5).
auto ReachesReadOnly(const Type& pointer) -> bool {
  const auto read_only = [](Mutability mutability) {
    switch (mutability) {
      case Mutability::kReadOnly:
        return true;
      case Mutability::kMutable:
        return false;
    }
    throw InternalError("mir verify: unknown mutability");
  };
  if (const auto* reference = pointer.As<ReferenceType>()) {
    return read_only(reference->mutability);
  }
  if (const auto* raw = pointer.As<PointerType>()) {
    return read_only(raw->mutability);
  }
  return false;
}

// A call on a library entry is held to the entry's one declaration, so no
// consumer of the call has to ask what it was applied to: an entry the source
// states over values of several kinds is named only where no other is stated
// for its first operand, as the operation over integral values or over queues
// is an entry of its own; an argument the entry reads as a machine value is no
// integral value, which would cross with nothing telling the entry how to read
// its bits; and an entry reaching by a key is called on a container reached by
// keys, one reaching by a position on something that numbers its parts; an
// entry answering a reference is typed as that reference; and no call names an
// allocation function or the reserved placement form, which only a
// new-expression reaches, or a constructor, which only a construction reaches.
void VerifyEntryDeclarations(
    const CompilationUnit& unit, const Block& block, const auto& describe) {
  const auto refuse = [&](ExprId id, std::string_view what) {
    throw InternalError(
        std::format(
            "mir verify: {} holds {} (expression {} of its block), which {} -- "
            "please report this as a bug",
            describe(), Describe(block.exprs.Get(id).data), id.value, what));
  };
  const auto type_of = [&](ExprId id) -> const Type& {
    return unit.types.Get(block.exprs.Get(id).type);
  };
  for (const ExprId id : block.exprs.Ids()) {
    const auto* call = std::get_if<CallExpr>(&block.exprs.Get(id).data);
    if (call == nullptr) {
      continue;
    }
    const std::optional<support::BuiltinFn> entry = DirectBuiltinFn(*call);
    if (!entry.has_value()) {
      continue;
    }
    const support::RuntimeEntry declared = support::RuntimeEntryOf(*entry);
    const std::optional<std::string_view> refusal = std::visit(
        Overloaded{
            [](const support::FreeFunction&)
                -> std::optional<std::string_view> { return std::nullopt; },
            [](const support::Method&) -> std::optional<std::string_view> {
              return std::nullopt;
            },
            [](const support::StaticFactory&)
                -> std::optional<std::string_view> { return std::nullopt; },
            [](const support::AllocationFunction&)
                -> std::optional<std::string_view> {
              return "calls an allocation function, which only a "
                     "new-expression reaches";
            },
            [](const support::ReservedPlacementForm&)
                -> std::optional<std::string_view> {
              return "calls the reserved placement form, which only a "
                     "new-expression reaches";
            },
            [](const support::Constructor&) -> std::optional<std::string_view> {
              return "calls a constructor, which only a construction reaches";
            }},
        declared.declaration);
    if (refusal.has_value()) {
      refuse(id, *refusal);
    }
    const std::optional<ExprId> receiver = CalleeReceiver(call->callee);
    const std::optional<ExprId> first = FirstOperand(*call);
    if (first.has_value() &&
        EntryOver(unit, *entry, block.exprs.Get(*first).type) != *entry) {
      refuse(
          id,
          "names an entry over values of several kinds where another is "
          "stated for its first operand");
    }
    // An entry answering a reference -- into its receiver, or to what the
    // wrapper it is taken on holds -- is typed as that reference, at the
    // mutability the entry answers it with, so its dereference is the part or
    // the contents and the access made there is the entry's.
    const Type& answered = unit.types.Get(block.exprs.Get(id).type);
    const auto typed_as_reference = [&](bool read_only) {
      if (!answered.Is<ReferenceType>() ||
          ReachesReadOnly(answered) != read_only) {
        refuse(id, "answers a reference and is typed as something else");
      }
    };
    switch (declared.answer) {
      case support::EntryAnswer::kReadOnlyReferenceIntoTheReceiver:
      case support::EntryAnswer::kReferenceToContents:
      case support::EntryAnswer::kReferenceToSampledContents:
        typed_as_reference(true);
        break;
      case support::EntryAnswer::kMutableReferenceIntoTheReceiver:
        typed_as_reference(false);
        break;
      case support::EntryAnswer::kNewValue:
        break;
    }
    // An operation over integral values states what it takes in its own
    // declaration, which the folder and each target read.
    if (declared.integral.has_value()) {
      continue;
    }
    const std::size_t leading = receiver.has_value() ? 1 : 0;
    for (std::size_t i = 0; i < call->arguments.size(); ++i) {
      switch (declared.operands.At(leading + i)) {
        case support::OperandReading::kMachine:
          if (type_of(call->arguments[i]).IsIntegral()) {
            refuse(
                id,
                "hands an integral value to an entry that reads that operand "
                "as a machine value");
          }
          break;
        case support::OperandReading::kHeld:
        case support::OperandReading::kBits:
        case support::OperandReading::kNumber:
        case support::OperandReading::kPosition:
        case support::OperandReading::kTyped:
        case support::OperandReading::kKey:
          break;
      }
    }
    if (!receiver.has_value()) {
      continue;
    }
    const bool keyed = ValueNamedBy(unit.types, block.exprs.Get(*receiver).type)
                           .IndexType()
                           .has_value();
    switch (ReceiverReachOf(declared.operands.At(1))) {
      case ReceiverReach::kAny:
        break;
      case ReceiverReach::kNumbered:
        if (keyed) {
          refuse(
              id,
              "reaches by a position into an associative array, which is "
              "reached by a key");
        }
        break;
      case ReceiverReach::kKeyed:
        if (!keyed) {
          refuse(
              id,
              "reaches by a key into something that is no associative array");
        }
        break;
    }
  }
  for (const BlockId id : block.child_scopes.Ids()) {
    VerifyEntryDeclarations(unit, block.child_scopes.Get(id), describe);
  }
}

// An assignment lands on a place. What a write lands on -- an assignment's
// target, bits of one included, or the object an entry changing its receiver
// acts on -- is not reached through a read-only
// reference, which an element's read form answers and whose referent may be
// the element default every read of its type shares (LRM 7.4.5).
void VerifyWritesLandInWritableStorage(
    const CompilationUnit& unit, const Block& block, const auto& describe) {
  for (const ExprId id : block.exprs.Ids()) {
    const Expr& expr = block.exprs.Get(id);
    std::optional<ExprId> reached;
    if (const auto* write = std::get_if<AssignExpr>(&expr.data)) {
      if (!IsPlace(block.exprs.Get(write->target).data)) {
        throw InternalError(
            std::format(
                "mir verify: {} assigns to {} (expression {} of its block), "
                "which names no place -- please report this as a bug",
                describe(), Describe(block.exprs.Get(write->target).data),
                write->target.value));
      }
      reached = write->target;
    } else if (const auto* call = std::get_if<CallExpr>(&expr.data)) {
      const std::optional<support::BuiltinFn> entry = DirectBuiltinFn(*call);
      if (entry.has_value() &&
          support::RuntimeEntryOf(*entry).mutates_receiver) {
        reached = CalleeReceiver(call->callee);
      }
    }
    // A field is written through the object it is reached on, a selected part
    // through the whole it is selected from, and what a reference is
    // dereferenced to through that reference; a write lands through each.
    while (reached.has_value()) {
      const ExprData& data = block.exprs.Get(*reached).data;
      if (const auto* field = std::get_if<FieldAccessExpr>(&data)) {
        reached = field->receiver;
      } else if (const auto* slice = std::get_if<SliceExpr>(&data)) {
        reached = slice->receiver;
      } else if (const auto* element = std::get_if<IndexExpr>(&data)) {
        reached = element->receiver;
      } else if (const auto* opened = std::get_if<DerefExpr>(&data)) {
        const Type& through =
            unit.types.Get(block.exprs.Get(opened->pointer).type);
        if (ReachesReadOnly(through)) {
          throw InternalError(
              std::format(
                  "mir verify: {} writes through a read-only reference "
                  "(expression {} of its block) -- please report this as a bug",
                  describe(), id.value));
        }
        // Storage a capability wrapper stands for is written through a write
        // opened on the wrapper, which is what tells whoever waits on it and
        // keeps what an interval needs of it (LRM 4.3, 9.2.2.2); a
        // write landing in it any other way would do neither.
        if (through.IsCapabilityWrapper()) {
          throw InternalError(
              std::format(
                  "mir verify: {} writes into what a capability wrapper holds "
                  "without opening a write on it (expression {} of its block) "
                  "-- please report this as a bug",
                  describe(), id.value));
        }
        reached = opened->pointer;
      } else if (
          const std::optional<ExprId> whole = SelectedFrom(block, *reached)) {
        reached = whole;
      } else {
        break;
      }
    }
  }
  for (const BlockId id : block.child_scopes.Ids()) {
    VerifyWritesLandInWritableStorage(
        unit, block.child_scopes.Get(id), describe);
  }
}

// Whether a value of `parameter` is passed as one of `other` is: the same
// type, or a handle of one class and of another, which are one object
// whatever class it is seen as, held or borrowed alike.
auto ArgumentAbiCompatible(
    const TypePool& types, TypeId parameter, TypeId other) -> bool {
  if (parameter == other) {
    return true;
  }
  const Type& one = types.Get(parameter);
  const Type& another = types.Get(other);
  if (one.Is<ManagedRefType>() && another.Is<ManagedRefType>()) {
    return true;
  }
  const auto* borrowed = one.As<ReferenceType>();
  const auto* borrowed_other = another.As<ReferenceType>();
  return borrowed != nullptr && borrowed_other != nullptr &&
         borrowed->mutability == borrowed_other->mutability &&
         types.Get(borrowed->referent).Is<ManagedRefType>() &&
         types.Get(borrowed_other->referent).Is<ManagedRefType>();
}

// Whether a code address of prototype `from` may be read as one of `to`:
// either is the erased prototype a table holds every body at, from which an
// address is restored to the prototype it was erased from before any call; or
// the target enters code of the one as it would code of the other, answering
// alike and handed each argument alike.
auto CodeReadsAsAnother(const TypePool& types, TypeId from, TypeId to) -> bool {
  if (IsErasedFunction(types, from) || IsErasedFunction(types, to)) {
    return true;
  }
  const auto& source = types.Get(from).Get<MachineFunctionType>();
  const auto& destination = types.Get(to).Get<MachineFunctionType>();
  if (source.result != destination.result ||
      source.params.size() != destination.params.size()) {
    return false;
  }
  for (std::size_t i = 0; i < source.params.size(); ++i) {
    if (!ArgumentAbiCompatible(
            types, source.params[i], destination.params[i])) {
      return false;
    }
  }
  return true;
}

// Whether `reference` holds the address `pointer` is: a borrowed pointer to
// the same pointee, which may be written through.
auto HoldsAs(const UnmanagedRefType& reference, const PointerType& pointer)
    -> bool {
  return pointer.pointee == reference.pointee &&
         pointer.ownership == PointerOwnership::kBorrowed &&
         pointer.mutability == Mutability::kMutable;
}

// Whether a cast may read a value of `from` as a value of `to`: each pair is
// one every target writes in its own conversion syntax -- C++'s cast notation,
// an LLVM cast instruction -- with the value unchanged or read as the machine
// boolean a condition tests (LRM 12.4). An integral value is read at a type
// laying it out alike, an enumeration's and its base's; a machine integer at
// another width; a pointer at another pointee; a code address as one the
// target enters alike; a class handle as one of another class, the same object
// whatever class it is seen as (LRM 8.15, 8.26.5); a reference owning nothing
// as the borrowed pointer to its pointee it holds and back, as C converts
// between a pointer and an integer of its size -- how a chandle crosses to a
// foreign call (LRM 35.5.6) and how a virtual interface is made of the
// instance it names (LRM 25.9); a real as the machine float of its width and
// back, which is the IEEE value it is (LRM 6.12) -- how a real crosses to a
// foreign call (LRM 35.5.6); a reference at the same referent made read-only,
// as clang states a qualification conversion (LRM 13.5.2); a reference
// held as a member read as the reference it holds, at the same referent and
// never more writable; and a pointer to the library's record of a location as
// the location, the one word holding its address. A value moved between
// representations, and a handle asked whether its object is of a class, is a
// call.
auto CastReadsAsAnother(const TypePool& types, TypeId from, TypeId to) -> bool {
  const Type& source = types.Get(from);
  const Type& destination = types.Get(to);
  if (destination.IsDeclaredBy(support::LibraryDecl::kSourceLocation)) {
    const auto* record = source.As<PointerType>();
    return record != nullptr &&
           types.Get(record->pointee)
               .IsDeclaredBy(support::LibraryDecl::kSourceLocationRecord);
  }
  const auto is_machine_integer = [](const Type& type) {
    return type.Is<MachineIntType>() || type.Is<MachineBoolType>();
  };
  if (source.IsIntegral() && destination.IsIntegral()) {
    return source.Integral() == destination.Integral();
  }
  if (destination.Is<MachineBoolType>()) {
    return is_machine_integer(source) || source.IsIntegral() ||
           source.Is<RealType>() || source.Is<ManagedRefType>() ||
           source.Is<UnmanagedRefType>();
  }
  if (const auto* real = source.As<RealType>()) {
    const auto* machine = destination.As<MachineFloatType>();
    return machine != nullptr && machine->width == real->width;
  }
  if (const auto* real = destination.As<RealType>()) {
    const auto* machine = source.As<MachineFloatType>();
    return machine != nullptr && machine->width == real->width;
  }
  if (const auto* held = source.As<UnmanagedRefType>()) {
    const auto* pointer = destination.As<PointerType>();
    return pointer != nullptr && HoldsAs(*held, *pointer);
  }
  if (const auto* held = destination.As<UnmanagedRefType>()) {
    const auto* pointer = source.As<PointerType>();
    return pointer != nullptr && HoldsAs(*held, *pointer);
  }
  if (is_machine_integer(source) && is_machine_integer(destination)) {
    return true;
  }
  if ((source.Is<PointerType>() && destination.Is<PointerType>()) ||
      (source.Is<ManagedRefType>() && destination.Is<ManagedRefType>())) {
    return true;
  }
  if (source.Is<MachineFunctionType>() &&
      destination.Is<MachineFunctionType>()) {
    return CodeReadsAsAnother(types, from, to);
  }
  const auto* to_ref = destination.As<VariableRefType>();
  if (to_ref == nullptr) return false;
  if (const auto* from_ref = source.As<VariableRefType>()) {
    return from_ref->pointee == to_ref->pointee &&
           to_ref->mutability == Mutability::kReadOnly;
  }
  const auto* from_member = source.As<ReferenceWrapperType>();
  return from_member != nullptr && from_member->pointee == to_ref->pointee &&
         (to_ref->mutability == Mutability::kReadOnly ||
          from_member->mutability == Mutability::kMutable);
}

// The code of `callable` where the unit states what a call meets: a body it
// holds, or the prototype of another unit's construction, which a construction
// of its object enters with the call's arguments as one naming it does. None
// where another unit holds the signature of a body it defines, and none for a
// function the foreign program defines, which a call reaches across the
// boundary its own way (LRM 35.5).
auto CodeHeld(const CompilationUnit& unit, CallableId callable)
    -> const CallableCode* {
  const CallableDecl& decl = unit.callables.Get(callable);
  const auto held_here = [&]() -> const CallableCode* {
    return IsDefinedElsewhere(unit, callable) ? nullptr : &decl.code;
  };
  return std::visit(
      Overloaded{
          [&](const DeclaredByClass&) { return held_here(); },
          [&](const ConstructsClass&) -> const CallableCode* {
            return &decl.code;
          },
          [&](const DeclaredByNamespace&) -> const CallableCode* {
            return std::holds_alternative<DefinedByForeignCode>(FormOf(decl))
                       ? nullptr
                       : held_here();
          },
          [&](const DeclaredByClosure&) -> const CallableCode* {
            return &decl.code;
          },
          [&](const DeclaredByRecord&) { return held_here(); }},
      decl.owner);
}

// The code a direct call enters, where the unit states what the call meets.
// None where another unit or the library holds it.
auto CodeEntered(const CompilationUnit& unit, const Direct& direct)
    -> const CallableCode* {
  return std::visit(
      Overloaded{
          [&](const CallableTarget& t) { return CodeHeld(unit, t.callable); },
          [](support::BuiltinFn) -> const CallableCode* { return nullptr; },
          // Such a body is built once something below this layer finds it
          // is needed, so no unit holds one.
          [](const TypeOperationTarget&) -> const CallableCode* {
            return nullptr;
          },
          [](const ExternalBringUpTarget&) -> const CallableCode* {
            return nullptr;
          }},
      direct.target);
}

auto TypeOf(const Block& in, ExprId id) -> TypeId {
  return in.exprs.Get(id).type;
}

// The code `target` names, where this unit holds it: a body of a class of this
// unit, or the body through which such a class enters one of its methods. None
// for code another unit holds, whose type is what that unit published, and for
// a target that names no code. A method of a class of this unit is entered
// only through an entry the class gives it. Code named as a value is a method
// or a body of a namespace, and an entry is a method's: a construction, a
// closure's body and a record's method are each entered only by what they are
// for, and naming one as a value is refused.
auto CodeNamed(
    const CompilationUnit& unit, const ReferenceTarget& target,
    const auto& describe) -> const CallableCode* {
  const auto none = [](const auto&) -> const CallableCode* { return nullptr; };
  const auto refuse = [&](std::string_view what) -> const CallableCode* {
    throw InternalError(
        std::format(
            "mir verify: {} {} -- please report this as a bug", describe(),
            what));
  };
  return std::visit(
      Overloaded{
          // Code is named as a value where something enters it without a
          // call naming it: a method of a class, or a body of a namespace. A
          // function the foreign program defines is no code of the program's
          // own to name, and no target spells one as a value.
          [&](const FunctionRef& r) -> const CallableCode* {
            const auto not_a_value = [&] {
              return refuse(
                  "names as code a callable that is no method nor namespace "
                  "body");
            };
            return std::visit(
                Overloaded{
                    [&](const DeclaredByClass&) {
                      if (IsDefinedElsewhere(unit, r.callable)) {
                        return refuse(
                            "names as code a method of a class another unit "
                            "defines");
                      }
                      if (std::holds_alternative<LeftAbstract>(
                              FormOf(unit.callables.Get(r.callable)))) {
                        return refuse(
                            "names as code a method its class leaves to what "
                            "extends it");
                      }
                      return CodeHeld(unit, r.callable);
                    },
                    [&](const DeclaredByNamespace&) {
                      if (std::holds_alternative<DefinedByForeignCode>(
                              FormOf(unit.callables.Get(r.callable)))) {
                        return refuse(
                            "names as code a function the foreign program "
                            "defines");
                      }
                      return CodeHeld(unit, r.callable);
                    },
                    [&](const ConstructsClass&) { return not_a_value(); },
                    [&](const DeclaredByClosure&) { return not_a_value(); },
                    [&](const DeclaredByRecord&) { return not_a_value(); }},
                unit.OwnerOf(r.callable));
          },
          [&](const DispatchEntryRef& r) -> const CallableCode* {
            const auto* declared_by =
                std::get_if<DeclaredByClass>(&unit.OwnerOf(r.method));
            if (declared_by == nullptr) {
              return refuse(
                  "names the entry of a callable no class declares as a "
                  "method");
            }
            const Class& owner = unit.GetClass(declared_by->cls);
            if (owner.defining_unit.has_value()) {
              return nullptr;
            }
            const std::optional<CallableId> entry = owner.EntryOf(r.method);
            if (!entry.has_value()) {
              throw InternalError(
                  std::format(
                      "mir verify: {} names the entry of a method its class "
                      "gives none -- please report this as a bug",
                      describe()));
            }
            return &unit.callables.Get(*entry).code;
          },
          [&](const ExternalObjectEntryRef& r) { return none(r); },
          [&](const DestructorRef& r) { return none(r); },
          [&](const DispatchTableRef& r) { return none(r); },
          [&](const InterfaceTableRef& r) { return none(r); },
          [&](const LocalRef& r) { return none(r); },
          [&](const DefinitionRef& r) { return none(r); },
          [&](const ConstantRef& r) { return none(r); },
          [&](const StaticPropertyRef& r) { return none(r); },
          [&](const StaticVariableRef& r) { return none(r); }},
      target);
}

// The type of each position a composite of `type` fills, in order: a
// product's components, an untagged union's one storage, an element list's
// elements, or the members of a structure of the library. Nothing for any
// other type, whose positions no MIR type states.
auto PositionsOf(const CompilationUnit& unit, TypeId type)
    -> std::optional<std::vector<TypeId>> {
  if (const auto components = ProductElements(unit, type)) {
    return std::vector<TypeId>(components->begin(), components->end());
  }
  if (const std::optional<TypeId> storage = UnionStorageOf(unit, type)) {
    return std::vector<TypeId>{*storage};
  }
  const Type& composed = unit.types.Get(type);
  if (const auto* elements = composed.As<MachineArrayType>()) {
    return std::vector<TypeId>(elements->size, elements->element);
  }
  if (const auto* record = composed.As<LibraryType>()) {
    return unit.LibraryRecordMembers(record->decl)
        .transform([](std::span<const TypeId> members) {
          return std::vector<TypeId>(members.begin(), members.end());
        });
  }
  return std::nullopt;
}

[[noreturn]] void RefuseValueOfType(
    const CompilationUnit& unit, std::string_view what, TypeId expected,
    TypeId handed, const auto& describe) {
  throw InternalError(
      std::format(
          "mir verify: {} hands {} a value of type {} where it states type {} "
          "-- please report this as a bug",
          describe(), what, DumpType(unit, handed), DumpType(unit, expected)));
}

// Every value an expression of `block` hands to something of a type it states
// is a value of that type: what an assignment writes and its target, what a
// whole write through a wrapper writes and what the wrapper holds, an arm of a
// choice, the value a block ends
// with, code named as a value, a constant named at its own type, what a
// capture is built from, a part of a
// composite, a view and the array it is built from, an argument of a
// callable this unit holds, and the call of one, which answers what the
// callable returns. Each block is asked on its own.
void VerifyExprsMeetTheirTypes(
    const CompilationUnit& unit, Phase phase, const Block& block,
    const auto& describe) {
  const auto refuse = [&](std::string_view what, TypeId expected,
                          TypeId handed) {
    RefuseValueOfType(unit, what, expected, handed, describe);
  };
  for (const ExprId id : block.exprs.Ids()) {
    const Expr& expr = block.exprs.Get(id);
    const auto* call = std::get_if<CallExpr>(&expr.data);
    if (const auto* write = std::get_if<AssignExpr>(&expr.data);
        write != nullptr &&
        TypeOf(block, write->target) != TypeOf(block, write->value)) {
      refuse(
          "an assignment's target", TypeOf(block, write->target),
          TypeOf(block, write->value));
    }
    if (call != nullptr && DirectBuiltinFn(*call) == support::BuiltinFn::kSet) {
      const TypeId held =
          unit.types.Get(TypeOf(block, *CalleeReceiver(call->callee)))
              .WrappedValueType();
      if (TypeOf(block, call->arguments.at(0)) != held) {
        refuse(
            "a whole write's storage", held,
            TypeOf(block, call->arguments.at(0)));
      }
    }
    if (const auto* reference = std::get_if<ReferenceExpr>(&expr.data)) {
      if (const auto* named = std::get_if<ConstantRef>(&reference->target)) {
        const TypeId held = unit.constants.Get(named->constant).type;
        if (held != expr.type) {
          refuse("a constant it names", held, expr.type);
        }
      }
    }
    if (const auto* cast = std::get_if<CastExpr>(&expr.data);
        cast != nullptr &&
        !CastReadsAsAnother(
            unit.types, TypeOf(block, cast->operand), expr.type)) {
      throw InternalError(
          std::format(
              "mir verify: {} casts a value of type {} to type {}, which a "
              "cast does not join -- please report this as a bug",
              describe(), DumpType(unit, TypeOf(block, cast->operand)),
              DumpType(unit, expr.type)));
    }
    // An address moved by a count stays an address of what it pointed at, and
    // the count is a machine integer ([expr.add]).
    if (const auto* moved = std::get_if<BinaryExpr>(&expr.data);
        moved != nullptr &&
        unit.types.Get(TypeOf(block, moved->lhs)).Is<PointerType>() &&
        MovesAnAddress(moved->op)) {
      if (!unit.types.Get(TypeOf(block, moved->rhs)).Is<MachineIntType>()) {
        throw InternalError(
            std::format(
                "mir verify: {} moves an address by a value of type {}, which "
                "is no machine integer -- please report this as a bug",
                describe(), DumpType(unit, TypeOf(block, moved->rhs))));
      }
      if (expr.type != TypeOf(block, moved->lhs)) {
        refuse("a moved address", TypeOf(block, moved->lhs), expr.type);
      }
    }
    // A value taken out of storage is of the type the storage held.
    if (call != nullptr &&
        DirectBuiltinFn(*call) == support::BuiltinFn::kPtrRead) {
      const auto* storage =
          call->arguments.size() == 1
              ? unit.types.Get(TypeOf(block, call->arguments.front()))
                    .As<PointerType>()
              : nullptr;
      if (storage == nullptr ||
          storage->ownership != PointerOwnership::kBorrowed) {
        throw InternalError(
            std::format(
                "mir verify: {} takes a value out of other than one address "
                "owning nothing -- please report this as a bug",
                describe()));
      }
      if (storage->pointee != expr.type) {
        refuse("a value taken out of storage", storage->pointee, expr.type);
      }
    }
    if (const auto* choice = std::get_if<ConditionalExpr>(&expr.data)) {
      for (const ExprId arm : {choice->then_value, choice->else_value}) {
        if (TypeOf(block, arm) != expr.type) {
          refuse("an arm of a choice", expr.type, TypeOf(block, arm));
        }
      }
    }
    if (const auto* steps = std::get_if<BlockExpr>(&expr.data)) {
      const Block& inner = block.child_scopes.Get(steps->scope);
      if (TypeOf(inner, steps->value) != expr.type) {
        refuse(
            "the value a block ends with", expr.type,
            TypeOf(inner, steps->value));
      }
    }
    if (const auto* composite = std::get_if<CompositeExpr>(&expr.data)) {
      const std::optional<std::vector<TypeId>> held =
          PositionsOf(unit, expr.type);
      if (!held.has_value()) {
        throw InternalError(
            std::format(
                "mir verify: {} builds a composite of a type with no "
                "positions, {} -- please report this as a bug",
                describe(), DumpType(unit, expr.type)));
      }
      const std::vector<TypeId>& positions = *held;
      if (positions.size() != composite->parts.size()) {
        throw InternalError(
            std::format(
                "mir verify: {} builds a composite of type {} from {} parts "
                "for {} positions -- please report this as a bug",
                describe(), DumpType(unit, expr.type), composite->parts.size(),
                positions.size()));
      }
      for (std::size_t i = 0; i < positions.size(); ++i) {
        if (TypeOf(block, composite->parts[i]) != positions[i]) {
          refuse(
              "a part of a composite", positions[i],
              TypeOf(block, composite->parts[i]));
        }
      }
    }
    // Code named as a value is of the type the code has, as a C function's
    // name is; reading it at another prototype is a cast.
    if (const auto* named = std::get_if<ReferenceExpr>(&expr.data)) {
      if (const CallableCode* code = CodeNamed(unit, named->target, describe);
          code != nullptr && expr.type != FunctionTypeOf(unit.types, *code)) {
        throw InternalError(
            std::format(
                "mir verify: {} names code of type {} at type {} -- please "
                "report this as a bug",
                describe(), DumpType(unit, FunctionTypeOf(unit.types, *code)),
                DumpType(unit, expr.type)));
      }
    }
    if (const auto* closure = std::get_if<ClosureExpr>(&expr.data)) {
      const ClosureDecl& decl = unit.GetClosure(closure->closure);
      for (const FieldInit& init : closure->field_inits) {
        const TypeId field = decl.fields.Get(init.target).type;
        if (TypeOf(block, init.value) != field) {
          refuse("a capture", field, TypeOf(block, init.value));
        }
      }
    }
    if (call == nullptr) {
      continue;
    }
    // A view is built from a borrowed, read-only address of the array it
    // views, whose entries are of the type the view's are.
    if (const auto* span = unit.types.Get(expr.type).As<MachineSpanType>();
        span != nullptr && std::holds_alternative<Construct>(call->callee)) {
      const auto* pointer =
          call->arguments.size() == 1
              ? unit.types.Get(TypeOf(block, call->arguments.front()))
                    .As<PointerType>()
              : nullptr;
      const auto* viewed =
          pointer != nullptr &&
                  pointer->ownership == PointerOwnership::kBorrowed &&
                  pointer->mutability == Mutability::kReadOnly
              ? unit.types.Get(pointer->pointee).As<MachineArrayType>()
              : nullptr;
      if (viewed == nullptr || viewed->element != span->element) {
        throw InternalError(
            std::format(
                "mir verify: {} builds a view of type {} from other than the "
                "read-only address of an array of its entries -- please report "
                "this as a bug",
                describe(), DumpType(unit, expr.type)));
      }
      continue;
    }
    const CallableCode* callee = nullptr;
    // A direct call answers what the code it enters returns; a construction
    // enters its object's constructor and answers the object.
    if (const auto* direct = std::get_if<Direct>(&call->callee)) {
      // A behavior its class leaves to what extends it has no body anywhere
      // (LRM 8.21), so only a dispatching call reaches it.
      if (const auto* target = std::get_if<CallableTarget>(&direct->target);
          target != nullptr && std::holds_alternative<LeftAbstract>(FormOf(
                                   unit.callables.Get(target->callable)))) {
        throw InternalError(
            std::format(
                "mir verify: {} calls directly a method its class leaves to "
                "what extends it -- please report this as a bug",
                describe()));
      }
      callee = CodeEntered(unit, *direct);
      if (callee != nullptr && expr.type != callee->result_type) {
        throw InternalError(
            std::format(
                "mir verify: {} calls a callable that answers type {} as "
                "though it answered type {} -- please report this as a bug",
                describe(), DumpType(unit, callee->result_type),
                DumpType(unit, expr.type)));
      }
    } else if (
        std::holds_alternative<Construct>(call->callee) &&
        unit.types.Get(expr.type).Is<ObjectType>()) {
      const std::optional<CallableId>& constructor =
          unit.CallablesOf(unit.types.Get(expr.type).Get<ObjectType>().of)
              .constructor;
      if (constructor.has_value()) {
        callee = CodeHeld(unit, *constructor);
      }
    }
    // What the call's arguments meet: the parameters of a body this unit
    // holds, or, for code reached through an address, what its type states.
    // Where the callee reports at its caller, the location it is handed is
    // stated by the conversion into the legalized phase, so a unit as built
    // hands every argument but that one.
    std::vector<TypeId> parameters;
    if (callee != nullptr) {
      for (const LocalId param : callee->params) {
        const bool stated_later =
            phase == Phase::kBuilt && param == callee->caller_location;
        if (param != callee->receiver && !stated_later) {
          parameters.push_back(callee->locals.Get(param).type);
        }
      }
    } else if (const auto* indirect = std::get_if<Indirect>(&call->callee)) {
      const Type& code_type = unit.types.Get(TypeOf(block, indirect->code));
      if (!code_type.Is<MachineFunctionType>()) {
        continue;
      }
      parameters = code_type.Get<MachineFunctionType>().params;
    } else {
      continue;
    }
    if (parameters.size() != call->arguments.size()) {
      throw InternalError(
          std::format(
              "mir verify: {} calls a callable with {} arguments where it "
              "takes {} ({}, expression {} of its block) -- please report "
              "this as a bug",
              describe(), call->arguments.size(), parameters.size(),
              Describe(expr.data), id.value));
    }
    for (std::size_t i = 0; i < parameters.size(); ++i) {
      if (TypeOf(block, call->arguments[i]) != parameters[i]) {
        refuse(
            "an argument of a callable", parameters[i],
            TypeOf(block, call->arguments[i]));
      }
    }
  }
}

// Every value handed to something of a type it states is a value of that
// type, as an assignment-like context converts its value to its target's type
// (LRM 10.8): what each expression hands over, a local's initializer, and what
// a body answers. A value only alike in representation is converted where it
// is built, so one handed over as it is is a value the program never stated.
void VerifyValuesMeetTheirTypes(
    const CompilationUnit& unit, Phase phase, const CallableCode& code,
    const Block& block, const auto& describe) {
  const Type& result = unit.types.Get(code.result_type);
  const auto* coroutine = result.As<CoroutineType>();
  const TypeId answered =
      coroutine != nullptr ? coroutine->payload : code.result_type;
  for (const Stmt& stmt : block.stmts) {
    if (const auto* decl = std::get_if<LocalDeclStmt>(&stmt.data)) {
      const TypeId declared = code.locals.Get(decl->target).type;
      if (TypeOf(block, decl->init) != declared) {
        RefuseValueOfType(
            unit, "a local's initializer", declared, TypeOf(block, decl->init),
            describe);
      }
    }
    if (const auto* returned = std::get_if<ReturnStmt>(&stmt.data)) {
      // A body stating a result answers one on every way out of it, so a
      // return handing back nothing belongs only to a body stating none.
      if (!returned->value.has_value() && answered != unit.builtins.void_type) {
        throw InternalError(
            std::format(
                "mir verify: {} returns nothing where it states type {} -- "
                "please report this as a bug",
                describe(), DumpType(unit, answered)));
      }
      if (returned->value.has_value() &&
          TypeOf(block, *returned->value) != answered) {
        RefuseValueOfType(
            unit, "what it answers", answered, TypeOf(block, *returned->value),
            describe);
      }
    }
  }
  // A local is named at the type it is declared, as a C name is; viewing it
  // otherwise is a conversion of what is read from it.
  for (const ExprId id : block.exprs.Ids()) {
    const Expr& expr = block.exprs.Get(id);
    const auto* named = std::get_if<ReferenceExpr>(&expr.data);
    const auto* local =
        named == nullptr ? nullptr : std::get_if<LocalRef>(&named->target);
    if (local != nullptr && code.locals.Get(local->var).type != expr.type) {
      RefuseValueOfType(
          unit, "a local it names", code.locals.Get(local->var).type, expr.type,
          describe);
    }
  }
  VerifyExprsMeetTheirTypes(unit, phase, block, describe);
  for (const BlockId id : block.child_scopes.Ids()) {
    VerifyValuesMeetTheirTypes(
        unit, phase, code, block.child_scopes.Get(id), describe);
  }
}

// `value` is a value of `type`: an integral's bits canonical for it, a machine
// integer or boolean as a number, a null where a pointer or code is held, a
// text as a C string, a pointer whose own type is `type` or one a cast reads
// as it, naming what it states, a structure of the library with a part of each
// member's type, an array as long as its type with elements of its element
// type, and a view of an array of the unit holding its element type.
void VerifyConstantMeetsItsType(
    const CompilationUnit& unit, TypeId type, const ConstantValue& value,
    const auto& describe) {
  const Type& held = unit.types.Get(type);
  const auto refuse = [&](std::string_view stated) {
    throw InternalError(
        std::format(
            "mir verify: {} states {} as a value of type {} -- please report "
            "this as a bug",
            describe(), stated, DumpType(unit, type)));
  };
  std::visit(
      Overloaded{
          [&](const IntegralValue& v) {
            if (!held.IsIntegral() ||
                v.planes !=
                    CanonicalIntegralConstant(held.Integral(), v.planes)) {
              refuse("bits not canonical for it");
            }
          },
          [&](const MachineIntValue&) {
            if (!held.Is<MachineIntType>() && !held.Is<MachineBoolType>()) {
              refuse("a machine integer");
            }
          },
          [&](const NullValue&) {
            if (!held.Is<PointerType>() && !held.Is<MachineFunctionType>()) {
              refuse("a null");
            }
          },
          [&](const CStringValue&) {
            if (!held.Is<MachineCStringType>()) {
              refuse("a text");
            }
          },
          [&](const PointerValue& v) {
            if (std::holds_alternative<LocalRef>(v.to)) {
              refuse("the address of a local");
            }
            if (const auto* named = std::get_if<ConstantRef>(&v.to);
                named != nullptr &&
                unit.constants.Get(named->constant).type != v.type) {
              refuse("the address of a constant at another type than its own");
            }
            if (const CallableCode* code = CodeNamed(unit, v.to, describe);
                code != nullptr &&
                FunctionTypeOf(unit.types, *code) != v.type) {
              refuse("code at another type than its own");
            }
            const TypeId own = OwnPointerType(unit.types, v);
            if (own != type && !CastReadsAsAnother(unit.types, own, type)) {
              refuse(std::format("a pointer of type {}", DumpType(unit, own)));
            }
          },
          [&](const RecordValue& v) {
            const auto* library = held.As<LibraryType>();
            const std::optional<std::span<const TypeId>> members =
                library == nullptr ? std::nullopt
                                   : unit.LibraryRecordMembers(library->decl);
            if (!members.has_value() || members->size() != v.parts.size()) {
              refuse(std::format("a structure of {} parts", v.parts.size()));
            }
            for (std::size_t i = 0; i < v.parts.size(); ++i) {
              VerifyConstantMeetsItsType(
                  unit, (*members)[i], v.parts[i], describe);
            }
          },
          [&](const ArrayValue& v) {
            const auto* array = held.As<MachineArrayType>();
            if (array == nullptr || array->size != v.elements.size()) {
              refuse(std::format("an array of {} elements", v.elements.size()));
            }
            for (const ConstantValue& element : v.elements) {
              VerifyConstantMeetsItsType(
                  unit, array->element, element, describe);
            }
          },
          [&](const SpanValue& v) {
            const auto* view = held.As<MachineSpanType>();
            const auto* array = unit.types.Get(unit.constants.Get(v.array).type)
                                    .As<MachineArrayType>();
            if (view == nullptr || array == nullptr ||
                array->element != view->element) {
              refuse("a view of a constant that is no array of its element");
            }
          }},
      value.value);
}

// Every constant of the unit is a value of its type, and the definition of
// every class it defines is the library's structure for one.
void VerifyConstantsMeetTheirTypes(
    const CompilationUnit& unit, std::string_view when) {
  for (const ConstantId id : unit.constants.Ids()) {
    const ConstantDecl& constant = unit.constants.Get(id);
    VerifyConstantMeetsItsType(unit, constant.type, constant.value, [&] {
      return std::format("constant {} of the unit, {}", id.value, when);
    });
  }
  const TypeId definition =
      LibraryTypeOf(unit.types, support::LibraryDecl::kObjectDefinition);
  for (const ClassId id : unit.classes.Ids()) {
    const std::optional<ConstantValue>& stated = unit.GetClass(id).definition;
    if (stated.has_value()) {
      VerifyConstantMeetsItsType(unit, definition, *stated, [&] {
        return std::format(
            "the definition of {}, {}", DescribeClass(unit, id), when);
      });
    }
  }
}

// Storage with no address of its own -- a member of a tagged union, and what
// lies under one -- is never what a reference the program holds ends on (LRM
// 13.5.2), so nothing takes its address; and bits, which lie at no address of
// their own, are bound by what a read of them makes, never where they lie.
void VerifyStorageWithoutAnAddress(
    const CompilationUnit& unit, const Block& block, const auto& describe) {
  for (const ExprId id : block.exprs.Ids()) {
    if (const auto* bound = std::get_if<BorrowExpr>(&block.exprs.Get(id).data);
        bound != nullptr && std::holds_alternative<SliceExpr>(
                                block.exprs.Get(bound->operand).data)) {
      throw InternalError(
          std::format(
              "mir verify: {} binds a reference where bits lie (expression {} "
              "of its block), and bits lie at no address of their own -- "
              "please report this as a bug",
              describe(), id.value));
    }
    const auto* address = std::get_if<AddressOfExpr>(&block.exprs.Get(id).data);
    if (address != nullptr && IsPlace(block.exprs.Get(address->operand).data) &&
        KindOfPlace(unit, block, address->operand) != PlaceKind::kOrdinary) {
      throw InternalError(
          std::format(
              "mir verify: {} takes the address of storage with none of its "
              "own (expression {} of its block) -- please report this as a "
              "bug",
              describe(), id.value));
    }
  }
  for (const BlockId id : block.child_scopes.Ids()) {
    VerifyStorageWithoutAnAddress(unit, block.child_scopes.Get(id), describe);
  }
}

// An operation over integral values whose every operand is fixed before the
// program runs has one value (LRM 11.2.1), which whatever builds the operation
// states as that constant. One left standing would be evaluated by each target
// at each run, and a target that evaluated it early would do so by a rule of
// its own. The operation is asked what it evaluates to, by the same question
// its builder asks, so what counts as fixed is decided in one place.
void VerifyConstantsAreFolded(
    const CompilationUnit& unit, const Block& block, const auto& describe) {
  const auto is_integral = [&](ExprId id) {
    return unit.types.Get(block.exprs.Get(id).type).IsIntegral();
  };
  for (const ExprId id : block.exprs.Ids()) {
    const Expr& expr = block.exprs.Get(id);
    const auto hold_folded = [&](support::IntegralOp op,
                                 std::span<const ExprId> operands) {
      if (FoldIntegral(unit, block, op, operands, expr.type).has_value()) {
        throw InternalError(
            std::format(
                "mir verify: {} holds {} (expression {} of its block), an "
                "operation over constants that is stated as an operation and "
                "not as the constant it evaluates to -- please report this as "
                "a bug",
                describe(), Describe(expr.data), id.value));
      }
    };
    std::visit(
        Overloaded{
            [&](const UnaryExpr& e) {
              if (is_integral(e.operand)) {
                hold_folded(IntegralOpOf(e.op), std::array{e.operand});
              }
            },
            [&](const BinaryExpr& e) {
              if (is_integral(e.lhs)) {
                hold_folded(IntegralOpOf(e.op), std::array{e.lhs, e.rhs});
              }
            },
            // Between two integral types a cast holds the same bits to another
            // type (LRM 6.19.3, 6.24.1), which is a conversion that moves none.
            [&](const CastExpr& e) {
              if (is_integral(e.operand) && is_integral(id)) {
                hold_folded(
                    support::IntegralOp::kConvert, std::array{e.operand});
              }
            },
            [&](const CallExpr& call) {
              const std::optional<support::BuiltinFn> entry =
                  DirectBuiltinFn(call);
              if (!entry.has_value()) {
                return;
              }
              const std::optional<support::IntegralOp> op =
                  support::RuntimeEntryOf(*entry).integral;
              if (!op.has_value()) {
                return;
              }
              std::vector<ExprId> operands;
              if (const std::optional<ExprId> receiver =
                      CalleeReceiver(call.callee)) {
                operands.push_back(*receiver);
              }
              operands.insert(
                  operands.end(), call.arguments.begin(), call.arguments.end());
              hold_folded(*op, operands);
            },
            [](const StringLiteral&) {}, [](const NullLiteral&) {},
            [](const MachineBoolLiteral&) {}, [](const MachineIntLiteral&) {},
            [](const FloatingLiteral&) {}, [](const ReferenceExpr&) {},
            [](const ConditionalExpr&) {}, [](const BlockExpr&) {},
            [](const AssignExpr&) {}, [](const NewExpr&) {},
            [](const DerefExpr&) {}, [](const AddressOfExpr&) {},
            [](const BorrowExpr&) {}, [](const MoveExpr&) {},
            [](const CopyExpr&) {}, [](const DiscriminantExpr&) {},
            [](const FieldAccessExpr&) {}, [](const SliceExpr&) {},
            [](const IndexExpr&) {}, [](const ClosureExpr&) {},
            [](const CompositeExpr&) {}, [](const TaggedUnionExpr&) {},
            [](const AwaitExpr&) {}, [](const WaitExpr&) {},
            [](const VectorGetExpr&) {}},
        expr.data);
  }
  for (const BlockId id : block.child_scopes.Ids()) {
    VerifyConstantsAreFolded(unit, block.child_scopes.Get(id), describe);
  }
}

// Storage a uniquely owning pointer or a class handle owns comes into existence
// only by a new-expression, whose initializer has the type allocated -- a value
// evaluated into that storage, or an object's construction, entered on the
// object once it is created whole -- or, for an object, none, where the code
// then enters its constructor by name. A placement new-expression's storage is
// its allocation function's instead, found by the types of its arguments.
void VerifyAllocations(
    const CompilationUnit& unit, const Block& block, const auto& describe) {
  const auto refuse = [&](ExprId id, std::string_view what) {
    throw InternalError(
        std::format(
            "mir verify: {} holds {} (expression {} of its block), which {} -- "
            "please report this as a bug",
            describe(), Describe(block.exprs.Get(id).data), id.value, what));
  };
  for (const ExprId id : block.exprs.Ids()) {
    const Expr& expr = block.exprs.Get(id);
    const std::optional<TypeId> allocated =
        AllocatedType(unit.types, expr.type);
    if (const auto* call = std::get_if<CallExpr>(&expr.data);
        call != nullptr && std::holds_alternative<Construct>(call->callee) &&
        allocated.has_value()) {
      refuse(id, "constructs what only a new-expression allocates");
    }
    const auto* allocation = std::get_if<NewExpr>(&expr.data);
    if (allocation == nullptr) {
      continue;
    }
    // Storage a placement's allocation function hands out is that function's,
    // so the expression answers a pointer owning nothing, to a value of the
    // initializer's type.
    if (allocation->placement.has_value()) {
      if (!allocation->initializer.has_value()) {
        refuse(id, "places nothing in what its allocation function hands out");
        continue;
      }
      const auto* place = unit.types.Get(expr.type).As<PointerType>();
      if (place == nullptr || place->ownership != PointerOwnership::kBorrowed ||
          block.exprs.Get(*allocation->initializer).type != place->pointee) {
        refuse(
            id,
            "places a value but answers other than a pointer owning nothing "
            "to it");
      }
      // An allocation function is found by the types of the placement
      // arguments, as C++ finds `operator new` by them ([expr.new]), so the one
      // named is the one they find: the allocation of the keeper the one
      // argument points to, the storage for an element of the container the
      // first argument refers to, which that container's type names, or the
      // storage the one argument points at itself.
      const std::vector<ExprId>& arguments = allocation->placement->arguments;
      const auto argument_type = [&](std::size_t i) -> const Type& {
        return unit.types.Get(block.exprs.Get(arguments[i]).type);
      };
      const auto* keeper = arguments.size() != 1
                               ? nullptr
                               : argument_type(0).template As<PointerType>();
      const std::optional<support::KeeperEntries> entries =
          keeper == nullptr ? std::nullopt
                            : KeeperEntriesOf(unit.types, keeper->pointee);
      const auto* container =
          arguments.empty() ? nullptr
                            : argument_type(0).template As<ReferenceType>();
      const std::optional<ElementAllocation> element =
          container == nullptr ? std::nullopt
                               : ElementAllocationOf(unit, container->referent);
      const bool builds_an_element =
          element.has_value() &&
          element->allocate == allocation->placement->allocate &&
          element->arguments == arguments.size();
      // The reserved placement form answers the storage its one argument
      // points at, which is of the type placed there.
      const bool places_where_it_points =
          support::PlacesAtItsArgument(
              support::RuntimeEntryOf(allocation->placement->allocate)
                  .declaration) &&
          keeper != nullptr && keeper->pointee == place->pointee;
      if (places_where_it_points) {
        continue;
      }
      if (!builds_an_element &&
          (!entries.has_value() ||
           entries->allocate != allocation->placement->allocate)) {
        refuse(
            id,
            "names an allocation function other than the one its argument "
            "allocates by");
      }
      continue;
    }
    if (!allocated.has_value()) {
      refuse(id, "yields a type that owns no storage it allocates");
    }
    if (!allocation->initializer.has_value()) {
      if (!unit.types.Get(*allocated).Is<ObjectType>()) {
        refuse(id, "leaves what it allocates without a value");
      }
      continue;
    }
    if (block.exprs.Get(*allocation->initializer).type != *allocated) {
      refuse(id, "initializes its storage from a value of another type");
    }
    if (unit.types.Get(*allocated).Is<ObjectType>()) {
      const auto* construction = std::get_if<CallExpr>(
          &block.exprs.Get(*allocation->initializer).data);
      if (construction == nullptr ||
          !std::holds_alternative<Construct>(construction->callee)) {
        refuse(id, "creates an object it does not construct");
      }
    }
  }
  for (const BlockId id : block.child_scopes.Ids()) {
    VerifyAllocations(unit, block.child_scopes.Get(id), describe);
  }
}

// What `holder` holds, where it is a box, a pointer or a reference.
auto Held(const TypePool& types, TypeId holder) -> std::optional<TypeId> {
  const Type& type = types.Get(holder);
  if (const auto* pointer = type.As<PointerType>()) {
    return pointer->pointee;
  }
  if (const auto* reference = type.As<ReferenceType>()) {
    return reference->referent;
  }
  return std::nullopt;
}

[[noreturn]] void RefuseErasure(
    ExprId id, std::string_view what, const auto& describe) {
  throw InternalError(
      std::format(
          "mir verify: {} makes a closure one of no known type {} (expression "
          "{} of its block) -- please report this as a bug",
          describe(), what, id.value));
}

// A closure is made one of no known type only from the box it was made in or a
// borrow of it, held the same way, as the closure of no known type the call is
// at, and only at the kind and the signature its body has, since whoever calls
// it through the erased box or borrow calls it so and nothing checks it there.
void VerifyErasures(
    const CompilationUnit& unit, const Block& block, const auto& describe) {
  for (const ExprId id : block.exprs.Ids()) {
    const Expr& expr = block.exprs.Get(id);
    const auto* call = std::get_if<CallExpr>(&expr.data);
    if (call == nullptr ||
        DirectBuiltinFn(*call) != support::BuiltinFn::kUnsize) {
      continue;
    }
    if (call->arguments.size() != 1) {
      RefuseErasure(id, "from other than one box or borrow", describe);
    }
    const std::optional<ClosureErasure> erasure = ClosureErasureOf(
        unit.types, block.exprs.Get(call->arguments.front()).type, expr.type);
    if (!erasure.has_value()) {
      RefuseErasure(
          id, "from anything but its own box or a borrow of it", describe);
    }
    if (std::get<Direct>(call->callee).type_arguments !=
        std::vector<TypeId>{erasure->erased}) {
      RefuseErasure(
          id, "at a type other than the one its answer holds", describe);
    }
    if (erasure->erased !=
        ErasedTypeOf(
            unit.types,
            unit.callables.Get(unit.BodyOf(erasure->closure)).code)) {
      RefuseErasure(
          id, "at a kind or a signature its body does not have", describe);
    }
  }
  for (const BlockId id : block.child_scopes.Ids()) {
    VerifyErasures(unit, block.child_scopes.Get(id), describe);
  }
}

// A value too large for a frame is made only in the storage a new-expression
// allocates for it, from the heap or from its placement, or, where a body
// answers its caller, in the storage the caller takes its answer in; it is
// otherwise only read where it lies. A conditional made in storage makes the
// arm it chooses there, and a block expression its value, as clang and rustc
// evaluate both into the destination of what yields them. `made_by_enclosing`
// are the nodes of `block` the block enclosing it makes in storage this way.
// The address of code takes such a value in a box, as the body it addresses
// does.
void VerifyLargeValuesLieInStorage(
    const CompilationUnit& unit, const Block& block, bool answers_caller,
    std::span<const ExprId> made_by_enclosing, const auto& describe) {
  std::vector<ExprId> made(made_by_enclosing.begin(), made_by_enclosing.end());
  for (const Expr& expr : block.exprs) {
    if (const auto* allocation = std::get_if<NewExpr>(&expr.data);
        allocation != nullptr && allocation->initializer.has_value()) {
      made.push_back(*allocation->initializer);
    }
  }
  if (answers_caller) {
    for (const Stmt& stmt : block.stmts) {
      if (const auto* returned = std::get_if<ReturnStmt>(&stmt.data);
          returned != nullptr && returned->value.has_value()) {
        made.push_back(*returned->value);
      }
    }
  }
  std::vector<bool> made_in_storage(block.exprs.size(), false);
  std::vector<std::vector<ExprId>> made_in_child(block.child_scopes.size());
  while (!made.empty()) {
    const ExprId id = made.back();
    made.pop_back();
    if (made_in_storage.at(id.value)) {
      continue;
    }
    made_in_storage.at(id.value) = true;
    if (const auto* chosen =
            std::get_if<ConditionalExpr>(&block.exprs.Get(id).data)) {
      made.push_back(chosen->then_value);
      made.push_back(chosen->else_value);
    } else if (
        const auto* yielded =
            std::get_if<BlockExpr>(&block.exprs.Get(id).data)) {
      made_in_child.at(yielded->scope.value).push_back(yielded->value);
    }
  }
  for (const ExprId id : block.exprs.Ids()) {
    if (TakesAValueTooLargeForAFrame(unit, block.exprs.Get(id).type)) {
      throw InternalError(
          std::format(
              "mir verify: {} states {} as code taking a value too large for a "
              "frame other than in a box -- please report this as a bug",
              describe(), Describe(block.exprs.Get(id).data)));
    }
    if (made_in_storage.at(id.value) ||
        !IsTooLargeForAFrame(unit, block.exprs.Get(id).type) ||
        ValueLiesInStorage(block, id)) {
      continue;
    }
    throw InternalError(
        std::format(
            "mir verify: {} makes {} too large for a frame outside the storage "
            "a new-expression allocates -- please report this as a bug",
            describe(), Describe(block.exprs.Get(id).data)));
  }
  for (const BlockId id : block.child_scopes.Ids()) {
    VerifyLargeValuesLieInStorage(
        unit, block.child_scopes.Get(id), answers_caller,
        made_in_child.at(id.value), describe);
  }
}

// The bytes the frame keeps for the values `block`, and the blocks nested in
// it, make and read.
auto MadeValueFrameBytes(const CompilationUnit& unit, const Block& block)
    -> std::size_t {
  std::size_t kept = 0;
  for (const ExprId id : ValuesAFrameKeeps(unit, block)) {
    kept += FrameBytesOf(unit, block.exprs.Get(id).type);
  }
  for (const Block& child : block.child_scopes) {
    kept += MadeValueFrameBytes(unit, child);
  }
  return kept;
}

// The node `id` of `block` as a report names it, with what it is made of to
// `depth` levels, so the report says which construct left it.
auto DescribeWithOperands(const Block& block, ExprId id, int depth)
    -> std::string {
  const ExprData& data = block.exprs.Get(id).data;
  std::string described = Describe(data);
  if (depth == 0) {
    return described;
  }
  std::vector<std::string> parts;
  ForEachOperand(data, [&](ExprId operand, OperandUse) {
    parts.push_back(DescribeWithOperands(block, operand, depth - 1));
  });
  if (parts.empty()) {
    return described;
  }
  described += " of (";
  for (std::size_t at = 0; at < parts.size(); ++at) {
    described += (at == 0 ? "" : ", ") + parts[at];
  }
  return described + ")";
}

// No node of `block`, or of a block nested in it, keeps an operand a target
// could run in the other order to another answer, beside what runs after it,
// so the order every target runs is the one the node states, whatever its own
// language leaves unordered.
void VerifyOrderIsExplicit(
    const Block& block, EvaluationOrder& order, const auto& describe) {
  for (const ExprId id : block.exprs.Ids()) {
    const ExprData& data = block.exprs.Get(id).data;
    std::vector<ExprId> operands;
    ForEachOperand(
        data, [&](ExprId operand, OperandUse) { operands.push_back(operand); });
    const std::vector<OperandOrder> orders =
        order.OperandsNeedingTemporaries(block, data, operands, {});
    const auto unordered =
        std::ranges::find_if(orders, &OperandOrder::needs_temporary);
    if (unordered == orders.end()) {
      continue;
    }
    const ExprId operand = operands[static_cast<std::size_t>(
        std::distance(orders.begin(), unordered))];
    throw InternalError(
        std::format(
            "mir verify: {} leaves {} (expression {}) an operand a target "
            "could run in the other order to another answer, {} -- please "
            "report this as a bug",
            describe(), Describe(data), id.value,
            DescribeWithOperands(block, operand, 3)));
  }
  for (const Block& child : block.child_scopes) {
    VerifyOrderIsExplicit(child, order, describe);
  }
}

// No body keeps a value too large for a frame in one -- a local it declares, or
// what its expressions make and read: every such value is held through a box.
void VerifyFramesHoldNoLargeValue(
    const CompilationUnit& unit, const CallableCode& code,
    const auto& describe) {
  for (const LocalDecl& local : code.locals) {
    if (IsTooLargeForAFrame(unit, local.type)) {
      throw InternalError(
          std::format(
              "mir verify: {} declares a local too large for a frame -- please "
              "report this as a bug",
              describe()));
    }
  }
  if (code.body.has_value()) {
    VerifyLargeValuesLieInStorage(
        unit, *code.body, !unit.types.Get(code.result_type).Is<CoroutineType>(),
        {}, describe);
  }
}

// No body keeps values together larger than a frame may keep of them -- the
// locals it declares and what its expressions make and read -- which is a sum
// over the whole body.
void VerifyFramesHoldNoMoreThanTheBudget(
    const CompilationUnit& unit, const CallableCode& code,
    const auto& describe) {
  std::size_t kept = 0;
  for (const LocalId id : code.locals.Ids()) {
    // A parameter lies where the signature the source wrote puts it, which no
    // frame chooses.
    if (!std::ranges::contains(code.params, id)) {
      kept += FrameBytesOf(unit, code.locals.Get(id).type);
    }
  }
  if (code.body.has_value()) {
    kept += MadeValueFrameBytes(unit, *code.body);
  }
  if (kept > value::kMaxFrameBytes) {
    throw InternalError(
        std::format(
            "mir verify: {} keeps {} bytes of values in its frame, more than "
            "{} -- please report this as a bug",
            describe(), kept, value::kMaxFrameBytes));
  }
}

// A label names one statement of the body, so a break naming it leaves that
// statement and no other, and a break stands inside the statement it names. A
// switch takes each value at one case at most, so the order of its cases means
// nothing. `around` holds the labels of the statements `block` stands inside,
// and `carried` every label met so far in the body; a block expression is left
// by no break, so the steps of one stand inside none.
void VerifyBreaksAndSwitches(
    const Block& block, std::vector<LabelId>& around,
    std::set<LabelId>& carried, const auto& describe) {
  const auto inside = [&](BlockId id, std::optional<LabelId> label) {
    if (label.has_value()) {
      if (!carried.insert(*label).second) {
        throw InternalError(
            std::format(
                "mir verify: {} gives label {} to two statements -- please "
                "report this as a bug",
                describe(), label->value));
      }
      around.push_back(*label);
    }
    VerifyBreaksAndSwitches(
        block.child_scopes.Get(id), around, carried, describe);
    if (label.has_value()) {
      around.pop_back();
    }
  };
  for (const Expr& expr : block.exprs) {
    if (const auto* steps = std::get_if<BlockExpr>(&expr.data)) {
      std::vector<LabelId> none;
      VerifyBreaksAndSwitches(
          block.child_scopes.Get(steps->scope), none, carried, describe);
    }
  }
  for (const StmtId id : block.root_stmts) {
    std::visit(
        Overloaded{
            [](const EmptyStmt&) {}, [](const LocalDeclStmt&) {},
            [](const ExprStmt&) {}, [](const RaiseStmt&) {},
            [](const ContinueStmt&) {}, [](const ReturnStmt&) {},
            [&](const BlockStmt& s) { inside(s.scope, s.break_label); },
            [&](const TryStmt& s) {
              inside(s.body, std::nullopt);
              inside(s.handler, std::nullopt);
            },
            [&](const FinallyStmt& s) {
              inside(s.body, std::nullopt);
              inside(s.cleanup, std::nullopt);
            },
            [&](const IfStmt& s) {
              inside(s.then_scope, std::nullopt);
              if (s.else_scope.has_value()) {
                inside(*s.else_scope, std::nullopt);
              }
            },
            [&](const SwitchStmt& s) {
              std::set<std::int64_t> taken;
              for (const SwitchCase& selected : s.cases) {
                for (const std::int64_t value : selected.values) {
                  if (!taken.insert(value).second) {
                    throw InternalError(
                        std::format(
                            "mir verify: {} switches to two cases at {} -- "
                            "please report this as a bug",
                            describe(), value));
                  }
                }
                inside(selected.body, std::nullopt);
              }
              if (s.default_body.has_value()) {
                inside(*s.default_body, std::nullopt);
              }
            },
            [&](const ForStmt& s) { inside(s.scope, s.break_label); },
            [&](const WhileStmt& s) { inside(s.scope, s.break_label); },
            [&](const DoWhileStmt& s) { inside(s.scope, s.break_label); },
            [&](const BreakStmt& s) {
              if (!std::ranges::contains(around, s.target)) {
                throw InternalError(
                    std::format(
                        "mir verify: {} breaks out of label {}, which stands "
                        "around no statement holding it -- please report "
                        "this as a bug",
                        describe(), s.target.value));
              }
            }},
        block.stmts.Get(id).data);
  }
}

// Whether `expr` is a node MIR can state cannot leave the code it stands in:
// a call of a runtime entry declared to return, a node that computes nothing --
// a name, a literal, a place, a read of one, a cast, which only reads a value
// as another type, any conversion that reshapes it being a call -- or one that
// only chooses or
// sequences nodes held to the same, a conditional or a block expression, whose
// parts are each checked where they stand. Whether a call can leave
// is its callee's declaration, and only a runtime entry declares that it
// cannot. Any node that computes may be realized below this layer by a call it
// does not state, so this layer can promise nothing of it, and a suspension is
// left whenever the execution is ended where it stands.
auto StatedNotToLeave(const Expr& expr) -> bool {
  return std::visit(
      Overloaded{
          [](const StringLiteral&) { return true; },
          [](const NullLiteral&) { return true; },
          [](const MachineBoolLiteral&) { return true; },
          [](const MachineIntLiteral&) { return true; },
          [](const FloatingLiteral&) { return true; },
          [](const ReferenceExpr&) { return true; },
          [](const FieldAccessExpr&) { return true; },
          [](const SliceExpr&) { return true; },
          [](const IndexExpr&) { return true; },
          [](const DerefExpr&) { return true; },
          [](const AddressOfExpr&) { return true; },
          [](const BorrowExpr&) { return true; },
          [](const MoveExpr&) { return true; },
          [](const CopyExpr&) { return true; },
          [](const CallExpr& call) {
            const std::optional<support::BuiltinFn> entry =
                DirectBuiltinFn(call);
            return entry.has_value() &&
                   support::RuntimeEntryOf(*entry).returns &&
                   !support::RuntimeEntryOf(*entry).can_unwind;
          },
          [](const CastExpr&) { return true; },
          [](const ConditionalExpr&) { return true; },
          [](const BlockExpr&) { return true; },
          [](const UnaryExpr&) { return false; },
          [](const BinaryExpr&) { return false; },
          [](const AssignExpr&) { return false; },
          [](const NewExpr&) { return false; },
          [](const DiscriminantExpr&) { return false; },
          [](const ClosureExpr&) { return false; },
          [](const CompositeExpr&) { return false; },
          [](const TaggedUnionExpr&) { return false; },
          [](const AwaitExpr&) { return false; },
          [](const WaitExpr&) { return false; },
          [](const VectorGetExpr&) { return false; }},
      expr.data);
}

// That `block`, part of a cleanup's code, holds no way out of that code but
// running to its end: nothing not stated not to leave it, no region, no raise,
// no return, and no break or continue naming a statement around the cleanup.
// `labelled` holds the labels of the statements inside the cleanup that stand
// around `block`, and `loops` how many loops inside it do.
void VerifyLeavesOnlyAtItsEnd(
    const Block& block, std::vector<LabelId>& labelled, int loops,
    const auto& refuse) {
  for (const Expr& expr : block.exprs) {
    if (!StatedNotToLeave(expr)) {
      refuse(
          std::format(
              "holds {}, which is not stated not to leave it",
              Describe(expr.data)));
    }
    if (const auto* steps = std::get_if<BlockExpr>(&expr.data)) {
      VerifyLeavesOnlyAtItsEnd(
          block.child_scopes.Get(steps->scope), labelled, loops, refuse);
    }
  }
  const auto inside = [&](BlockId id, std::optional<LabelId> label,
                          int loops_around) {
    if (label.has_value()) {
      labelled.push_back(*label);
    }
    VerifyLeavesOnlyAtItsEnd(
        block.child_scopes.Get(id), labelled, loops_around, refuse);
    if (label.has_value()) {
      labelled.pop_back();
    }
  };
  for (const StmtId id : block.root_stmts) {
    std::visit(
        Overloaded{
            [](const EmptyStmt&) {}, [](const LocalDeclStmt&) {},
            [](const ExprStmt&) {}, [&](const RaiseStmt&) { refuse("raises"); },
            [&](const ReturnStmt&) { refuse("returns"); },
            [&](const ContinueStmt&) {
              if (loops == 0) {
                refuse("continues a loop that stands around it");
              }
            },
            [&](const BreakStmt& s) {
              if (!std::ranges::contains(labelled, s.target)) {
                refuse(
                    std::format(
                        "breaks out of label {}, which stands around it",
                        s.target.value));
              }
            },
            [&](const BlockStmt& s) { inside(s.scope, s.break_label, loops); },
            [&](const TryStmt&) {
              refuse("holds a region, whose handler asks what arrived");
            },
            [&](const FinallyStmt& s) {
              inside(s.body, std::nullopt, loops);
              inside(s.cleanup, std::nullopt, loops);
            },
            [&](const IfStmt& s) {
              inside(s.then_scope, std::nullopt, loops);
              if (s.else_scope.has_value()) {
                inside(*s.else_scope, std::nullopt, loops);
              }
            },
            [&](const SwitchStmt& s) {
              for (const SwitchCase& selected : s.cases) {
                inside(selected.body, std::nullopt, loops);
              }
              if (s.default_body.has_value()) {
                inside(*s.default_body, std::nullopt, loops);
              }
            },
            [&](const ForStmt& s) {
              inside(s.scope, s.break_label, loops + 1);
            },
            [&](const WhileStmt& s) {
              inside(s.scope, s.break_label, loops + 1);
            },
            [&](const DoWhileStmt& s) {
              inside(s.scope, s.break_label, loops + 1);
            }},
        block.stmts.Get(id).data);
  }
}

// A cleanup runs on every way out of its body, a departure on its way out
// included, so it has one way out of its own, running to its end: a second way
// out starting inside it would have nowhere to go, and one leaving it partway
// would leave undone what the body's ways out owe.
void VerifyCleanupsLeaveOnlyAtTheirEnd(
    const Block& block, const auto& describe) {
  for (const Stmt& stmt : block.stmts) {
    const auto* paired = std::get_if<FinallyStmt>(&stmt.data);
    if (paired == nullptr) {
      continue;
    }
    std::vector<LabelId> labelled;
    VerifyLeavesOnlyAtItsEnd(
        block.child_scopes.Get(paired->cleanup), labelled, 0,
        [&](std::string_view what) {
          throw InternalError(
              std::format(
                  "mir verify: in {}, a cleanup {}, though a cleanup leaves "
                  "only by running to its end -- please report this as a bug",
                  describe(), what));
        });
  }
  for (const Block& child : block.child_scopes) {
    VerifyCleanupsLeaveOnlyAtTheirEnd(child, describe);
  }
}

// Whether a value of `type` can hold an address: a reference, a pointer, or a
// tuple holding one, which is what a body keeps a found element's address in.
auto HoldsAnAddress(const TypePool& types, TypeId type) -> bool {
  const Type& held = types.Get(type);
  if (held.Is<ReferenceType>() || held.Is<PointerType>()) {
    return true;
  }
  if (const auto* tuple = held.As<TupleType>()) {
    return std::ranges::any_of(tuple->elements, [&](TypeId element) {
      return HoldsAnAddress(types, element);
    });
  }
  return false;
}

// An address of a local's own storage stored where it could reach the local
// past the statement storing it, in a holder living as long as the blocks
// `holder_depth` deep: a local declared there, a parameter at the body's own
// depth, or storage no local of the body holds, which outlives the body's
// blocks as a parameter does. `what` says what forms the address and the value
// storing it.
struct StoredAddress {
  int holder_depth = 0;
  std::string what;
};

// What one statement does with each local it reaches: how often it moves it,
// how often it names it otherwise, and the stored address of its storage whose
// holder is declared least deep, where there is one.
struct LocalUses {
  int moves = 0;
  int names = 0;
  std::optional<StoredAddress> stored;
};
using UsesOfLocals = std::map<LocalId, LocalUses>;

void AddStored(LocalUses& into, const StoredAddress& stored) {
  if (!into.stored.has_value() ||
      stored.holder_depth < into.stored->holder_depth) {
    into.stored = stored;
  }
}

void AddUses(UsesOfLocals& into, const UsesOfLocals& from) {
  for (const auto& [local, uses] : from) {
    LocalUses& merged = into[local];
    merged.moves += uses.moves;
    merged.names += uses.names;
    if (uses.stored.has_value()) {
      AddStored(merged, *uses.stored);
    }
  }
}

// What each statement of a block does with each local, the blocks it runs
// included, and last, where the block is a block expression's, what the value
// it yields does.
using BlockUses = std::vector<UsesOfLocals>;

// What each block of one body does with each local, worked out once per block:
// the walk of a block adds in what each block it runs does, and the check of
// that block reads the same summary, so no node is walked twice however deep
// the blocks nest.
class UseCollector {
 public:
  UseCollector(const TypePool& types, const CallableCode& code)
      : types_(&types), code_(&code) {
    for (const LocalId param : code.params) {
      depth_of_.emplace(param, 0);
    }
    if (code.body.has_value()) {
      RecordDepths(*code.body, 0);
    }
  }

  // How many blocks enclose the declaration of `local`, a parameter's being
  // the body's and a loop's induction variable's its body's; a local no
  // statement declares lies deeper than every block.
  [[nodiscard]] auto DepthOf(LocalId local) const -> int {
    const auto found = depth_of_.find(local);
    return found == depth_of_.end() ? kDeclaredByNoStatement : found->second;
  }

  // What each statement of `block`, `depth` blocks deep, does, and last what
  // `yields`, the value it yields, does.
  auto UsesOf(const Block& block, int depth, std::optional<ExprId> yields)
      -> const BlockUses& {
    if (const auto found = made_.find(&block); found != made_.end()) {
      return found->second;
    }
    const std::map<BlockId, ExprId> yielded = YieldsOf(block);
    std::vector<std::size_t> stamp(block.exprs.size(), 0);
    Walk at{
        .block = &block,
        .depth = depth,
        .yielded = &yielded,
        .stamp = &stamp,
        .walk = 0};
    BlockUses uses;
    for (const StmtId id : block.root_stmts) {
      ++at.walk;
      Statement(at, block.stmts.Get(id), uses.emplace_back());
    }
    if (yields.has_value()) {
      ++at.walk;
      Node(at, *yields, uses.emplace_back());
    }
    return made_.emplace(&block, std::move(uses)).first->second;
  }

  // The value each block expression of `block` yields, by the block it runs.
  static auto YieldsOf(const Block& block) -> std::map<BlockId, ExprId> {
    std::map<BlockId, ExprId> yielded;
    for (const Expr& expr : block.exprs) {
      if (const auto* steps = std::get_if<BlockExpr>(&expr.data)) {
        yielded.emplace(steps->scope, steps->value);
      }
    }
    return yielded;
  }

 private:
  static constexpr int kDeclaredByNoStatement = std::numeric_limits<int>::max();

  // One block being walked, `depth` blocks deep: the value each of its block
  // expressions yields, and for each of its nodes the walk that last reached
  // it, so a node standing at two places of one statement counts once there.
  struct Walk {
    const Block* block;
    int depth;
    const std::map<BlockId, ExprId>* yielded;
    std::vector<std::size_t>* stamp;
    std::size_t walk;
  };

  // A loop's induction variable lives as long as the loop's body, which is
  // one block deeper than the loop.
  void RecordDepths(const Block& block, int depth) {
    for (const StmtId id : block.root_stmts) {
      const StmtData& data = block.stmts.Get(id).data;
      if (const auto* decl = std::get_if<LocalDeclStmt>(&data)) {
        depth_of_.emplace(decl->target, depth);
      }
      if (const auto* loop = std::get_if<ForStmt>(&data)) {
        for (const ForInit& init : loop->init) {
          if (const auto* induction = std::get_if<ForInitDecl>(&init)) {
            depth_of_.emplace(induction->induction_var, depth + 1);
          }
        }
      }
    }
    for (const Block& child : block.child_scopes) {
      RecordDepths(child, depth + 1);
    }
  }

  // Adds what the block `id` nested in the walked one does: each of its
  // statements, and the value it yields.
  void AddBlock(const Walk& at, BlockId id, UsesOfLocals& uses) {
    const auto found = at.yielded->find(id);
    for (const UsesOfLocals& step : UsesOf(
             at.block->child_scopes.Get(id), at.depth + 1,
             found == at.yielded->end() ? std::nullopt
                                        : std::optional{found->second})) {
      AddUses(uses, step);
    }
  }

  void Statement(const Walk& at, const Stmt& stmt, UsesOfLocals& uses) {
    if (const auto* decl = std::get_if<LocalDeclStmt>(&stmt.data);
        decl != nullptr &&
        HoldsAnAddress(*types_, code_->locals.Get(decl->target).type)) {
      AddressesIn(*at.block, decl->init, decl->init, at.depth, uses);
    }
    ForEachExpr(stmt.data, [&](ExprId id) { Node(at, id, uses); });
    ForEachBlockRun(stmt.data, [&](BlockId id) { AddBlock(at, id, uses); });
  }

  void Node(const Walk& at, ExprId id, UsesOfLocals& uses) {
    std::vector<std::size_t>& stamp = *at.stamp;
    if (stamp.at(id.value) == at.walk) {
      return;
    }
    stamp.at(id.value) = at.walk;
    const Block& block = *at.block;
    const ExprData& data = block.exprs.Get(id).data;
    if (const auto* move = std::get_if<MoveExpr>(&data)) {
      if (const std::optional<LocalId> local =
              ReferencedLocal(block.exprs.Get(move->operand).data)) {
        ++uses[*local].moves;
        stamp.at(move->operand.value) = at.walk;
        return;
      }
    }
    if (const std::optional<LocalId> local = ReferencedLocal(data)) {
      ++uses[*local].names;
    }
    if (const auto* write = std::get_if<AssignExpr>(&data);
        write != nullptr &&
        HoldsAnAddress(*types_, block.exprs.Get(write->target).type)) {
      const std::optional<LocalId> holder =
          LocalLyingUnder(block, write->target);
      AddressesIn(
          block, write->value, write->value,
          holder.has_value() ? DepthOf(*holder) : 0, uses);
    }
    // A block expression runs its statements and then yields its value, a
    // node of its own block.
    if (const auto* steps = std::get_if<BlockExpr>(&data)) {
      AddBlock(at, steps->scope, uses);
    }
    ForEachOperand(data, [&](const ExprId& operand, OperandUse) {
      Node(at, operand, uses);
    });
  }

  // The local whose own storage the place `place` lies in: one named outright
  // or a field of one. A place a dereference reaches lies in storage the value
  // owns elsewhere, which a move hands on with the value.
  static auto LocalLyingUnder(const Block& block, ExprId place)
      -> std::optional<LocalId> {
    const ExprData& data = block.exprs.Get(place).data;
    if (const auto* field = std::get_if<FieldAccessExpr>(&data)) {
      return LocalLyingUnder(block, field->receiver);
    }
    if (const auto* slice = std::get_if<SliceExpr>(&data)) {
      return LocalLyingUnder(block, slice->receiver);
    }
    if (const auto* element = std::get_if<IndexExpr>(&data)) {
      return LocalLyingUnder(block, element->receiver);
    }
    return ReferencedLocal(data);
  }

  // The locals the value `id` holds an address of the own storage of, kept
  // in a holder declared `holder_depth` blocks deep: one taken of a place
  // lying in a local, and one a call answers into what it is entered on where
  // that is such an address of a value laying its parts in its own storage. A
  // library container keeps its elements in storage of their own, which a
  // move hands on with the value, and what any other call answers is its own,
  // whatever it was handed. `stored` is the value holding it.
  void AddressesIn(
      const Block& block, ExprId stored, ExprId id, int holder_depth,
      UsesOfLocals& uses) {
    const ExprData& data = block.exprs.Get(id).data;
    const auto taken = [&](ExprId place) {
      if (const std::optional<LocalId> local = LocalLyingUnder(block, place)) {
        AddStored(
            uses[*local],
            StoredAddress{
                .holder_depth = holder_depth,
                .what = std::format(
                    "{} within {} (expression {} of its block)", Describe(data),
                    Describe(block.exprs.Get(stored).data), stored.value)});
      }
    };
    if (const auto* address = std::get_if<AddressOfExpr>(&data)) {
      taken(address->operand);
      return;
    }
    if (const auto* borrow = std::get_if<BorrowExpr>(&data)) {
      taken(borrow->operand);
      return;
    }
    if (const auto* call = std::get_if<CallExpr>(&data)) {
      if (const std::optional<ExprId> receiver = CalleeReceiver(call->callee);
          receiver.has_value() && AnswersReferenceIntoReceiver(*call) &&
          !EnteredOnAContainer(block, *receiver)) {
        AddressesIn(block, stored, *receiver, holder_depth, uses);
      }
      return;
    }
    if (const auto* steps = std::get_if<BlockExpr>(&data)) {
      AddressesIn(
          block.child_scopes.Get(steps->scope), steps->value, steps->value,
          holder_depth, uses);
      return;
    }
    // A read answers what the storage holds, and a dereference names the
    // storage an address reaches, so neither carries on an address its
    // operand computes.
    if (std::holds_alternative<CopyExpr>(data) ||
        std::holds_alternative<MoveExpr>(data) ||
        std::holds_alternative<DerefExpr>(data)) {
      return;
    }
    ForEachOperand(data, [&](const ExprId& operand, OperandUse) {
      AddressesIn(block, stored, operand, holder_depth, uses);
    });
  }

  // Whether the receiver `receiver` is, or reaches, a library container.
  [[nodiscard]] auto EnteredOnAContainer(
      const Block& block, ExprId receiver) const -> bool {
    const Type& entered = types_->Get(block.exprs.Get(receiver).type);
    TypeId held = block.exprs.Get(receiver).type;
    if (const auto* reference = entered.As<ReferenceType>()) {
      held = reference->referent;
    }
    if (const auto* pointer = entered.As<PointerType>()) {
      held = pointer->pointee;
    }
    return types_->Get(held).IsLibraryContainer();
  }

  const TypePool* types_;
  const CallableCode* code_;
  std::map<LocalId, int> depth_of_;
  std::map<const Block*, BlockUses> made_;
};

// A move is the last use of a local, as in Rust: what names the local before
// it in the order its block runs is fine, and nothing names it in the
// statement that moves it or after -- the statements of the block declaring
// it, or of the body for a parameter. An address of its storage kept in a
// local still living at the move could reach it after, so a local any
// statement keeps so is not moved at all. Each block nested in that one is
// checked the same way, its own statements standing in for the local's
// block's, so an address kept in a local it declares counts there. `depth` is
// how many blocks enclose `block`, and `yields` the value a block expression's
// block yields after its statements, which is the last thing it does.
void VerifyNothingFollowsAMove(
    UseCollector& collect, const Block& block, int depth,
    std::optional<ExprId> yields, const auto& describe) {
  const BlockUses& by_statement = collect.UsesOf(block, depth, yields);
  // For each local this block's statements reach, where this block or one
  // around it declares it: the first statement moving it, and the first
  // address of it kept in a holder living as long as this block.
  struct Moved {
    std::optional<std::size_t> at;
    std::optional<std::string> stored;
  };
  std::map<LocalId, Moved> moved;
  for (std::size_t at = 0; at < by_statement.size(); ++at) {
    for (const auto& [local, use] : by_statement[at]) {
      if (collect.DepthOf(local) > depth) {
        continue;
      }
      Moved& state = moved[local];
      if (!state.stored.has_value() && use.stored.has_value() &&
          use.stored->holder_depth <= depth) {
        state.stored = use.stored->what;
      }
      if (!state.at.has_value() && use.moves > 0) {
        state.at = at;
      }
    }
  }
  const auto refuse = [&](LocalId local, std::string_view what) {
    throw InternalError(
        std::format(
            "mir verify: {} moves local {} and {}; a move is the last use of "
            "a local",
            describe(), local.value, what));
  };
  for (const auto& [local, state] : moved) {
    if (state.at.has_value() && state.stored.has_value()) {
      refuse(
          local, std::format(
                     "stores an address of its storage in a value, {}",
                     *state.stored));
    }
  }
  for (std::size_t at = 0; at < by_statement.size(); ++at) {
    for (const auto& [local, use] : by_statement[at]) {
      const auto found = moved.find(local);
      if (found == moved.end() || !found->second.at.has_value() ||
          at < *found->second.at) {
        continue;
      }
      if (at == *found->second.at && (use.names > 0 || use.moves > 1)) {
        refuse(local, "names it again in the statement that moves it");
      }
      if (at > *found->second.at && (use.names > 0 || use.moves > 0)) {
        refuse(local, "names it again after the move");
      }
    }
  }
  const std::map<BlockId, ExprId> yielded = UseCollector::YieldsOf(block);
  for (const BlockId id : block.child_scopes.Ids()) {
    const auto found = yielded.find(id);
    VerifyNothingFollowsAMove(
        collect, block.child_scopes.Get(id), depth + 1,
        found == yielded.end() ? std::nullopt : std::optional{found->second},
        describe);
  }
}

// Whether a call `call` answering `type` can be unwound out of: as its
// library entry, or the declaration of the type's operation it names or is,
// says; any other callee can, as far as this can see.
auto CallCanUnwind(
    const CompilationUnit& unit, const CallExpr& call, TypeId type) -> bool {
  if (const std::optional<support::BuiltinFn> fn = DirectBuiltinFn(call)) {
    return support::RuntimeEntryOf(*fn).can_unwind;
  }
  if (const auto* direct = std::get_if<Direct>(&call.callee)) {
    if (const auto* operation =
            std::get_if<TypeOperationTarget>(&direct->target)) {
      if (const std::optional<CallableCode> declared = OperationDeclarationOf(
              unit, TypeOperationSource{
                        .type = operation->type,
                        .answers = operation->answers,
                        .arguments = operation->arguments})) {
        return declared->can_unwind;
      }
    }
  }
  if (const std::optional<TypeOperationTarget> operation =
          ConstructedByTheTypesDefault(unit.types, call, type)) {
    return OperationDeclarationOf(
               unit,
               TypeOperationSource{
                   .type = operation->type, .answers = operation->answers})
        .value()
        .can_unwind;
  }
  return true;
}

// A body declared not to unwind lets no unwind leave it: it raises nothing,
// waits on nothing, which a release of its stack would unwind, and calls, and
// takes storage from, only what cannot unwind itself.
void VerifyCannotUnwind(
    const CompilationUnit& unit, const Block& block, const auto& describe) {
  const auto refuse = [&](std::string_view what) {
    throw InternalError(
        std::format(
            "mir verify: {} is declared not to unwind and {} -- please report "
            "this as a bug",
            describe(), what));
  };
  for (const Stmt& stmt : block.stmts) {
    if (std::holds_alternative<RaiseStmt>(stmt.data)) {
      refuse("raises");
    }
  }
  for (const ExprId id : block.exprs.Ids()) {
    const Expr& expr = block.exprs.Get(id);
    if (std::holds_alternative<AwaitExpr>(expr.data) ||
        std::holds_alternative<WaitExpr>(expr.data)) {
      refuse("waits");
    }
    if (const auto* made = std::get_if<NewExpr>(&expr.data);
        made != nullptr && made->placement.has_value() &&
        support::RuntimeEntryOf(made->placement->allocate).can_unwind) {
      refuse(
          std::format(
              "places a value in storage '{}' hands out, which may unwind",
              support::RuntimeEntryOf(made->placement->allocate).name));
    }
    if (const auto* call = std::get_if<CallExpr>(&expr.data);
        call != nullptr && CallCanUnwind(unit, *call, expr.type)) {
      const auto* direct = std::get_if<Direct>(&call->callee);
      const auto* operation =
          direct == nullptr ? nullptr
                            : std::get_if<TypeOperationTarget>(&direct->target);
      refuse(
          operation == nullptr
              ? std::format("makes {}, which may unwind", Describe(expr.data))
              : std::format(
                    "calls the operation \"{}\" of type {}, which may unwind",
                    support::ValueOperationName(operation->answers),
                    operation->type.value));
    }
  }
  for (const Block& inner : block.child_scopes) {
    VerifyCannotUnwind(unit, inner, describe);
  }
}

// Marks `id` and every node of `block` it takes a value from.
void MarkReached(const Block& block, ExprId id, std::vector<bool>& marked) {
  if (marked.at(id.value)) {
    return;
  }
  marked.at(id.value) = true;
  ForEachOperand(
      block.exprs.Get(id).data, [&](const ExprId& operand, OperandUse) {
        MarkReached(block, operand, marked);
      });
}

// A move hands a local's value on together with the local's end, so it moves
// the whole local, run exactly once on every way the body runs that reaches
// that end: nothing that runs on only some ways or more than once stands
// between the move and the block declaring the local, or the body for a
// parameter -- only blocks a run enters once whenever the one around them
// runs, a block statement and a block expression that neither an arm of a
// choice nor a loop's test or step holds. A local moved on some ways or many
// times would be ended twice or not at all; that nothing names it after the
// move is checked apart. `entered_with` is what a block entered that way
// inherits.
void VerifyMovesRunOnce(
    const Block& block, std::set<LocalId> entered_with, const auto& describe) {
  std::vector<bool> not_once(block.exprs.size());
  for (const ExprId id : block.exprs.Ids()) {
    if (const auto* choice =
            std::get_if<ConditionalExpr>(&block.exprs.Get(id).data)) {
      MarkReached(block, choice->then_value, not_once);
      MarkReached(block, choice->else_value, not_once);
    }
  }
  for (const Stmt& stmt : block.stmts) {
    std::visit(
        Overloaded{
            [&](const WhileStmt& s) {
              MarkReached(block, s.condition, not_once);
            },
            [&](const DoWhileStmt& s) {
              MarkReached(block, s.condition, not_once);
            },
            [&](const ForStmt& s) {
              if (s.condition.has_value()) {
                MarkReached(block, *s.condition, not_once);
              }
              for (const ExprId step : s.step) {
                MarkReached(block, step, not_once);
              }
            },
            [](const EmptyStmt&) {}, [](const LocalDeclStmt&) {},
            [](const ExprStmt&) {}, [](const BlockStmt&) {},
            [](const TryStmt&) {}, [](const RaiseStmt&) {},
            [](const FinallyStmt&) {}, [](const IfStmt&) {},
            [](const SwitchStmt&) {}, [](const BreakStmt&) {},
            [](const ContinueStmt&) {}, [](const ReturnStmt&) {}},
        stmt.data);
  }
  std::set<LocalId> movable = std::move(entered_with);
  for (const Stmt& stmt : block.stmts) {
    if (const auto* decl = std::get_if<LocalDeclStmt>(&stmt.data)) {
      movable.insert(decl->target);
    }
  }
  const auto refuse = [&](std::string_view what, ExprId id) {
    throw InternalError(
        std::format(
            "mir verify: {} holds {} (expression {} of its block); a move is "
            "the last use of a whole local on every way that reaches its end",
            describe(), what, id.value));
  };
  for (const ExprId id : block.exprs.Ids()) {
    const auto* move = std::get_if<MoveExpr>(&block.exprs.Get(id).data);
    // A move of what names no place is refused where a place is asked for.
    if (move == nullptr || !IsPlace(block.exprs.Get(move->operand).data)) {
      continue;
    }
    const std::optional<LocalId> local =
        ReferencedLocal(block.exprs.Get(move->operand).data);
    if (!local.has_value()) {
      continue;
    }
    if (not_once.at(id.value) || !movable.contains(*local)) {
      refuse("a move not run exactly once through its local's block", id);
    }
  }
  std::set<BlockId> entered_whole;
  for (const Stmt& stmt : block.stmts) {
    if (const auto* inner = std::get_if<BlockStmt>(&stmt.data)) {
      entered_whole.insert(inner->scope);
    }
  }
  for (const ExprId id : block.exprs.Ids()) {
    if (const auto* steps = std::get_if<BlockExpr>(&block.exprs.Get(id).data);
        steps != nullptr && !not_once.at(id.value)) {
      entered_whole.insert(steps->scope);
    }
  }
  for (const BlockId id : block.child_scopes.Ids()) {
    VerifyMovesRunOnce(
        block.child_scopes.Get(id),
        entered_whole.contains(id) ? movable : std::set<LocalId>{}, describe);
  }
}

// A move hands a local's value on together with the local's end, so what it
// moves is a whole local: a part of one has no end of its own to hand on.
void VerifyMovesTakeWholeLocals(const Block& block, const auto& describe) {
  for (const ExprId id : block.exprs.Ids()) {
    const auto* move = std::get_if<MoveExpr>(&block.exprs.Get(id).data);
    // A move of what names no place is refused where a place is asked for.
    if (move == nullptr || !IsPlace(block.exprs.Get(move->operand).data) ||
        ReferencedLocal(block.exprs.Get(move->operand).data).has_value()) {
      continue;
    }
    throw InternalError(
        std::format(
            "mir verify: {} holds a move of part of a value (expression {} of "
            "its block); a move is the last use of a whole local on every way "
            "that reaches its end",
            describe(), id.value));
  }
  for (const Block& child : block.child_scopes) {
    VerifyMovesTakeWholeLocals(child, describe);
  }
}

// A class handle is opened by the library's call, which answers a pointer, so
// once a unit is legalized nothing reaches an object through a handle itself
// -- neither a dereference nor an entry acting on the object: opening one can
// fail the design, and only a call is handed where it was written.
void VerifyHandlesOpenedByTheLibrary(
    const CompilationUnit& unit, const Block& block, const auto& describe) {
  for (const ExprId id : block.exprs.Ids()) {
    if (HandleReached(unit.types, block, block.exprs.Get(id)).has_value()) {
      throw InternalError(
          std::format(
              "mir verify: {} reaches an object through a class handle itself "
              "(expression {} of its block) -- please report this as a bug",
              describe(), id.value));
    }
  }
  for (const Block& child : block.child_scopes) {
    VerifyHandlesOpenedByTheLibrary(unit, child, describe);
  }
}

// Every call of something that reports at its caller is handed, last, a value
// of the library's location type, and a body handed one names no location of
// its own, so what it hands on is what it was handed.
void VerifyCallerLocationsStated(
    const CompilationUnit& unit, const CallableCode& code, const Block& block,
    const auto& describe) {
  const auto refuse = [&](std::string_view what, ExprId id) {
    throw InternalError(
        std::format(
            "mir verify: {} {} (expression {} of its block) -- please report "
            "this as a bug",
            describe(), what, id.value));
  };
  const auto names_a_location = [&](const Expr& expr) {
    const auto* named = std::get_if<ReferenceExpr>(&expr.data);
    return named != nullptr &&
           std::holds_alternative<ConstantRef>(named->target) &&
           expr.type == unit.builtins.source_location_record;
  };
  for (const ExprId id : block.exprs.Ids()) {
    const Expr& expr = block.exprs.Get(id);
    if (names_a_location(expr) && code.caller_location.has_value()) {
      refuse(
          "states a location of its own, though it reports where it was "
          "called",
          id);
    }
    const auto* call = std::get_if<CallExpr>(&expr.data);
    if (call == nullptr || !TakesCallerLocation(unit, *call, expr.type)) {
      continue;
    }
    const bool states_a_location =
        !call->arguments.empty() &&
        block.exprs.Get(call->arguments.back()).type ==
            unit.builtins.source_location;
    if (!states_a_location) {
      refuse(
          std::format(
              "holds {}, which reports where it was called and is not handed "
              "where that is",
              Describe(expr.data)),
          id);
    }
  }
  for (const Block& child : block.child_scopes) {
    VerifyCallerLocationsStated(unit, code, child, describe);
  }
}

// What holds across the whole of a body rather than at one node or statement:
// each is a property a rewrite or the lowering established for the body, which
// a check has to work out again over all of it -- the order a target runs
// operands in, a computation reached once, a move as a local's last use, the
// bytes a frame keeps together, what a block expression yields outliving it.
void VerifyAcrossTheBody(
    const CompilationUnit& unit, Phase phase, const CallableCode& code,
    const auto& describe) {
  switch (phase) {
    case Phase::kBuilt:
      break;
    case Phase::kLegalized:
      VerifyFramesHoldNoMoreThanTheBudget(unit, code, describe);
      if (code.body.has_value()) {
        EvaluationOrder order(unit.types, *code.body);
        VerifyOrderIsExplicit(*code.body, order, describe);
      }
      break;
  }
  if (!code.body.has_value()) {
    return;
  }
  VerifyNoRunEvaluatesTwice(unit, *code.body, {}, describe);
  VerifyYieldsOutliveTheirBlocks(unit, *code.body, describe);
  VerifyMovesRunOnce(
      *code.body, std::set<LocalId>(code.params.begin(), code.params.end()),
      describe);
  UseCollector collect(unit.types, code);
  VerifyNothingFollowsAMove(collect, *code.body, 0, std::nullopt, describe);
}

// An operator over an integral value wider than a machine operates on at once
// is stated, once the unit is legalized, by what it expands to, so no node of
// a legalized body states one the expansion would rewrite.
void VerifyWideOperatorsExpanded(
    const CompilationUnit& unit, const Block& block, const auto& describe) {
  for (const ExprId id : block.exprs.Ids()) {
    const Expr& expr = block.exprs.Get(id);
    const std::optional<IntegralOperator> applied =
        IntegralOperatorOf(unit, block, expr);
    if (applied.has_value() &&
        WideOperatorExpansionOf(unit, block, expr, *applied).has_value()) {
      throw InternalError(
          std::format(
              "mir verify: {} holds {} (expression {} of its block) over a "
              "value wider than a machine operates on at once, which a "
              "legalized unit states by what it expands to -- please report "
              "this as a bug",
              describe(), Describe(expr.data), id.value));
    }
  }
  for (const Block& child : block.child_scopes) {
    VerifyWideOperatorsExpanded(unit, child, describe);
  }
}

// What one node or one statement of a body states, which every build checks,
// at a cost that follows the body's size.
void VerifyEachNode(
    const CompilationUnit& unit, Phase phase, const CallableCode& code,
    const auto& describe) {
  switch (phase) {
    case Phase::kBuilt:
      break;
    case Phase::kLegalized:
      VerifyFramesHoldNoLargeValue(unit, code, describe);
      if (code.body.has_value()) {
        VerifyHandlesOpenedByTheLibrary(unit, *code.body, describe);
        VerifyWideOperatorsExpanded(unit, *code.body, describe);
        VerifyCallerLocationsStated(unit, code, *code.body, describe);
      }
      break;
  }
  if (!code.body.has_value()) {
    return;
  }
  VerifyCleanupsLeaveOnlyAtTheirEnd(*code.body, describe);
  VerifyPlacesAreRead(unit, *code.body, {}, describe);
  VerifyMovesTakeWholeLocals(*code.body, describe);
  if (!code.can_unwind) {
    VerifyCannotUnwind(unit, *code.body, describe);
  }
  VerifyWritesLandInWritableStorage(unit, *code.body, describe);
  VerifyValuesMeetTheirTypes(unit, phase, code, *code.body, describe);
  VerifyStorageWithoutAnAddress(unit, *code.body, describe);
  VerifyMemberReceivers(unit, *code.body, describe);
  VerifyReferenceBindings(unit, *code.body, describe);
  VerifyTypeArguments(unit, *code.body, describe);
  VerifyTypeOperationCalls(unit, *code.body, describe);
  VerifyUnionValues(unit, *code.body, describe);
  VerifyOrdinals(unit, *code.body, describe);
  VerifyEntryDeclarations(unit, *code.body, describe);
  VerifyConstantsAreFolded(unit, *code.body, describe);
  VerifyAllocations(unit, *code.body, describe);
  VerifyErasures(unit, *code.body, describe);
  std::vector<LabelId> around;
  std::set<LabelId> carried;
  VerifyBreaksAndSwitches(*code.body, around, carried, describe);
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

// `describe` names the body, and is asked only once there is something to
// report: the check runs over every body of every unit, and composing a name
// for each one costs more than the check itself. What holds across the body is
// checked after what each node states, which it reads as given.
void VerifyCode(
    const CompilationUnit& unit, Phase phase, const CallableCode& code,
    Checks checks, const auto& describe) {
  VerifyEachNode(unit, phase, code, describe);
  switch (checks) {
    case Checks::kEveryBuild:
      break;
    case Checks::kAll:
      VerifyAcrossTheBody(unit, phase, code, describe);
      break;
  }
}

// The closure a value of `type` hands over where it is a pointer owning nothing
// to a closure of known type, as a placement answers for one made in storage
// another owns, and nothing otherwise. Such a pointer is itself callable, as
// Rust's `&F` is wherever `F` is, so whatever takes one takes a closure.
auto MadeInPlace(const TypePool& types, TypeId type) -> const ClosureType* {
  const auto* pointer = types.Get(type).As<PointerType>();
  return pointer != nullptr && pointer->ownership == PointerOwnership::kBorrowed
             ? types.Get(pointer->pointee).As<ClosureType>()
             : nullptr;
}

// Whether a value of `type` hands closures or started bodies over: a box or a
// borrow of a closure of no known type, a closure made in place, a started
// body, or a run of them.
auto HandsCallablesOver(const TypePool& types, TypeId type) -> bool {
  const Type& t = types.Get(type);
  if (const auto* run = t.As<MachineArrayType>()) {
    return HandsCallablesOver(types, run->element);
  }
  if (t.Is<CoroutineType>() || MadeInPlace(types, type) != nullptr) {
    return true;
  }
  const std::optional<TypeId> held = Held(types, type);
  return held.has_value() && types.Get(*held).Is<ErasedCallableType>();
}

// Whether a value of `type` crosses to the library as `kind`.
auto CrossesAs(
    const CompilationUnit& unit, TypeId type, support::MachineKind kind)
    -> bool {
  const Type& t = unit.types.Get(type);
  const auto integer = [&](MachineIntWidth width) {
    const auto* machine = t.As<MachineIntType>();
    return machine != nullptr && machine->width == width;
  };
  const auto floating = [&](support::FloatWidth width) {
    const auto* machine = t.As<MachineFloatType>();
    return machine != nullptr && machine->width == width;
  };
  switch (kind) {
    case support::MachineKind::kVoid:
      return type == unit.builtins.void_type;
    case support::MachineKind::kAddress:
      return t.Is<PointerType>();
    case support::MachineKind::kTruth:
      return t.Is<MachineBoolType>();
    case support::MachineKind::kInt8:
      return integer(MachineIntWidth::k8);
    case support::MachineKind::kInt32:
      return integer(MachineIntWidth::k32);
    case support::MachineKind::kInt64:
      return integer(MachineIntWidth::k64);
    case support::MachineKind::kFloat:
      return floating(support::FloatWidth::k32);
    case support::MachineKind::kDouble:
      return floating(support::FloatWidth::k64);
  }
  throw InternalError("mir verify: unknown MachineKind");
}

// Whether a closure of no known type `erased` is called at `signature`: it
// takes what the entry hands it and answers what the entry takes back.
auto IsCalledAt(
    const CompilationUnit& unit, const ErasedCallableType& erased,
    const support::MachineSignature& signature) -> bool {
  const auto crosses = [&](TypeId type, support::MachineKind kind) {
    return CrossesAs(unit, type, kind);
  };
  return std::ranges::equal(erased.params, signature.Takes(), crosses) &&
         CrossesAs(unit, erased.result, signature.Returns());
}

// Whether the closure whose body is `invoke` is called at `signature` as that
// body is: it takes what the entry hands it and answers what the entry takes
// back.
auto IsCalledAt(
    const CompilationUnit& unit, const CallableCode& invoke,
    const support::MachineSignature& signature) -> bool {
  const auto crosses = [&](LocalId param, support::MachineKind kind) {
    return CrossesAs(unit, invoke.locals.Get(param).type, kind);
  };
  return std::ranges::equal(invoke.params, signature.Takes(), crosses) &&
         CrossesAs(unit, invoke.result_type, signature.Returns());
}

// Whether a value of `type`, which hands closures or started bodies over,
// hands over what `trait` says the entry taking it asks for: a box or a borrow
// of a closure of no known type, or a closure made in place, whose kind serves
// the kind the entry asks, at the signature it calls it at, or a started body,
// alone or in a run of them.
auto HandsOverAs(
    const CompilationUnit& unit, TypeId type,
    const support::CallableTrait& trait) -> bool {
  const Type& t = unit.types.Get(type);
  if (const auto* run = t.As<MachineArrayType>()) {
    return HandsOverAs(unit, run->element, trait);
  }
  return std::visit(
      Overloaded{
          [&](const support::StartedBody&) { return t.Is<CoroutineType>(); },
          [&](const support::CalledClosure& called) {
            if (const ClosureType* made = MadeInPlace(unit.types, type)) {
              const CallableCode& invoke =
                  unit.callables.Get(unit.BodyOf(made->closure_id)).code;
              return support::Extends(
                         ClosureKindOf(unit.types, invoke), called.asks) &&
                     IsCalledAt(unit, invoke, called.signature);
            }
            const std::optional<TypeId> held = Held(unit.types, type);
            const auto* erased =
                held.has_value()
                    ? unit.types.Get(*held).As<ErasedCallableType>()
                    : nullptr;
            return erased != nullptr &&
                   support::Extends(erased->kind, called.asks) &&
                   IsCalledAt(unit, *erased, called.signature);
          }},
      trait);
}

// How long the storage a closure's reference reaches lives, against the frame
// that made the closure: for the whole run, as long as an object, or as long
// as that frame -- reached from one of its locals, where it is.
struct LivesForTheWholeRun {};
struct LivesWithTheObject {};
struct LivesWithTheFrame {
  std::optional<LocalId> local;
};
using Reached =
    std::variant<LivesForTheWholeRun, LivesWithTheObject, LivesWithTheFrame>;

// A runtime entry states, for each operand handing it closures, what it asks
// of them and how long it keeps them, so what is handed to it has a type that
// serves what it asks, at the signature it calls it at, since nothing below
// this checks that signature where the closure is called. A closure it keeps
// past the frame that handed it over can run once that frame has ended, so
// every reference the closure holds without owning what it refers to reaches
// storage that lives for the whole run; an object it reaches it holds a share
// of, and reaches through that share. Followed through every closure a
// reference was captured through, since a capture of a capture lives only as
// long as the first one's referent.
class HandedClosures {
 public:
  explicit HandedClosures(const CompilationUnit& unit) : unit_(&unit) {
    ForEachCode(unit, [&](const CodeSource&, const CallableCode& code) {
      if (code.body.has_value()) {
        IndexSites(code, *code.body);
      }
    });
  }

  void Check(const CallableCode& code, const auto& describe) {
    if (code.body.has_value()) {
      CheckHandOns(code, *code.body, describe);
    }
  }

 private:
  struct Site {
    const CallableCode* body;
    const Block* block;
    const ClosureExpr* closure;
  };

  void IndexSites(const CallableCode& code, const Block& block) {
    for (const Expr& expr : block.exprs) {
      const auto* made = std::get_if<ClosureExpr>(&expr.data);
      if (made != nullptr &&
          !sites_
               .emplace(
                   made->closure,
                   Site{.body = &code, .block = &block, .closure = made})
               .second) {
        throw InternalError(
            std::format(
                "mir verify: closure {} of unit '{}' is made at two sites -- "
                "please report this as a bug",
                made->closure.value, unit_->name));
      }
    }
    for (const Stmt& stmt : block.stmts) {
      if (const auto* declared = std::get_if<LocalDeclStmt>(&stmt.data)) {
        inits_.insert_or_assign(
            std::pair{&code, declared->target},
            std::pair{&block, declared->init});
      }
    }
    for (const BlockId id : block.child_scopes.Ids()) {
      IndexSites(code, block.child_scopes.Get(id));
    }
  }

  void CheckHandOns(
      const CallableCode& code, const Block& block, const auto& describe) {
    for (const Expr& expr : block.exprs) {
      const auto* call = std::get_if<CallExpr>(&expr.data);
      if (call == nullptr) {
        continue;
      }
      const std::optional<support::BuiltinFn> builtin = DirectBuiltinFn(*call);
      if (!builtin.has_value()) {
        continue;
      }
      const support::RuntimeEntry entry = support::RuntimeEntryOf(*builtin);
      const std::span<const support::CallableBounds> declared =
          entry.callables.All();
      std::size_t next = 0;
      for (const ExprId argument : call->arguments) {
        if (!HandsCallablesOver(unit_->types, block.exprs.Get(argument).type)) {
          continue;
        }
        if (next == declared.size()) {
          RefuseEntry(*builtin, "more operands handing it closures", describe);
        }
        const support::CallableBounds bounds = declared[next];
        ++next;
        if (!HandsOverAs(
                *unit_, block.exprs.Get(argument).type, bounds.trait)) {
          RefuseTrait(*builtin, describe);
        }
        if (support::OutlivesTheCaller(bounds.kept)) {
          for (const ClosureId handed : Handed(code, block, argument)) {
            CheckDetached(handed, describe);
          }
        }
      }
      if (next != declared.size()) {
        RefuseEntry(*builtin, "fewer operands handing it closures", describe);
      }
    }
    for (const BlockId id : block.child_scopes.Ids()) {
      CheckHandOns(code, block.child_scopes.Get(id), describe);
    }
  }

  // The closures `id` hands over: made there, made in place, boxed, borrowed,
  // made one of no known type, gathered, or started as a body that takes its
  // box over. A body started that is no closure's hands none.
  auto Handed(const CallableCode& code, const Block& block, ExprId id)
      -> std::vector<ClosureId> {
    const ExprData& data = block.exprs.Get(id).data;
    if (const auto* made = std::get_if<ClosureExpr>(&data)) {
      return {made->closure};
    }
    if (const auto* boxed = std::get_if<NewExpr>(&data);
        boxed != nullptr && boxed->initializer.has_value()) {
      return Handed(code, block, *boxed->initializer);
    }
    if (const auto* borrowed = std::get_if<BorrowExpr>(&data)) {
      return Handed(code, block, borrowed->operand);
    }
    // A closure made one of no known type is the closure it was.
    if (const auto* call = std::get_if<CallExpr>(&data);
        call != nullptr &&
        DirectBuiltinFn(*call) == support::BuiltinFn::kUnsize) {
      return Handed(code, block, call->arguments.front());
    }
    if (const auto* moved = std::get_if<MoveExpr>(&data)) {
      return Handed(code, block, moved->operand);
    }
    if (const auto* gathered = std::get_if<CompositeExpr>(&data)) {
      std::vector<ClosureId> all;
      for (const ExprId part : gathered->parts) {
        std::ranges::copy(Handed(code, block, part), std::back_inserter(all));
      }
      return all;
    }
    if (const auto* call = std::get_if<CallExpr>(&data)) {
      const auto* direct = std::get_if<Direct>(&call->callee);
      const auto* target = direct != nullptr
                               ? std::get_if<CallableTarget>(&direct->target)
                               : nullptr;
      if (target != nullptr &&
          std::holds_alternative<DeclaredByClosure>(
              unit_->OwnerOf(target->callable)) &&
          direct->receiver.has_value()) {
        return Handed(code, block, *direct->receiver);
      }
      return {};
    }
    if (const std::optional<LocalId> local = ReferencedLocal(data)) {
      const auto init = inits_.find(std::pair{&code, *local});
      if (init != inits_.end()) {
        return Handed(code, *init->second.first, init->second.second);
      }
    }
    throw InternalError(
        std::format(
            "mir verify: a closure handed to the runtime in unit '{}' is "
            "reached by a step the verifier does not follow (expression {}) "
            "-- please report this as a bug",
            unit_->name, id.value));
  }

  void CheckDetached(ClosureId id, const auto& describe) {
    const ClosureDecl& closure = unit_->GetClosure(id);
    const Site& site = SiteOf(id);
    for (const FieldInit& init : site.closure->field_inits) {
      const TypeId type = closure.fields.Get(init.target).type;
      if (!RefersWithoutOwning(unit_->types, type)) {
        continue;
      }
      std::visit(
          Overloaded{
              [](const LivesForTheWholeRun&) {},
              [&](const LivesWithTheObject&) {
                Refuse(
                    id,
                    "is kept past its maker's end yet refers into an object "
                    "it holds no share of",
                    describe);
              },
              [&](const LivesWithTheFrame& reached) {
                Refuse(
                    id,
                    std::format(
                        "is kept past its maker's end yet captures in field {} "
                        "a reference to storage of the frame that made it{}",
                        init.target.value,
                        reached.local.has_value()
                            ? std::format(" (local {})", reached.local->value)
                            : std::string()),
                    describe);
              }},
          ReachOf(*site.body, *site.block, init.value));
    }
  }

  [[noreturn]] void Refuse(
      ClosureId id, std::string_view what, const auto& describe) const {
    throw InternalError(
        std::format(
            "mir verify: {} hands closure {} to the runtime, and the closure "
            "{} -- please report this as a bug",
            describe(), id.value, what));
  }

  [[noreturn]] static void RefuseEntry(
      support::BuiltinFn entry, std::string_view what, const auto& describe) {
    throw InternalError(
        std::format(
            "mir verify: {} calls '{}' with {} than it states -- please report "
            "this as a bug",
            describe(), support::RuntimeEntryOf(entry).name, what));
  }

  [[noreturn]] static void RefuseTrait(
      support::BuiltinFn entry, const auto& describe) {
    throw InternalError(
        std::format(
            "mir verify: {} hands '{}' a closure or a started body it neither "
            "calls at the kind and the signature it states nor starts -- "
            "please report this as a bug",
            describe(), support::RuntimeEntryOf(entry).name));
  }

  [[noreturn]] void RefuseUnfollowed(ExprId id) const {
    throw InternalError(
        std::format(
            "mir verify: a reference a closure of unit '{}' captures is "
            "reached by a step the verifier does not follow (expression {}) "
            "-- please report this as a bug",
            unit_->name, id.value));
  }

  [[nodiscard]] auto SiteOf(ClosureId id) const -> const Site& {
    const auto it = sites_.find(id);
    if (it == sites_.end()) {
      throw InternalError(
          std::format(
              "mir verify: closure {} of unit '{}' is made nowhere -- please "
              "report this as a bug",
              id.value, unit_->name));
    }
    return it->second;
  }

  // How long the object `pointee` lives, where it is one.
  [[nodiscard]] auto ReachOfObject(TypeId pointee) const
      -> std::optional<Reached> {
    const std::optional<support::ObjectLifetime> lifetime =
        ObjectLifetimeOf(*unit_, pointee);
    if (!lifetime.has_value()) {
      return std::nullopt;
    }
    switch (*lifetime) {
      case support::ObjectLifetime::kTheRun:
        return LivesForTheWholeRun{};
      case support::ObjectLifetime::kWhileReferenced:
        return LivesWithTheObject{};
    }
    throw InternalError("mir verify: unknown ObjectLifetime");
  }

  // How long the object a value of type `reference` refers to lives, where the
  // type says it refers to one: through a borrowed pointer, a managed reference
  // or an unmanaged one. An owning pointer's referent ends with the owner.
  [[nodiscard]] auto ReachThrough(TypeId reference) const
      -> std::optional<Reached> {
    const Type& type = unit_->types.Get(reference);
    if (const auto* pointer = type.As<PointerType>()) {
      return pointer->ownership == PointerOwnership::kBorrowed
                 ? ReachOfObject(pointer->pointee)
                 : std::nullopt;
    }
    if (const auto* managed = type.As<ManagedRefType>()) {
      return ReachOfObject(managed->pointee);
    }
    if (const auto* unmanaged = type.As<UnmanagedRefType>()) {
      return ReachOfObject(unmanaged->pointee);
    }
    return std::nullopt;
  }

  // Where the storage `id` refers to lies, followed back through the steps
  // that reach it to the binding it starts from. A value referring to an object
  // and a dereference of one reach the object, wherever the reference came
  // from; a place holding such a reference is storage of whatever holds it, as
  // a borrow of it refers to the reference rather than to the object.
  auto ReachOf(const CallableCode& code, const Block& block, ExprId id)
      -> Reached {
    if (const Expr& expr = block.exprs.Get(id); !IsPlace(expr.data)) {
      if (const std::optional<Reached> lives = ReachThrough(expr.type)) {
        return *lives;
      }
    }
    const auto from = [&](ExprId operand) {
      return ReachOf(code, block, operand);
    };
    return std::visit(
        Overloaded{
            [&](const ReferenceExpr& named) {
              return ReachOfName(code, named.target);
            },
            [&](const FieldAccessExpr& access) {
              if (const auto* captured =
                      std::get_if<ClosureFieldTarget>(&access.field)) {
                return ReachOfCapture(captured->owner, captured->slot);
              }
              return from(access.receiver);
            },
            [&](const SliceExpr& slice) { return from(slice.receiver); },
            [&](const IndexExpr& element) { return from(element.receiver); },
            [&](const DerefExpr& deref) {
              const std::optional<Reached> lives =
                  ReachThrough(block.exprs.Get(deref.pointer).type);
              return lives.has_value() ? *lives : from(deref.pointer);
            },
            [&](const CopyExpr& copied) { return from(copied.place); },
            [&](const MoveExpr& moved) { return from(moved.operand); },
            [&](const BorrowExpr& borrowed) { return from(borrowed.operand); },
            [&](const AddressOfExpr& address) { return from(address.operand); },
            [&](const CastExpr& cast) { return from(cast.operand); },
            [&](const CallExpr& call) {
              return ReachOfAnswer(code, block, id, call);
            },
            [&](const StringLiteral&) -> Reached { RefuseUnfollowed(id); },
            [&](const NullLiteral&) -> Reached { RefuseUnfollowed(id); },
            [&](const MachineBoolLiteral&) -> Reached { RefuseUnfollowed(id); },
            [&](const MachineIntLiteral&) -> Reached { RefuseUnfollowed(id); },
            [&](const FloatingLiteral&) -> Reached { RefuseUnfollowed(id); },
            [&](const UnaryExpr&) -> Reached { RefuseUnfollowed(id); },
            [&](const BinaryExpr&) -> Reached { RefuseUnfollowed(id); },
            [&](const ConditionalExpr&) -> Reached { RefuseUnfollowed(id); },
            [&](const BlockExpr&) -> Reached { RefuseUnfollowed(id); },
            [&](const AssignExpr&) -> Reached { RefuseUnfollowed(id); },
            [&](const NewExpr&) -> Reached { RefuseUnfollowed(id); },
            [&](const DiscriminantExpr&) -> Reached { RefuseUnfollowed(id); },
            [&](const ClosureExpr&) -> Reached { RefuseUnfollowed(id); },
            [&](const CompositeExpr&) -> Reached { RefuseUnfollowed(id); },
            [&](const TaggedUnionExpr&) -> Reached { RefuseUnfollowed(id); },
            [&](const AwaitExpr&) -> Reached { RefuseUnfollowed(id); },
            [&](const WaitExpr&) -> Reached { RefuseUnfollowed(id); },
            [&](const VectorGetExpr&) -> Reached { RefuseUnfollowed(id); }},
        block.exprs.Get(id).data);
  }

  // What a name reaches: a local is followed to what it was bound to, and
  // every other name reaches storage, a constant or code that exists for the
  // whole run.
  auto ReachOfName(const CallableCode& code, const ReferenceTarget& target)
      -> Reached {
    return std::visit(
        Overloaded{
            [&](const LocalRef& local) {
              return ReachOfLocal(code, local.var);
            },
            [](const DefinitionRef&) -> Reached {
              return LivesForTheWholeRun{};
            },
            [](const ConstantRef&) -> Reached { return LivesForTheWholeRun{}; },
            [](const StaticPropertyRef&) -> Reached {
              return LivesForTheWholeRun{};
            },
            [](const StaticVariableRef&) -> Reached {
              return LivesForTheWholeRun{};
            },
            [](const FunctionRef&) -> Reached { return LivesForTheWholeRun{}; },
            [](const DispatchEntryRef&) -> Reached {
              return LivesForTheWholeRun{};
            },
            [](const DestructorRef&) -> Reached {
              return LivesForTheWholeRun{};
            },
            [](const DispatchTableRef&) -> Reached {
              return LivesForTheWholeRun{};
            },
            [](const InterfaceTableRef&) -> Reached {
              return LivesForTheWholeRun{};
            },
            [](const ExternalObjectEntryRef&) -> Reached {
              return LivesForTheWholeRun{};
            }},
        target);
  }

  // What a call's answer reaches, which the library entry it calls states: a
  // reference into the object it acts on, or to what the wrapper it is taken on
  // holds, reaches what that first operand reaches, as a closure made one of
  // no known type does, and a construction of a reference what it is built
  // over. Nothing else answers a reference a
  // closure may capture -- a value the call builds is no storage, and a body
  // the source wrote answers none.
  auto ReachOfAnswer(
      const CallableCode& code, const Block& block, ExprId id,
      const CallExpr& call) -> Reached {
    const auto first = [&]() -> Reached {
      const std::optional<ExprId> operand = FirstOperand(call);
      if (!operand.has_value()) {
        RefuseUnfollowed(id);
      }
      return ReachOf(code, block, *operand);
    };
    return std::visit(
        Overloaded{
            [&](const Direct& direct) -> Reached {
              const auto* entry =
                  std::get_if<support::BuiltinFn>(&direct.target);
              if (entry == nullptr) {
                RefuseUnfollowed(id);
              }
              // A closure made one of no known type lies where it was made.
              if (*entry == support::BuiltinFn::kUnsize) {
                return first();
              }
              switch (support::RuntimeEntryOf(*entry).answer) {
                case support::EntryAnswer::kReadOnlyReferenceIntoTheReceiver:
                case support::EntryAnswer::kMutableReferenceIntoTheReceiver:
                case support::EntryAnswer::kReferenceToContents:
                case support::EntryAnswer::kReferenceToSampledContents:
                  return first();
                case support::EntryAnswer::kNewValue:
                  RefuseUnfollowed(id);
              }
              throw InternalError("mir verify: unknown EntryAnswer");
            },
            [&](const Construct&) { return first(); },
            [&](const Indirect&) -> Reached { RefuseUnfollowed(id); },
            [&](const Virtual&) -> Reached { RefuseUnfollowed(id); }},
        call.callee);
  }

  // A local of `code`: a reference the body keeps reaches what its
  // declaration bound it to, and anything else is storage of the frame.
  auto ReachOfLocal(const CallableCode& code, LocalId local) -> Reached {
    const auto init = inits_.find(std::pair{&code, local});
    if (init != inits_.end() &&
        RefersWithoutOwning(unit_->types, code.locals.Get(local).type)) {
      return ReachOf(code, *init->second.first, init->second.second);
    }
    return LivesWithTheFrame{.local = local};
  }

  // A capture of the closure a body runs in: what it reaches is what its
  // initializer reached where that closure was made, and a capture holding a
  // value of its own lives only as long as that closure.
  auto ReachOfCapture(ClosureId owner, FieldId slot) -> Reached {
    const ClosureDecl& closure = unit_->GetClosure(owner);
    if (!RefersWithoutOwning(unit_->types, closure.fields.Get(slot).type)) {
      return LivesWithTheFrame{};
    }
    const Site& site = SiteOf(owner);
    for (const FieldInit& init : site.closure->field_inits) {
      if (init.target == slot) {
        return ReachOf(*site.body, *site.block, init.value);
      }
    }
    throw InternalError(
        std::format(
            "mir verify: closure {} of unit '{}' is made without a capture "
            "its body reads -- please report this as a bug",
            owner.value, unit_->name));
  }

  const CompilationUnit* unit_;
  std::map<ClosureId, Site> sites_;
  std::map<
      std::pair<const CallableCode*, LocalId>, std::pair<const Block*, ExprId>>
      inits_;
};

// A type the library declares is named with one argument for each parameter
// its declaration takes, each of the kind that parameter takes: a type, or a
// queue's bound. A reader asks an argument by the role its parameter plays, so
// one missing or of the other kind would be read as something it is not.
void VerifyLibraryTypes(const CompilationUnit& unit, std::string_view when) {
  for (const TypeId id : unit.types.Ids()) {
    const auto* library = unit.types.Get(id).As<LibraryType>();
    if (library == nullptr) {
      continue;
    }
    const std::span<const support::LibraryParam> params =
        support::DeclarationOf(library->decl).params;
    const bool matches =
        library->args.size() == params.size() &&
        std::ranges::equal(
            library->args, params,
            [](const LibraryType::Arg& arg, support::LibraryParam param) {
              return std::holds_alternative<TypeId>(arg) ==
                     support::TakesAType(param);
            });
    if (!matches) {
      throw InternalError(
          std::format(
              "mir verify: unit '{}', {}, names a {} with arguments its "
              "declaration does not take -- please report this as a bug",
              unit.name, when, support::DeclarationOf(library->decl).name));
    }
  }
}

// Each callable's owner is the one statement of who declares it and where it
// stands. Where a callable stands is handed out by its owner's take, and only
// an owner the unit has an identity for takes one, so what is left to hold is
// what the takes cannot rule out: every identity taken is stated, a class has
// at most one construction, a closure exactly one body, and a record answers
// each operation with one method at most.
void VerifyMembers(const CompilationUnit& unit, std::string_view when) {
  const auto refuse = [&](CallableId callable, std::string_view what) {
    throw InternalError(
        std::format(
            "mir verify: unit '{}', {}, {} callable {} -- please report this "
            "as a bug",
            unit.name, when, what, callable.value));
  };
  if (const std::optional<CallableId> unstated = unit.callables.AnyUnstated()) {
    refuse(*unstated, "never states the taken");
  }
  std::vector<std::optional<CallableId>> constructions(unit.classes.size());
  std::vector<std::optional<CallableId>> closure_bodies(unit.closures.size());
  std::vector<std::vector<support::ValueOperation>> answered(
      unit.records.size());
  for (const CallableId callable : unit.callables.Ids()) {
    std::visit(
        Overloaded{
            [](const DeclaredByNamespace&) {}, [](const DeclaredByClass&) {},
            [&](const ConstructsClass& owner) {
              if (constructions[owner.cls.value].has_value()) {
                refuse(callable, "gives a class a second construction in the");
              }
              constructions[owner.cls.value] = callable;
            },
            [&](const DeclaredByClosure& owner) {
              if (closure_bodies[owner.closure.value].has_value()) {
                refuse(callable, "gives a closure a second body in the");
              }
              closure_bodies[owner.closure.value] = callable;
            },
            [&](const DeclaredByRecord& owner) {
              std::vector<support::ValueOperation>& of_record =
                  answered[owner.record.value];
              if (std::ranges::find(of_record, owner.answers) !=
                  of_record.end()) {
                refuse(
                    callable, "answers one operation of a record twice in the");
              }
              of_record.push_back(owner.answers);
            }},
        unit.callables.Get(callable).owner);
  }
  const auto refuse_owner = [&](std::string_view what) {
    throw InternalError(
        std::format(
            "mir verify: unit '{}', {}, {} -- please report this as a bug",
            unit.name, when, what));
  };
  for (const ClosureId id : unit.closures.Ids()) {
    if (!closure_bodies[id.value].has_value()) {
      refuse_owner(
          std::format("declares closure {} with no body to invoke", id.value));
    }
  }
}

// An entry has one identity here however the name reaching it was written:
// one this unit defines is never declared as another unit's, and one another
// unit defines is declared once, by the unit and the name it published it
// under. Such an entry is a declaration only -- a class states its methods'
// names, dispatch and prototypes and what its construction takes, a subroutine
// its signature, a record and a variable their type -- and holds nothing this
// unit would emit, so no walk emitting definitions can emit a second one of a
// symbol its unit already defines.
void VerifyDeclaredEntries(const CompilationUnit& unit, std::string_view when) {
  const auto refuse = [&](const support::DefPath& class_path,
                          std::string_view what) {
    throw InternalError(
        std::format(
            "mir verify: unit '{}', {}, {} '{}' -- please report this as a bug",
            unit.name, when, what, support::DisplayOf(class_path)));
  };
  std::vector<std::pair<std::string_view, const support::DefPath*>> declared;
  for (const ClassId id : unit.classes.Ids()) {
    const Class& cls = unit.GetClass(id);
    if (!cls.defining_unit.has_value()) continue;
    const support::DefPath* class_path = support::PathOf(cls.identity);
    if (class_path == nullptr) {
      throw InternalError(
          std::format(
              "mir verify: unit '{}', {}, declares a class of unit '{}' that "
              "has no path -- please report this as a bug",
              unit.name, when, *cls.defining_unit));
    }
    if (*cls.defining_unit == unit.name) {
      refuse(*class_path, "declares as another unit's its own class");
    }
    for (const auto& [unit_name, path] : declared) {
      if (unit_name == *cls.defining_unit && *path == *class_path) {
        refuse(*class_path, "declares twice the class");
      }
    }
    declared.emplace_back(*cls.defining_unit, class_path);
    // What it states of the class is prototypes: a body of a method or of the
    // construction is the defining unit's.
    const auto has_body = [&](CallableId callable) {
      return unit.callables.Get(callable).code.body.has_value();
    };
    const DeclaredCallables& declared_callables = unit.CallablesOf(id);
    const bool holds_code =
        std::ranges::any_of(declared_callables.callables, has_body) ||
        (declared_callables.constructor.has_value() &&
         has_body(*declared_callables.constructor));
    if (holds_code || !cls.contained.empty() || cls.definition.has_value()) {
      refuse(*class_path, "holds code or data of its own for the class");
    }
  }
  for (const RecordId id : unit.records.Ids()) {
    const RecordDecl& record = unit.GetRecord(id);
    if (!record.defining_unit.has_value()) continue;
    if (*record.defining_unit == unit.name) {
      refuse(record.path, "declares as another unit's its own record");
    }
    if (std::ranges::any_of(unit.RecordMethodsOf(id), [&](CallableId method) {
          return unit.callables.Get(method).code.body.has_value();
        })) {
      refuse(record.path, "states methods of its own for the record");
    }
  }
  const auto refuse_member = [&](std::string_view what, std::uint32_t at) {
    throw InternalError(
        std::format(
            "mir verify: unit '{}', {}, {} {} -- please report this as a bug",
            unit.name, when, what, at));
  };
  // A method of a class this unit defines has its body here unless the class
  // leaves it to what extends it, and so does its construction, which nothing
  // leaves to anything; a method of a record this unit defines has its body
  // here, and a subroutine of this unit's namespace has its body here unless
  // foreign code defines it: nothing else defines one.
  for (const RecordId id : unit.records.Ids()) {
    if (unit.GetRecord(id).defining_unit.has_value()) continue;
    for (const CallableId method : unit.RecordMethodsOf(id)) {
      if (!unit.callables.Get(method).code.body.has_value()) {
        refuse_member(
            "leaves with no body a method of a record it defines, callable",
            method.value);
      }
    }
  }
  for (const ClassId id : unit.classes.Ids()) {
    const Class& cls = unit.GetClass(id);
    if (cls.defining_unit.has_value()) continue;
    const DeclaredCallables& declared = unit.CallablesOf(id);
    for (const CallableId callable : declared.callables) {
      if (std::holds_alternative<DefinedByAnotherUnit>(
              FormOf(unit.callables.Get(callable)))) {
        refuse_member(
            "leaves with no body a method of a class it defines, callable",
            callable.value);
      }
    }
    if (declared.constructor.has_value() &&
        !unit.callables.Get(*declared.constructor).code.body.has_value()) {
      refuse_member(
          "leaves with no body the construction of a class it defines, "
          "callable",
          declared.constructor->value);
    }
  }
  for (const CallableId id : unit.NamespaceCallables()) {
    const CallableDecl& callable = unit.callables.Get(id);
    const std::optional<std::string>& defining_unit =
        std::get<DeclaredByNamespace>(callable.owner).defining_unit;
    if (!defining_unit.has_value()) {
      if (std::holds_alternative<DefinedByAnotherUnit>(FormOf(callable))) {
        refuse_member("leaves with no body its own subroutine", id.value);
      }
      continue;
    }
    if (*defining_unit == unit.name) {
      refuse_member("declares as another unit's its own subroutine", id.value);
    }
    if (!NameOf(unit.named_callables, id).has_value()) {
      refuse_member(
          "declares with no name a subroutine another unit defines", id.value);
    }
  }
  for (const StaticVariableId id : unit.static_variables.Ids()) {
    const StaticVariableDecl& variable = unit.static_variables.Get(id);
    if (!variable.defining_unit.has_value()) continue;
    if (*variable.defining_unit == unit.name) {
      refuse_member("declares as another unit's its own variable", id.value);
    }
    if (NameOf(unit.named_static_variables, id) == nullptr) {
      refuse_member(
          "declares with no name a variable another unit defines", id.value);
    }
  }
}

auto IsInternal(support::Linkage linkage) -> bool {
  switch (linkage) {
    case support::Linkage::kInternal:
      return true;
    case support::Linkage::kExternal:
    case support::Linkage::kShared:
      return false;
  }
  throw InternalError("mir verify: unknown linkage");
}

auto IsShared(support::Linkage linkage) -> bool {
  switch (linkage) {
    case support::Linkage::kShared:
      return true;
    case support::Linkage::kInternal:
    case support::Linkage::kExternal:
      return false;
  }
  throw InternalError("mir verify: unknown linkage");
}

using Named = std::optional<std::string_view>;

// What a class `owner` names that is one body's own: the class itself, where
// only one body has it.
auto InternalClass(const CompilationUnit& unit, ClassId owner) -> Named {
  if (IsInternal(LinkageOf(unit, owner))) {
    return "a class only one body has";
  }
  return std::nullopt;
}

// What naming `callable` names that is one body's own: the class declaring
// it, where only one body has that class, a function of the unit's namespace
// only one body has, or a closure, which one body's site builds. A record is no
// body's alone.
auto InternalCallable(const CompilationUnit& unit, CallableId callable)
    -> Named {
  return std::visit(
      Overloaded{
          [&](const DeclaredByClass& owner) {
            return InternalClass(unit, owner.cls);
          },
          [&](const ConstructsClass& owner) {
            return InternalClass(unit, owner.cls);
          },
          [&](const DeclaredByNamespace&) -> Named {
            if (IsInternal(
                    LinkageOf(unit, CallableTarget{.callable = callable}))) {
              return "a function only one body has";
            }
            return std::nullopt;
          },
          [](const DeclaredByClosure&) -> Named { return "a closure"; },
          [](const DeclaredByRecord&) -> Named { return std::nullopt; }},
      unit.OwnerOf(callable));
}

// What `target` names that is one body's own, or nothing. A constant of the
// unit is the value it is, which every artifact holding it holds alike, so
// naming one names nothing one body has alone; what it holds is followed
// instead. An object entry is named by the body it makes.
auto InternalNamedBy(const CompilationUnit& unit, const ReferenceTarget& target)
    -> Named {
  const auto of_class = [&](ClassId owner) {
    return InternalClass(unit, owner);
  };
  return std::visit(
      Overloaded{
          [](const LocalRef&) -> Named { return std::nullopt; },
          [&](const DefinitionRef& r) { return of_class(r.of); },
          [](const ConstantRef&) -> Named { return std::nullopt; },
          [&](const StaticPropertyRef& r) { return of_class(r.owner); },
          [](const StaticVariableRef&) -> Named { return std::nullopt; },
          [&](const FunctionRef& r) {
            return InternalCallable(unit, r.callable);
          },
          [&](const DispatchEntryRef& r) {
            return InternalCallable(unit, r.method);
          },
          [&](const DestructorRef& r) { return of_class(r.of); },
          [&](const DispatchTableRef& r) { return of_class(r.of); },
          [&](const InterfaceTableRef& r) { return of_class(r.of); },
          [](const ExternalObjectEntryRef&) -> Named { return std::nullopt; }},
      target);
}

// What `data` names that is one body's own, or nothing. That is what a body
// numbers by where it stands among its own -- a closure, a class or a function
// only it states -- since the same number in another body is another thing.
// An operation of a type is named by the type and the operation, so each
// body's is the same; a record and a builtin are no body's alone.
auto InternalNamedBy(const CompilationUnit& unit, const ExprData& data)
    -> Named {
  const auto of_class = [&](ClassId owner) {
    return InternalClass(unit, owner);
  };
  if (std::holds_alternative<ClosureExpr>(data)) {
    return "a closure";
  }
  if (const auto* access = std::get_if<FieldAccessExpr>(&data)) {
    const auto* field = std::get_if<ClassFieldTarget>(&access->field);
    return field != nullptr ? of_class(field->owner) : std::nullopt;
  }
  if (const auto* reference = std::get_if<ReferenceExpr>(&data)) {
    return InternalNamedBy(unit, reference->target);
  }
  const auto* call = std::get_if<CallExpr>(&data);
  const auto* direct =
      call != nullptr ? std::get_if<Direct>(&call->callee) : nullptr;
  if (direct == nullptr) {
    return std::nullopt;
  }
  return std::visit(
      Overloaded{
          [&](const CallableTarget& t) {
            return InternalCallable(unit, t.callable);
          },
          [](const support::BuiltinFn&) -> Named { return std::nullopt; },
          [](const TypeOperationTarget&) -> Named { return std::nullopt; },
          [](const ExternalBringUpTarget&) -> Named { return std::nullopt; }},
      direct->target);
}

[[noreturn]] void RefuseNamingInternal(
    std::string_view named, const auto& describe) {
  throw InternalError(
      std::format(
          "mir verify: {} is defined alike by every body stating it, and names "
          "{} -- please report this as a bug",
          describe(), named));
}

// What `value` holds, and every constant it reaches, names nothing one body
// has alone.
void VerifyValueNamesNothingInternal(
    const CompilationUnit& unit, const ConstantValue& value,
    const auto& describe) {
  ForEachNameReached(unit.constants, value, [&](const ReferenceTarget& to) {
    if (const Named named = InternalNamedBy(unit, to)) {
      RefuseNamingInternal(*named, describe);
    }
  });
}

void VerifyNamesNothingInternal(
    const CompilationUnit& unit, const Block& block, const auto& describe) {
  for (const Expr& expr : block.exprs) {
    if (const Named named = InternalNamedBy(unit, expr.data)) {
      RefuseNamingInternal(*named, describe);
    }
    const auto* reference = std::get_if<ReferenceExpr>(&expr.data);
    const auto* constant = reference == nullptr
                               ? nullptr
                               : std::get_if<ConstantRef>(&reference->target);
    if (constant != nullptr) {
      VerifyValueNamesNothingInternal(
          unit, unit.constants.Get(constant->constant).value, describe);
    }
  }
  for (const BlockId id : block.child_scopes.Ids()) {
    VerifyNamesNothingInternal(unit, block.child_scopes.Get(id), describe);
  }
}

// A definition every artifact stating it defines alike is kept in place of
// the others, so it has to mean the same in each: it names nothing whose
// content is one body's own. That covers its code, a class's definition, and
// every constant either reaches.
void VerifySharedCodeNamesNothingInternal(
    const CompilationUnit& unit, std::string_view when) {
  ForEachCode(unit, [&](const CodeSource& at, const CallableCode& code) {
    if (IsShared(LinkageOf(unit, at)) && code.body.has_value()) {
      VerifyNamesNothingInternal(unit, *code.body, [&] {
        return std::format("{}, {}", Describe(unit, at), when);
      });
    }
  });
  for (const ClassId id : unit.classes.Ids()) {
    const std::optional<ConstantValue>& definition =
        unit.GetClass(id).definition;
    if (!definition.has_value() || !IsShared(LinkageOf(unit, id))) {
      continue;
    }
    VerifyValueNamesNothingInternal(unit, *definition, [&] {
      return std::format(
          "the definition of {}, {}", DescribeClass(unit, id), when);
    });
  }
}

}  // namespace

auto EvaluatesNothing(
    const CompilationUnit& unit, const Block& block, ExprId id) -> bool {
  if (!ComputesNothingItself(unit, block, id)) {
    return false;
  }
  bool nothing = true;
  ForEachOperand(block.exprs.Get(id).data, [&](ExprId operand, OperandUse) {
    nothing = nothing && EvaluatesNothing(unit, block, operand);
  });
  return nothing;
}

void RequireLegalized(const CompilationUnit& unit, std::string_view reader) {
  if (unit.phase != Phase::kLegalized) {
    throw InternalError(
        std::format(
            "{}: unit '{}' reached it before it was legalized -- please "
            "report this as a bug",
            reader, unit.name));
  }
}

void VerifyBody(
    const CompilationUnit& unit, Phase phase, const CodeSource& at,
    const CallableCode& code, std::string_view when, Checks checks) {
  VerifyCode(unit, phase, code, checks, [&unit, &at, when] {
    return std::format("{}, {}", Describe(unit, at), when);
  });
}

void Verify(const CompilationUnit& unit, std::string_view when, Checks checks) {
  VerifyMembers(unit, when);
  VerifyLibraryTypes(unit, when);
  VerifyDeclaredEntries(unit, when);
  VerifySharedCodeNamesNothingInternal(unit, when);
  const auto describer = [&](const CodeSource& at) {
    return [&unit, &at, when] {
      return std::format("{}, {}", Describe(unit, at), when);
    };
  };
  ForEachCode(unit, [&](const CodeSource& at, const CallableCode& code) {
    VerifyCode(unit, unit.phase, code, checks, describer(at));
  });
  VerifyConstantsMeetTheirTypes(unit, when);
  switch (checks) {
    case Checks::kEveryBuild:
      return;
    case Checks::kAll:
      break;
  }
  // Every body is checked on its own first, since a closure handed over in one
  // is followed into the bodies that made what it captured, across the unit.
  HandedClosures handed(unit);
  ForEachCode(unit, [&](const CodeSource& at, const CallableCode& code) {
    handed.Check(code, describer(at));
  });
}

}  // namespace lyra::mir
