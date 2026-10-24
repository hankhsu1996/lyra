#include "lyra/mir/verify.hpp"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <format>
#include <iterator>
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
#include "lyra/mir/block_id.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/closure_decl.hpp"
#include "lyra/mir/closure_signature.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/declared_class.hpp"
#include "lyra/mir/dump.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/external_class.hpp"
#include "lyra/mir/frame_size.hpp"
#include "lyra/mir/integral_constant_folding.hpp"
#include "lyra/mir/object_lifetime.hpp"
#include "lyra/mir/operand_use.hpp"
#include "lyra/mir/record_decl.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/storage_part.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/callable_bounds.hpp"
#include "lyra/support/def_path.hpp"
#include "lyra/support/integral_operation.hpp"
#include "lyra/support/value_operation.hpp"

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

// Whether a node computes nothing of its own: it names a declared thing, spells
// a constant, forms a place over what it reaches, or reads one. What happens
// where such a node stands is that place's, so one of them reached from two
// places and two of them are the same program, and the evaluations it stands
// for are those of what it reaches.
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
          [](const BorrowExpr&) { return true; },
          [](const CopyExpr&) { return true; },
          [](const DiscriminantExpr&) { return true; },
          [](const FieldAccessExpr&) { return true; },
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
          [](const UnionExpr&) { return false; },
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
          [](const ClosureExpr&) -> std::string {
            return "a closure construction";
          },
          [](const CompositeExpr&) -> std::string { return "a composite"; },
          [](const UnionExpr&) -> std::string { return "a union literal"; },
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
  ForEachOperand(data, [&](ExprId operand, OperandUse) {
    Reach(block, reached, operand, id, under);
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

// A place stands as a value only under a read, and a read reads a place that
// holds a value. Storage with no value to read stands as itself wherever it is
// handed on, and what takes the storage itself is never handed a read, which
// would make it act on a copy.
//
// `entered_at` is what the enclosing block's block expressions name as their
// value among this block's nodes.
void VerifyPlacesAreRead(
    const CompilationUnit& unit, const Block& block,
    std::span<const ExprId> entered_at, const auto& describe) {
  const auto as_value = [&](ExprId id, std::string_view taker) {
    if (IsReadablePlace(unit.types, block.exprs.Get(id))) {
      throw InternalError(
          std::format(
              "mir verify: in {}, {} takes {} (expression {} of its block) as "
              "a value, and a place stands as one only under a read",
              describe(), taker, Describe(block.exprs.Get(id).data), id.value));
    }
  };
  // A read of overlaid storage is the value a read through that member makes
  // of the bits there, which lies in no storage, so it is held as any value
  // is.
  const auto as_storage = [&](ExprId id, std::string_view taker) {
    if (std::holds_alternative<CopyExpr>(block.exprs.Get(id).data) &&
        ValueLiesInStorage(unit, block, id)) {
      throw InternalError(
          std::format(
              "mir verify: in {}, {} takes a read (expression {} of its "
              "block) where it takes the storage itself, so it would act on a "
              "copy",
              describe(), taker, id.value));
    }
  };
  for (const StmtId id : block.root_stmts) {
    ForEachExpr(block.stmts.Get(id).data, [&](ExprId operand) {
      as_value(operand, "a statement");
    });
  }
  for (const ExprId id : entered_at) {
    as_value(id, "a block expression");
  }
  std::vector<std::vector<ExprId>> entries(block.child_scopes.size());
  for (const ExprId id : block.exprs.Ids()) {
    const ExprData& data = block.exprs.Get(id).data;
    const std::string taker = Describe(data);
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
                        describe(), taker,
                        Describe(block.exprs.Get(operand).data),
                        operand.value));
              }
              return;
            // A dereference opens a pointer it reads, as clang reads the
            // operand of builtin `*p`. One that owns what it reaches is entered
            // as an object, as C++ `*unique_ptr` enters `operator*` on it.
            case OperandUse::kPointer:
              if (unit.types.Get(block.exprs.Get(operand).type).OwnsPointee()) {
                as_storage(operand, taker);
              } else {
                as_value(operand, taker);
              }
              return;
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
  const auto* closure = std::get_if<ClosureTarget>(&direct->target);
  if (closure == nullptr) {
    return false;
  }
  const ClosureDecl& decl = unit.GetClosure(closure->closure);
  switch (ClosureKindOf(unit.types, decl)) {
    case support::ClosureKind::kFn:
    case support::ClosureKind::kFnMut:
      return false;
    case support::ClosureKind::kFnOnce:
      return handed == decl.invoke.locals.Get(*decl.invoke.receiver).type;
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
          receiver.Is<ObjectWriteType>() || receiver.IsVariableRef()) {
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
         t.Is<ReferenceType>() || t.Is<BitsReferenceType>();
}

// A reference is bound once, as it is created, and never re-seated; a
// reference held as a member is bound after its object is built, and again,
// only by the operations that bind one. So no store names a reference of
// either kind, and a binding names only a reference held as a member. A local
// referring to storage it does not own is bound by its declaration alone, so
// what it refers to is what its initializer reached.
void VerifyReferenceBindings(
    const CompilationUnit& unit, const Block& block, const auto& describe) {
  const auto refuse = [&](ExprId id, std::string_view what) {
    throw InternalError(
        std::format(
            "mir verify: {} {} (expression {} of its block) -- please report "
            "this as a bug",
            describe(), what, id.value));
  };
  for (const ExprId id : block.exprs.Ids()) {
    const ExprData& data = block.exprs.Get(id).data;
    if (const auto* assign = std::get_if<AssignExpr>(&data)) {
      const TypeId target = block.exprs.Get(assign->target).type;
      if (unit.types.Get(target).IsVariableRef()) {
        refuse(id, "stores into a reference, which only a binding names");
      }
      if (ReferencedLocal(block.exprs.Get(assign->target).data).has_value() &&
          RefersWithoutOwning(unit.types, target)) {
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

// A callee generic over a type states the type it is called at, and no other
// callee states one: a target that resolves types where the call is written
// names the type there, so one left out is a call it cannot write, and one a
// callee does not take is a type argument the library never declared. A
// factory is reached on the type it builds, so the type it is called at is the
// type the call answers with.
void VerifyTypeArguments(const Block& block, const auto& describe) {
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
      if (direct->type_argument.has_value()) {
        refuse(id, "states a type argument to a callee generic over none");
      }
      continue;
    }
    const support::RuntimeEntry declared = support::RuntimeEntryOf(*entry);
    if (declared.takes_a_type_argument && !direct->type_argument.has_value()) {
      refuse(id, "calls an entry generic over a type and states none");
    }
    if (!declared.takes_a_type_argument && direct->type_argument.has_value()) {
      refuse(id, "states a type argument to an entry generic over none");
    }
    if (direct->type_argument.has_value() &&
        std::holds_alternative<support::StaticFactory>(declared.declaration) &&
        *direct->type_argument != expr.type) {
      refuse(
          id,
          "calls a factory at one type and answers with a value of "
          "another");
    }
  }
  for (const BlockId id : block.child_scopes.Ids()) {
    VerifyTypeArguments(block.child_scopes.Get(id), describe);
  }
}

// A call naming an operation of a type the unit states per type, or a method of
// a record the unit declares, names a body the unit states; one naming another
// leaves the call bound to nothing.
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
    // A method of a record this unit declares is one its declaration states.
    if (const auto* method = std::get_if<RecordMethodTarget>(&direct->target);
        method != nullptr && method->declaration.unit_name == unit.name) {
      const auto records = unit.records.Ids();
      const auto declared = std::ranges::find_if(records, [&](RecordId record) {
        return unit.GetRecord(record).path == method->declaration.path;
      });
      if (declared == records.end() ||
          std::ranges::none_of(
              unit.GetRecordMethods(*declared), [&](const RecordMethod& m) {
                return m.answers == method->answers;
              })) {
        throw InternalError(
            std::format(
                "mir verify: {} calls the method \"{}\" of record '{}' "
                "(expression {} of its block), which its declaration does not "
                "state -- please report this as a bug",
                describe(), support::ValueOperationName(method->answers),
                support::DisplayOf(method->declaration.path), id.value));
      }
    }
    const auto* target = std::get_if<TypeOperationTarget>(&direct->target);
    if (target != nullptr &&
        FindTypeOperation(unit, target->type, target->answers) == nullptr) {
      throw InternalError(
          std::format(
              "mir verify: {} calls the operation \"{}\" of type {} "
              "(expression {} of its block), which its unit states no body "
              "for -- please report this as a bug",
              describe(), support::ValueOperationName(target->answers),
              target->type.value, id.value));
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
    if (const auto* built = std::get_if<UnionExpr>(&expr.data)) {
      const std::optional<std::span<const TypeId>> members =
          OverlaidMembers(unit, expr.type);
      if (!members.has_value() || built->member.value >= members->size()) {
        throw InternalError(
            std::format(
                "mir verify: {} builds a union holding a member its type does "
                "not have (expression {} of its block) -- please report this "
                "as a bug",
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
// a slice does, of bits and of elements alike (LRM 7.4.6, 11.5.1), which is
// what its first operand is.
auto StartsAtAnOrdinal(std::optional<support::PartSelection> selects) -> bool {
  if (!selects.has_value()) {
    return false;
  }
  switch (*selects) {
    case support::PartSelection::kSlice:
    case support::PartSelection::kBits:
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
// default where its position names no element (LRM 7.4.6).
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
// states over values of several kinds is named only where its first operand is
// not integral, the operation over integral values being an entry of its own;
// an argument the entry reads as a machine value is no integral value, which
// would cross with nothing telling the entry how to read its bits; and an
// entry reaching by a key is called on a container reached by keys, one
// reaching by a position on something that numbers its parts; and an entry
// answering a reference is typed as that reference.
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
    const std::optional<ExprId> receiver = CalleeReceiver(call->callee);
    const std::optional<ExprId> first = FirstOperand(*call);
    if (declared.over_integral_values.has_value() && first.has_value() &&
        type_of(*first).IsIntegral()) {
      refuse(
          id,
          "names an entry over values of several kinds where its first "
          "operand is integral");
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
      case support::EntryAnswer::kExecution:
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
                           .Is<AssociativeArrayType>();
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
// target, the object an entry changing its receiver acts on, or the value a
// reference to bits is taken on -- is not reached through a read-only
// reference, which an element's read form answers and whose referent may be
// the element default every read of its type shares (LRM 7.4.6).
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
          (support::RuntimeEntryOf(*entry).mutates_receiver ||
           *entry == support::BuiltinFn::kSliceMut)) {
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
      } else if (const auto* opened = std::get_if<DerefExpr>(&data)) {
        if (ReachesReadOnly(
                unit.types.Get(block.exprs.Get(opened->pointer).type))) {
          throw InternalError(
              std::format(
                  "mir verify: {} writes through a read-only reference "
                  "(expression {} of its block) -- please report this as a bug",
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

// Whether a cast may read a value of `from` as a value of `to`: each pair is
// one every target writes in its own conversion syntax -- C++'s cast notation,
// an LLVM cast instruction -- with the value unchanged or read as the machine
// boolean a condition tests (LRM 12.4). An integral value is read at a type
// laying it out alike, an enumeration's and its base's; a machine integer at
// another width; a pointer at another pointee, as a code address is at another
// signature; a reference at the same referent made read-only, as clang states
// a qualification conversion (LRM 13.5.2); and a reference held as a member
// read as the reference it holds, at the same referent and never more
// writable. A value moved between representations, and a handle seen as
// another class's, is a call.
auto CastReadsAsAnother(const TypePool& types, TypeId from, TypeId to) -> bool {
  const Type& source = types.Get(from);
  const Type& destination = types.Get(to);
  const auto is_machine_integer = [](const Type& type) {
    return type.Is<MachineIntType>() || type.Is<MachineBoolType>();
  };
  if (source.IsIntegral() && destination.IsIntegral()) {
    return source.Integral() == destination.Integral();
  }
  if (destination.Is<MachineBoolType>()) {
    return is_machine_integer(source) || source.IsIntegral() ||
           source.IsRealFamily() || source.Is<ManagedRefType>() ||
           source.Is<ChandleType>();
  }
  if (is_machine_integer(source) && is_machine_integer(destination)) {
    return true;
  }
  if ((source.Is<PointerType>() && destination.Is<PointerType>()) ||
      (source.Is<MachineFunctionType>() &&
       destination.Is<MachineFunctionType>())) {
    return true;
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

// Every value handed to something of a type it states is a value of that
// type, as an assignment-like context converts its value to its target's type
// (LRM 10.8): what an assignment writes and its target, what a whole write
// through a wrapper or a reference to bits writes and what the wrapper holds
// or the bits reach, a local's initializer, what a body answers, an arm of a
// choice, the value a block ends with, what a capture is built from, and an
// argument of a callable this unit declares. A value only alike in
// representation is converted where it is built, so one handed over as it is
// is a value the program never stated.
void VerifyValuesMeetTheirTypes(
    const CompilationUnit& unit, const CallableCode& code, const Block& block,
    const auto& describe) {
  const auto refuse = [&](std::string_view what, TypeId expected,
                          TypeId handed) {
    throw InternalError(
        std::format(
            "mir verify: {} hands {} a value of type {} where it states type "
            "{} -- please report this as a bug",
            describe(), what, DumpType(unit, handed),
            DumpType(unit, expected)));
  };
  const auto type_of = [&](const Block& in, ExprId id) {
    return in.exprs.Get(id).type;
  };
  const Type& result = unit.types.Get(code.result_type);
  const auto* coroutine = result.As<CoroutineType>();
  const TypeId answered =
      coroutine != nullptr ? coroutine->payload : code.result_type;
  for (const Stmt& stmt : block.stmts) {
    if (const auto* decl = std::get_if<LocalDeclStmt>(&stmt.data)) {
      const TypeId declared = code.locals.Get(decl->target).type;
      if (type_of(block, decl->init) != declared) {
        refuse("a local's initializer", declared, type_of(block, decl->init));
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
          type_of(block, *returned->value) != answered) {
        refuse("what it answers", answered, type_of(block, *returned->value));
      }
    }
  }
  for (const ExprId id : block.exprs.Ids()) {
    const Expr& expr = block.exprs.Get(id);
    const auto* call = std::get_if<CallExpr>(&expr.data);
    if (const auto* write = std::get_if<AssignExpr>(&expr.data);
        write != nullptr &&
        type_of(block, write->target) != type_of(block, write->value)) {
      refuse(
          "an assignment's target", type_of(block, write->target),
          type_of(block, write->value));
    }
    if (call != nullptr && DirectBuiltinFn(*call) == support::BuiltinFn::kSet) {
      const Type& through =
          unit.types.Get(type_of(block, *CalleeReceiver(call->callee)));
      const auto* bits = through.As<BitsReferenceType>();
      const TypeId held =
          bits != nullptr ? bits->bits : through.WrappedValueType();
      if (type_of(block, call->arguments.at(0)) != held) {
        refuse(
            "a whole write's storage", held,
            type_of(block, call->arguments.at(0)));
      }
    }
    if (const auto* cast = std::get_if<CastExpr>(&expr.data);
        cast != nullptr &&
        !CastReadsAsAnother(
            unit.types, type_of(block, cast->operand), expr.type)) {
      throw InternalError(
          std::format(
              "mir verify: {} casts a value of type {} to type {}, which a "
              "cast "
              "does not join -- please report this as a bug",
              describe(), DumpType(unit, type_of(block, cast->operand)),
              DumpType(unit, expr.type)));
    }
    if (const auto* choice = std::get_if<ConditionalExpr>(&expr.data)) {
      for (const ExprId arm : {choice->then_value, choice->else_value}) {
        if (type_of(block, arm) != expr.type) {
          refuse("an arm of a choice", expr.type, type_of(block, arm));
        }
      }
    }
    if (const auto* steps = std::get_if<BlockExpr>(&expr.data)) {
      const Block& inner = block.child_scopes.Get(steps->scope);
      if (type_of(inner, steps->value) != expr.type) {
        refuse(
            "the value a block ends with", expr.type,
            type_of(inner, steps->value));
      }
    }
    if (const auto* closure = std::get_if<ClosureExpr>(&expr.data)) {
      const ClosureDecl& decl = unit.GetClosure(closure->closure);
      for (const FieldInit& init : closure->field_inits) {
        const TypeId field = decl.fields.Get(init.target).type;
        if (type_of(block, init.value) != field) {
          refuse("a capture", field, type_of(block, init.value));
        }
      }
    }
    const auto* direct =
        call != nullptr ? std::get_if<Direct>(&call->callee) : nullptr;
    if (direct == nullptr) {
      continue;
    }
    const CallableCode* callee = nullptr;
    if (const auto* method = std::get_if<CallableTarget>(&direct->target)) {
      callee = &unit.GetClass(method->owner).callables.Get(method->slot).code;
    } else if (
        const auto* free = std::get_if<UnitCallableTarget>(&direct->target)) {
      callee = &unit.callables.Get(free->slot).code;
    } else if (
        const auto* constructor =
            std::get_if<ConstructorTarget>(&direct->target)) {
      if (const auto* own = std::get_if<IntraUnitClassRef>(&constructor->cls);
          own != nullptr && unit.GetClass(own->class_id).constructor) {
        callee = &unit.GetClass(own->class_id).constructor->code;
      }
    }
    if (callee == nullptr) {
      continue;
    }
    std::vector<TypeId> parameters;
    for (const LocalId param : callee->params) {
      if (param != callee->receiver) {
        parameters.push_back(callee->locals.Get(param).type);
      }
    }
    if (parameters.size() != call->arguments.size()) {
      throw InternalError(
          std::format(
              "mir verify: {} calls a callable with {} arguments where it "
              "takes "
              "{} -- please report this as a bug",
              describe(), call->arguments.size(), parameters.size()));
    }
    for (std::size_t i = 0; i < parameters.size(); ++i) {
      if (type_of(block, call->arguments[i]) != parameters[i]) {
        refuse(
            "an argument of a callable", parameters[i],
            type_of(block, call->arguments[i]));
      }
    }
  }
  for (const BlockId id : block.child_scopes.Ids()) {
    VerifyValuesMeetTheirTypes(
        unit, code, block.child_scopes.Get(id), describe);
  }
}

// Storage with no address of its own -- a member of a union, and what lies
// under one -- is never what a reference the program holds ends on (LRM
// 13.5.2), so nothing takes its address. Storage another member of an
// untagged union overlays holds bits that make a value of its type only once
// read, so a borrow binds its read, and a call is entered on it only to reach
// a part of it or to change it.
void VerifyStorageWithoutAnAddress(
    const CompilationUnit& unit, const Block& block, const auto& describe) {
  const auto kind_of = [&](ExprId operand) {
    return IsPlace(block.exprs.Get(operand).data)
               ? KindOfPlace(unit, block, operand)
               : PlaceKind::kOrdinary;
  };
  const auto refuse = [&](ExprId id, std::string_view what) {
    throw InternalError(
        std::format(
            "mir verify: {} {} (expression {} of its block) -- please report "
            "this as a bug",
            describe(), what, id.value));
  };
  for (const ExprId id : block.exprs.Ids()) {
    const ExprData& data = block.exprs.Get(id).data;
    if (const auto* address = std::get_if<AddressOfExpr>(&data)) {
      if (kind_of(address->operand) != PlaceKind::kOrdinary) {
        refuse(id, "takes the address of storage with none of its own");
      }
    } else if (const auto* borrow = std::get_if<BorrowExpr>(&data)) {
      if (kind_of(borrow->operand) == PlaceKind::kOverlaid) {
        refuse(id, "borrows storage another member of a union overlays");
      }
    } else if (const auto* call = std::get_if<CallExpr>(&data)) {
      const std::optional<ExprId> receiver = CalleeReceiver(call->callee);
      if (!receiver.has_value() || kind_of(*receiver) != PlaceKind::kOverlaid) {
        continue;
      }
      // Only a step into a part, and a write of a window of elements, which
      // writes what it is handed without reading what it replaces, are
      // entered where overlaid storage lies; a method changing such storage
      // reads what it changes, so it is entered on a read of it, written back.
      const std::optional<support::BuiltinFn> entry = DirectBuiltinFn(*call);
      if (!entry.has_value() ||
          (!SelectionOf(*call).has_value() &&
           *entry != support::BuiltinFn::kAssignElementSlice)) {
        refuse(
            id,
            "enters a call on storage another member of a union overlays "
            "without reading it");
      }
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
            [](const MachineFloatLiteral&) {}, [](const ReferenceExpr&) {},
            [](const ConditionalExpr&) {}, [](const BlockExpr&) {},
            [](const AssignExpr&) {}, [](const NewExpr&) {},
            [](const DerefExpr&) {}, [](const AddressOfExpr&) {},
            [](const BorrowExpr&) {}, [](const MoveExpr&) {},
            [](const CopyExpr&) {}, [](const DiscriminantExpr&) {},
            [](const FieldAccessExpr&) {}, [](const ClosureExpr&) {},
            [](const CompositeExpr&) {}, [](const UnionExpr&) {},
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
// object once it is created whole.
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
    if (!allocated.has_value()) {
      refuse(id, "yields a type that owns no storage it allocates");
    }
    if (block.exprs.Get(allocation->initializer).type != *allocated) {
      refuse(id, "initializes its storage from a value of another type");
    }
    if (unit.types.Get(*allocated).Is<ObjectType>()) {
      const auto* construction =
          std::get_if<CallExpr>(&block.exprs.Get(allocation->initializer).data);
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
    if (std::get<Direct>(call->callee).type_argument != erasure->erased) {
      RefuseErasure(
          id, "at a type other than the one its answer holds", describe);
    }
    if (erasure->erased !=
        ErasedTypeOf(unit.types, unit.GetClosure(erasure->closure))) {
      RefuseErasure(
          id, "at a kind or a signature its body does not have", describe);
    }
  }
  for (const BlockId id : block.child_scopes.Ids()) {
    VerifyErasures(unit, block.child_scopes.Get(id), describe);
  }
}

// A value too large for a frame is made only in the storage a new-expression
// allocates for it, or, where a body answers its caller, in the storage the
// caller takes its answer in; it is otherwise only read where it lies. A
// conditional made in storage makes the arm it chooses there, and a block
// expression its value, as clang and rustc evaluate both into the destination
// of what yields them. `made_by_enclosing` are the nodes of `block` the block
// enclosing it makes in storage this way. The address of code takes such a
// value in a box, as the body it addresses does.
void VerifyLargeValuesLieInStorage(
    const CompilationUnit& unit, const Block& block, bool answers_caller,
    std::span<const ExprId> made_by_enclosing, const auto& describe) {
  std::vector<ExprId> made(made_by_enclosing.begin(), made_by_enclosing.end());
  for (const Expr& expr : block.exprs) {
    if (const auto* allocation = std::get_if<NewExpr>(&expr.data)) {
      made.push_back(allocation->initializer);
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
        ValueLiesInStorage(unit, block, id)) {
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

// No body keeps a value too large for a frame in one: every such local is held
// through a box.
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

// `describe` names the body, and is asked only once there is something to
// report: the check runs over every body of every unit, and composing a name
// for each one costs more than the check itself.
void VerifyCode(
    const CompilationUnit& unit, Phase phase, const CallableCode& code,
    const auto& describe) {
  switch (phase) {
    case Phase::kBuilt:
      break;
    case Phase::kForBackends:
      VerifyFramesHoldNoLargeValue(unit, code, describe);
      break;
  }
  if (!code.body.has_value()) {
    return;
  }
  VerifyNoRunEvaluatesTwice(*code.body, {}, describe);
  VerifyPlacesAreRead(unit, *code.body, {}, describe);
  VerifyWritesLandInWritableStorage(unit, *code.body, describe);
  VerifyValuesMeetTheirTypes(unit, code, *code.body, describe);
  VerifyStorageWithoutAnAddress(unit, *code.body, describe);
  VerifyMemberReceivers(unit, *code.body, describe);
  VerifyReferenceBindings(unit, *code.body, describe);
  VerifyTypeArguments(*code.body, describe);
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

// Whether a value of `type` hands closures or started bodies over: a box or a
// borrow of a closure of no known type, a started body, or a run of them.
auto HandsCallablesOver(const TypePool& types, TypeId type) -> bool {
  const Type& t = types.Get(type);
  if (const auto* run = t.As<MachineArrayType>()) {
    return HandsCallablesOver(types, run->element);
  }
  if (t.Is<CoroutineType>()) {
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
  const auto floating = [&](MachineFloatWidth width) {
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
      return floating(MachineFloatWidth::k32);
    case support::MachineKind::kDouble:
      return floating(MachineFloatWidth::k64);
    // A closure is called with machine values one at a time, so no structure
    // crosses whole.
    case support::MachineKind::kAggregate:
      return false;
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

// Whether a value of `type`, which hands closures or started bodies over,
// hands over what `trait` says the entry taking it asks for: a box or a borrow
// of a closure of no known type whose kind serves the kind the entry asks, at
// the signature it calls it at, or a started body, alone or in a run of them.
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

  // The closures `id` hands over: made there, boxed, borrowed, made one of no
  // known type, gathered, or started as a body that takes its box over. A body
  // started that is no closure's hands none.
  auto Handed(const CallableCode& code, const Block& block, ExprId id)
      -> std::vector<ClosureId> {
    const ExprData& data = block.exprs.Get(id).data;
    if (const auto* made = std::get_if<ClosureExpr>(&data)) {
      return {made->closure};
    }
    if (const auto* boxed = std::get_if<NewExpr>(&data)) {
      return Handed(code, block, boxed->initializer);
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
      if (direct != nullptr &&
          std::holds_alternative<ClosureTarget>(direct->target) &&
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
    const std::optional<ObjectLifetime> lifetime =
        ObjectLifetimeOf(*unit_, pointee);
    if (!lifetime.has_value()) {
      return std::nullopt;
    }
    switch (*lifetime) {
      case ObjectLifetime::kTheRun:
        return LivesForTheWholeRun{};
      case ObjectLifetime::kWhileReferenced:
        return LivesWithTheObject{};
    }
    throw InternalError("mir verify: unknown ObjectLifetime");
  }

  // Where the storage `id` refers to lies, followed back through the steps
  // that reach it to the binding it starts from.
  auto ReachOf(const CallableCode& code, const Block& block, ExprId id)
      -> Reached {
    // A pointer to an object reaches storage that lives as long as the object
    // does, wherever the pointer came from.
    const auto* pointer =
        unit_->types.Get(block.exprs.Get(id).type).As<PointerType>();
    if (pointer != nullptr &&
        pointer->ownership == PointerOwnership::kBorrowed) {
      if (const std::optional<Reached> lives =
              ReachOfObject(pointer->pointee)) {
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
            [&](const DerefExpr& deref) { return from(deref.pointer); },
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
            [&](const MachineFloatLiteral&) -> Reached {
              RefuseUnfollowed(id);
            },
            [&](const UnaryExpr&) -> Reached { RefuseUnfollowed(id); },
            [&](const BinaryExpr&) -> Reached { RefuseUnfollowed(id); },
            [&](const ConditionalExpr&) -> Reached { RefuseUnfollowed(id); },
            [&](const BlockExpr&) -> Reached { RefuseUnfollowed(id); },
            [&](const AssignExpr&) -> Reached { RefuseUnfollowed(id); },
            [&](const NewExpr&) -> Reached { RefuseUnfollowed(id); },
            [&](const DiscriminantExpr&) -> Reached { RefuseUnfollowed(id); },
            [&](const ClosureExpr&) -> Reached { RefuseUnfollowed(id); },
            [&](const CompositeExpr&) -> Reached { RefuseUnfollowed(id); },
            [&](const UnionExpr&) -> Reached { RefuseUnfollowed(id); },
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
            [](const EnumTableRef&) -> Reached {
              return LivesForTheWholeRun{};
            },
            [](const IntegralConstantRef&) -> Reached {
              return LivesForTheWholeRun{};
            },
            [](const StaticPropertyRef&) -> Reached {
              return LivesForTheWholeRun{};
            },
            [](const StaticVariableRef&) -> Reached {
              return LivesForTheWholeRun{};
            },
            [](const ExternalUnitVariableRef&) -> Reached {
              return LivesForTheWholeRun{};
            },
            [](const ExternalStaticPropertyRef&) -> Reached {
              return LivesForTheWholeRun{};
            },
            [](const ClassConstantRef&) -> Reached {
              return LivesForTheWholeRun{};
            },
            [](const FunctionRef&) -> Reached { return LivesForTheWholeRun{}; },
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
                case support::EntryAnswer::kExecution:
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

// A class of this unit has one identity here, its id, however the name
// reaching it was written -- through another instance of this unit, or a
// signature naming this unit. Naming it as another unit's class would give one
// class two identities in one unit, which both backends would then have to
// agree are one.
void VerifyOwnClassesNamedAsOwn(
    const CompilationUnit& unit, std::string_view when) {
  const auto refuse = [&](const support::DefPath& class_path,
                          std::string_view where) {
    throw InternalError(
        std::format(
            "mir verify: unit '{}', {}, names its own class '{}' as another "
            "unit's, in {} -- please report this as a bug",
            unit.name, when, support::DisplayOf(class_path), where));
  };
  for (const Type& type : unit.types) {
    const auto* object = type.As<ObjectType>();
    if (object == nullptr) continue;
    const auto* other = std::get_if<CrossUnitClassRef>(&object->of);
    if (other != nullptr && other->unit_name == unit.name) {
      refuse(other->class_path, "a type");
    }
  }
  // What a record of another unit's class names may lead back to this unit,
  // where that class's lineage passes through one of its own; a reference in a
  // body to another unit's class is checked where it consumed the signature.
  const auto refuse_own = [&](const CrossUnitClassRef* other,
                              std::string_view where) {
    if (other != nullptr && other->unit_name == unit.name) {
      refuse(other->class_path, where);
    }
  };
  for (const ExternalClass& record : unit.external_classes) {
    if (record.unit_name == unit.name) {
      refuse(record.class_path, "its record of what a unit published");
    }
    if (record.base.has_value()) {
      refuse_own(
          std::get_if<CrossUnitClassRef>(&*record.base),
          "the base of its record of another unit's class");
    }
    for (const DeclaredClassRef& iface : record.implements) {
      refuse_own(
          std::get_if<CrossUnitClassRef>(&iface),
          "what its record of another unit's class implements");
    }
    for (const PublishedOverride& overriding : record.overrides) {
      const auto* other =
          std::get_if<ExternalVirtualSlot>(&overriding.behavior);
      if (other != nullptr && other->unit_name == unit.name) {
        refuse(
            other->class_path,
            "a behavior its record of another unit's class overrides");
      }
    }
  }
  for (const ConsumedSignature& consumed : unit.consumed_signatures) {
    const auto* cls = std::get_if<ConsumedClass>(&consumed);
    if (cls != nullptr && cls->unit_name == unit.name) {
      refuse(cls->class_path, "what it consumed of a signature");
    }
  }
}

// Every element type of an array -- fixed-size, dynamic, associative or a
// queue -- has its default stated, since the code holding the array asks for
// it by the element type alone (LRM 7.4.5); a type whose operations are stated
// per unit has its other operations stated, and no other type has any. A pair
// stated twice leaves which body answers to chance.
void VerifyTypeOperations(const CompilationUnit& unit, std::string_view when) {
  const auto refuse = [&](TypeId type, std::string_view defect) {
    throw InternalError(
        std::format(
            "mir verify: in unit '{}', {}, {} (type {}) -- please report this "
            "as a bug",
            unit.name, when, defect, DumpType(unit, type)));
  };
  const support::ValueOperation default_value =
      support::BuiltinFn::kDeclaredDefault;
  const std::vector<TypeId> elements = ArrayElementTypes(unit.types);
  std::vector<bool> is_element(unit.types.size(), false);
  for (const TypeId element : elements) {
    is_element[element.value] = true;
  }
  std::set<std::pair<TypeId, support::ValueOperation>> answered;
  for (const TypeOperation& stated : unit.type_operations) {
    if (!OperationsAreStatedPerUnit(unit.types.Get(stated.type)) &&
        (stated.answers != default_value || !is_element[stated.type.value])) {
      refuse(
          stated.type,
          "an operation is stated for a type whose operations are not stated "
          "per unit, or a default for a type no array holds");
    }
    if (!answered.emplace(stated.type, stated.answers).second) {
      refuse(stated.type, "a type has one operation stated twice");
    }
    if (stated.answers == default_value &&
        stated.code.result_type != stated.type) {
      refuse(stated.type, "a type's default answers a value of another type");
    }
  }
  for (const TypeId element : elements) {
    if (!answered.contains({element, default_value})) {
      refuse(element, "the element type of an array has no default stated");
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
  ForEachOperand(data, [&](ExprId operand, OperandUse) {
    nothing = nothing && EvaluatesNothing(block, operand);
  });
  return nothing;
}

void Verify(const CompilationUnit& unit, Phase phase, std::string_view when) {
  VerifyOwnClassesNamedAsOwn(unit, when);
  VerifyTypeOperations(unit, when);
  const auto describer = [&](const CodeSource& at) {
    return [&unit, &at, when] {
      return std::format("{}, {}", Describe(unit, at), when);
    };
  };
  ForEachCode(unit, [&](const CodeSource& at, const CallableCode& code) {
    VerifyCode(unit, phase, code, describer(at));
  });
  // Every body is checked on its own first, since a closure handed over in one
  // is followed into the bodies that made what it captured.
  HandedClosures handed(unit);
  ForEachCode(unit, [&](const CodeSource& at, const CallableCode& code) {
    handed.Check(code, describer(at));
  });
}

}  // namespace lyra::mir
