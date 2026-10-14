#include "lyra/mir/verify.hpp"

#include <algorithm>
#include <array>
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
#include "lyra/mir/frame_size.hpp"
#include "lyra/mir/integral_constant_folding.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/storage_part.hpp"
#include "lyra/mir/struct_decl.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/def_path.hpp"
#include "lyra/support/integral_operation.hpp"
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

// How an operation takes an operand, as clang's Sema decides whether an
// operand undergoes the lvalue-to-rvalue conversion and rustc's builder
// whether it lowers one as an operand or as a place.
enum class OperandUse : std::uint8_t {
  // As a value: a place stands here only under a read.
  kValue,
  // As the storage itself, which a read would replace with a copy: what a
  // write lands in, what a borrow or an address-of takes, the object a field
  // is reached on.
  kStorage,
  // What a read or a move takes: a place holding a value.
  kRead,
  // The object a call is entered on: named where it lies, as C++ binds an
  // implicit object argument, or already read. An entry taking it by value,
  // as a Rust method taking `self` does, is handed it read by each target's
  // realization of the call, so either stands here.
  kObject,
  // The pointer a dereference opens.
  kPointer,
};

// Calls `reach` with each node `data` takes a value from, and how it takes
// it. Every one is named in the block holding `data` but a block expression's
// value, which is named in the block of its steps and is reached from there.
void ForEachOperand(const ExprData& data, const auto& reach) {
  const auto value = [&](ExprId operand) {
    reach(operand, OperandUse::kValue);
  };
  std::visit(
      Overloaded{
          [](const StringLiteral&) {},
          [](const NullLiteral&) {},
          [](const MachineBoolLiteral&) {},
          [](const MachineIntLiteral&) {},
          [](const MachineFloatLiteral&) {},
          [](const ReferenceExpr&) {},
          [&](const UnaryExpr& e) { value(e.operand); },
          [&](const BinaryExpr& e) {
            value(e.lhs);
            value(e.rhs);
          },
          [&](const CastExpr& e) { value(e.operand); },
          [&](const DynamicCastExpr& e) { value(e.operand); },
          [&](const ConditionalExpr& e) {
            value(e.condition);
            value(e.then_value);
            value(e.else_value);
          },
          [](const BlockExpr&) {},
          [&](const AssignExpr& e) {
            reach(e.target, OperandUse::kStorage);
            value(e.value);
          },
          [&](const CallExpr& e) {
            std::visit(
                Overloaded{
                    [&](const Direct& callee) {
                      if (callee.receiver.has_value()) {
                        reach(*callee.receiver, OperandUse::kObject);
                      }
                    },
                    [&](const Indirect& callee) { value(callee.code); },
                    [](const Construct&) {},
                    [&](const Virtual& callee) {
                      reach(callee.receiver, OperandUse::kObject);
                    }},
                e.callee);
            for (const ExprId argument : e.arguments) {
              value(argument);
            }
          },
          [&](const NewExpr& e) { value(e.initializer); },
          [&](const DerefExpr& e) { reach(e.pointer, OperandUse::kPointer); },
          [&](const AddressOfExpr& e) {
            reach(e.operand, OperandUse::kStorage);
          },
          [&](const BorrowExpr& e) { reach(e.operand, OperandUse::kStorage); },
          [&](const MoveExpr& e) { reach(e.operand, OperandUse::kRead); },
          [&](const CopyExpr& e) { reach(e.place, OperandUse::kRead); },
          [&](const FieldAccessExpr& e) {
            reach(e.receiver, OperandUse::kStorage);
          },
          [&](const ClosureExpr& e) {
            for (const FieldInit& init : e.field_inits) {
              value(init.value);
            }
          },
          [&](const CompositeExpr& e) {
            for (const ExprId part : e.parts) {
              value(part);
            }
          },
          [&](const AwaitExpr& e) { value(e.execution); },
          [&](const WaitExpr& e) { value(e.park); },
          [&](const VectorGetExpr& e) {
            value(e.vector);
            value(e.index);
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
          [](const FieldAccessExpr&) { return true; },
          [](const UnaryExpr&) { return false; },
          [](const BinaryExpr&) { return false; },
          [](const CastExpr&) { return false; },
          [](const DynamicCastExpr&) { return false; },
          [](const ConditionalExpr&) { return false; },
          [](const BlockExpr&) { return false; },
          [](const AssignExpr&) { return false; },
          [](const CallExpr&) { return false; },
          [](const NewExpr&) { return false; },
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
          [](const ReferenceExpr&) -> std::string { return "a name"; },
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
  const auto as_storage = [&](ExprId id, std::string_view taker) {
    if (std::holds_alternative<CopyExpr>(block.exprs.Get(id).data)) {
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
    // A reference is constructed over the storage it refers to, which it binds
    // as a C++ reference parameter binds an lvalue, without reading it.
    const auto* call = std::get_if<CallExpr>(&data);
    const bool binds_storage =
        call != nullptr && std::holds_alternative<Construct>(call->callee) &&
        unit.types.Get(block.exprs.Get(id).type).Is<VariableRefType>();
    ForEachOperand(data, [&](ExprId operand, OperandUse use) {
      switch (binds_storage ? OperandUse::kStorage : use) {
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
                    describe(), taker, Describe(block.exprs.Get(operand).data),
                    operand.value));
          }
          return;
        // A dereference opens a pointer it reads, as clang reads the operand
        // of builtin `*p`. One that owns what it reaches is entered as an
        // object, as C++ `*unique_ptr` enters `operator*` on it.
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
          receiver.Is<ObjectWriteType>() || receiver.IsVariableRef()) {
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

// A reference is bound once, as it is created, and never re-seated; a
// reference held as a member is bound after its object is built, and again,
// only by the operations that bind one. So no store names a reference of
// either kind, and a binding names only a reference held as a member.
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
      if (unit.types.Get(block.exprs.Get(assign->target).type).IsVariableRef()) {
        refuse(id, "stores into a reference, which only a binding names");
      }
    }
    const auto* call = std::get_if<CallExpr>(&data);
    if (call == nullptr) continue;
    const std::optional<support::BuiltinFn> entry = DirectBuiltinFn(*call);
    if (entry != support::BuiltinFn::kBindMember &&
        entry != support::BuiltinFn::kBindAlias) {
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
        typed_as_reference(true);
        break;
      case support::EntryAnswer::kMutableReferenceIntoTheReceiver:
        typed_as_reference(false);
        break;
      case support::EntryAnswer::kNewValue:
      case support::EntryAnswer::kTheReceiver:
      case support::EntryAnswer::kPartOfTheReceiver:
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

// What a write lands on -- an assignment's target, the object an entry
// changing its receiver acts on, or the value a reference to bits is taken on
// -- is not reached through a read-only reference, which an element's read
// form answers and whose referent may be the element default every read of its
// type shares (LRM 7.4.6).
void VerifyWritesLandInWritableStorage(
    const CompilationUnit& unit, const Block& block, const auto& describe) {
  for (const ExprId id : block.exprs.Ids()) {
    const Expr& expr = block.exprs.Get(id);
    std::optional<ExprId> reached;
    if (const auto* write = std::get_if<AssignExpr>(&expr.data)) {
      reached = write->target;
    } else if (const auto* call = std::get_if<CallExpr>(&expr.data)) {
      const std::optional<support::BuiltinFn> entry = DirectBuiltinFn(*call);
      if (entry.has_value() &&
          (support::RuntimeEntryOf(*entry).mutates_receiver ||
           *entry == support::BuiltinFn::kSliceMut)) {
        reached = CalleeReceiver(call->callee);
      }
    }
    // A field is written through the object it is reached on, and a selected
    // part through the whole it is selected from.
    while (reached.has_value()) {
      const ExprData& data = block.exprs.Get(*reached).data;
      if (const auto* field = std::get_if<FieldAccessExpr>(&data)) {
        reached = field->receiver;
      } else if (
          const std::optional<ExprId> whole = SelectedFrom(block, *reached)) {
        reached = whole;
      } else {
        break;
      }
    }
    if (!reached.has_value()) {
      continue;
    }
    const auto* opened =
        std::get_if<DerefExpr>(&block.exprs.Get(*reached).data);
    if (opened != nullptr && ReachesReadOnly(unit.types.Get(
                                 block.exprs.Get(opened->pointer).type))) {
      throw InternalError(
          std::format(
              "mir verify: {} writes through a read-only reference (expression "
              "{} of its block) -- please report this as a bug",
              describe(), id.value));
    }
  }
  for (const BlockId id : block.child_scopes.Ids()) {
    VerifyWritesLandInWritableStorage(
        unit, block.child_scopes.Get(id), describe);
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
            [](const DynamicCastExpr&) {}, [](const ConditionalExpr&) {},
            [](const BlockExpr&) {}, [](const AssignExpr&) {},
            [](const NewExpr&) {}, [](const DerefExpr&) {},
            [](const AddressOfExpr&) {}, [](const BorrowExpr&) {},
            [](const MoveExpr&) {}, [](const CopyExpr&) {},
            [](const FieldAccessExpr&) {}, [](const ClosureExpr&) {},
            [](const CompositeExpr&) {}, [](const AwaitExpr&) {},
            [](const WaitExpr&) {}, [](const VectorGetExpr&) {}},
        expr.data);
  }
  for (const BlockId id : block.child_scopes.Ids()) {
    VerifyConstantsAreFolded(unit, block.child_scopes.Get(id), describe);
  }
}

// Storage a uniquely owning pointer or a class handle owns comes into existence
// only by a new-expression, whose initializer is evaluated into that storage
// and so has the type allocated.
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
  }
  for (const BlockId id : block.child_scopes.Ids()) {
    VerifyAllocations(unit, block.child_scopes.Get(id), describe);
  }
}

// A value too large for a frame is made only in the storage a new-expression
// allocates for it, or, where a body answers its caller, in the storage the
// caller takes its answer in; it is otherwise only read where it lies.
void VerifyLargeValuesLieInStorage(
    const CompilationUnit& unit, const Block& block, bool answers_caller,
    const auto& describe) {
  std::vector<bool> made_in_storage(block.exprs.size(), false);
  // What a write lands on names where it lands, and makes nothing: the target
  // and each part it is reached through, outermost last.
  const auto mark_target = [&](ExprId target) {
    std::optional<ExprId> reached = target;
    while (reached.has_value()) {
      made_in_storage.at(reached->value) = true;
      reached = SelectedFrom(block, *reached);
    }
  };
  for (const Expr& expr : block.exprs) {
    if (const auto* allocation = std::get_if<NewExpr>(&expr.data)) {
      made_in_storage.at(allocation->initializer.value) = true;
    }
    if (const auto* write = std::get_if<AssignExpr>(&expr.data)) {
      mark_target(write->target);
    }
  }
  if (answers_caller) {
    for (const Stmt& stmt : block.stmts) {
      if (const auto* returned = std::get_if<ReturnStmt>(&stmt.data);
          returned != nullptr && returned->value.has_value()) {
        made_in_storage.at(returned->value->value) = true;
      }
    }
  }
  for (const ExprId id : block.exprs.Ids()) {
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
        unit, block.child_scopes.Get(id), answers_caller, describe);
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
        describe);
  }
}

// `describe` names the body, and is asked only once there is something to
// report: the check runs over every body of every unit, and composing a name
// for each one costs more than the check itself.
void VerifyCode(
    const CompilationUnit& unit, const CallableCode& code,
    const auto& describe) {
  VerifyFramesHoldNoLargeValue(unit, code, describe);
  if (!code.body.has_value()) {
    return;
  }
  VerifyNoRunEvaluatesTwice(*code.body, {}, describe);
  VerifyPlacesAreRead(unit, *code.body, {}, describe);
  VerifyWritesLandInWritableStorage(unit, *code.body, describe);
  VerifyMemberReceivers(unit, *code.body, describe);
  VerifyReferenceBindings(unit, *code.body, describe);
  VerifyTypeArguments(*code.body, describe);
  VerifyOrdinals(unit, *code.body, describe);
  VerifyEntryDeclarations(unit, *code.body, describe);
  VerifyConstantsAreFolded(unit, *code.body, describe);
  VerifyAllocations(unit, *code.body, describe);
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

// The code holding an array -- fixed-size, dynamic, associative or a queue --
// asks for its element's default by the element type alone (LRM 7.4.5), and no
// call building one states it. So the unit answers for exactly the types some
// array of it has as its element: one with no default leaves the question
// unanswered, one with two leaves which body answers to chance, and a default
// no array asks for is a body nothing runs.
void VerifyTypeDefaults(const CompilationUnit& unit) {
  const auto refuse = [&](TypeId type, std::string_view defect) {
    throw InternalError(
        std::format(
            "mir verify: in unit '{}', {} (type {}) -- please report this as a "
            "bug",
            unit.name, defect, type.value));
  };
  const std::vector<TypeId> elements = ArrayElementTypes(unit.types);
  std::vector<bool> asked_for(unit.types.size(), false);
  for (const TypeId element : elements) {
    asked_for[element.value] = true;
  }
  std::vector<bool> answered(unit.types.size(), false);
  for (const TypeDefault& stated : unit.type_defaults) {
    if (!asked_for[stated.type.value]) {
      refuse(
          stated.type,
          "a default is stated for a type no array has as its element");
    }
    if (answered[stated.type.value]) {
      refuse(stated.type, "a type has two defaults stated");
    }
    answered[stated.type.value] = true;
    if (stated.code.result_type != stated.type) {
      refuse(stated.type, "a type's default answers a value of another type");
    }
    VerifyCode(unit, stated.code, [&] {
      return std::format(
          "the default of type {} in unit '{}'", stated.type.value, unit.name);
    });
  }
  for (const TypeId element : elements) {
    if (!answered[element.value]) {
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

void Verify(const CompilationUnit& unit) {
  VerifyOwnClassesNamedAsOwn(unit);
  for (const CallableId id : unit.callables.Ids()) {
    VerifyCode(unit, unit.callables.Get(id).code, [&] {
      return std::format(
          "{} of unit '{}'",
          BodyLabel(NameOf(unit.named_callables, id), id.value), unit.name);
    });
  }
  VerifyTypeDefaults(unit);
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
