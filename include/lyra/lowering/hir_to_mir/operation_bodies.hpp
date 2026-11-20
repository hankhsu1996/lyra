#pragma once

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/base/diag_message.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/predicate.hpp"
#include "lyra/lowering/hir_to_mir/value_operations.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/type_interface.hpp"
#include "lyra/mir/value_parts.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::lowering::hir_to_mir {

// The rows of `first` followed by those of `second`, which a kind's table of
// bodies is built of: its own methods' rows, then its language traits'.
template <typename Row, std::size_t M, std::size_t N>
consteval auto Concatenated(
    const std::array<Row, M>& first, const std::array<Row, N>& second)
    -> std::array<Row, M + N> {
  std::array<Row, M + N> rows{};
  std::ranges::copy(first, rows.begin());
  std::ranges::copy(second, rows.begin() + M);
  return rows;
}

// The receiver and the operands of a call of the operation `declared`
// declares, each handed as its parameter takes it: borrowed where it takes a
// reference, mutably where the operation writes through it, and as the value
// where it takes the value. The operands follow the receiver among the
// parameters, and where the operation reports where it was called the call
// names that last.
struct PassedOperands {
  std::optional<mir::ExprId> receiver;
  std::vector<mir::ExprId> arguments;
};
[[nodiscard]] auto PassedAsDeclared(
    const mir::CompilationUnit& unit, mir::Builder block,
    const mir::CallableCode& declared, std::optional<mir::ExprId> receiver,
    std::vector<mir::ExprId> operands) -> PassedOperands;

// A call of the body `target` names, which answers `answers` of `type`, handing
// it each value as the operation's declaration takes it: borrowed where it
// takes a reference, as the value where it takes the value.
[[nodiscard]] auto OwnOperationCall(
    const mir::CompilationUnit& unit, mir::Builder block, mir::TypeId type,
    mir::DirectTarget target, support::ValueOperation answers,
    std::optional<mir::ExprId> receiver, std::vector<mir::ExprId> operands,
    mir::TypeId result) -> mir::ExprId;

// What every body answering an operation of one type is built from: the type,
// its members' types, and how a body names another operation of the same
// type. Each body takes the parameters its operation takes of any value, and
// returns one expression built from the members.
class OperationBodies {
 public:
  // How a body names the body answering another operation of the type.
  using OwnTarget = std::function<mir::DirectTarget(support::ValueOperation)>;

  OperationBodies(
      const mir::CompilationUnit& unit, mir::TypeId whole,
      std::vector<mir::TypeId> members, OwnTarget own)
      : unit_(&unit),
        whole_(whole),
        members_(std::move(members)),
        equality_answer_(mir::EqualityAnswerType(unit, whole)),
        own_(std::move(own)) {
  }

 protected:
  // `then_value` where `condition` holds and `else_value` where it does not,
  // both of `type`.
  static auto Chosen(
      mir::Builder block, mir::ExprId condition, mir::ExprId then_value,
      mir::ExprId else_value, mir::TypeId type) -> mir::ExprId {
    return block.Add(
        mir::Expr{
            .data =
                mir::ConditionalExpr{
                    .condition = condition,
                    .then_value = then_value,
                    .else_value = else_value},
            .type = type});
  }

  // A value of `type` with the `count` positions from `start` of `into` taken
  // from `from`, both of that type, where the run names at least one position.
  // `start` and `count` are where the two machine numbers lie, which a type's
  // own body borrows and the library's entry reads. A value taken whole is all
  // of the run.
  static auto PositionsTaken(
      const mir::CompilationUnit& unit, mir::Builder block, mir::TypeId type,
      mir::ExprId into, mir::ExprId from, mir::ExprId start, mir::ExprId count)
      -> mir::ExprId {
    if (mir::TakenWhole(unit, type)) {
      return block.AsValue(unit.types, from);
    }
    return BuildValueOperation(
        unit, block, support::PositionsItem::kTakePositions, into,
        {from, start, count}, type);
  }

  [[nodiscard]] auto Unit() const -> const mir::CompilationUnit& {
    return *unit_;
  }
  [[nodiscard]] auto WholeType() const -> mir::TypeId {
    return whole_;
  }
  [[nodiscard]] auto Members() const -> std::span<const mir::TypeId> {
    return members_;
  }
  // The one-bit type `==` answers over two values of this type.
  [[nodiscard]] auto EqualityAnswer() const -> mir::TypeId {
    return equality_answer_;
  }

  // Whether the stream of an array of `element` is one this lowering builds.
  // An element holding a class handle streams the object the handle refers to
  // (LRM 6.24.3), which is not yet supported, so each question about such an
  // array's stream is a body telling the design so where it is asked,
  // answering `answer`.
  [[nodiscard]] auto StreamsItsElements(mir::TypeId element) const -> bool {
    return !mir::HoldsClassHandle(Unit(), element);
  }
  void RefuseTheStream(mir::Builder block, mir::ExprId answer) const {
    mir::AppendRunTimeCheck(
        Unit(), block, BuildMachineBool(Unit(), block, false),
        "streaming the object a class handle refers to is not yet supported");
    block.AppendStmt(mir::ReturnStmt{.value = answer});
  }

  // `return` of what `answer(taken)` builds where `condition` holds, stated
  // after what `block` holds. A choice among the members is one such statement
  // per member, one after another, as a `switch` is lowered, so nothing is one
  // expression nesting as deep as the type has members.
  template <typename Answer>
  void ReturnWhere(mir::Builder block, mir::ExprId condition, Answer answer) {
    mir::Block taken;
    mir::Builder taken_builder = block.Into(taken);
    const mir::ExprId answered = answer(taken_builder);
    taken_builder.AppendStmt(mir::ReturnStmt{.value = answered});
    block.AppendStmt(
        mir::IfStmt{
            .condition = ReduceToCondition(Unit(), block, condition),
            .then_scope = block.AddScope(std::move(taken)),
            .else_scope = std::nullopt});
  }

  // The body of `answers` at this type, which MIR declares, whose one
  // statement returns what `answer` builds from the parameters' locals. Where
  // the operation is asked of a value, the first parameter is that value. Each
  // is taken by reference and read through it.
  template <typename Answer>
  auto Body(support::ValueOperation answers, Answer answer)
      -> mir::CallableCode {
    return BodyOf(
        answers, [&](mir::CallableCode&, mir::Builder block,
                     const std::vector<mir::LocalId>& locals) {
          return answer(block, locals);
        });
  }

  // The same, where what builds the answer also declares locals of the body
  // and states what runs before it returns.
  template <typename Answer>
  auto BodyOf(support::ValueOperation answers, Answer answer)
      -> mir::CallableCode {
    return Defined(
        Declaration(answers), [&](mir::CallableCode& code, mir::Builder body,
                                  const std::vector<mir::LocalId>& locals) {
          const mir::ExprId answered = answer(code, body, locals);
          body.AppendStmt(mir::ReturnStmt{.value = answered});
        });
  }

  // The declaration MIR states for `answers` at this type, taken at
  // `argument` where it is taken at a type.
  [[nodiscard]] auto Declaration(
      support::ValueOperation answers,
      std::optional<mir::TypeId> argument = std::nullopt) const
      -> mir::CallableCode {
    mir::TypeOperationSource source{.type = whole_, .answers = answers};
    if (argument.has_value()) {
      source.arguments.push_back(*argument);
    }
    return Declaration(source);
  }
  // The declaration MIR states for `source`, which every operation a body is
  // built for has.
  [[nodiscard]] auto Declaration(const mir::TypeOperationSource& source) const
      -> mir::CallableCode {
    std::optional<mir::CallableCode> declared =
        mir::OperationDeclarationOf(Unit(), source);
    if (!declared.has_value()) {
      throw InternalError(
          "hir_to_mir: a body is built for an operation MIR declares nothing "
          "for -- please report this as a bug");
    }
    return *std::move(declared);
  }

  // `declared` with the body whose statements `build(code, block, locals)`
  // states, the locals being the parameters' -- every one it declares, and
  // not where the operation was called.
  template <typename Build>
  static auto Defined(mir::CallableCode declared, Build build)
      -> mir::CallableCode {
    mir::CallableCode code = std::move(declared);
    code.body.emplace();
    std::vector<mir::LocalId> locals;
    for (const mir::LocalId param : code.params) {
      if (param != code.caller_location) {
        locals.push_back(param);
      }
    }
    mir::Builder block(code.Body(), diag::SourceSpan{});
    build(code, block, locals);
    return code;
  }

  // The body answering an item of the equality trait, which every type whose
  // operations the compiler builds implements (LRM 11.4.5): `==` and `===`
  // are `bodies`' own, and `!=` the negation of its `==`.
  template <typename Bodies>
  static auto AnswerEquality(Bodies& bodies, support::EqualityItem item)
      -> mir::CallableCode {
    switch (item) {
      case support::EqualityItem::kEqual:
        return bodies.Equality();
      case support::EqualityItem::kNotEqual:
        return bodies.Inequality();
      case support::EqualityItem::kCaseEqual:
        return bodies.CaseEqual();
    }
    throw InternalError("hir_to_mir: unknown equality item");
  }

  // The body answering the item of the change-test trait (LRM 9.4.2).
  template <typename Bodies>
  static auto AnswerChangeTest(Bodies& bodies, support::ChangeTestItem item)
      -> mir::CallableCode {
    switch (item) {
      case support::ChangeTestItem::kBitIdentical:
        return bodies.BitIdentical();
    }
    throw InternalError("hir_to_mir: unknown change-test item");
  }

  // The body answering an item of the positions trait (LRM 7.3, 6.7.1), which
  // `bodies` builds part by part; how many positions the type has is the
  // type's alone.
  template <typename Bodies>
  static auto AnswerPositions(Bodies& bodies, support::PositionsItem item)
      -> mir::CallableCode {
    switch (item) {
      case support::PositionsItem::kTakePositions:
        return bodies.TakePositions();
      case support::PositionsItem::kPositionCount:
        return bodies.PositionCount();
      case support::PositionsItem::kToPositions:
        return bodies.ToPositions();
      case support::PositionsItem::kWritePositions:
        return bodies.WritePositions();
      case support::PositionsItem::kFromPositions:
        return bodies.FromPositions();
    }
    throw InternalError("hir_to_mir: unknown positions item");
  }

  // The body answering an item of the default trait for a type whose values
  // its declaration or its parts build: its declared default. A container's
  // own empty value is a run-time-sized container's alone.
  template <typename Bodies>
  static auto AnswerDeclaredDefault(Bodies& bodies, support::DefaultItem item)
      -> mir::CallableCode {
    switch (item) {
      case support::DefaultItem::kDeclaredDefault:
        return bodies.DeclaredDefault();
    }
    throw InternalError("hir_to_mir: unknown default item");
  }

  // A product or a union reaches its parts by their positions, which its type
  // lays out, so it implements neither the element access nor the resize an
  // array's elements a machine index reaches have.
  [[noreturn]] static void NoElementAccess(support::ElementAccessItem item) {
    switch (item) {
      case support::ElementAccessItem::kGetUnchecked:
      case support::ElementAccessItem::kGetUncheckedMut:
      case support::ElementAccessItem::kElementPlace:
        NoBodyBuilt();
    }
    throw InternalError("hir_to_mir: unknown element-access item");
  }
  [[noreturn]] static void NoResize(support::ResizeItem item) {
    switch (item) {
      case support::ResizeItem::kResized:
        NoBodyBuilt();
    }
    throw InternalError("hir_to_mir: unknown resize item");
  }

  // The body answering `answers`, built by `answer`, which takes the item of
  // each trait and each operation the type is called by. A step of a value's
  // life and a bound-free conversion are never one of these types' own.
  template <typename Answer>
  static auto Dispatched(support::ValueOperation answers, Answer answer)
      -> mir::CallableCode {
    return std::visit(
        Overloaded{
            [&](support::EqualityItem item) -> mir::CallableCode {
              return answer(item);
            },
            [&](support::LibraryAnsweredItem item) -> mir::CallableCode {
              return std::visit(answer, item);
            },
            [&](support::BuiltinFn fn) -> mir::CallableCode {
              return answer(fn);
            },
            [](support::BoundFreeItem) -> mir::CallableCode {
              NoBoundFreeBody();
            },
            [](support::ValueStep) -> mir::CallableCode {
              throw InternalError(
                  "hir_to_mir: a step of a value's life is asked of a type "
                  "whose steps are its parts' -- please report this as a bug");
            }},
        answers);
  }

  // An operation the type's interface declares that no body of this kind is
  // built for.
  [[noreturn]] static void NoBodyBuilt() {
    throw InternalError(
        "hir_to_mir: a type's interface declares an operation no body here is "
        "built for -- please report this as a bug");
  }

  // A bound-free conversion, which no interface declares: its body is built
  // apart from a type's own bodies.
  [[noreturn]] static void NoBoundFreeBody() {
    throw InternalError(
        "hir_to_mir: a bound-free conversion is asked of a type's own bodies, "
        "and its body is built apart from them -- please report this as a "
        "bug");
  }

  // A call of the body answering another operation of this type.
  auto Own(
      mir::Builder block, support::ValueOperation answers, mir::ExprId receiver,
      std::vector<mir::ExprId> operands, mir::TypeId result) -> mir::ExprId {
    return OwnOperationCall(
        Unit(), block, whole_, own_(answers), answers, receiver,
        std::move(operands), result);
  }

  // What the parameter `local` refers to, of `type`, as the place it is,
  // reached through a reference of `mutability`, read-only where none is said.
  auto Referent(mir::Builder block, mir::LocalId local, mir::TypeId type)
      -> mir::ExprId {
    return block.Deref(
        Unit().types, block.LocalPlace(local, ReferenceTo(Unit(), type)), type);
  }
  auto Referent(
      mir::Builder block, mir::LocalId local, mir::TypeId type,
      mir::Mutability mutability) -> mir::ExprId {
    const mir::TypeId reference = Unit().types.Intern(
        mir::Type{
            mir::ReferenceType{.referent = type, .mutability = mutability}});
    return block.Deref(Unit().types, block.LocalPlace(local, reference), type);
  }

  // The value of this type the parameter `local` refers to.
  auto Whole(mir::Builder block, mir::LocalId local) -> mir::ExprId {
    return Referent(block, local, whole_);
  }

  // What the parameter `local` refers to, a value of this type, as the place
  // it is, to read or to write.
  auto Array(mir::Builder block, mir::LocalId local, mir::Mutability mutability)
      -> mir::ExprId {
    return Referent(block, local, whole_, mutability);
  }
  auto Read(mir::Builder block, mir::LocalId local) -> mir::ExprId {
    return Array(block, local, mir::Mutability::kReadOnly);
  }
  auto Written(mir::Builder block, mir::LocalId local) -> mir::ExprId {
    return Array(block, local, mir::Mutability::kMutable);
  }

  // A call of the core's entry `fn` on the array the place `array` names.
  static auto Core(
      mir::Builder block, support::BuiltinFn fn, mir::ExprId array,
      std::vector<mir::ExprId> arguments, mir::TypeId result) -> mir::ExprId {
    return block.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee = mir::Direct{.target = fn, .receiver = array},
                    .arguments = std::move(arguments)},
            .type = result});
  }
  static void Statement(mir::Builder block, mir::ExprId expr) {
    block.AppendStmt(mir::ExprStmt{.expr = expr});
  }
  static auto CastTo(mir::Builder block, mir::ExprId value, mir::TypeId type)
      -> mir::ExprId {
    return block.Add(
        mir::Expr{.data = mir::CastExpr{.operand = value}, .type = type});
  }
  // Whether the address `address` is no address.
  auto IsNull(mir::Builder block, mir::ExprId address) -> mir::ExprId {
    return BuildIsNull(Unit(), block, address);
  }

  // A call of the entry `fn` on the words of a memory task (LRM 21.4, 21.5)
  // the parameter `words` refers to.
  auto AskWords(
      mir::Builder block, mir::LocalId words, support::BuiltinFn fn,
      std::vector<mir::ExprId> arguments, mir::TypeId result) -> mir::ExprId {
    return Core(block, fn, Words(block, words), std::move(arguments), result);
  }
  // The words the parameter `words` refers to, which a task's reads and
  // plans and a dump's words change.
  auto Words(mir::Builder block, mir::LocalId words) -> mir::ExprId {
    return Referent(
        block, words,
        mir::LibraryTypeOf(Unit().types, support::LibraryDecl::kMemoryWords),
        mir::Mutability::kMutable);
  }
  void TellWords(
      mir::Builder block, mir::LocalId words, support::BuiltinFn fn,
      std::vector<mir::ExprId> arguments) {
    Statement(
        block,
        AskWords(
            block, words, fn, std::move(arguments), Unit().builtins.void_type));
  }
  // What the entry `fn` answers on `receiver` for its element `k`, a machine
  // index, told the type `type` it answers at: a memory task's word, or a
  // DPI-C open array image's element, each at the type of the array's words.
  static auto ElementAt(
      mir::Builder block, support::BuiltinFn fn, mir::ExprId receiver,
      mir::ExprId k, mir::TypeId type) -> mir::ExprId {
    return block.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target = fn,
                            .receiver = receiver,
                            .type_arguments = {type}},
                    .arguments = {k}},
            .type = type});
  }

  // Member `i` of the value the parameter `local` refers to, where it lies:
  // each operation asked of it settles whether it reads it or borrows it.
  auto Member(mir::Builder block, mir::LocalId local, std::size_t i)
      -> mir::ExprId {
    return block.Add(
        mir::MakeFieldAccessExpr(
            Whole(block, local),
            mir::ComponentFieldTarget{
                .position =
                    base::ComponentIndex{static_cast<std::uint32_t>(i)}},
            members_[i]));
  }

  // A local of `code` holding a value of `type`, declared after what `block`
  // holds and starting at `initial`.
  static auto Declared(
      mir::CallableCode& code, mir::Builder block, mir::TypeId type,
      mir::ExprId initial) -> mir::LocalId {
    const mir::LocalId local = code.AddLocal(type);
    block.AppendStmt(mir::LocalDeclStmt{.target = local, .init = initial});
    return local;
  }

  // Such a local holding a machine number, a count or an index; what it holds
  // now; it moved to `value`; and a count the lowering holds, written as one.
  auto DeclaredCount(
      mir::CallableCode& code, mir::Builder block, mir::ExprId initial)
      -> mir::LocalId {
    return Declared(code, block, Unit().builtins.machine_int64, initial);
  }
  auto CountNow(mir::Builder block, mir::LocalId count) -> mir::ExprId {
    return block.ReadLocal(count, Unit().builtins.machine_int64);
  }
  void SetCount(mir::Builder block, mir::LocalId count, mir::ExprId value) {
    block.AppendStmt(
        mir::ExprStmt{
            .expr = block.Add(BuildStoreExpr(
                Unit(), block,
                AccessPath{
                    .owner =
                        block.LocalPlace(count, Unit().builtins.machine_int64),
                    .descent = {}},
                value))});
  }
  auto CountLiteral(mir::Builder block, std::uint64_t count) -> mir::ExprId {
    return mir::BuildMachineCount(Unit(), block, count);
  }

  // A loop over the machine indexes below the machine count `count`, read
  // once ahead of the loop, after what `block` holds, whose body
  // `each(body, index)` states, `index` the local holding each in turn. It
  // counts a machine integer, as Rust's `for idx in 0..len` over a slice
  // counts a `usize`, so no step of the loop is a four-state one.
  template <typename Each>
  void ForEachIndex(
      mir::CallableCode& code, mir::Builder block, mir::ExprId count,
      Each each) {
    ForEachIndexBetween(code, block, CountLiteral(block, 0), count, each);
  }
  // The same over the machine indexes from `from` up to `below`.
  template <typename Each>
  void ForEachIndexBetween(
      mir::CallableCode& code, mir::Builder block, mir::ExprId from,
      mir::ExprId below, Each each) {
    StepThrough(code, block, from, below, 1, each);
  }
  // A loop counting a machine integer from `from` up to `below`, read once
  // ahead of the loop, by `stride`, after what `block` holds, whose body
  // `each(body, at)` states, `at` the local holding each value in turn.
  template <typename Each>
  void StepThrough(
      mir::CallableCode& code, mir::Builder block, mir::ExprId from,
      mir::ExprId below, std::int64_t stride, Each each) {
    const mir::TypeId machine = Unit().builtins.machine_int64;
    const mir::LocalId limit = DeclaredCount(code, block, below);
    const mir::LocalId at = code.AddLocal(machine);
    mir::Block body;
    mir::Builder body_builder = block.Into(body);
    each(body_builder, at);
    const mir::BlockId scope = block.AddScope(std::move(body));
    const mir::ExprId within = mir::BuildMachineIntComparison(
        Unit(), block, mir::BinaryOp::kLessThan, block.ReadLocal(at, machine),
        block.ReadLocal(limit, machine));
    const mir::ExprId next = mir::BuildMachineIntBinary(
        Unit(), block, mir::BinaryOp::kAdd, block.ReadLocal(at, machine),
        mir::BuildMachineIntLiteral(Unit(), block, stride));
    std::vector<mir::ForInit> init;
    init.emplace_back(mir::ForInitDecl{.induction_var = at, .init = from});
    block.AppendStmt(
        mir::ForStmt{
            .init = std::move(init),
            .condition = within,
            .step = {block.Add(
                mir::MakeAssignExpr(
                    Unit().builtins, block.LocalPlace(at, machine), next))},
            .scope = scope});
  }

  // The bits a part whose width the program decides takes of the run of
  // `length` bits read from `at`, where the parts after it take `after` where
  // they are empty (LRM 11.4.14.4); a run too short for them is an error.
  auto PartAllotment(
      mir::CallableCode& code, mir::Builder block, mir::LocalId length,
      mir::LocalId at, mir::ExprId after) -> mir::LocalId {
    const mir::ExprId length_now = CountNow(block, length);
    const mir::ExprId at_now = CountNow(block, at);
    const mir::LocalId allotted = DeclaredCount(
        code, block,
        mir::BuildMachineIntBinary(
            Unit(), block, mir::BinaryOp::kSub,
            mir::BuildMachineIntBinary(
                Unit(), block, mir::BinaryOp::kSub, length_now, at_now),
            after));
    const mir::ExprId allotted_now = CountNow(block, allotted);
    const mir::ExprId zero = CountLiteral(block, 0);
    mir::AppendRunTimeCheck(
        Unit(), block,
        mir::BuildMachineIntComparison(
            Unit(), block, mir::BinaryOp::kGreaterEqual, allotted_now, zero),
        base::DiagMessage(
            "a stream of bits holds fewer bits than the value it is read into "
            "takes")
            .TakeText());
    return allotted;
  }

  // A value of `type` built by `value` after what `block` holds, kept in a
  // local of `code` so that what follows cannot change what it reads.
  static auto Held(
      mir::CallableCode& code, mir::Builder block, mir::TypeId type,
      mir::ExprId value) -> mir::ExprId {
    return block.ReadLocal(Declared(code, block, type, value), type);
  }

  // LRM 11.4.5 `==` of two values of `part`, a member or an element, at this
  // type's equality answer type.
  auto PartEqual(
      mir::Builder block, mir::ExprId lhs, mir::ExprId rhs, mir::TypeId part)
      -> mir::ExprId {
    return ConvertToType(
        Unit(), block,
        block.Add(BuildMirBinaryExpr(
            Unit(), block, hir::BinaryOp::kEquality, lhs, rhs,
            mir::OneBitAnswerType(Unit(), std::array{part}))),
        equality_answer_);
  }

  // A known 1 or 0 of the one-bit type `type`.
  auto KnownBit(mir::Builder block, mir::TypeId type, bool holds)
      -> mir::ExprId {
    return mir::BuildIntegralLiteral(
        Unit(), block, type,
        mir::IntegralConstant{
            .value_words = {holds ? 1U : 0U}, .state_words = {}});
  }

  // LRM 11.4.5: `!=` is the negation of `==`, unknown where that is.
  auto Inequality() -> mir::CallableCode {
    const mir::TypeId answer_type = equality_answer_;
    return Body(
        support::EqualityItem::kNotEqual,
        [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
          const mir::ExprId lhs = Whole(block, p[0]);
          const mir::ExprId rhs = Whole(block, p[1]);
          const mir::ExprId equal = Own(
              block, support::EqualityItem::kEqual, lhs, {rhs}, answer_type);
          return BuildLogicalNot(Unit(), block, equal);
        });
  }

  // The value storage of this type holds before anything writes it (LRM
  // Table 6-7, 7.3).
  auto DeclaredDefault() -> mir::CallableCode {
    return Body(
        support::DefaultItem::kDeclaredDefault,
        [&](mir::Builder block, const std::vector<mir::LocalId>&) {
          return block.Add(BuildDefaultValueExpr(Unit(), block, whole_));
        });
  }

  // How many positions a value of this type has, which the library asks of a
  // value laid out by its type where it holds an array of such values or a net
  // resolves one.
  auto PositionCount() -> mir::CallableCode {
    return Body(
        support::PositionsItem::kPositionCount,
        [&](mir::Builder block, const std::vector<mir::LocalId>&) {
          return CountLiteral(block, mir::PositionCountOf(Unit(), whole_));
        });
  }

  // The positions of the part of `type` lying at `place`, as one integral
  // value.
  auto PartPositions(mir::Builder block, mir::TypeId type, mir::ExprId place)
      -> mir::ExprId {
    return BuildToPositions(
        Unit(), block, place, mir::PositionsTypeOf(Unit(), type));
  }

  // `positions`, a local of `positions_type`, with the positions of a part
  // laid in from `at`, a position.
  void LayPositions(
      mir::Builder block, mir::LocalId positions, mir::TypeId positions_type,
      mir::ExprId at, mir::ExprId part, mir::TypeId part_positions_type) {
    block.AppendStmt(
        mir::ExprStmt{
            .expr = block.Add(BuildStoreExpr(
                Unit(), block,
                AccessPath{
                    .owner = block.LocalPlace(positions, positions_type),
                    .descent = {DescentStep{
                        .to = StepToBits{.start = at},
                        .part_type = part_positions_type}}},
                part))});
  }

  // What a body writing positions a net resolved in place is handed: the
  // value, written where it lies; the view of them, where in the value its
  // first position stands, and how many it holds, each the value itself; and
  // the local folding in whether any part changed.
  struct PositionsWrite {
    mir::LocalId written;
    mir::LocalId view;
    mir::LocalId start;
    mir::LocalId count;
    mir::LocalId changed;
  };
  auto PositionsReadType() -> mir::TypeId {
    return mir::LibraryTypeOf(
        Unit().types, support::LibraryDecl::kPositionsRead);
  }
  auto HoldPositionsWrite(
      mir::CallableCode& code, mir::Builder block,
      const std::vector<mir::LocalId>& p) -> PositionsWrite {
    const mir::ExprId unchanged = BuildMachineBool(Unit(), block, false);
    return {
        .written = p[0],
        .view = p[1],
        .start = p[2],
        .count = p[3],
        .changed =
            Declared(code, block, Unit().builtins.machine_bool, unchanged)};
  }
  // The value a body writing positions writes, where it lies.
  auto Written(mir::Builder block, const PositionsWrite& write) -> mir::ExprId {
    return Written(block, write.written);
  }
  auto ViewOf(mir::Builder block, const PositionsWrite& write) -> mir::ExprId {
    return block.ReadLocal(write.view, PositionsReadType());
  }

  // Where `write`'s view reaches positions `lo` to `lo + count` of the value,
  // `reached(body)` stated in a scope running only then.
  template <typename Reached>
  void WhereReached(
      mir::Builder block, const PositionsWrite& write, mir::ExprId lo,
      std::uint64_t count, Reached reached) {
    const mir::ExprId hi = mir::BuildMachineIntBinary(
        Unit(), block, mir::BinaryOp::kAdd, lo, CountLiteral(block, count));
    const mir::ExprId starts_before_end = mir::BuildMachineIntComparison(
        Unit(), block, mir::BinaryOp::kLessThan, CountNow(block, write.start),
        hi);
    const mir::ExprId view_end = mir::BuildMachineIntBinary(
        Unit(), block, mir::BinaryOp::kAdd, CountNow(block, write.start),
        CountNow(block, write.count));
    const mir::ExprId ends_after_start = mir::BuildMachineIntComparison(
        Unit(), block, mir::BinaryOp::kGreaterThan, view_end, lo);
    mir::Block taken;
    mir::Builder taken_builder = block.Into(taken);
    reached(taken_builder);
    block.AppendStmt(
        mir::IfStmt{
            .condition = AllHold(
                Unit(), block, std::array{starts_before_end, ends_after_start}),
            .then_scope = block.AddScope(std::move(taken)),
            .else_scope = std::nullopt});
  }

  // The positions `write`'s view reaches of the part of `type` that `path`
  // names, whose first position stands at position `lo` of the value,
  // replaced in place, and whether any changed folded into `write.changed`.
  // An integral part is rebuilt with them and stored back; any other part
  // writes its own.
  void PartWritten(
      mir::CallableCode& code, mir::Builder block, mir::TypeId type,
      const AccessPath& path, mir::ExprId lo, const PositionsWrite& write) {
    const mir::TypeId boolean = Unit().builtins.machine_bool;
    const mir::ExprId start = mir::BuildMachineIntBinary(
        Unit(), block, mir::BinaryOp::kSub, CountNow(block, write.start), lo);
    mir::ExprId part_changed{};
    if (Unit().types.Get(type).IsIntegral()) {
      const mir::ExprId before = Held(
          code, block, type,
          block.AsValue(Unit().types, PathPlace(Unit(), block, path)));
      const mir::ExprId after = Held(
          code, block, type,
          block.Add(
              mir::MakeBuiltinCall(
                  Unit(), block.Body(),
                  support::BuiltinFn::kIntegralWithPositions, before,
                  {ViewOf(block, write), start}, type)));
      block.AppendStmt(
          mir::ExprStmt{
              .expr = block.Add(BuildStoreExpr(Unit(), block, path, after))});
      part_changed = BuildConditionNot(
          Unit(), block, BuildBitIdentity(Unit(), block, before, after));
    } else {
      part_changed = Held(
          code, block, boolean,
          BuildValueOperation(
              Unit(), block, support::PositionsItem::kWritePositions,
              PathPlace(Unit(), block, path),
              {ViewOf(block, write), start, CountNow(block, write.count)},
              boolean));
    }
    const mir::ExprId was = block.ReadLocal(write.changed, boolean);
    const mir::ExprId changed =
        AnyHolds(Unit(), block, std::array{part_changed, was});
    block.AppendStmt(
        mir::ExprStmt{
            .expr = block.Add(
                mir::MakeAssignExpr(
                    Unit().builtins, block.LocalPlace(write.changed, boolean),
                    changed))});
  }

  // The run of positions a body answering which positions it takes is
  // handed, kept in locals so that each place reaching it reads them: its
  // first position, and the one past its last.
  struct HeldRun {
    mir::LocalId first;
    mir::LocalId end;
  };
  auto HoldRun(
      mir::CallableCode& code, mir::Builder block, mir::LocalId first,
      mir::LocalId count) -> HeldRun {
    const mir::TypeId number = Unit().builtins.machine_int64;
    const mir::LocalId held_first = code.AddLocal(number);
    block.AppendStmt(
        mir::LocalDeclStmt{
            .target = held_first,
            .init =
                block.AsValue(Unit().types, Referent(block, first, number))});
    const mir::LocalId end = code.AddLocal(number);
    const mir::ExprId first_now = block.ReadLocal(held_first, number);
    const mir::ExprId count_now =
        block.AsValue(Unit().types, Referent(block, count, number));
    block.AppendStmt(
        mir::LocalDeclStmt{
            .target = end,
            .init = mir::BuildMachineIntBinary(
                Unit(), block, mir::BinaryOp::kAdd, first_now, count_now)});
    return {.first = held_first, .end = end};
  }

  // A part of type `part` lying at the positions from `at`, a machine number:
  // where `run` reaches it, `into` with the positions the run names of it
  // taken from `from`, and elsewhere `into` as it stands.
  auto PartTaken(
      mir::CallableCode& code, mir::Builder block, mir::TypeId part,
      mir::ExprId into, mir::ExprId from, mir::ExprId at, const HeldRun& run)
      -> mir::ExprId {
    const mir::TypeId number = Unit().builtins.machine_int64;
    const auto held = [&](mir::ExprId init) {
      return DeclaredCount(code, block, init);
    };
    const auto read = [&](mir::LocalId local) {
      return block.ReadLocal(local, number);
    };
    const mir::LocalId lo = held(at);
    const mir::ExprId lo_now = read(lo);
    const mir::ExprId part_count =
        CountLiteral(block, mir::PositionCountOf(Unit(), part));
    const mir::LocalId hi = held(
        mir::BuildMachineIntBinary(
            Unit(), block, mir::BinaryOp::kAdd, lo_now, part_count));
    const mir::ExprId run_first = read(run.first);
    const mir::ExprId part_end = read(hi);
    const mir::ExprId run_starts_before_end = mir::BuildMachineIntComparison(
        Unit(), block, mir::BinaryOp::kLessThan, run_first, part_end);
    const mir::ExprId run_end = read(run.end);
    const mir::ExprId part_start = read(lo);
    const mir::ExprId run_ends_after_start = mir::BuildMachineIntComparison(
        Unit(), block, mir::BinaryOp::kGreaterThan, run_end, part_start);
    const std::array reaches{run_starts_before_end, run_ends_after_start};
    const mir::ExprId first_reached =
        mir::BuildMachineIntMax(Unit(), block, read(run.first), read(lo));
    const mir::ExprId start_base = read(lo);
    const mir::ExprId start = mir::BuildMachineIntBinary(
        Unit(), block, mir::BinaryOp::kSub, first_reached, start_base);
    const mir::ExprId end_reached =
        mir::BuildMachineIntMin(Unit(), block, read(run.end), read(hi));
    const mir::ExprId stop_base = read(lo);
    const mir::ExprId stop = mir::BuildMachineIntBinary(
        Unit(), block, mir::BinaryOp::kSub, end_reached, stop_base);
    const mir::LocalId first_taken = held(start);
    const mir::LocalId count = held(
        mir::BuildMachineIntBinary(
            Unit(), block, mir::BinaryOp::kSub, stop, read(first_taken)));
    const mir::ExprId reached = AllHold(Unit(), block, reaches);
    const mir::ExprId first_place = block.LocalPlace(first_taken, number);
    const mir::ExprId count_place = block.LocalPlace(count, number);
    const mir::ExprId taken = PositionsTaken(
        Unit(), block, part, into, from, first_place, count_place);
    const mir::ExprId untouched = block.AsValue(Unit().types, into);
    return Chosen(block, reached, taken, untouched, part);
  }

 private:
  const mir::CompilationUnit* unit_;
  mir::TypeId whole_;
  std::vector<mir::TypeId> members_;
  mir::TypeId equality_answer_;
  OwnTarget own_;
};

}  // namespace lyra::lowering::hir_to_mir
