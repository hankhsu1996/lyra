#include "lyra/lowering/hir_to_mir/value_operations.hpp"

#include <algorithm>
#include <array>
#include <bit>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/base/diag_message.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/case_keyword.hpp"
#include "lyra/hir/type_definition.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/bitstream.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/concatenation.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/expression/selects.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/bit_vector.hpp"
#include "lyra/lowering/hir_to_mir/predicate.hpp"
#include "lyra/lowering/hir_to_mir/select_position.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/record_decl.hpp"
#include "lyra/mir/storage_part.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The types a value of `type` holds values of -- a product's components, a
// union's members, a container's elements -- which is what every question
// below is answered over, since each asks whether anything inside the value
// lacks something.
auto PartTypes(const mir::CompilationUnit& unit, mir::TypeId type)
    -> std::vector<mir::TypeId> {
  if (const std::optional<std::span<const mir::TypeId>> parts =
          mir::ProductElements(unit, type)) {
    return {parts->begin(), parts->end()};
  }
  if (const std::optional<std::span<const mir::TypeId>> members =
          mir::OverlaidMembers(unit, type)) {
    return {members->begin(), members->end()};
  }
  const mir::Type& t = unit.types.Get(type);
  if (const std::optional<mir::TypeId> element = t.ContainerElementType()) {
    return {*element};
  }
  return {};
}

template <typename Predicate>
auto EveryPart(
    const mir::CompilationUnit& unit, std::span<const mir::TypeId> parts,
    Predicate p) -> bool {
  return std::ranges::all_of(
      parts, [&](mir::TypeId part) { return p(unit, part); });
}

auto HasBitStream(const mir::CompilationUnit& unit, mir::TypeId type) -> bool;

// What a record of `kind` over `members` answers, which a record answers of
// its members alone, so a declaration still being made answers it too. A
// stream takes an untagged union as its first member and a tagged one not at
// all (LRM 11.4.14.1); a net resolves a structure or an untagged union of
// what it resolves, and no tagged union, which no net type's tables read; and
// comparing two untagged unions compares their first members, as every
// whole-value use of one reads it.
auto HasBitStreamOver(
    const mir::CompilationUnit& unit, mir::RecordKind kind,
    std::span<const mir::TypeId> members) -> bool {
  switch (kind) {
    case mir::RecordKind::kStruct:
      return EveryPart(unit, members, HasBitStream);
    case mir::RecordKind::kUnion:
      return HasBitStream(unit, members.front());
    case mir::RecordKind::kTaggedUnion:
      return false;
  }
  throw InternalError("HasBitStreamOver: unknown record kind");
}

auto NetResolvesOver(
    const mir::CompilationUnit& unit, mir::RecordKind kind,
    std::span<const mir::TypeId> members) -> bool {
  switch (kind) {
    case mir::RecordKind::kStruct:
    case mir::RecordKind::kUnion:
      return EveryPart(unit, members, NetResolves);
    case mir::RecordKind::kTaggedUnion:
      return false;
  }
  throw InternalError("NetResolvesOver: unknown record kind");
}

// How many positions the tag of a tagged union of `members` members takes:
// the bits that count them (LRM 7.3.2).
auto TagPositionsFor(std::size_t members) -> std::uint64_t {
  return members <= 1 ? 0 : std::bit_width(members - 1);
}

// Whether every bit of a value of `type` is a fixed one: an integral value's,
// a real's IEEE 754 bits (LRM 6.12), and a structure's, a union's or a
// fixed-size array's made of such. A member of an untagged union is a view of
// positions of these (LRM 7.3); a string, a handle and a container that grows
// have none.
auto HasPositions(const mir::CompilationUnit& unit, mir::TypeId type) -> bool {
  const mir::Type& t = unit.types.Get(type);
  if (t.IsIntegral() || t.Is<mir::RealType>()) {
    return true;
  }
  if (mir::RecordKindOf(unit, type).has_value() ||
      t.Is<mir::UnpackedArrayType>() || t.Is<mir::TupleType>()) {
    return EveryPart(unit, PartTypes(unit, type), HasPositions);
  }
  return false;
}

auto EqualityAnswerOver(
    const mir::CompilationUnit& unit, mir::RecordKind kind,
    std::span<const mir::TypeId> members) -> mir::TypeId {
  switch (kind) {
    case mir::RecordKind::kStruct:
    case mir::RecordKind::kTaggedUnion:
      return OneBitAnswerType(unit, members);
    case mir::RecordKind::kUnion:
      return OneBitAnswerType(unit, members.first(1));
  }
  throw InternalError("EqualityAnswerOver: unknown record kind");
}

// LRM 6.24.3: neither a real nor a reference owning nothing -- a chandle, a
// virtual interface -- is a bit-stream type, and a value holding one has no
// stream either. A stream takes an untagged union as its first member and a
// tagged one not at all (LRM 11.4.14.1).
auto HasBitStream(const mir::CompilationUnit& unit, mir::TypeId type) -> bool {
  if (const std::optional<mir::RecordKind> kind =
          mir::RecordKindOf(unit, type)) {
    return HasBitStreamOver(unit, *kind, PartTypes(unit, type));
  }
  const mir::Type& t = unit.types.Get(type);
  return !t.Is<mir::RealType>() && !t.Is<mir::UnmanagedRefType>() &&
         EveryPart(unit, PartTypes(unit, type), HasBitStream);
}

auto IsFourState(mir::IntegralStateKind state) -> bool {
  switch (state) {
    case mir::IntegralStateKind::kTwoState:
      return false;
    case mir::IntegralStateKind::kFourState:
      return true;
  }
  throw InternalError("IsFourState: unknown integral state kind");
}

// The one-bit type `==` and `!=` answer over two values of `type`, which can
// be unknown exactly where what they compare can hold an unknown: every member
// of a struct or a tagged union, every element of an array, the first member
// of an untagged union.
auto EqualityAnswerType(const mir::CompilationUnit& unit, mir::TypeId type)
    -> mir::TypeId {
  if (const std::optional<mir::RecordKind> kind =
          mir::RecordKindOf(unit, type)) {
    return EqualityAnswerOver(unit, *kind, PartTypes(unit, type));
  }
  return OneBitAnswerType(unit, PartTypes(unit, type));
}

auto CallOf(
    mir::Builder block, mir::Direct callee, std::vector<mir::ExprId> arguments,
    mir::TypeId result) -> mir::ExprId {
  return block.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = std::move(callee),
                  .arguments = std::move(arguments)},
          .type = result});
}

// A call of the body `target` names, borrowing what it is asked about.
auto OwnOperationCall(
    const mir::CompilationUnit& unit, mir::Builder block,
    mir::DirectTarget target, std::optional<mir::ExprId> receiver,
    std::vector<mir::ExprId> operands, mir::TypeId result) -> mir::ExprId {
  for (mir::ExprId& operand : operands) {
    operand = mir::Borrow(unit, block, operand);
  }
  return CallOf(
      block,
      mir::Direct{
          .target = std::move(target),
          .receiver = receiver.transform([&](mir::ExprId object) {
            return mir::Borrow(unit, block, object);
          })},
      std::move(operands), result);
}

// The operations on a whole value a type with operations of its own answers
// with a body: the equalities, which every such type has, and these. Every
// other entry -- an array's methods, a container's -- is the library's
// whatever it is asked of.
constexpr std::array kWholeValueEntries{
    support::BuiltinFn::kDeclaredDefault, support::BuiltinFn::kCaseEqual,
    support::BuiltinFn::kBitIdentical,    support::BuiltinFn::kHasUnknown,
    support::BuiltinFn::kBitstreamWidth,  support::BuiltinFn::kCountBits,
    support::BuiltinFn::kToBitstream,     support::BuiltinFn::kFromBitstream,
    support::BuiltinFn::kToBitVec,        support::BuiltinFn::kFromBitVec,
    support::BuiltinFn::kPositionCount,   support::BuiltinFn::kTakePositions,
    support::BuiltinFn::kToPositions,     support::BuiltinFn::kWritePositions,
    support::BuiltinFn::kFromPositions,
};

// The method of the record `record` answering `answers`, as a call names it.
auto RecordMethodTarget(
    const mir::CompilationUnit& unit, mir::RecordId record,
    support::ValueOperation answers) -> mir::DirectTarget {
  return mir::CallableTarget{
      .callable = mir::RecordMethodOf(unit, record, answers)};
}

// The body answering `answers` for a value of `type`, where the type answers
// it with a body of its own: the method of the structure or the union it is,
// or the structural operation of the fixed-size array it is.
auto OwnOperationTarget(
    const mir::CompilationUnit& unit, mir::TypeId type,
    support::ValueOperation answers) -> std::optional<mir::DirectTarget> {
  if (!IsWholeValueOperation(answers)) {
    return std::nullopt;
  }
  const mir::Type& t = unit.types.Get(type);
  if (const mir::RecordId* record = mir::RecordOf(t)) {
    return RecordMethodTarget(unit, *record, answers);
  }
  if (mir::StructuralOperationsTypeOf(t).has_value()) {
    return mir::TypeOperationTarget{.type = type, .answers = answers};
  }
  return std::nullopt;
}

// Whether a value holds an unknown bit, as a machine boolean.
auto BuildHasUnknown(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId value)
    -> mir::ExprId {
  return BuildValueOperation(
      unit, block, support::BuiltinFn::kHasUnknown, value, {},
      unit.builtins.machine_bool);
}

// `then_value` where `condition` holds and `else_value` where it does not,
// both of `type`.
auto Chosen(
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

// Whether a run of positions reaching any of a value of `type` takes all of
// it: a value no select names part of -- a real (LRM 11.5.1), a tagged union,
// whose tag decides which member lies there (LRM 7.3.2), and a value held
// whole as one position.
auto TakenWhole(const mir::CompilationUnit& unit, mir::TypeId type) -> bool {
  const mir::Type& t = unit.types.Get(type);
  if (t.IsIntegral() || t.Is<mir::UnpackedArrayType>() ||
      t.Is<mir::TupleType>()) {
    return false;
  }
  if (const std::optional<mir::RecordKind> kind =
          mir::RecordKindOf(unit, type)) {
    switch (*kind) {
      case mir::RecordKind::kStruct:
      case mir::RecordKind::kUnion:
        return false;
      case mir::RecordKind::kTaggedUnion:
        return true;
    }
    throw InternalError("TakenWhole: unknown record kind");
  }
  return true;
}

// A value of `type` with the `count` positions from `start` of `into` taken
// from `from`, both of that type, where the run names at least one position.
// `start` and `count` are where the two machine numbers lie, which a type's own
// body borrows and the library's entry reads. A value taken whole is all of
// the run.
auto PositionsTaken(
    const mir::CompilationUnit& unit, mir::Builder block, mir::TypeId type,
    mir::ExprId into, mir::ExprId from, mir::ExprId start, mir::ExprId count)
    -> mir::ExprId {
  if (TakenWhole(unit, type)) {
    return block.AsValue(unit.types, from);
  }
  return BuildValueOperation(
      unit, block, support::BuiltinFn::kTakePositions, into,
      {from, start, count}, type);
}

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
      std::vector<mir::TypeId> members, mir::TypeId equality_answer,
      OwnTarget own)
      : unit_(&unit),
        whole_(whole),
        members_(std::move(members)),
        equality_answer_(equality_answer),
        own_(std::move(own)) {
  }

 protected:
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

  // The body of `answers`, taking parameters of `params` and answering
  // `result`, whose one statement returns what `answer` builds from the
  // parameters' locals. Where the operation is asked of a value, the first
  // parameter is that value. Each is taken by reference and read through it.
  template <typename Answer>
  auto Body(
      support::ValueOperation answers, std::vector<mir::TypeId> params,
      mir::TypeId result, Answer answer) -> mir::CallableCode {
    return BodyOf(
        answers, std::move(params), result,
        [&](mir::CallableCode&, mir::Builder block,
            const std::vector<mir::LocalId>& locals) {
          return answer(block, locals);
        });
  }

  // The same, where what builds the answer also declares locals of the body
  // and states what runs before it returns.
  template <typename Answer>
  auto BodyOf(
      support::ValueOperation answers, std::vector<mir::TypeId> params,
      mir::TypeId result, Answer answer) -> mir::CallableCode {
    for (mir::TypeId& param : params) {
      param = ReferenceTo(Unit(), param);
    }
    return BodyTaking(answers, std::move(params), result, answer);
  }

  // The same, taking parameters of exactly `params`.
  template <typename Answer>
  auto BodyTaking(
      support::ValueOperation answers, std::vector<mir::TypeId> params,
      mir::TypeId result, Answer answer) -> mir::CallableCode {
    mir::CallableCode code = mir::CallableCode::Defined(diag::SourceSpan{});
    for (const mir::TypeId param : params) {
      code.params.push_back(code.AddLocal(param));
    }
    if (support::ReachesAReceiver(answers)) {
      code.receiver = code.params.front();
    }
    code.result_type = result;
    const std::vector<mir::LocalId> locals = code.params;
    if (support::TakesCallerLocation(answers)) {
      code.caller_location = code.AddLocal(Unit().builtins.source_location);
      code.params.push_back(*code.caller_location);
    }
    mir::Builder body(code.Body(), diag::SourceSpan{});
    const mir::ExprId answered = answer(code, body, locals);
    body.AppendStmt(mir::ReturnStmt{.value = answered});
    return code;
  }

  // A call of the body answering another operation of this type.
  auto Own(
      mir::Builder block, support::ValueOperation answers, mir::ExprId receiver,
      std::vector<mir::ExprId> operands, mir::TypeId result) -> mir::ExprId {
    return OwnOperationCall(
        Unit(), block, own_(answers), receiver, std::move(operands), result);
  }

  // What the parameter `local` refers to, of `type`, as the place it is.
  auto Referent(mir::Builder block, mir::LocalId local, mir::TypeId type)
      -> mir::ExprId {
    return block.Deref(
        Unit().types, block.LocalPlace(local, ReferenceTo(Unit(), type)), type);
  }

  // The value of this type the parameter `local` refers to.
  auto Whole(mir::Builder block, mir::LocalId local) -> mir::ExprId {
    return Referent(block, local, whole_);
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

  // Such a local holding a machine count; what it holds now; it moved to
  // `value`; and a count the lowering holds, written as one.
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
            OneBitAnswerType(Unit(), std::array{part}))),
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
        support::ValueOperator::kInequality, {whole_, whole_}, answer_type,
        [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
          const mir::ExprId lhs = Whole(block, p[0]);
          const mir::ExprId rhs = Whole(block, p[1]);
          const mir::ExprId equal =
              Own(block, support::ValueOperator::kEquality, lhs, {rhs},
                  answer_type);
          return BuildLogicalNot(Unit(), block, equal);
        });
  }

  // The value storage of this type holds before anything writes it (LRM
  // Table 6-7, 7.3).
  auto DeclaredDefault() -> mir::CallableCode {
    return Body(
        support::BuiltinFn::kDeclaredDefault, {}, whole_,
        [&](mir::Builder block, const std::vector<mir::LocalId>&) {
          return block.Add(BuildDefaultValueExpr(Unit(), block, whole_));
        });
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
      const mir::LocalId local = code.AddLocal(number);
      block.AppendStmt(mir::LocalDeclStmt{.target = local, .init = init});
      return local;
    };
    const auto read = [&](mir::LocalId local) {
      return block.ReadLocal(local, number);
    };
    const mir::LocalId lo = held(at);
    const mir::ExprId lo_now = read(lo);
    const mir::ExprId part_count =
        CountLiteral(block, PositionCountOf(Unit(), part));
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

  // How many positions a value of this type has, which the library asks of a
  // value laid out by its type where it holds an array of such values or a net
  // resolves one.
  auto PositionCount() -> mir::CallableCode {
    return Body(
        support::BuiltinFn::kPositionCount, {}, Unit().builtins.machine_int64,
        [&](mir::Builder block, const std::vector<mir::LocalId>&) {
          return CountLiteral(block, PositionCountOf(Unit(), whole_));
        });
  }

  // The positions of the part of `type` lying at `place`, as one integral
  // value.
  auto PartPositions(mir::Builder block, mir::TypeId type, mir::ExprId place)
      -> mir::ExprId {
    return BuildToPositions(
        Unit(), block, place, PositionsTypeOf(Unit(), type));
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
  // view of them, where in the value its first position stands, and how many
  // it holds; and the local folding in whether any part changed.
  struct PositionsWrite {
    mir::LocalId view;
    mir::LocalId start;
    mir::LocalId count;
    mir::LocalId changed;
  };
  // The value is written where it lies, so it is taken by a reference that
  // writes; the view and the two numbers are taken as they are.
  auto TypesOfPositionsWrite() -> std::vector<mir::TypeId> {
    const mir::TypeId number = Unit().builtins.machine_int64;
    return {
        Unit().types.Intern(
            mir::Type{mir::ReferenceType{
                .referent = WholeType(),
                .mutability = mir::Mutability::kMutable}}),
        PositionsReadType(), number, number};
  }
  auto PositionsReadType() -> mir::TypeId {
    return mir::LibraryTypeOf(
        Unit().types, support::LibraryDecl::kPositionsRead);
  }
  // The value a body writing positions writes, where it lies.
  auto Written(mir::Builder block, mir::LocalId local) -> mir::ExprId {
    return block.Deref(
        Unit().types, block.LocalPlace(local, TypesOfPositionsWrite().front()),
        whole_);
  }
  auto HoldPositionsWrite(
      mir::CallableCode& code, mir::Builder block,
      const std::vector<mir::LocalId>& p) -> PositionsWrite {
    const mir::ExprId unchanged = BuildMachineBool(Unit(), block, false);
    return {
        .view = p[1],
        .start = p[2],
        .count = p[3],
        .changed =
            Declared(code, block, Unit().builtins.machine_bool, unchanged)};
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
          Unit(), block,
          BuildValueOperation(
              Unit(), block, support::BuiltinFn::kBitIdentical, before, {after},
              boolean));
    } else {
      const mir::ExprId written = block.Add(
          mir::Expr{
              .data =
                  mir::BorrowExpr{.operand = PathPlace(Unit(), block, path)},
              .type = Unit().types.Intern(
                  mir::Type{mir::ReferenceType{
                      .referent = type,
                      .mutability = mir::Mutability::kMutable}})});
      part_changed = Held(
          code, block, boolean,
          block.Add(
              mir::MakeCallExpr(
                  mir::Direct{
                      .target = *OwnOperationTarget(
                          Unit(), type, support::BuiltinFn::kWritePositions),
                      .receiver = written},
                  {ViewOf(block, write), start, CountNow(block, write.count)},
                  boolean)));
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

 private:
  const mir::CompilationUnit* unit_;
  mir::TypeId whole_;
  std::vector<mir::TypeId> members_;
  mir::TypeId equality_answer_;
  OwnTarget own_;
};

// How a body of the record `record` declares names another of its methods: by
// the record being declared rather than by its type, whose methods are
// declared before any of their bodies is built.
auto MethodsOf(const mir::CompilationUnit& unit, mir::RecordId record)
    -> OperationBodies::OwnTarget {
  return [&unit, record](support::ValueOperation answers) -> mir::DirectTarget {
    return RecordMethodTarget(unit, record, answers);
  };
}

// Builds the operations of one product, a structure (LRM 7.2) or a tuple: its
// components side by side, so every operation on the whole value is that
// operation on each component in turn, the first most significant in a
// stream. Every fact about the product is read off its components rather than
// off a declaration, since a structure's methods are built while its
// declaration is being made, and another of its operations is named as `own`
// says.
class ProductBodies : public OperationBodies {
 public:
  ProductBodies(
      const mir::CompilationUnit& unit, mir::TypeId product,
      std::vector<mir::TypeId> components, OwnTarget own)
      : OperationBodies(
            unit, product, components, OneBitAnswerType(unit, components),
            std::move(own)) {
  }

  auto Equality() -> mir::CallableCode;
  auto CaseEqual() -> mir::CallableCode;
  auto BitIdentical() -> mir::CallableCode;
  auto HasUnknown() -> mir::CallableCode;
  auto BitWidth() -> mir::CallableCode;
  auto CountBits() -> mir::CallableCode;
  auto ToBitstream() -> mir::CallableCode;
  auto FromBitstream() -> mir::CallableCode;
  auto ToBitVec() -> mir::CallableCode;
  auto FromBitVec() -> mir::CallableCode;
  auto TakePositions() -> mir::CallableCode;
  auto ToPositions() -> mir::CallableCode;
  auto WritePositions() -> mir::CallableCode;
  auto FromPositions() -> mir::CallableCode;
  using OperationBodies::DeclaredDefault;
  using OperationBodies::Inequality;
  using OperationBodies::PositionCount;

 private:
  // The stream this product makes, which every body over one has.
  auto Stream() -> StreamShape {
    return *FixedStreamShapeOfParts(Unit(), Members());
  }

  // The `int` sum of what `term(i)` answers for each component `i`, added into
  // a local of `code` one statement per component.
  auto SumOverMembers(
      mir::CallableCode& code, mir::Builder block, const auto& term)
      -> mir::ExprId {
    const mir::TypeId count = Unit().builtins.int_type;
    std::vector<mir::ExprId> terms;
    terms.reserve(Members().size());
    for (std::size_t i = 0; i < Members().size(); ++i) {
      terms.push_back(term(i));
    }
    const mir::ExprId zero = mir::BuildIntLiteral(Unit(), block, 0);
    const mir::LocalId sum = code.AddLocal(count);
    return AccumulateMirBinary(
        Unit(), block, mir::BinaryOp::kAdd, zero, sum, count, terms);
  }

  // The product built from one value per component.
  auto Built(mir::Builder block, std::vector<mir::ExprId> members)
      -> mir::ExprId {
    return block.Add(
        mir::Expr{
            .data = mir::CompositeExpr{.parts = std::move(members)},
            .type = WholeType()});
  }
};

// The methods of one struct, named by the declaration being made, and what a
// variable of it starts as, which its declaration gives.
class StructBodies : public ProductBodies {
 public:
  StructBodies(
      UnitLowerer& lowerer, const hir::UnpackedStructDefinition& source,
      mir::RecordId record, mir::TypeId structure,
      std::vector<mir::TypeId> members)
      : ProductBodies(
            lowerer.Unit(), structure, std::move(members),
            MethodsOf(lowerer.Unit(), record)),
        lowerer_(&lowerer),
        source_(&source) {
  }

  auto DeclaredDefault() -> mir::CallableCode;

 private:
  const UnitLowerer* lowerer_;
  const hir::UnpackedStructDefinition* source_;
};

// LRM 11.4.5: equal when every pair of members is, and unknown where no pair is
// unequal and some pair is unknown -- which is the logical AND of the members'
// own answers, each taken to the product's own answer type first.
auto ProductBodies::Equality() -> mir::CallableCode {
  const mir::TypeId answer_type = EqualityAnswer();
  return BodyOf(
      support::ValueOperator::kEquality, {WholeType(), WholeType()},
      answer_type,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> equal;
        equal.reserve(Members().size());
        for (std::size_t i = 0; i < Members().size(); ++i) {
          const mir::ExprId lhs = Member(block, p[0], i);
          const mir::ExprId rhs = Member(block, p[1], i);
          equal.push_back(PartEqual(block, lhs, rhs, Members()[i]));
        }
        return AccumulateMirLogicalAnd(
            Unit(), block, code.AddLocal(answer_type), answer_type, equal);
      });
}

auto ProductBodies::CaseEqual() -> mir::CallableCode {
  const mir::TypeId bit = Unit().builtins.bit1;
  return BodyOf(
      support::BuiltinFn::kCaseEqual, {WholeType(), WholeType()}, bit,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> equal;
        equal.reserve(Members().size());
        for (std::size_t i = 0; i < Members().size(); ++i) {
          const mir::ExprId lhs = Member(block, p[0], i);
          const mir::ExprId rhs = Member(block, p[1], i);
          equal.push_back(BuildCaseEquality(Unit(), block, lhs, rhs));
        }
        return AccumulateMirLogicalAnd(
            Unit(), block, code.AddLocal(bit), bit, equal);
      });
}

// Identical where every pair of members is, asked in order and answered at the
// first pair that is not: one statement per member, one after another.
auto ProductBodies::BitIdentical() -> mir::CallableCode {
  const mir::TypeId boolean = Unit().builtins.machine_bool;
  return Body(
      support::BuiltinFn::kBitIdentical, {WholeType(), WholeType()}, boolean,
      [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
        for (std::size_t i = 0; i < Members().size(); ++i) {
          const mir::ExprId lhs = Member(block, p[0], i);
          const mir::ExprId rhs = Member(block, p[1], i);
          ReturnWhere(
              block,
              BuildConditionNot(
                  Unit(), block, BuildBitIdentity(Unit(), block, lhs, rhs)),
              [&](mir::Builder taken) {
                return BuildMachineBool(Unit(), taken, false);
              });
        }
        return BuildMachineBool(Unit(), block, true);
      });
}

// Holding an x or z where any member does, asked in order and answered at the
// first member that holds one.
auto ProductBodies::HasUnknown() -> mir::CallableCode {
  const mir::TypeId boolean = Unit().builtins.machine_bool;
  return Body(
      support::BuiltinFn::kHasUnknown, {WholeType()}, boolean,
      [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
        for (std::size_t i = 0; i < Members().size(); ++i) {
          ReturnWhere(
              block, BuildHasUnknown(Unit(), block, Member(block, p[0], i)),
              [&](mir::Builder taken) {
                return BuildMachineBool(Unit(), taken, true);
              });
        }
        return BuildMachineBool(Unit(), block, false);
      });
}

// LRM 20.6.2: a structure, as any product, holds the bits its members hold.
auto ProductBodies::BitWidth() -> mir::CallableCode {
  return BodyOf(
      support::BuiltinFn::kBitstreamWidth, {WholeType()},
      Unit().builtins.int_type,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        return SumOverMembers(code, block, [&](std::size_t i) {
          return BuildBitWidth(Unit(), block, Member(block, p[0], i));
        });
      });
}

// LRM 20.9: a product's stream is its members' laid end to end, so the count
// over it is the sum of theirs under the same control set.
auto ProductBodies::CountBits() -> mir::CallableCode {
  const mir::TypeId control = BitCountControlType(Unit());
  return BodyOf(
      support::BuiltinFn::kCountBits, {WholeType(), control},
      Unit().builtins.int_type,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        return SumOverMembers(code, block, [&](std::size_t i) {
          const mir::ExprId member = Member(block, p[0], i);
          const mir::ExprId control_set = Referent(block, p[1], control);
          return BuildBitCount(Unit(), block, member, control_set);
        });
      });
}

// LRM 6.24.3: the first member's stream most significant, each after it
// concatenated below. A stream is only built where the type fixes its width, so
// no member can refuse.
auto ProductBodies::ToBitstream() -> mir::CallableCode {
  const StreamShape shape = Stream();
  const mir::TypeId stream =
      mir::PackedVectorOf(Unit().types, shape.width, shape.state_kind);
  return BodyOf(
      support::BuiltinFn::kToBitstream, {WholeType()}, stream,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> streams;
        for (std::size_t i = 0; i < Members().size(); ++i) {
          auto member = BuildToBitstream(
              Unit(), block, Member(block, p[0], i), diag::SourceSpan{});
          if (!member) {
            throw InternalError(
                "ProductBodies::ToBitstream: a component of a product whose "
                "stream is fixed has none");
          }
          streams.push_back(block.AsValue(Unit().types, *member));
        }
        return BuildBitsJoin(
            Unit(), block,
            code.AddLocal(JoinedBitsType(Unit(), block.Body(), streams)),
            streams);
      });
}

// The inverse: each member reads its own width off the stream, the first from
// the most significant end (LRM 11.4.14.3). The type fixes every member's
// width, so where each one starts is known here.
auto ProductBodies::FromBitstream() -> mir::CallableCode {
  const StreamShape shape = Stream();
  const mir::TypeId stream =
      mir::PackedVectorOf(Unit().types, shape.width, shape.state_kind);
  return Body(
      support::BuiltinFn::kFromBitstream, {stream}, WholeType(),
      [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> members;
        std::uint64_t taken = 0;
        for (const mir::TypeId member : Members()) {
          const std::uint64_t width = FixedStreamShapeOf(Unit(), member)->width;
          taken += width;
          const mir::ExprId bits = block.Add(BuildPackedBitsRead(
              Unit(), block, Referent(block, p[0], stream), shape.width - taken,
              mir::PackedVectorOf(Unit().types, width, shape.state_kind)));
          members.push_back(BuildFromBitstream(Unit(), block, bits, member));
        }
        return Built(block, std::move(members));
      });
}

// LRM 6.24.3: where a member's width is the running program's, the members'
// streams laid end to end into a run of bits, the first most significant.
auto ProductBodies::ToBitVec() -> mir::CallableCode {
  const mir::TypeId run = Unit().builtins.bit_vec;
  return BodyOf(
      support::BuiltinFn::kToBitVec, {WholeType()}, run,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> streams;
        for (std::size_t i = 0; i < Members().size(); ++i) {
          auto member = BuildToBitstream(
              Unit(), block, Member(block, p[0], i), diag::SourceSpan{});
          if (!member) {
            throw InternalError(
                "ProductBodies::ToBitVec: a component of a product holding a "
                "stream has none");
          }
          streams.push_back(block.AsValue(Unit().types, *member));
        }
        return BuildRunJoin(Unit(), block, code.AddLocal(run), streams);
      });
}

// The inverse, out of a run holding exactly the product's bits: a member
// whose type fixes its width takes that many, and one whose width is the
// program's takes what the members after it leave where they are empty, so
// the first such member takes the rest (LRM 11.4.14.4).
auto ProductBodies::FromBitVec() -> mir::CallableCode {
  const mir::TypeId run = Unit().builtins.bit_vec;
  return BodyOf(
      support::BuiltinFn::kFromBitVec, {run}, WholeType(),
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        const mir::LocalId length = DeclaredCount(
            code, block,
            BuildRunLength(Unit(), block, Referent(block, p[0], run)));
        const mir::LocalId at =
            DeclaredCount(code, block, CountLiteral(block, 0));
        std::uint64_t after = 0;
        for (const mir::TypeId member : Members()) {
          after += StreamFillOf(Unit(), member).empty_width;
        }
        std::vector<mir::ExprId> members;
        for (const mir::TypeId member : Members()) {
          after -= StreamFillOf(Unit(), member).empty_width;
          if (const std::optional<StreamShape> fixed =
                  FixedStreamShapeOf(Unit(), member)) {
            const mir::ExprId source = Referent(block, p[0], run);
            const mir::ExprId start = CountNow(block, at);
            members.push_back(Held(
                code, block, member,
                BuildFromBitstream(
                    Unit(), block,
                    BuildRunLoad(Unit(), block, source, start, *fixed),
                    member)));
            const mir::ExprId lhs = CountNow(block, at);
            const mir::ExprId rhs = CountLiteral(block, fixed->width);
            SetCount(
                block, at,
                mir::BuildMachineIntBinary(
                    Unit(), block, mir::BinaryOp::kAdd, lhs, rhs));
            continue;
          }
          const mir::LocalId allotted = PartAllotment(
              code, block, length, at, CountLiteral(block, after));
          const mir::ExprId source = Referent(block, p[0], run);
          const mir::ExprId start = CountNow(block, at);
          const mir::ExprId width = CountNow(block, allotted);
          members.push_back(Held(
              code, block, member,
              BuildFromBitVec(
                  Unit(), block,
                  BuildRunExtract(Unit(), block, source, start, width),
                  member)));
          const mir::ExprId lhs = CountNow(block, at);
          const mir::ExprId rhs = CountNow(block, allotted);
          SetCount(
              block, at,
              mir::BuildMachineIntBinary(
                  Unit(), block, mir::BinaryOp::kAdd, lhs, rhs));
        }
        return Built(block, std::move(members));
      });
}

// What a variable of the struct starts as: each member at what the struct's
// declaration gives it (LRM 7.2.2), the rest at their type's default.
auto StructBodies::DeclaredDefault() -> mir::CallableCode {
  return Body(
      support::BuiltinFn::kDeclaredDefault, {}, WholeType(),
      [&](mir::Builder block, const std::vector<mir::LocalId>&) {
        return block.Add(
            BuildDeclaredStructValue(*lowerer_, block, *source_, WholeType()));
      });
}

// A product's positions are its members', the first member's first.
auto ProductBodies::TakePositions() -> mir::CallableCode {
  const mir::TypeId number = Unit().builtins.machine_int64;
  return BodyOf(
      support::BuiltinFn::kTakePositions,
      {WholeType(), WholeType(), number, number}, WholeType(),
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        const HeldRun run = HoldRun(code, block, p[2], p[3]);
        std::vector<mir::ExprId> members;
        std::uint64_t at = 0;
        for (std::size_t i = 0; i < Members().size(); ++i) {
          const mir::ExprId into = Member(block, p[0], i);
          const mir::ExprId from = Member(block, p[1], i);
          const mir::ExprId start = mir::BuildMachineCount(Unit(), block, at);
          members.push_back(
              PartTaken(code, block, Members()[i], into, from, start, run));
          at += PositionCountOf(Unit(), Members()[i]);
        }
        return Built(block, std::move(members));
      });
}

// The product whose components' positions an integral value of its positions
// holds, each read from where the components before it end.
auto ProductBodies::FromPositions() -> mir::CallableCode {
  const mir::TypeId positions_type = PositionsTypeOf(Unit(), WholeType());
  return Body(
      support::BuiltinFn::kFromPositions, {positions_type}, WholeType(),
      [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> members;
        std::uint64_t at = 0;
        for (const mir::TypeId member : Members()) {
          const mir::ExprId bits = block.Add(BuildPackedBitsRead(
              Unit(), block, Referent(block, p[0], positions_type), at,
              PositionsTypeOf(Unit(), member)));
          members.push_back(
              block.Add(BuildFromPositions(Unit(), block, bits, member)));
          at += PositionCountOf(Unit(), member);
        }
        return Built(block, std::move(members));
      });
}

// A product's positions are its components', the first at position 0, each
// laid in where the components before it end.
auto ProductBodies::ToPositions() -> mir::CallableCode {
  const mir::TypeId positions_type = PositionsTypeOf(Unit(), WholeType());
  return BodyOf(
      support::BuiltinFn::kToPositions, {WholeType()}, positions_type,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        const mir::LocalId positions = Declared(
            code, block, positions_type,
            block.Add(BuildDefaultValueExpr(Unit(), block, positions_type)));
        std::uint64_t at = 0;
        for (std::size_t i = 0; i < Members().size(); ++i) {
          LayPositions(
              block, positions, positions_type,
              BuildConstantPosition(
                  Unit(), block, static_cast<std::int64_t>(at)),
              PartPositions(block, Members()[i], Member(block, p[0], i)),
              PositionsTypeOf(Unit(), Members()[i]));
          at += PositionCountOf(Unit(), Members()[i]);
        }
        return block.ReadLocal(positions, positions_type);
      });
}

// Each component the view reaches is written, and no other.
auto ProductBodies::WritePositions() -> mir::CallableCode {
  return BodyTaking(
      support::BuiltinFn::kWritePositions, TypesOfPositionsWrite(),
      Unit().builtins.machine_bool,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        const PositionsWrite write = HoldPositionsWrite(code, block, p);
        std::uint64_t at = 0;
        for (std::size_t i = 0; i < Members().size(); ++i) {
          const std::uint64_t count = PositionCountOf(Unit(), Members()[i]);
          WhereReached(
              block, write, CountLiteral(block, at), count,
              [&](mir::Builder reached) {
                PartWritten(
                    code, reached, Members()[i],
                    AccessPath{
                        .owner = Written(reached, p[0]),
                        .descent = {DescentStep{
                            .to =
                                StepToComponent{
                                    .position =
                                        base::ComponentIndex{
                                            static_cast<std::uint32_t>(i)}},
                            .part_type = Members()[i]}}},
                    CountLiteral(reached, at), write);
              });
          at += count;
        }
        return block.ReadLocal(write.changed, Unit().builtins.machine_bool);
      });
}

// Builds the operations of one untagged union (LRM 7.3). Its one component is
// its storage, an integral value every member is a view of from its first
// position, so what holds of the whole value holds of the storage: whether it
// changed, the positions a net resolves or a procedural continuous assignment
// takes. Every operation the language defines on the whole value is its first
// member's, read through the storage, as LRM 21.2.1.6 and 11.4.14.1 have
// printing and streaming read that member; one answering a union answers one
// whose storage holds that member's positions and nothing above them.
class UnionBodies : public OperationBodies {
 public:
  UnionBodies(
      const mir::CompilationUnit& unit, mir::RecordId record,
      mir::TypeId untagged, std::vector<mir::TypeId> members)
      : OperationBodies(
            unit, untagged, members,
            EqualityAnswerOver(unit, mir::RecordKind::kUnion, members),
            MethodsOf(unit, record)) {
  }

  auto Equality() -> mir::CallableCode;
  auto CaseEqual() -> mir::CallableCode;
  auto BitIdentical() -> mir::CallableCode;
  auto HasUnknown() -> mir::CallableCode;
  auto BitWidth() -> mir::CallableCode;
  auto CountBits() -> mir::CallableCode;
  auto ToBitstream() -> mir::CallableCode;
  auto FromBitstream() -> mir::CallableCode;
  auto DeclaredDefault() -> mir::CallableCode;
  auto TakePositions() -> mir::CallableCode;
  auto ToPositions() -> mir::CallableCode;
  auto WritePositions() -> mir::CallableCode;
  auto FromPositions() -> mir::CallableCode;
  using OperationBodies::Inequality;
  using OperationBodies::PositionCount;

 private:
  [[nodiscard]] auto StorageType() const -> mir::TypeId {
    return *mir::UnionStorageOf(Unit(), WholeType());
  }
  // The storage of the union the parameter `local` refers to, as a value.
  auto Storage(mir::Builder block, mir::LocalId local) -> mir::ExprId {
    return block.AsValue(
        Unit().types,
        UnionStorage(Unit(), block, Referent(block, local, WholeType())));
  }
  // Member `i` of the union the parameter `local` refers to, read through the
  // union's storage.
  auto Read(mir::Builder block, mir::LocalId local, std::size_t i)
      -> mir::ExprId {
    return block.Add(StepRead(
        Unit(), block,
        DescentStep{
            .to =
                StepToUnionMember{
                    .position =
                        base::ComponentIndex{static_cast<std::uint32_t>(i)}},
            .part_type = Members()[i]},
        Referent(block, local, WholeType())));
  }
  auto First(mir::Builder block, mir::LocalId local) -> mir::ExprId {
    return Read(block, local, 0);
  }
  [[nodiscard]] auto FirstType() const -> mir::TypeId {
    return Members().front();
  }
  // The union whose storage is `storage`.
  auto Of(mir::Builder block, mir::ExprId storage) -> mir::ExprId {
    return block.Add(
        mir::Expr{
            .data = mir::CompositeExpr{.parts = {storage}},
            .type = WholeType()});
  }
  // The union holding `value` as its first member: the storage holding the
  // member's positions and zero above them, as a literal naming a member
  // leaves everything else zero (LRM 7.3).
  auto Holding(mir::Builder block, mir::ExprId value) -> mir::ExprId {
    const mir::TypeId storage = StorageType();
    const mir::TypeId bits = StorageBitsFor(Unit(), storage, FirstType());
    return Of(
        block, ConvertToType(
                   Unit(), block, BuildToPositions(Unit(), block, value, bits),
                   storage));
  }
  // The stream the first member makes, which every body over one has.
  auto Stream() -> mir::TypeId {
    const StreamShape shape = *FixedStreamShapeOf(Unit(), FirstType());
    return mir::PackedVectorOf(Unit().types, shape.width, shape.state_kind);
  }
};

auto UnionBodies::Equality() -> mir::CallableCode {
  const mir::TypeId answer_type = EqualityAnswer();
  return Body(
      support::ValueOperator::kEquality, {WholeType(), WholeType()},
      answer_type, [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
        const mir::ExprId lhs = First(block, p[0]);
        const mir::ExprId rhs = First(block, p[1]);
        return block.Add(BuildMirBinaryExpr(
            Unit(), block, hir::BinaryOp::kEquality, lhs, rhs, answer_type));
      });
}

auto UnionBodies::CaseEqual() -> mir::CallableCode {
  return Body(
      support::BuiltinFn::kCaseEqual, {WholeType(), WholeType()},
      Unit().builtins.bit1,
      [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
        const mir::ExprId lhs = First(block, p[0]);
        const mir::ExprId rhs = First(block, p[1]);
        return BuildCaseEquality(Unit(), block, lhs, rhs);
      });
}

// Every member is a view of the storage, so a read through any of them sees a
// change exactly where the storage's bits changed.
auto UnionBodies::BitIdentical() -> mir::CallableCode {
  return Body(
      support::BuiltinFn::kBitIdentical, {WholeType(), WholeType()},
      Unit().builtins.machine_bool,
      [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
        return BuildBitIdentity(
            Unit(), block, Storage(block, p[0]), Storage(block, p[1]));
      });
}

auto UnionBodies::HasUnknown() -> mir::CallableCode {
  return Body(
      support::BuiltinFn::kHasUnknown, {WholeType()},
      Unit().builtins.machine_bool,
      [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
        return BuildHasUnknown(Unit(), block, First(block, p[0]));
      });
}

auto UnionBodies::BitWidth() -> mir::CallableCode {
  return Body(
      support::BuiltinFn::kBitstreamWidth, {WholeType()},
      Unit().builtins.int_type,
      [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
        return BuildBitWidth(Unit(), block, First(block, p[0]));
      });
}

auto UnionBodies::CountBits() -> mir::CallableCode {
  const mir::TypeId control = BitCountControlType(Unit());
  return Body(
      support::BuiltinFn::kCountBits, {WholeType(), control},
      Unit().builtins.int_type,
      [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
        const mir::ExprId member = First(block, p[0]);
        const mir::ExprId control_set = Referent(block, p[1], control);
        return BuildBitCount(Unit(), block, member, control_set);
      });
}

auto UnionBodies::ToBitstream() -> mir::CallableCode {
  return Body(
      support::BuiltinFn::kToBitstream, {WholeType()}, Stream(),
      [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
        auto stream = BuildToBitstream(Unit(), block, First(block, p[0]), {});
        if (!stream) {
          throw InternalError(
              "UnionBodies::ToBitstream: the first member of a union whose "
              "stream is fixed has none");
        }
        return *stream;
      });
}

auto UnionBodies::FromBitstream() -> mir::CallableCode {
  const mir::TypeId stream = Stream();
  return Body(
      support::BuiltinFn::kFromBitstream, {stream}, WholeType(),
      [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
        const mir::ExprId bits = block.Add(BuildPackedBitsRead(
            Unit(), block, Referent(block, p[0], stream), 0, stream));
        return Holding(
            block, BuildFromBitstream(Unit(), block, bits, FirstType()));
      });
}

// A union nothing wrote holds its first member at that member's default (LRM
// Table 7-1).
auto UnionBodies::DeclaredDefault() -> mir::CallableCode {
  return Body(
      support::BuiltinFn::kDeclaredDefault, {}, WholeType(),
      [&](mir::Builder block, const std::vector<mir::LocalId>&) {
        return Holding(
            block,
            block.Add(BuildDefaultValueExpr(Unit(), block, FirstType())));
      });
}

// A union's positions are its storage's, so a run of them is taken there.
auto UnionBodies::TakePositions() -> mir::CallableCode {
  const mir::TypeId number = Unit().builtins.machine_int64;
  return BodyOf(
      support::BuiltinFn::kTakePositions,
      {WholeType(), WholeType(), number, number}, WholeType(),
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        const HeldRun run = HoldRun(code, block, p[2], p[3]);
        return Of(
            block, PartTaken(
                       code, block, StorageType(), Storage(block, p[0]),
                       Storage(block, p[1]), CountLiteral(block, 0), run));
      });
}

auto UnionBodies::ToPositions() -> mir::CallableCode {
  const mir::TypeId positions_type = PositionsTypeOf(Unit(), WholeType());
  return Body(
      support::BuiltinFn::kToPositions, {WholeType()}, positions_type,
      [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
        return ConvertToType(
            Unit(), block, Storage(block, p[0]), positions_type);
      });
}

auto UnionBodies::WritePositions() -> mir::CallableCode {
  return BodyTaking(
      support::BuiltinFn::kWritePositions, TypesOfPositionsWrite(),
      Unit().builtins.machine_bool,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        const PositionsWrite write = HoldPositionsWrite(code, block, p);
        WhereReached(
            block, write, CountLiteral(block, 0),
            PositionCountOf(Unit(), StorageType()), [&](mir::Builder reached) {
              PartWritten(
                  code, reached, StorageType(),
                  AccessPath{
                      .owner = Written(reached, p[0]),
                      .descent = {DescentStep{
                          .to =
                              StepToComponent{
                                  .position = base::ComponentIndex{0}},
                          .part_type = StorageType()}}},
                  CountLiteral(reached, 0), write);
            });
        return block.ReadLocal(write.changed, Unit().builtins.machine_bool);
      });
}

auto UnionBodies::FromPositions() -> mir::CallableCode {
  const mir::TypeId positions_type = PositionsTypeOf(Unit(), WholeType());
  return Body(
      support::BuiltinFn::kFromPositions, {positions_type}, WholeType(),
      [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
        return Of(
            block, ConvertToType(
                       Unit(), block,
                       block.AsValue(
                           Unit().types, Referent(block, p[0], positions_type)),
                       StorageType()));
      });
}

// Builds the operations of one tagged union (LRM 7.3.2, 11.9): each is the
// operation of the member the tag names, which two values compared have to
// name alike, and two values holding different members are unequal.
class TaggedUnionBodies : public OperationBodies {
 public:
  TaggedUnionBodies(
      const mir::CompilationUnit& unit, mir::RecordId record,
      mir::TypeId tagged, std::vector<mir::TypeId> members)
      : OperationBodies(
            unit, tagged, members,
            EqualityAnswerOver(unit, mir::RecordKind::kTaggedUnion, members),
            MethodsOf(unit, record)) {
  }

  auto Equality() -> mir::CallableCode;
  auto CaseEqual() -> mir::CallableCode;
  auto BitIdentical() -> mir::CallableCode;
  auto HasUnknown() -> mir::CallableCode;
  // A tagged union holding its first member at that member's default: LRM
  // 11.9 leaves an uninitialized tagged union undefined, and its first member
  // is Lyra's answer, as Table 7-1's is for an untagged one.
  auto DeclaredDefault() -> mir::CallableCode {
    return Body(
        support::BuiltinFn::kDeclaredDefault, {}, WholeType(),
        [&](mir::Builder block, const std::vector<mir::LocalId>&) {
          constexpr base::ComponentIndex kFirstMember{0};
          return block.Add(
              mir::MakeTaggedUnionExpr(
                  block.Add(
                      BuildDefaultValueExpr(Unit(), block, Members().front())),
                  kFirstMember, WholeType()));
        });
  }
  // Taken whole, since its tag decides which member lies there (LRM 7.3.2):
  // a run naming any of its positions takes all of it.
  auto TakePositions() -> mir::CallableCode {
    const mir::TypeId number = Unit().builtins.machine_int64;
    return Body(
        support::BuiltinFn::kTakePositions,
        {WholeType(), WholeType(), number, number}, WholeType(),
        [&](mir::Builder block, const std::vector<mir::LocalId>& p) {
          return block.AsValue(Unit().types, Whole(block, p[1]));
        });
  }
  auto ToPositions() -> mir::CallableCode;
  auto FromPositions() -> mir::CallableCode;
  using OperationBodies::Inequality;
  using OperationBodies::PositionCount;

 private:
  // Where its tag lies among its positions: above its widest member's.
  [[nodiscard]] auto TagAt() const -> std::uint64_t {
    return WidestPositionCountOf(Unit(), Members());
  }

  // What `held(taken, i)` answers where every one of the values `locals`
  // refers to holds member `i`, returned by the member they hold, and what
  // `neither(taken)` builds where they hold no one member alike. Each value's
  // tag is read once, and two tags are compared before any member is, as
  // rustc's derived comparison of an enum compares the discriminants first.
  // Each answer is built in the scope that returns it, so a member is read only
  // where it is held.
  template <typename Held, typename Neither>
  auto ByHeldMember(
      mir::CallableCode& code, mir::Builder block,
      std::span<const mir::LocalId> locals, Held held, Neither neither)
      -> mir::ExprId {
    const mir::TypeId tag_type = Unit().builtins.machine_int64;
    const mir::TypeId boolean = Unit().builtins.machine_bool;
    std::vector<mir::LocalId> tags;
    tags.reserve(locals.size());
    for (const mir::LocalId local : locals) {
      const mir::LocalId tag = code.AddLocal(tag_type);
      block.AppendStmt(
          mir::LocalDeclStmt{
              .target = tag,
              .init = block.Add(
                  mir::MakeDiscriminantExpr(Whole(block, local), tag_type))});
      tags.push_back(tag);
    }
    for (const mir::LocalId other : std::span{tags}.subspan(1)) {
      const mir::ExprId lhs = block.ReadLocal(tags.front(), tag_type);
      const mir::ExprId rhs = block.ReadLocal(other, tag_type);
      ReturnWhere(
          block,
          block.Add(
              mir::MakeBinary(
                  Unit(), block.Body(), mir::BinaryOp::kInequality, lhs, rhs,
                  boolean)),
          neither);
    }
    for (std::size_t i = 0; i < Members().size(); ++i) {
      const mir::ExprId lhs = block.ReadLocal(tags.front(), tag_type);
      const mir::ExprId rhs = mir::BuildMachineIntLiteral(
          Unit(), block, static_cast<std::int64_t>(i));
      ReturnWhere(
          block,
          block.Add(
              mir::MakeBinary(
                  Unit(), block.Body(), mir::BinaryOp::kEquality, lhs, rhs,
                  boolean)),
          [&](mir::Builder taken) { return held(taken, i); });
    }
    return neither(block);
  }
};

// A tagged union's positions are the member it holds from the first and its
// tag above its widest member's, as a packed tagged union lays them out (LRM
// 7.3.2), which an untagged union holding one is a view of (LRM 7.3). A tag
// naming no member is bits another member of such a union wrote, and holds no
// positions of a member.
auto TaggedUnionBodies::ToPositions() -> mir::CallableCode {
  const mir::TypeId positions_type = PositionsTypeOf(Unit(), WholeType());
  const std::uint64_t tag_positions = TagPositionsFor(Members().size());
  return BodyOf(
      support::BuiltinFn::kToPositions, {WholeType()}, positions_type,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        const auto none = [&](mir::Builder at) {
          return at.Add(BuildDefaultValueExpr(Unit(), at, positions_type));
        };
        return ByHeldMember(
            code, block, std::span{p}.first(1),
            [&](mir::Builder taken, std::size_t i) {
              const mir::LocalId positions =
                  Declared(code, taken, positions_type, none(taken));
              const mir::TypeId member = Members()[i];
              if (PositionCountOf(Unit(), member) != 0) {
                LayPositions(
                    taken, positions, positions_type,
                    BuildConstantPosition(Unit(), taken, 0),
                    PartPositions(taken, member, Member(taken, p[0], i)),
                    PositionsTypeOf(Unit(), member));
              }
              if (tag_positions != 0) {
                const mir::TypeId tag = mir::PackedVectorOf(
                    Unit().types, tag_positions,
                    mir::IntegralStateKind::kFourState);
                LayPositions(
                    taken, positions, positions_type,
                    BuildConstantPosition(
                        Unit(), taken, static_cast<std::int64_t>(TagAt())),
                    taken.Add(
                        mir::MakeBuiltinCall(
                            Unit(), taken.Body(),
                            support::BuiltinFn::kIntegralFromInt, std::nullopt,
                            {mir::BuildMachineIntLiteral(
                                Unit(), taken, static_cast<std::int64_t>(i))},
                            tag)),
                    tag);
              }
              return taken.ReadLocal(positions, positions_type);
            },
            none);
      });
}

// The tagged union an integral value of its positions holds: the member its
// tag names, made from the positions below the tag; one naming no member, by
// its value or by an unknown bit, holds the first member at its default, which
// is Lyra's answer for bits another member of an untagged union holding it
// wrote (LRM 7.3.2).
auto TaggedUnionBodies::FromPositions() -> mir::CallableCode {
  const mir::TypeId positions_type = PositionsTypeOf(Unit(), WholeType());
  const std::uint64_t tag_positions = TagPositionsFor(Members().size());
  return BodyOf(
      support::BuiltinFn::kFromPositions, {positions_type}, WholeType(),
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        const mir::TypeId number = Unit().builtins.machine_int64;
        // One member has no tag bits, and names that member.
        mir::ExprId tag_read = mir::BuildMachineIntLiteral(Unit(), block, 0);
        if (tag_positions != 0) {
          const mir::TypeId tag_bits = mir::PackedVectorOf(
              Unit().types, tag_positions, mir::IntegralStateKind::kFourState);
          const auto tag_bits_read = [&] {
            return block.Add(BuildPackedBitsRead(
                Unit(), block, Referent(block, p[0], positions_type), TagAt(),
                tag_bits));
          };
          // A tag with an unknown bit names no member either.
          ReturnWhere(
              block,
              BuildValueOperation(
                  Unit(), block, support::BuiltinFn::kHasUnknown,
                  tag_bits_read(), {}, Unit().builtins.machine_bool),
              [&](mir::Builder taken) {
                return taken.Add(
                    BuildDefaultValueExpr(Unit(), taken, WholeType()));
              });
          tag_read = BuildToInt64Call(Unit(), block, tag_bits_read());
        }
        const mir::LocalId tag = Declared(code, block, number, tag_read);
        for (std::size_t i = 0; i < Members().size(); ++i) {
          ReturnWhere(
              block,
              block.Add(
                  mir::MakeBinary(
                      Unit(), block.Body(), mir::BinaryOp::kEquality,
                      block.ReadLocal(tag, number),
                      mir::BuildMachineIntLiteral(
                          Unit(), block, static_cast<std::int64_t>(i)),
                      Unit().builtins.machine_bool)),
              [&](mir::Builder taken) {
                const mir::TypeId member = Members()[i];
                const mir::ExprId value =
                    PositionCountOf(Unit(), member) == 0
                        ? taken.Add(
                              BuildDefaultValueExpr(Unit(), taken, member))
                        : taken.Add(BuildFromPositions(
                              Unit(), taken,
                              taken.Add(BuildPackedBitsRead(
                                  Unit(), taken,
                                  Referent(taken, p[0], positions_type), 0,
                                  PositionsTypeOf(Unit(), member))),
                              member));
                return taken.Add(
                    mir::MakeTaggedUnionExpr(
                        value,
                        base::ComponentIndex{static_cast<std::uint32_t>(i)},
                        WholeType()));
              });
        }
        return block.Add(BuildDefaultValueExpr(Unit(), block, WholeType()));
      });
}

// LRM 11.4.5: equal where both hold one member and that member's values are,
// unequal where they hold different members.
auto TaggedUnionBodies::Equality() -> mir::CallableCode {
  const mir::TypeId answer_type = EqualityAnswer();
  return BodyOf(
      support::ValueOperator::kEquality, {WholeType(), WholeType()},
      answer_type,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        return ByHeldMember(
            code, block, p,
            [&](mir::Builder taken, std::size_t i) {
              const mir::ExprId lhs = Member(taken, p[0], i);
              const mir::ExprId rhs = Member(taken, p[1], i);
              return PartEqual(taken, lhs, rhs, Members()[i]);
            },
            [&](mir::Builder taken) {
              return KnownBit(taken, answer_type, false);
            });
      });
}

auto TaggedUnionBodies::CaseEqual() -> mir::CallableCode {
  const mir::TypeId bit = Unit().builtins.bit1;
  return BodyOf(
      support::BuiltinFn::kCaseEqual, {WholeType(), WholeType()}, bit,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        return ByHeldMember(
            code, block, p,
            [&](mir::Builder taken, std::size_t i) {
              const mir::ExprId lhs = Member(taken, p[0], i);
              const mir::ExprId rhs = Member(taken, p[1], i);
              return BuildCaseEquality(Unit(), taken, lhs, rhs);
            },
            [&](mir::Builder taken) {
              return mir::BuildBit1Literal(Unit(), taken, false);
            });
      });
}

auto TaggedUnionBodies::BitIdentical() -> mir::CallableCode {
  const mir::TypeId boolean = Unit().builtins.machine_bool;
  return BodyOf(
      support::BuiltinFn::kBitIdentical, {WholeType(), WholeType()}, boolean,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        return ByHeldMember(
            code, block, p,
            [&](mir::Builder taken, std::size_t i) {
              const mir::ExprId lhs = Member(taken, p[0], i);
              const mir::ExprId rhs = Member(taken, p[1], i);
              return BuildBitIdentity(Unit(), taken, lhs, rhs);
            },
            [&](mir::Builder taken) {
              return BuildMachineBool(Unit(), taken, false);
            });
      });
}

auto TaggedUnionBodies::HasUnknown() -> mir::CallableCode {
  const mir::TypeId boolean = Unit().builtins.machine_bool;
  return BodyOf(
      support::BuiltinFn::kHasUnknown, {WholeType()}, boolean,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        return ByHeldMember(
            code, block, std::span{p}.first(1),
            [&](mir::Builder taken, std::size_t i) {
              return BuildHasUnknown(Unit(), taken, Member(taken, p[0], i));
            },
            [&](mir::Builder taken) {
              return BuildMachineBool(Unit(), taken, false);
            });
      });
}

// Builds the operations of one fixed-size unpacked array type (LRM 7.4.2).
// An array is its elements one after another and nothing else, so every
// operation on the whole value is that operation on each element in turn
// (LRM 11.4.5, 6.24.3, 6.7.1), carried out by a loop over the positions, the
// first most significant in a stream.
class ArrayBodies : public OperationBodies {
 public:
  ArrayBodies(
      const mir::CompilationUnit& unit, mir::TypeId array, mir::TypeId element,
      std::uint64_t count)
      : OperationBodies(
            unit, array, {element}, EqualityAnswerType(unit, array),
            [array](support::ValueOperation answers) -> mir::DirectTarget {
              return mir::TypeOperationTarget{
                  .type = array, .answers = answers};
            }),
        count_(count) {
  }

  auto Equality() -> mir::CallableCode;
  auto CaseEqual() -> mir::CallableCode;
  auto BitIdentical() -> mir::CallableCode;
  auto HasUnknown() -> mir::CallableCode;
  auto BitWidth() -> mir::CallableCode;
  auto CountBits() -> mir::CallableCode;
  auto ToBitstream() -> mir::CallableCode;
  auto FromBitstream() -> mir::CallableCode;
  auto ToBitVec() -> mir::CallableCode;
  auto FromBitVec() -> mir::CallableCode;
  auto TakePositions() -> mir::CallableCode;
  auto ToPositions() -> mir::CallableCode;
  auto WritePositions() -> mir::CallableCode;
  auto FromPositions() -> mir::CallableCode;
  using OperationBodies::DeclaredDefault;
  using OperationBodies::Inequality;
  using OperationBodies::PositionCount;

 private:
  [[nodiscard]] auto ElementType() const -> mir::TypeId {
    return Members().front();
  }

  // The element at the position `position` of the array the parameter
  // `local` refers to, where it lies.
  auto Element(mir::Builder block, mir::LocalId local, mir::ExprId position)
      -> mir::ExprId {
    return block.Add(StepForRead(
        Unit(), block,
        DescentStep{
            .to = StepToElement{.position = position},
            .part_type = ElementType()},
        Whole(block, local)));
  }

  // A loop over every position of the array, after what `block` holds, whose
  // body `each(body, position)` states.
  template <typename Each>
  void ForEachPosition(mir::CallableCode& code, mir::Builder block, Each each) {
    const mir::TypeId position_type = mir::PositionType(Unit().types);
    const mir::LocalId position = code.AddLocal(position_type);
    mir::Block body;
    mir::Builder body_builder = block.Into(body);
    each(body_builder, body_builder.ReadLocal(position, position_type));
    const mir::BlockId scope = block.AddScope(std::move(body));
    const mir::ExprId tested = block.ReadLocal(position, position_type);
    const mir::ExprId bound =
        BuildConstantPosition(Unit(), block, static_cast<std::int64_t>(count_));
    const mir::ExprId within = block.Add(
        mir::MakeBinary(
            Unit(), block.Body(), mir::BinaryOp::kLessThan, tested, bound,
            OneBitAnswerType(Unit(), std::array{position_type})));
    const mir::ExprId current = block.ReadLocal(position, position_type);
    const mir::ExprId one = BuildConstantPosition(Unit(), block, 1);
    const mir::ExprId next = block.Add(
        mir::MakeBinary(
            Unit(), block.Body(), mir::BinaryOp::kAdd, current, one,
            position_type));
    std::vector<mir::ForInit> init;
    init.emplace_back(
        mir::ForInitDecl{
            .induction_var = position,
            .init = BuildConstantPosition(Unit(), block, 0)});
    block.AppendStmt(
        mir::ForStmt{
            .init = std::move(init),
            .condition = ReduceToCondition(Unit(), block, within),
            .step = {block.Add(
                mir::MakeAssignExpr(
                    Unit().builtins, block.LocalPlace(position, position_type),
                    next))},
            .scope = scope});
  }

  // What `combine(body, kept, each)` makes of every element's `each(body,
  // position)`, kept in a local of `type` that starts as `first` builds it.
  template <typename First, typename Each, typename Combine>
  auto Reduced(
      mir::CallableCode& code, mir::Builder block, mir::TypeId type,
      First first, Each each, Combine combine) -> mir::ExprId {
    const mir::LocalId kept = code.AddLocal(type);
    block.AppendStmt(mir::LocalDeclStmt{.target = kept, .init = first(block)});
    ForEachPosition(code, block, [&](mir::Builder body, mir::ExprId position) {
      const mir::ExprId so_far = body.ReadLocal(kept, type);
      const mir::ExprId element = each(body, position);
      const mir::ExprId combined = combine(body, so_far, element);
      body.AppendStmt(
          mir::ExprStmt{
              .expr = body.Add(
                  mir::MakeAssignExpr(
                      Unit().builtins, body.LocalPlace(kept, type),
                      combined))});
    });
    return block.ReadLocal(kept, type);
  }

  // The array whose element at every position `each(body, position)` builds.
  template <typename Each>
  auto Built(mir::CallableCode& code, mir::Builder block, Each each)
      -> mir::ExprId {
    const mir::LocalId built = code.AddLocal(WholeType());
    block.AppendStmt(
        mir::LocalDeclStmt{
            .target = built,
            .init =
                block.Add(BuildDefaultValueExpr(Unit(), block, WholeType()))});
    ForEachPosition(code, block, [&](mir::Builder body, mir::ExprId position) {
      const mir::ExprId element = each(body, position);
      body.AppendStmt(
          mir::ExprStmt{
              .expr = body.Add(BuildStoreExpr(
                  Unit(), body,
                  AccessPath{
                      .owner = body.LocalPlace(built, WholeType()),
                      .descent = {DescentStep{
                          .to = StepToElement{.position = position},
                          .part_type = ElementType()}}},
                  element))});
    });
    return block.ReadLocal(built, WholeType());
  }

  // The stream each element makes, and where the one at `position` starts in
  // the array's stream: the element at position 0 is the most significant.
  [[nodiscard]] auto ElementStream() -> StreamShape {
    return *FixedStreamShapeOf(Unit(), ElementType());
  }
  auto Stream() -> mir::TypeId {
    const StreamShape shape = *FixedStreamShapeOf(Unit(), WholeType());
    return mir::PackedVectorOf(Unit().types, shape.width, shape.state_kind);
  }
  auto ElementStart(mir::Builder block, mir::ExprId position) -> mir::ExprId {
    const mir::TypeId position_type = mir::PositionType(Unit().types);
    const auto width = static_cast<std::int64_t>(ElementStream().width);
    const mir::ExprId last_start = BuildConstantPosition(
        Unit(), block, (static_cast<std::int64_t>(count_) - 1) * width);
    const mir::ExprId stride = BuildConstantPosition(Unit(), block, width);
    const mir::ExprId offset = block.Add(
        mir::MakeBinary(
            Unit(), block.Body(), mir::BinaryOp::kMul, position, stride,
            position_type));
    return block.Add(
        mir::MakeBinary(
            Unit(), block.Body(), mir::BinaryOp::kSub, last_start, offset,
            position_type));
  }
  // The bits of the stream one element takes.
  auto ElementBits() -> mir::TypeId {
    const StreamShape element = ElementStream();
    return mir::PackedVectorOf(Unit().types, element.width, element.state_kind);
  }

  std::uint64_t count_;
};

// LRM 11.4.5: equal when every pair of elements is, and unknown where no pair
// is unequal and some pair is unknown.
auto ArrayBodies::Equality() -> mir::CallableCode {
  const mir::TypeId answer_type = EqualityAnswer();
  return BodyOf(
      support::ValueOperator::kEquality, {WholeType(), WholeType()},
      answer_type,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        return Reduced(
            code, block, answer_type,
            [&](mir::Builder at) { return KnownBit(at, answer_type, true); },
            [&](mir::Builder body, mir::ExprId position) {
              const mir::ExprId lhs = Element(body, p[0], position);
              const mir::ExprId rhs = Element(body, p[1], position);
              return PartEqual(body, lhs, rhs, ElementType());
            },
            [&](mir::Builder body, mir::ExprId kept, mir::ExprId each) {
              return BuildMirLogicalAnd(
                  Unit(), body, answer_type, std::vector{kept, each});
            });
      });
}

auto ArrayBodies::CaseEqual() -> mir::CallableCode {
  const mir::TypeId bit = Unit().builtins.bit1;
  return BodyOf(
      support::BuiltinFn::kCaseEqual, {WholeType(), WholeType()}, bit,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        return Reduced(
            code, block, bit,
            [&](mir::Builder at) {
              return mir::BuildBit1Literal(Unit(), at, true);
            },
            [&](mir::Builder body, mir::ExprId position) {
              const mir::ExprId lhs = Element(body, p[0], position);
              const mir::ExprId rhs = Element(body, p[1], position);
              return BuildCaseEquality(Unit(), body, lhs, rhs);
            },
            [&](mir::Builder body, mir::ExprId kept, mir::ExprId each) {
              return BuildMirLogicalAnd(
                  Unit(), body, bit, std::vector{kept, each});
            });
      });
}

auto ArrayBodies::BitIdentical() -> mir::CallableCode {
  const mir::TypeId boolean = Unit().builtins.machine_bool;
  return BodyOf(
      support::BuiltinFn::kBitIdentical, {WholeType(), WholeType()}, boolean,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        return Reduced(
            code, block, boolean,
            [&](mir::Builder at) { return BuildMachineBool(Unit(), at, true); },
            [&](mir::Builder body, mir::ExprId position) {
              const mir::ExprId lhs = Element(body, p[0], position);
              const mir::ExprId rhs = Element(body, p[1], position);
              return BuildBitIdentity(Unit(), body, lhs, rhs);
            },
            [&](mir::Builder body, mir::ExprId kept, mir::ExprId each) {
              return AllHold(Unit(), body, std::vector{kept, each});
            });
      });
}

auto ArrayBodies::HasUnknown() -> mir::CallableCode {
  const mir::TypeId boolean = Unit().builtins.machine_bool;
  return BodyOf(
      support::BuiltinFn::kHasUnknown, {WholeType()}, boolean,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        return Reduced(
            code, block, boolean,
            [&](mir::Builder at) {
              return BuildMachineBool(Unit(), at, false);
            },
            [&](mir::Builder body, mir::ExprId position) {
              return BuildHasUnknown(
                  Unit(), body, Element(body, p[0], position));
            },
            [&](mir::Builder body, mir::ExprId kept, mir::ExprId each) {
              return AnyHolds(Unit(), body, std::vector{kept, each});
            });
      });
}

// LRM 20.6.2, 20.9: an array holds the bits its elements hold, and the count
// over it is the sum of theirs under the same control set.
auto ArrayBodies::BitWidth() -> mir::CallableCode {
  const mir::TypeId count = Unit().builtins.int_type;
  return BodyOf(
      support::BuiltinFn::kBitstreamWidth, {WholeType()}, count,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        return Reduced(
            code, block, count,
            [&](mir::Builder at) {
              return mir::BuildIntLiteral(Unit(), at, 0);
            },
            [&](mir::Builder body, mir::ExprId position) {
              return BuildBitWidth(Unit(), body, Element(body, p[0], position));
            },
            [&](mir::Builder body, mir::ExprId kept, mir::ExprId each) {
              return body.Add(
                  mir::MakeBinary(
                      Unit(), body.Body(), mir::BinaryOp::kAdd, kept, each,
                      count));
            });
      });
}

auto ArrayBodies::CountBits() -> mir::CallableCode {
  const mir::TypeId count = Unit().builtins.int_type;
  const mir::TypeId control = BitCountControlType(Unit());
  return BodyOf(
      support::BuiltinFn::kCountBits, {WholeType(), control}, count,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        return Reduced(
            code, block, count,
            [&](mir::Builder at) {
              return mir::BuildIntLiteral(Unit(), at, 0);
            },
            [&](mir::Builder body, mir::ExprId position) {
              const mir::ExprId element = Element(body, p[0], position);
              const mir::ExprId control_set = Referent(body, p[1], control);
              return BuildBitCount(Unit(), body, element, control_set);
            },
            [&](mir::Builder body, mir::ExprId kept, mir::ExprId each) {
              return body.Add(
                  mir::MakeBinary(
                      Unit(), body.Body(), mir::BinaryOp::kAdd, kept, each,
                      count));
            });
      });
}

// LRM 6.24.3: the elements' own streams laid end to end, the one at position
// 0 most significant.
auto ArrayBodies::ToBitstream() -> mir::CallableCode {
  const mir::TypeId stream = Stream();
  return BodyOf(
      support::BuiltinFn::kToBitstream, {WholeType()}, stream,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        const mir::LocalId bits = code.AddLocal(stream);
        block.AppendStmt(
            mir::LocalDeclStmt{
                .target = bits,
                .init = mir::BuildIntegralLiteral(
                    Unit(), block, stream, mir::IntegralConstant{})});
        ForEachPosition(
            code, block, [&](mir::Builder body, mir::ExprId position) {
              auto element = BuildToBitstream(
                  Unit(), body, Element(body, p[0], position), {});
              if (!element) {
                throw InternalError(
                    "ArrayBodies::ToBitstream: an element of an array whose "
                    "stream is fixed has none");
              }
              const mir::ExprId owner = body.LocalPlace(bits, stream);
              const mir::ExprId start = ElementStart(body, position);
              const mir::TypeId part_type = ElementBits();
              const mir::ExprId value = body.AsValue(Unit().types, *element);
              body.AppendStmt(
                  mir::ExprStmt{
                      .expr = body.Add(BuildStoreExpr(
                          Unit(), body,
                          AccessPath{
                              .owner = owner,
                              .descent = {DescentStep{
                                  .to = StepToBits{.start = start},
                                  .part_type = part_type}}},
                          value))});
            });
        return block.ReadLocal(bits, stream);
      });
}

// The inverse: each element reads its own width off the stream, the first from
// the most significant end (LRM 11.4.14.3).
auto ArrayBodies::FromBitstream() -> mir::CallableCode {
  const mir::TypeId stream = Stream();
  return BodyOf(
      support::BuiltinFn::kFromBitstream, {stream}, WholeType(),
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        return Built(code, block, [&](mir::Builder body, mir::ExprId position) {
          const mir::ExprId start = ElementStart(body, position);
          const mir::TypeId part_type = ElementBits();
          const mir::ExprId source = Referent(body, p[0], stream);
          const mir::ExprId bits = body.Add(StepRead(
              Unit(), body,
              DescentStep{
                  .to = StepToBits{.start = start}, .part_type = part_type},
              source));
          return BuildFromBitstream(Unit(), body, bits, ElementType());
        });
      });
}

// LRM 6.24.3: where an element's width is the running program's, the elements'
// streams laid end to end into a run of bits, the one at position 0 first.
auto ArrayBodies::ToBitVec() -> mir::CallableCode {
  const mir::TypeId run = Unit().builtins.bit_vec;
  return BodyOf(
      support::BuiltinFn::kToBitVec, {WholeType()}, run,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        // The run is as wide as the array's stream before any element is
        // written, so each element's bits are written where they lie and none
        // is copied again for the elements after it.
        const mir::LocalId bits = Declared(
            code, block, run,
            BuildRunOfZeros(
                Unit(), block,
                BuildToInt64Call(
                    Unit(), block,
                    BuildBitWidth(
                        Unit(), block, Referent(block, p[0], WholeType())))));
        const mir::LocalId at =
            DeclaredCount(code, block, CountLiteral(block, 0));
        ForEachPosition(
            code, block, [&](mir::Builder body, mir::ExprId position) {
              auto element = BuildToBitstream(
                  Unit(), body, Element(body, p[0], position), {});
              if (!element) {
                throw InternalError(
                    "ArrayBodies::ToBitVec: an element of an array holding a "
                    "stream has none");
              }
              const mir::LocalId part = Declared(
                  code, body, run,
                  BuildRunOf(
                      Unit(), body, body.AsValue(Unit().types, *element)));
              const mir::ExprId start = CountNow(body, at);
              const mir::ExprId written = body.ReadLocal(part, run);
              InsertIntoRun(Unit(), body, bits, start, written);
              const mir::ExprId lhs = CountNow(body, at);
              const mir::ExprId rhs =
                  BuildRunLength(Unit(), body, body.ReadLocal(part, run));
              SetCount(
                  body, at,
                  mir::BuildMachineIntBinary(
                      Unit(), body, mir::BinaryOp::kAdd, lhs, rhs));
            });
        return block.ReadLocal(bits, run);
      });
}

// The inverse, out of a run holding exactly the array's bits: each element
// takes what the elements after it leave where they are empty, so the first
// takes the rest (LRM 11.4.14.4).
auto ArrayBodies::FromBitVec() -> mir::CallableCode {
  const mir::TypeId run = Unit().builtins.bit_vec;
  const std::uint64_t empty = StreamFillOf(Unit(), ElementType()).empty_width;
  return BodyOf(
      support::BuiltinFn::kFromBitVec, {run}, WholeType(),
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        const mir::LocalId length = DeclaredCount(
            code, block,
            BuildRunLength(Unit(), block, Referent(block, p[0], run)));
        const mir::LocalId at =
            DeclaredCount(code, block, CountLiteral(block, 0));
        const mir::LocalId after = DeclaredCount(
            code, block, CountLiteral(block, (count_ - 1) * empty));
        return Built(code, block, [&](mir::Builder body, mir::ExprId) {
          const mir::LocalId allotted =
              PartAllotment(code, body, length, at, CountNow(body, after));
          const mir::ExprId source = Referent(body, p[0], run);
          const mir::ExprId start = CountNow(body, at);
          const mir::ExprId width = CountNow(body, allotted);
          const mir::ExprId element = Held(
              code, body, ElementType(),
              BuildFromBitVec(
                  Unit(), body,
                  BuildRunExtract(Unit(), body, source, start, width),
                  ElementType()));
          const mir::ExprId at_now = CountNow(body, at);
          const mir::ExprId taken = CountNow(body, allotted);
          SetCount(
              body, at,
              mir::BuildMachineIntBinary(
                  Unit(), body, mir::BinaryOp::kAdd, at_now, taken));
          const mir::ExprId after_now = CountNow(body, after);
          const mir::ExprId element_empty = CountLiteral(body, empty);
          SetCount(
              body, after,
              mir::BuildMachineIntBinary(
                  Unit(), body, mir::BinaryOp::kSub, after_now, element_empty));
          return element;
        });
      });
}

// An array's positions are its elements', the one at position 0 first.
auto ArrayBodies::TakePositions() -> mir::CallableCode {
  const mir::TypeId number = Unit().builtins.machine_int64;
  return BodyOf(
      support::BuiltinFn::kTakePositions,
      {WholeType(), WholeType(), number, number}, WholeType(),
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        const HeldRun run = HoldRun(code, block, p[2], p[3]);
        const std::uint64_t per_element =
            PositionCountOf(Unit(), ElementType());
        return Built(code, block, [&](mir::Builder body, mir::ExprId position) {
          const mir::ExprId into = Element(body, p[0], position);
          const mir::ExprId from = Element(body, p[1], position);
          const mir::ExprId index = BuildToInt64Call(Unit(), body, position);
          const mir::ExprId stride = CountLiteral(body, per_element);
          const mir::ExprId start = mir::BuildMachineIntBinary(
              Unit(), body, mir::BinaryOp::kMul, index, stride);
          return PartTaken(code, body, ElementType(), into, from, start, run);
        });
      });
}

// An array's positions are its elements', the one at position 0 first.
auto ArrayBodies::ToPositions() -> mir::CallableCode {
  const mir::TypeId positions_type = PositionsTypeOf(Unit(), WholeType());
  const mir::TypeId position_type = mir::PositionType(Unit().types);
  return BodyOf(
      support::BuiltinFn::kToPositions, {WholeType()}, positions_type,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        const mir::LocalId positions = Declared(
            code, block, positions_type,
            block.Add(BuildDefaultValueExpr(Unit(), block, positions_type)));
        const auto per_element =
            static_cast<std::int64_t>(PositionCountOf(Unit(), ElementType()));
        ForEachPosition(
            code, block, [&](mir::Builder body, mir::ExprId position) {
              const mir::ExprId at = body.Add(
                  mir::MakeBinary(
                      Unit(), body.Body(), mir::BinaryOp::kMul, position,
                      BuildConstantPosition(Unit(), body, per_element),
                      position_type));
              LayPositions(
                  body, positions, positions_type, at,
                  PartPositions(
                      body, ElementType(), Element(body, p[0], position)),
                  PositionsTypeOf(Unit(), ElementType()));
            });
        return block.ReadLocal(positions, positions_type);
      });
}

// The array whose elements' positions an integral value of its positions
// holds, the one at position 0 first.
auto ArrayBodies::FromPositions() -> mir::CallableCode {
  const mir::TypeId positions_type = PositionsTypeOf(Unit(), WholeType());
  const mir::TypeId position_type = mir::PositionType(Unit().types);
  return BodyOf(
      support::BuiltinFn::kFromPositions, {positions_type}, WholeType(),
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        const auto per_element =
            static_cast<std::int64_t>(PositionCountOf(Unit(), ElementType()));
        return Built(code, block, [&](mir::Builder body, mir::ExprId position) {
          const mir::ExprId start = body.Add(
              mir::MakeBinary(
                  Unit(), body.Body(), mir::BinaryOp::kMul, position,
                  BuildConstantPosition(Unit(), body, per_element),
                  position_type));
          const mir::ExprId bits = body.Add(StepRead(
              Unit(), body,
              DescentStep{
                  .to = StepToBits{.start = start},
                  .part_type = PositionsTypeOf(Unit(), ElementType())},
              Referent(body, p[0], positions_type)));
          return body.Add(
              BuildFromPositions(Unit(), body, bits, ElementType()));
        });
      });
}

// Only the elements the view reaches are written: the loop starts at the
// element holding the first position the view reaches, found by dividing,
// and ends after the one holding its last, so a view over one element costs
// that element whatever the array's size.
auto ArrayBodies::WritePositions() -> mir::CallableCode {
  return BodyTaking(
      support::BuiltinFn::kWritePositions, TypesOfPositionsWrite(),
      Unit().builtins.machine_bool,
      [&](mir::CallableCode& code, mir::Builder block,
          const std::vector<mir::LocalId>& p) {
        const PositionsWrite write = HoldPositionsWrite(code, block, p);
        const mir::TypeId number = Unit().builtins.machine_int64;
        const mir::TypeId position_type = mir::PositionType(Unit().types);
        const std::uint64_t per_element =
            PositionCountOf(Unit(), ElementType());
        const mir::ExprId reached_from = mir::BuildMachineIntMax(
            Unit(), block, CountNow(block, write.start),
            CountLiteral(block, 0));
        const mir::LocalId first_element = DeclaredCount(
            code, block,
            mir::BuildMachineIntBinary(
                Unit(), block, mir::BinaryOp::kDiv, reached_from,
                CountLiteral(block, per_element)));
        const mir::ExprId view_end = mir::BuildMachineIntBinary(
            Unit(), block, mir::BinaryOp::kAdd, CountNow(block, write.start),
            CountNow(block, write.count));
        const mir::LocalId reached_to = DeclaredCount(code, block, view_end);
        const mir::LocalId element = code.AddLocal(number);

        mir::Block body;
        mir::Builder body_builder = block.Into(body);
        const mir::ExprId lo = mir::BuildMachineIntBinary(
            Unit(), body_builder, mir::BinaryOp::kMul,
            body_builder.ReadLocal(element, number),
            CountLiteral(body_builder, per_element));
        // The element's position, held so that the read of the element and
        // the write back both reach it without computing it twice.
        const mir::LocalId position = Declared(
            code, body_builder, position_type,
            body_builder.Add(
                mir::MakeBuiltinCall(
                    Unit(), body_builder.Body(),
                    support::BuiltinFn::kIntegralFromInt, std::nullopt,
                    {body_builder.ReadLocal(element, number)}, position_type)));
        PartWritten(
            code, body_builder, ElementType(),
            AccessPath{
                .owner = Written(body_builder, p[0]),
                .descent = {DescentStep{
                    .to =
                        StepToElement{
                            .position = body_builder.ReadLocal(
                                position, position_type)},
                    .part_type = ElementType()}}},
            lo, write);
        const mir::BlockId scope = block.AddScope(std::move(body));

        const mir::ExprId element_lo = mir::BuildMachineIntBinary(
            Unit(), block, mir::BinaryOp::kMul,
            block.ReadLocal(element, number), CountLiteral(block, per_element));
        const mir::ExprId before_view_end = mir::BuildMachineIntComparison(
            Unit(), block, mir::BinaryOp::kLessThan, element_lo,
            CountNow(block, reached_to));
        const mir::ExprId before_array_end = mir::BuildMachineIntComparison(
            Unit(), block, mir::BinaryOp::kLessThan,
            block.ReadLocal(element, number), CountLiteral(block, count_));
        const mir::ExprId next = mir::BuildMachineIntBinary(
            Unit(), block, mir::BinaryOp::kAdd,
            block.ReadLocal(element, number), CountLiteral(block, 1));
        std::vector<mir::ForInit> init;
        init.emplace_back(
            mir::ForInitDecl{
                .induction_var = element,
                .init = CountNow(block, first_element)});
        block.AppendStmt(
            mir::ForStmt{
                .init = std::move(init),
                .condition = AllHold(
                    Unit(), block,
                    std::array{before_view_end, before_array_end}),
                .step = {block.Add(
                    mir::MakeAssignExpr(
                        Unit().builtins, block.LocalPlace(element, number),
                        next))},
                .scope = scope});
        return block.ReadLocal(write.changed, Unit().builtins.machine_bool);
      });
}

// Which of the operations a type may lack it has: a stream, one of fixed width,
// and, where its width is the running program's, a stream written out where
// the type holds no class handle, whose object's stream is not yet built (LRM
// 11.4.14.1), and one read back where the type may be a destination (LRM
// 6.24.3); its positions read out and built from, where every bit it has is a
// fixed one (LRM 7.3); and what a net asks of it, where a net resolves it
// (LRM 6.7.1).
struct OperationsHad {
  bool stream;
  bool fixed_stream;
  bool run_written;
  bool run_read;
  bool positions;
  bool net_positions;
};

// Hands `add` each operation on the whole value every such type has, in one
// order for every kind of type, with what builds its body from `bodies`, so
// that whoever states only some of them builds only those.
template <typename Bodies, typename Add>
void StateWholeValue(Bodies& bodies, Add add) {
  add(support::BuiltinFn::kDeclaredDefault,
      [&] { return bodies.DeclaredDefault(); });
  add(support::ValueOperator::kEquality, [&] { return bodies.Equality(); });
  add(support::ValueOperator::kInequality, [&] { return bodies.Inequality(); });
  add(support::BuiltinFn::kCaseEqual, [&] { return bodies.CaseEqual(); });
  add(support::BuiltinFn::kBitIdentical, [&] { return bodies.BitIdentical(); });
  add(support::BuiltinFn::kHasUnknown, [&] { return bodies.HasUnknown(); });
  add(support::BuiltinFn::kTakePositions,
      [&] { return bodies.TakePositions(); });
  add(support::BuiltinFn::kPositionCount,
      [&] { return bodies.PositionCount(); });
}

// Hands `add` a stream's operations (LRM 6.24.3), the positions operations
// and what a net asks (LRM 6.7.1) where `had` says the type has them. A tagged
// union has no stream and no net holds one, so it states its positions
// operations itself.
template <typename Bodies, typename Add>
void StateStreamAndPositions(
    Bodies& bodies, const OperationsHad& had, Add add) {
  if (had.stream) {
    add(support::BuiltinFn::kBitstreamWidth, [&] { return bodies.BitWidth(); });
    add(support::BuiltinFn::kCountBits, [&] { return bodies.CountBits(); });
    if (had.fixed_stream) {
      add(support::BuiltinFn::kToBitstream,
          [&] { return bodies.ToBitstream(); });
      add(support::BuiltinFn::kFromBitstream,
          [&] { return bodies.FromBitstream(); });
    }
  }
  if (had.positions) {
    add(support::BuiltinFn::kToPositions, [&] { return bodies.ToPositions(); });
    add(support::BuiltinFn::kFromPositions,
        [&] { return bodies.FromPositions(); });
  }
  if (had.net_positions) {
    add(support::BuiltinFn::kWritePositions,
        [&] { return bodies.WritePositions(); });
  }
}

// Hands `add` a stream's operations as a run of bits where `had` says the
// type holds a stream whose width the running program decides (LRM 6.24.3):
// a structure or an array holding a dynamic array, a queue, an associative
// array or a string somewhere.
template <typename Bodies, typename Add>
void StateRunBodies(Bodies& bodies, const OperationsHad& had, Add add) {
  if (had.run_written) {
    add(support::BuiltinFn::kToBitVec, [&] { return bodies.ToBitVec(); });
  }
  if (had.run_read) {
    add(support::BuiltinFn::kFromBitVec, [&] { return bodies.FromBitVec(); });
  }
}

// Whether a stream of `shape` is one a value is written into and read out of,
// which takes a fixed width of at least one bit.
auto IsFixedStream(const std::optional<StreamShape>& shape) -> bool {
  return shape.has_value() && shape->width != 0;
}

// What an untagged union has of the operations a type may lack, read off its
// members.
auto OperationsHadByUnion(
    const mir::CompilationUnit& unit, std::span<const mir::TypeId> members)
    -> OperationsHad {
  return OperationsHad{
      .stream = HasBitStreamOver(unit, mir::RecordKind::kUnion, members),
      .fixed_stream = IsFixedStream(FixedStreamShapeOf(unit, members.front())),
      .run_written = false,
      .run_read = false,
      .positions = EveryPart(unit, members, HasPositions),
      .net_positions = NetResolvesOver(unit, mir::RecordKind::kUnion, members)};
}

// What a product has of the operations a type may lack, read off its
// components: a stream where every component has one, written out as a run of
// bits where none holds a class handle and read back where every one may be a
// destination, and a net's positions where a net resolves every component.
auto OperationsHadByProduct(
    const mir::CompilationUnit& unit, std::span<const mir::TypeId> components)
    -> OperationsHad {
  const bool stream = EveryPart(unit, components, HasBitStream);
  const bool fixed_stream =
      IsFixedStream(FixedStreamShapeOfParts(unit, components));
  return OperationsHad{
      .stream = stream,
      .fixed_stream = fixed_stream,
      .run_written = stream && !fixed_stream &&
                     std::ranges::none_of(
                         components,
                         [&](mir::TypeId component) {
                           return HoldsClassHandle(unit, component);
                         }),
      .run_read = stream && !fixed_stream &&
                  std::ranges::all_of(
                      components,
                      [&](mir::TypeId component) {
                        return IsStreamDestination(unit, component);
                      }),
      .positions = EveryPart(unit, components, HasPositions),
      .net_positions = EveryPart(unit, components, NetResolves)};
}

// Hands `add` every operation a product has, which a structure and a tuple
// have alike.
template <typename Bodies, typename Add>
void StateProductOperations(Bodies& bodies, const OperationsHad& had, Add add) {
  StateWholeValue(bodies, add);
  StateStreamAndPositions(bodies, had, add);
  StateRunBodies(bodies, had, add);
}

// What a fixed-size unpacked array has of the operations a type may lack,
// read off the array itself, which is interned whole before any body is
// built: a stream where its element has one, written out as a run of bits
// where it holds no class handle and read back where it may be a destination,
// and a net's positions where a net resolves it.
auto OperationsHadByArray(const mir::CompilationUnit& unit, mir::TypeId array)
    -> OperationsHad {
  const bool stream = HasBitStream(unit, array);
  const bool fixed_stream = IsFixedStream(FixedStreamShapeOf(unit, array));
  return OperationsHad{
      .stream = stream,
      .fixed_stream = fixed_stream,
      .run_written = stream && !fixed_stream && !HoldsClassHandle(unit, array),
      .run_read = stream && !fixed_stream && IsStreamDestination(unit, array),
      .positions = HasPositions(unit, array),
      .net_positions = NetResolves(unit, array)};
}

// Hands `add` every operation on a whole value `type`, a type no declaration
// states them for, has, with what builds its body: a
// fixed-size unpacked array's each element's in turn (LRM 7.4.2), and a
// tuple's each component's in turn where the tuple is a data type, every
// component being one; a tuple of anything else has none.
template <typename Add>
void ForEachStructuralOperation(
    const mir::CompilationUnit& unit, mir::TypeId type, Add add) {
  const std::optional<mir::StructuralOperationsType> kind =
      mir::StructuralOperationsTypeOf(unit.types.Get(type));
  if (!kind.has_value()) {
    throw InternalError(
        "hir_to_mir: a unit is asked the operations of a type a declaration "
        "states them for, or none does -- please report this as a bug");
  }
  switch (*kind) {
    case mir::StructuralOperationsType::kFixedArray: {
      const mir::UnpackedArrayType array =
          unit.types.Get(type).Get<mir::UnpackedArrayType>();
      const OperationsHad had = OperationsHadByArray(unit, type);
      ArrayBodies bodies(unit, type, array.element_type, array.count);
      StateWholeValue(bodies, add);
      StateStreamAndPositions(bodies, had, add);
      StateRunBodies(bodies, had, add);
      return;
    }
    case mir::StructuralOperationsType::kTuple: {
      if (!mir::IsDataType(unit.types, type)) {
        return;
      }
      std::vector<mir::TypeId> components =
          unit.types.Get(type).Get<mir::TupleType>().elements;
      const OperationsHad had = OperationsHadByProduct(unit, components);
      ProductBodies bodies(
          unit, type, std::move(components),
          [type](support::ValueOperation answers) -> mir::DirectTarget {
            return mir::TypeOperationTarget{.type = type, .answers = answers};
          });
      StateProductOperations(bodies, had, add);
      return;
    }
  }
  throw InternalError("hir_to_mir: unknown type of structural operations");
}

}  // namespace

auto NetResolves(const mir::CompilationUnit& unit, mir::TypeId type) -> bool {
  const mir::Type& t = unit.types.Get(type);
  if (t.IsIntegral()) {
    return true;
  }
  if (const std::optional<mir::RecordKind> kind =
          mir::RecordKindOf(unit, type)) {
    return NetResolvesOver(unit, *kind, PartTypes(unit, type));
  }
  if (t.Is<mir::UnpackedArrayType>()) {
    return EveryPart(unit, PartTypes(unit, type), NetResolves);
  }
  return false;
}

auto ReferenceTo(const mir::CompilationUnit& unit, mir::TypeId type)
    -> mir::TypeId {
  return unit.types.Intern(
      mir::Type{mir::ReferenceType{
          .referent = type, .mutability = mir::Mutability::kReadOnly}});
}

auto BuildBitIdentity(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId lhs,
    mir::ExprId rhs) -> mir::ExprId {
  return BuildValueOperation(
      unit, block, support::BuiltinFn::kBitIdentical, lhs, {rhs},
      unit.builtins.machine_bool);
}

namespace {

// Hands `add` every operation on a whole value the record `record` names has,
// over values of `type`, with what builds its body: a struct's read off its
// members, its start value off what its declaration `structure` gives (LRM
// 7.2), an untagged union's off its first member read through its storage
// and off the storage itself (LRM 7.3), and a tagged union's each that of the
// member its tag names, with no stream and nothing a net asks.
template <typename Add>
void ForEachRecordMethod(
    UnitLowerer& lowerer, const hir::UnpackedStructDefinition* structure,
    mir::RecordId record, mir::TypeId type, Add add) {
  const mir::CompilationUnit& unit = lowerer.Unit();
  const std::span<const mir::TypeId> members =
      mir::MembersOf(unit.GetRecord(record));
  switch (mir::RecordKindOf(unit, record)) {
    case mir::RecordKind::kStruct: {
      if (structure == nullptr) {
        throw InternalError(
            "hir_to_mir: a struct's methods are asked for without its "
            "declaration -- please report this as a bug");
      }
      StructBodies bodies(
          lowerer, *structure, record, type, {members.begin(), members.end()});
      StateProductOperations(
          bodies, OperationsHadByProduct(unit, members), add);
      return;
    }
    case mir::RecordKind::kUnion: {
      const OperationsHad had = OperationsHadByUnion(unit, members);
      UnionBodies bodies(unit, record, type, {members.begin(), members.end()});
      StateWholeValue(bodies, add);
      StateStreamAndPositions(bodies, had, add);
      return;
    }
    case mir::RecordKind::kTaggedUnion: {
      TaggedUnionBodies bodies(
          unit, record, type, {members.begin(), members.end()});
      StateWholeValue(bodies, add);
      // An untagged union holding it is a view of its positions (LRM 7.3).
      if (EveryPart(unit, members, HasPositions)) {
        add(support::BuiltinFn::kToPositions,
            [&] { return bodies.ToPositions(); });
        add(support::BuiltinFn::kFromPositions,
            [&] { return bodies.FromPositions(); });
      }
      return;
    }
  }
  throw InternalError("hir_to_mir: unknown record kind");
}

}  // namespace

auto RecordOperationsOf(
    UnitLowerer& lowerer, const hir::UnpackedStructDefinition* structure,
    mir::RecordId record, mir::TypeId type)
    -> std::vector<support::ValueOperation> {
  std::vector<support::ValueOperation> operations;
  ForEachRecordMethod(
      lowerer, structure, record, type,
      [&](support::ValueOperation answers, const auto&) {
        operations.push_back(answers);
      });
  return operations;
}

auto RecordMethodBodies(
    UnitLowerer& lowerer, const hir::UnpackedStructDefinition* structure,
    mir::RecordId record, mir::TypeId type) -> std::vector<RecordMethodBody> {
  std::vector<RecordMethodBody> bodies;
  ForEachRecordMethod(
      lowerer, structure, record, type,
      [&](support::ValueOperation answers, const auto& build) {
        bodies.push_back(RecordMethodBody{.answers = answers, .code = build()});
      });
  return bodies;
}

auto BuildStructuralOperationBody(
    const mir::CompilationUnit& unit, mir::TypeId type,
    support::ValueOperation answers) -> std::optional<mir::CallableCode> {
  std::optional<mir::CallableCode> answering;
  ForEachStructuralOperation(
      unit, type, [&](support::ValueOperation stated, const auto& build) {
        if (stated == answers) {
          answering = build();
        }
      });
  return answering;
}

auto CarriesUnknowns(const mir::CompilationUnit& unit, mir::TypeId type)
    -> bool {
  const mir::Type& t = unit.types.Get(type);
  if (t.IsIntegral()) {
    return IsFourState(t.Integral().state_kind);
  }
  return std::ranges::any_of(PartTypes(unit, type), [&](mir::TypeId part) {
    return CarriesUnknowns(unit, part);
  });
}

auto OneBitAnswerType(
    const mir::CompilationUnit& unit, std::span<const mir::TypeId> operands)
    -> mir::TypeId {
  const bool carries_unknowns = std::ranges::any_of(
      operands,
      [&](mir::TypeId operand) { return CarriesUnknowns(unit, operand); });
  return carries_unknowns
             ? mir::PackedVectorOf(
                   unit.types, 1, mir::IntegralStateKind::kFourState)
             : unit.builtins.bit1;
}

auto PositionCountOf(const mir::CompilationUnit& unit, mir::TypeId type)
    -> std::uint64_t {
  const mir::Type& t = unit.types.Get(type);
  if (t.IsIntegral()) {
    return t.Integral().bit_width;
  }
  if (const auto* real = t.As<mir::RealType>()) {
    return support::BitsOf(real->width);
  }
  if (const auto* array = t.As<mir::UnpackedArrayType>()) {
    return array->count * PositionCountOf(unit, array->element_type);
  }
  if (const std::optional<std::span<const mir::TypeId>> components =
          mir::ProductElements(unit, type)) {
    std::uint64_t count = 0;
    for (const mir::TypeId component : *components) {
      count += PositionCountOf(unit, component);
    }
    return count;
  }
  // An untagged union's are its storage's, which every member is a view of
  // from its first position (LRM 7.3).
  if (const std::optional<mir::TypeId> storage =
          mir::UnionStorageOf(unit, type)) {
    return PositionCountOf(unit, *storage);
  }
  // A tagged union's are its widest member's, every member's from the first,
  // and then its tag's, as a packed tagged union lays them out (LRM 7.3.2).
  if (mir::IsTaggedUnion(unit, type)) {
    const std::span<const mir::TypeId> members =
        *mir::OverlaidMembers(unit, type);
    return WidestPositionCountOf(unit, members) +
           TagPositionsFor(members.size());
  }
  return 1;
}

auto WidestPositionCountOf(
    const mir::CompilationUnit& unit, std::span<const mir::TypeId> members)
    -> std::uint64_t {
  std::uint64_t widest = 0;
  for (const mir::TypeId member : members) {
    widest = std::max(widest, PositionCountOf(unit, member));
  }
  return widest;
}

auto PositionsTypeOf(const mir::CompilationUnit& unit, mir::TypeId type)
    -> mir::TypeId {
  return mir::PackedVectorOf(
      unit.types, PositionCountOf(unit, type),
      mir::IntegralStateKind::kFourState);
}

auto BuildToPositions(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId value,
    mir::TypeId positions_type) -> mir::ExprId {
  const mir::TypeId type = block.Get(value).type;
  const mir::Type& t = unit.types.Get(type);
  if (t.IsIntegral()) {
    return ConvertToType(
        unit, block, block.AsValue(unit.types, value), positions_type);
  }
  if (t.Is<mir::RealType>()) {
    const mir::ExprId pattern = block.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target = support::BuiltinFn::kToBits,
                            .receiver = block.AsValue(unit.types, value)},
                    .arguments = {}},
            .type = unit.builtins.machine_int64});
    return block.Add(
        mir::MakeBuiltinCall(
            unit, block.Body(), support::BuiltinFn::kIntegralFromInt,
            std::nullopt, {pattern}, positions_type));
  }
  return ConvertToType(
      unit, block,
      BuildValueOperation(
          unit, block, support::BuiltinFn::kToPositions, value, {},
          PositionsTypeOf(unit, type)),
      positions_type);
}

auto BuildFromPositions(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId positions,
    mir::TypeId type) -> mir::Expr {
  const mir::Type& t = unit.types.Get(type);
  if (t.IsIntegral()) {
    return BuildValueConversion(unit, block, positions, type);
  }
  if (t.Is<mir::RealType>()) {
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee =
                    mir::Direct{
                        .target = support::BuiltinFn::kFromBits,
                        .type_arguments = {type}},
                .arguments = {BuildToInt64Call(unit, block, positions)}},
        .type = type};
  }
  // A type's own operation takes each value it is handed by reference.
  return mir::MakeCallExpr(
      mir::Direct{
          .target = *OwnOperationTarget(
              unit, type, support::BuiltinFn::kFromPositions)},
      {mir::Borrow(
          unit, block,
          ConvertToType(unit, block, positions, PositionsTypeOf(unit, type)))},
      type);
}

auto IsWholeValueOperation(support::ValueOperation operation) -> bool {
  return std::visit(
      Overloaded{
          [](support::ValueOperator) { return true; },
          [](support::BuiltinFn entry) {
            return std::ranges::contains(kWholeValueEntries, entry);
          }},
      operation);
}

auto BuildValueOperation(
    const mir::CompilationUnit& unit, mir::Builder block,
    support::BuiltinFn entry, std::optional<mir::ExprId> receiver,
    std::vector<mir::ExprId> operands, mir::TypeId result) -> mir::ExprId {
  const mir::TypeId deciding =
      receiver.has_value() ? block.Get(*receiver).type : result;
  if (std::optional<mir::DirectTarget> own =
          OwnOperationTarget(unit, deciding, entry)) {
    return OwnOperationCall(
        unit, block, *std::move(own), receiver, std::move(operands), result);
  }
  for (mir::ExprId& operand : operands) {
    operand = block.AsValue(unit.types, operand);
  }
  return block.Add(
      mir::MakeBuiltinCall(
          unit, block.Body(), entry, receiver, std::move(operands), result));
}

auto BuildEquality(
    const mir::CompilationUnit& unit, mir::Builder block,
    support::ValueOperator comparison, mir::ExprId lhs, mir::ExprId rhs,
    mir::TypeId result) -> mir::ExprId {
  const mir::TypeId type = block.Get(lhs).type;
  if (std::optional<mir::DirectTarget> own =
          OwnOperationTarget(unit, type, comparison)) {
    return ConvertToType(
        unit, block,
        OwnOperationCall(
            unit, block, *std::move(own), lhs, {rhs},
            EqualityAnswerType(unit, type)),
        result);
  }
  const auto operator_of = [&] {
    switch (comparison) {
      case support::ValueOperator::kEquality:
        return mir::BinaryOp::kEquality;
      case support::ValueOperator::kInequality:
        return mir::BinaryOp::kInequality;
    }
    throw InternalError("BuildEquality: unknown value operator");
  };
  const mir::BinaryOp op = operator_of();
  const mir::ExprId lhs_value = block.AsValue(unit.types, lhs);
  const mir::ExprId rhs_value = block.AsValue(unit.types, rhs);
  return block.Add(
      mir::MakeBinary(unit, block.Body(), op, lhs_value, rhs_value, result));
}

auto BuildCaseEquality(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId lhs,
    mir::ExprId rhs) -> mir::ExprId {
  return BuildValueOperation(
      unit, block, support::BuiltinFn::kCaseEqual, lhs, {rhs},
      unit.builtins.bit1);
}

auto BuildCaseComparison(
    const mir::CompilationUnit& unit, mir::Builder block,
    hir::CaseKeyword keyword, mir::ExprId value, mir::ExprId label)
    -> mir::ExprId {
  const auto entry = [&] {
    switch (keyword) {
      case hir::CaseKeyword::kCase:
        return support::BuiltinFn::kCaseEqual;
      case hir::CaseKeyword::kCasez:
        return support::BuiltinFn::kCasezEquals;
      case hir::CaseKeyword::kCasex:
        return support::BuiltinFn::kCasexEquals;
    }
    throw InternalError("BuildCaseComparison: unknown case keyword");
  };
  return BuildValueOperation(
      unit, block, entry(), value, {label}, unit.builtins.bit1);
}

auto BuildUnknownTest(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId value)
    -> mir::ExprId {
  return block.Add(
      mir::MakeBuiltinCall(
          unit, block.Body(), support::BuiltinFn::kFromBool, std::nullopt,
          {BuildHasUnknown(unit, block, value)}, unit.builtins.bit1));
}

auto BuildBitCount(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId value,
    mir::ExprId control) -> mir::ExprId {
  return BuildValueOperation(
      unit, block, support::BuiltinFn::kCountBits, value, {control},
      unit.builtins.int_type);
}

auto BuildBitWidth(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId value)
    -> mir::ExprId {
  // An integral value is as many bits as its type is wide.
  const mir::Type& type = unit.types.Get(block.Get(value).type);
  if (type.IsIntegral()) {
    return mir::BuildIntLiteral(
        unit, block, static_cast<std::int64_t>(type.Integral().bit_width));
  }
  return BuildValueOperation(
      unit, block, support::BuiltinFn::kBitstreamWidth, value, {},
      unit.builtins.int_type);
}

}  // namespace lyra::lowering::hir_to_mir
