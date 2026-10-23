#include "lyra/lowering/hir_to_mir/value_operations.hpp"

#include <algorithm>
#include <array>
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
#include "lyra/hir/type.hpp"
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
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/record_decl.hpp"
#include "lyra/mir/storage_part.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_declaration_ref.hpp"
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
auto IsValidForNet(const mir::CompilationUnit& unit, mir::TypeId type) -> bool;

// What a record of `kind` over `members` answers, which a record answers of
// its members alone, so a declaration still being made answers it too. A
// stream takes an untagged union as its first member and a tagged one not at
// all (LRM 11.4.14.1); a net admits a structure or a union of what a net
// admits (LRM 6.7.1), where a net of a tagged union is refused before any
// method is stated, so no tagged union states a net's folds; and comparing two
// untagged unions compares their first members (LRM 7.3.2).
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

auto IsValidForNetOver(
    const mir::CompilationUnit& unit, mir::RecordKind kind,
    std::span<const mir::TypeId> members) -> bool {
  switch (kind) {
    case mir::RecordKind::kStruct:
    case mir::RecordKind::kUnion:
      return EveryPart(unit, members, IsValidForNet);
    case mir::RecordKind::kTaggedUnion:
      return false;
  }
  throw InternalError("IsValidForNetOver: unknown record kind");
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

// LRM 6.24.3: neither a real nor a chandle is a bit-stream type, and a value
// holding one has no stream either. A stream takes an untagged union as its
// first member and a tagged one not at all (LRM 11.4.14.1).
auto HasBitStream(const mir::CompilationUnit& unit, mir::TypeId type) -> bool {
  if (const std::optional<mir::RecordKind> kind =
          mir::RecordKindOf(unit, type)) {
    return HasBitStreamOver(unit, *kind, PartTypes(unit, type));
  }
  const mir::Type& t = unit.types.Get(type);
  return !t.IsRealFamily() && !t.Is<mir::ChandleType>() &&
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

// LRM 6.7.1: a net's data type is a 4-state integral one, or a fixed-size
// unpacked array, structure or union of such.
auto IsValidForNet(const mir::CompilationUnit& unit, mir::TypeId type) -> bool {
  const mir::Type& t = unit.types.Get(type);
  if (t.IsIntegral()) {
    return IsFourState(t.Integral().state_kind);
  }
  if (const std::optional<mir::RecordKind> kind =
          mir::RecordKindOf(unit, type)) {
    return IsValidForNetOver(unit, *kind, PartTypes(unit, type));
  }
  if (t.Is<mir::UnpackedArrayType>()) {
    return EveryPart(unit, PartTypes(unit, type), IsValidForNet);
  }
  return false;
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

// A net's own contribution is a value of its type every bit of which is one
// fill (LRM 6.7.1).
auto FillType(const mir::CompilationUnit& unit) -> mir::TypeId {
  return mir::PackedVectorOf(unit.types, 1, mir::IntegralStateKind::kFourState);
}

auto CallOf(
    mir::Block& block, mir::Direct callee, std::vector<mir::ExprId> arguments,
    mir::TypeId result) -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = std::move(callee),
                  .arguments = std::move(arguments)},
          .type = result});
}

// How a body answering an operation of a type takes each value it is asked
// about, the value itself and every other operand: by a read-only reference,
// as Rust's comparison operators borrow theirs, so a value is read where it
// lies rather than copied in.
auto ReferenceTo(const mir::CompilationUnit& unit, mir::TypeId type)
    -> mir::TypeId {
  return unit.types.Intern(
      mir::Type{mir::ReferenceType{
          .referent = type, .mutability = mir::Mutability::kReadOnly}});
}

// A call of the body `target` names, borrowing what it is asked about.
auto OwnOperationCall(
    const mir::CompilationUnit& unit, mir::Block& block,
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
    support::BuiltinFn::kIsUnknown,       support::BuiltinFn::kBitstreamWidth,
    support::BuiltinFn::kCountBits,       support::BuiltinFn::kToBitstream,
    support::BuiltinFn::kFromBitstream,   support::BuiltinFn::kToBitVec,
    support::BuiltinFn::kFromBitVec,      support::BuiltinFn::kResolveTriState,
    support::BuiltinFn::kResolveWiredAnd, support::BuiltinFn::kResolveWiredOr,
    support::BuiltinFn::kDominating,      support::BuiltinFn::kFilledLike,
    support::BuiltinFn::kPositionCount,   support::BuiltinFn::kTakePositions,
};

// The body answering `answers` for a value of `type`, where the type answers
// it with a body of its own: the method of the structure or the union it is,
// or the body the unit states for the fixed-size array it is.
auto OwnOperationTarget(
    const mir::CompilationUnit& unit, mir::TypeId type,
    support::ValueOperation answers) -> std::optional<mir::DirectTarget> {
  if (!IsWholeValueOperation(answers)) {
    return std::nullopt;
  }
  const mir::Type& t = unit.types.Get(type);
  if (const mir::RecordRef* record = mir::RecordOf(t)) {
    return mir::RecordMethodTarget{
        .declaration = mir::RecordDeclarationOf(unit, *record),
        .answers = answers};
  }
  if (mir::OperationsAreStatedPerUnit(t)) {
    return mir::TypeOperationTarget{.type = type, .answers = answers};
  }
  return std::nullopt;
}

// Whether a value holds an unknown bit, as a machine boolean.
auto BuildHasUnknown(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId value)
    -> mir::ExprId {
  return BuildValueOperation(
      unit, block, support::BuiltinFn::kHasUnknown, value, {},
      unit.builtins.machine_bool);
}

// `value`, of `type`, with every bit the fill `fill` holds: a packed value is
// the fill repeated across its width (LRM 11.4.12.1), at the signedness it
// declares, and any other value answers for itself.
auto BuildFilled(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId value,
    mir::TypeId type, mir::ExprId fill) -> mir::ExprId {
  const mir::Type& t = unit.types.Get(type);
  if (!t.IsIntegral()) {
    return BuildValueOperation(
        unit, block, support::BuiltinFn::kFilledLike, std::nullopt,
        {value, fill}, type);
  }
  const mir::TypeId copies = mir::PackedVectorOf(
      unit.types, t.Integral().bit_width,
      unit.types.Get(block.exprs.Get(fill).type).Integral().state_kind);
  const mir::ExprId filled = block.exprs.Add(
      mir::MakeBuiltinCall(
          unit, block, support::BuiltinFn::kReplicateBits, fill, {}, copies));
  return ConvertToType(unit, block, filled, type);
}

// A net's two-contribution folds: each truth table, and what a stronger
// contribution leaves a weaker one.
constexpr std::array<support::BuiltinFn, 4> kNetFolds{
    support::BuiltinFn::kResolveTriState,
    support::BuiltinFn::kResolveWiredAnd,
    support::BuiltinFn::kResolveWiredOr,
    support::BuiltinFn::kDominating,
};

// `then_value` where `condition` holds and `else_value` where it does not,
// both of `type`.
auto Chosen(
    mir::Block& block, mir::ExprId condition, mir::ExprId then_value,
    mir::ExprId else_value, mir::TypeId type) -> mir::ExprId {
  return block.exprs.Add(
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
// `start` and `count` are where the two machine numbers lie, which a type's own
// body borrows and the library's entry reads. A value of one position is held
// whole, so the run is all of it.
auto PositionsTaken(
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId type,
    mir::ExprId into, mir::ExprId from, mir::ExprId start, mir::ExprId count)
    -> mir::ExprId {
  if (PositionCountOf(unit, type) == 1) {
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
      UnitLowerer& lowerer, mir::TypeId whole, std::vector<mir::TypeId> members,
      mir::TypeId equality_answer, OwnTarget own)
      : lowerer_(&lowerer),
        whole_(whole),
        members_(std::move(members)),
        equality_answer_(equality_answer),
        own_(std::move(own)) {
  }

 protected:
  auto Lowerer() -> UnitLowerer& {
    return *lowerer_;
  }
  auto Unit() -> mir::CompilationUnit& {
    return lowerer_->Unit();
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
  void ReturnWhere(mir::Block& block, mir::ExprId condition, Answer answer) {
    mir::Block taken;
    const mir::ExprId answered = answer(taken);
    taken.AppendStmt(mir::ReturnStmt{.value = answered});
    block.AppendStmt(
        mir::IfStmt{
            .condition = ReduceToCondition(Unit(), block, condition),
            .then_scope = block.child_scopes.Add(std::move(taken)),
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
        [&](mir::CallableCode&, mir::Block& block,
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
    mir::CallableCode code = mir::CallableCode::Defined();
    for (const mir::TypeId param : params) {
      code.params.push_back(code.AddLocal(ReferenceTo(Unit(), param)));
    }
    if (support::ReachesAReceiver(answers)) {
      code.receiver = code.params.front();
    }
    code.result_type = result;
    const std::vector<mir::LocalId> locals = code.params;
    const mir::ExprId answered = answer(code, code.Body(), locals);
    code.Body().AppendStmt(mir::ReturnStmt{.value = answered});
    return code;
  }

  // A call of the body answering another operation of this type.
  auto Own(
      mir::Block& block, support::ValueOperation answers, mir::ExprId receiver,
      std::vector<mir::ExprId> operands, mir::TypeId result) -> mir::ExprId {
    return OwnOperationCall(
        Unit(), block, own_(answers), receiver, std::move(operands), result);
  }

  // What the parameter `local` refers to, of `type`, as the place it is.
  auto Referent(mir::Block& block, mir::LocalId local, mir::TypeId type)
      -> mir::ExprId {
    return block.Deref(
        Unit().types, block.LocalPlace(local, ReferenceTo(Unit(), type)), type);
  }

  // The value of this type the parameter `local` refers to.
  auto Whole(mir::Block& block, mir::LocalId local) -> mir::ExprId {
    return Referent(block, local, whole_);
  }

  // Member `i` of the value the parameter `local` refers to, where it lies:
  // each operation asked of it settles whether it reads it or borrows it.
  auto Member(mir::Block& block, mir::LocalId local, std::size_t i)
      -> mir::ExprId {
    return block.exprs.Add(
        mir::MakeFieldAccessExpr(
            Whole(block, local),
            mir::ComponentFieldTarget{
                .position =
                    base::ComponentIndex{static_cast<std::uint32_t>(i)}},
            members_[i]));
  }

  // A local of `code` holding a value of `type`, declared after what `block`
  // holds and starting at `initial`.
  auto Declared(
      mir::CallableCode& code, mir::Block& block, mir::TypeId type,
      mir::ExprId initial) -> mir::LocalId {
    const mir::LocalId local = code.AddLocal(type);
    block.AppendStmt(mir::LocalDeclStmt{.target = local, .init = initial});
    return local;
  }

  // Such a local holding a machine count; what it holds now; it moved to
  // `value`; and a count the lowering holds, written as one.
  auto DeclaredCount(
      mir::CallableCode& code, mir::Block& block, mir::ExprId initial)
      -> mir::LocalId {
    return Declared(code, block, Unit().builtins.machine_int64, initial);
  }
  auto CountNow(mir::Block& block, mir::LocalId count) -> mir::ExprId {
    return block.ReadLocal(count, Unit().builtins.machine_int64);
  }
  void SetCount(mir::Block& block, mir::LocalId count, mir::ExprId value) {
    block.AppendStmt(
        mir::ExprStmt{
            .expr = block.exprs.Add(BuildStoreExpr(
                Unit(), block,
                AccessPath{
                    .owner =
                        block.LocalPlace(count, Unit().builtins.machine_int64),
                    .descent = {}},
                value))});
  }
  auto CountLiteral(mir::Block& block, std::uint64_t count) -> mir::ExprId {
    return mir::BuildMachineCount(Unit(), block, count);
  }

  // The bits a part whose width the program decides takes of the run of
  // `length` bits read from `at`, where the parts after it take `after` where
  // they are empty (LRM 11.4.14.4); a run too short for them is an error.
  auto PartAllotment(
      mir::CallableCode& code, mir::Block& block, mir::LocalId length,
      mir::LocalId at, mir::ExprId after) -> mir::LocalId {
    const mir::LocalId allotted = DeclaredCount(
        code, block,
        mir::BuildMachineIntBinary(
            Unit(), block, mir::BinaryOp::kSub,
            mir::BuildMachineIntBinary(
                Unit(), block, mir::BinaryOp::kSub, CountNow(block, length),
                CountNow(block, at)),
            after));
    mir::AppendRunTimeCheck(
        Unit(), block,
        mir::BuildMachineIntComparison(
            Unit(), block, mir::BinaryOp::kGreaterEqual,
            CountNow(block, allotted), CountLiteral(block, 0)),
        base::DiagMessage(
            "a stream of bits holds fewer bits than the value it is read into "
            "takes")
            .TakeText());
    return allotted;
  }

  // A value of `type` built by `value` after what `block` holds, kept in a
  // local of `code` so that what follows cannot change what it reads.
  auto Held(
      mir::CallableCode& code, mir::Block& block, mir::TypeId type,
      mir::ExprId value) -> mir::ExprId {
    return block.ReadLocal(Declared(code, block, type, value), type);
  }

  // LRM 11.4.5 `==` of two values of `part`, a member or an element, at this
  // type's equality answer type.
  auto PartEqual(
      mir::Block& block, mir::ExprId lhs, mir::ExprId rhs, mir::TypeId part)
      -> mir::ExprId {
    return ConvertToType(
        Unit(), block,
        block.exprs.Add(BuildMirBinaryExpr(
            Unit(), block, hir::BinaryOp::kEquality, lhs, rhs,
            OneBitAnswerType(Unit(), std::array{part}))),
        equality_answer_);
  }

  // A known 1 or 0 of the one-bit type `type`.
  auto KnownBit(mir::Block& block, mir::TypeId type, bool holds)
      -> mir::ExprId {
    return mir::BuildIntegralLiteral(
        Unit(), block, type,
        mir::IntegralConstant{
            .value_words = {holds ? 1U : 0U}, .state_words = {}});
  }

  // LRM 20.9: `$isunknown` is the bit whether a value holds an unknown answers.
  auto IsUnknown() -> mir::CallableCode {
    const mir::TypeId bit = Unit().builtins.bit1;
    return Body(
        support::BuiltinFn::kIsUnknown, {whole_}, bit,
        [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
          const mir::ExprId holds_unknown =
              Own(block, support::BuiltinFn::kHasUnknown, Whole(block, p[0]),
                  {}, Unit().builtins.machine_bool);
          return block.exprs.Add(
              mir::MakeBuiltinCall(
                  Unit(), block, support::BuiltinFn::kFromBool, std::nullopt,
                  {holds_unknown}, bit));
        });
  }

  // LRM 11.4.5: `!=` is the negation of `==`, unknown where that is.
  auto Inequality() -> mir::CallableCode {
    const mir::TypeId answer_type = equality_answer_;
    return Body(
        support::ValueOperator::kInequality, {whole_, whole_}, answer_type,
        [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
          const mir::ExprId equal =
              Own(block, support::ValueOperator::kEquality, Whole(block, p[0]),
                  {Whole(block, p[1])}, answer_type);
          return BuildLogicalNot(Unit(), block, equal);
        });
  }

  // The value storage of this type holds before anything writes it (LRM
  // Table 6-7, 7.3).
  auto DeclaredDefault() -> mir::CallableCode {
    return Body(
        support::BuiltinFn::kDeclaredDefault, {}, whole_,
        [&](mir::Block& block, const std::vector<mir::LocalId>&) {
          return block.exprs.Add(BuildDefaultValueExpr(Unit(), block, whole_));
        });
  }

  // A union holding its first member at that member's default (LRM Table
  // 7-1); LRM 11.9 leaves an uninitialized tagged union undefined, and its
  // first member is Lyra's answer for one too.
  auto FirstMemberDefault() -> mir::CallableCode {
    return Body(
        support::BuiltinFn::kDeclaredDefault, {}, whole_,
        [&](mir::Block& block, const std::vector<mir::LocalId>&) {
          constexpr base::ComponentIndex kFirstMember{0};
          return block.exprs.Add(
              mir::MakeUnionExpr(
                  block.exprs.Add(
                      BuildDefaultValueExpr(Unit(), block, members_.front())),
                  kFirstMember, whole_));
        });
  }

  // A value held whole is one position, so a run naming any of it names all of
  // it, and taking it is taking the value it is taken from.
  auto PositionsTakenWhole() -> mir::CallableCode {
    const mir::TypeId number = Unit().builtins.machine_int64;
    return Body(
        support::BuiltinFn::kTakePositions, {whole_, whole_, number, number},
        whole_, [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
          return block.AsValue(Unit().types, Whole(block, p[1]));
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
      mir::CallableCode& code, mir::Block& block, mir::LocalId first,
      mir::LocalId count) -> HeldRun {
    const mir::TypeId number = Unit().builtins.machine_int64;
    const mir::LocalId held_first = code.AddLocal(number);
    block.AppendStmt(
        mir::LocalDeclStmt{
            .target = held_first,
            .init =
                block.AsValue(Unit().types, Referent(block, first, number))});
    const mir::LocalId end = code.AddLocal(number);
    block.AppendStmt(
        mir::LocalDeclStmt{
            .target = end,
            .init = mir::BuildMachineIntBinary(
                Unit(), block, mir::BinaryOp::kAdd,
                block.ReadLocal(held_first, number),
                block.AsValue(Unit().types, Referent(block, count, number)))});
    return {.first = held_first, .end = end};
  }

  // A part of type `part` lying at the positions from `at`, a machine number:
  // where `run` reaches it, `into` with the positions the run names of it
  // taken from `from`, and elsewhere `into` as it stands.
  auto PartTaken(
      mir::CallableCode& code, mir::Block& block, mir::TypeId part,
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
    const mir::LocalId hi = held(
        mir::BuildMachineIntBinary(
            Unit(), block, mir::BinaryOp::kAdd, read(lo),
            CountLiteral(block, PositionCountOf(Unit(), part))));
    const std::array reaches{
        mir::BuildMachineIntComparison(
            Unit(), block, mir::BinaryOp::kLessThan, read(run.first), read(hi)),
        mir::BuildMachineIntComparison(
            Unit(), block, mir::BinaryOp::kGreaterThan, read(run.end),
            read(lo))};
    const mir::ExprId start = mir::BuildMachineIntBinary(
        Unit(), block, mir::BinaryOp::kSub,
        Chosen(
            block,
            mir::BuildMachineIntComparison(
                Unit(), block, mir::BinaryOp::kGreaterThan, read(run.first),
                read(lo)),
            read(run.first), read(lo), number),
        read(lo));
    const mir::ExprId stop = mir::BuildMachineIntBinary(
        Unit(), block, mir::BinaryOp::kSub,
        Chosen(
            block,
            mir::BuildMachineIntComparison(
                Unit(), block, mir::BinaryOp::kLessThan, read(run.end),
                read(hi)),
            read(run.end), read(hi), number),
        read(lo));
    const mir::LocalId first_taken = held(start);
    const mir::LocalId count = held(
        mir::BuildMachineIntBinary(
            Unit(), block, mir::BinaryOp::kSub, stop, read(first_taken)));
    return Chosen(
        block, AllHold(Unit(), block, reaches),
        PositionsTaken(
            Unit(), block, part, into, from,
            block.LocalPlace(first_taken, number),
            block.LocalPlace(count, number)),
        block.AsValue(Unit().types, into), part);
  }

 private:
  UnitLowerer* lowerer_;
  mir::TypeId whole_;
  std::vector<mir::TypeId> members_;
  mir::TypeId equality_answer_;
  OwnTarget own_;
};

// How a body of the record `declaration` declares names another of its
// methods: by the declaration being made rather than by the record's type,
// which has no declaration to read until every method is built.
auto MethodsOf(const mir::TypeDeclarationRef& declaration)
    -> OperationBodies::OwnTarget {
  return [declaration](support::ValueOperation answers) -> mir::DirectTarget {
    return mir::RecordMethodTarget{
        .declaration = declaration, .answers = answers};
  };
}

// Builds the methods of one struct while its declaration is being made, so
// every fact about the struct itself is read off its members rather than off a
// declaration that does not exist yet, and another of its methods is named by
// the declaration being made.
class StructBodies : public OperationBodies {
 public:
  StructBodies(
      UnitLowerer& lowerer, const hir::UnpackedStructType& source,
      const mir::TypeDeclarationRef& declaration, mir::TypeId structure,
      std::vector<mir::TypeId> members)
      : OperationBodies(
            lowerer, structure, members,
            OneBitAnswerType(lowerer.Unit(), members), MethodsOf(declaration)),
        source_(&source) {
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
  auto Fold(support::BuiltinFn fold) -> mir::CallableCode;
  auto FilledLike() -> mir::CallableCode;
  auto DeclaredDefault() -> mir::CallableCode;
  auto PositionCount() -> mir::CallableCode;
  auto TakePositions() -> mir::CallableCode;
  using OperationBodies::Inequality;
  using OperationBodies::IsUnknown;

 private:
  // The stream this struct makes, which every method over one has.
  auto Stream() -> StreamShape {
    return *FixedStreamShapeOfParts(Unit(), Members());
  }

  // The `int` sum of what `term(i)` answers for each member `i`, added into a
  // local of `code` one statement per member.
  auto SumOverMembers(
      mir::CallableCode& code, mir::Block& block, const auto& term)
      -> mir::ExprId {
    const mir::TypeId count = Unit().builtins.int_type;
    std::vector<mir::ExprId> terms;
    terms.reserve(Members().size());
    for (std::size_t i = 0; i < Members().size(); ++i) {
      terms.push_back(term(i));
    }
    return AccumulateMirBinary(
        Unit(), block, mir::BinaryOp::kAdd,
        mir::BuildIntLiteral(Unit(), block, 0), code.AddLocal(count), count,
        terms);
  }

  // The struct built from one value per member.
  auto Built(mir::Block& block, std::vector<mir::ExprId> members)
      -> mir::ExprId {
    return block.exprs.Add(
        mir::Expr{
            .data = mir::CompositeExpr{.parts = std::move(members)},
            .type = WholeType()});
  }

  const hir::UnpackedStructType* source_;
};

// LRM 11.4.5: equal when every pair of members is, and unknown where no pair is
// unequal and some pair is unknown -- which is the logical AND of the members'
// own answers, each taken to the structure's own answer type first.
auto StructBodies::Equality() -> mir::CallableCode {
  const mir::TypeId answer_type = EqualityAnswer();
  return BodyOf(
      support::ValueOperator::kEquality, {WholeType(), WholeType()},
      answer_type,
      [&](mir::CallableCode& code, mir::Block& block,
          const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> equal;
        equal.reserve(Members().size());
        for (std::size_t i = 0; i < Members().size(); ++i) {
          equal.push_back(PartEqual(
              block, Member(block, p[0], i), Member(block, p[1], i),
              Members()[i]));
        }
        return AccumulateMirLogicalAnd(
            Unit(), block, code.AddLocal(answer_type), answer_type, equal);
      });
}

auto StructBodies::CaseEqual() -> mir::CallableCode {
  const mir::TypeId bit = Unit().builtins.bit1;
  return BodyOf(
      support::BuiltinFn::kCaseEqual, {WholeType(), WholeType()}, bit,
      [&](mir::CallableCode& code, mir::Block& block,
          const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> equal;
        equal.reserve(Members().size());
        for (std::size_t i = 0; i < Members().size(); ++i) {
          equal.push_back(BuildCaseEquality(
              Unit(), block, Member(block, p[0], i), Member(block, p[1], i)));
        }
        return AccumulateMirLogicalAnd(
            Unit(), block, code.AddLocal(bit), bit, equal);
      });
}

// Identical where every pair of members is, asked in order and answered at the
// first pair that is not: one statement per member, one after another.
auto StructBodies::BitIdentical() -> mir::CallableCode {
  const mir::TypeId boolean = Unit().builtins.machine_bool;
  return Body(
      support::BuiltinFn::kBitIdentical, {WholeType(), WholeType()}, boolean,
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        for (std::size_t i = 0; i < Members().size(); ++i) {
          ReturnWhere(
              block,
              BuildConditionNot(
                  Unit(), block,
                  BuildBitIdentity(
                      Unit(), block, Member(block, p[0], i),
                      Member(block, p[1], i))),
              [&](mir::Block& taken) {
                return BuildMachineBool(Unit(), taken, false);
              });
        }
        return BuildMachineBool(Unit(), block, true);
      });
}

// Holding an x or z where any member does, asked in order and answered at the
// first member that holds one.
auto StructBodies::HasUnknown() -> mir::CallableCode {
  const mir::TypeId boolean = Unit().builtins.machine_bool;
  return Body(
      support::BuiltinFn::kHasUnknown, {WholeType()}, boolean,
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        for (std::size_t i = 0; i < Members().size(); ++i) {
          ReturnWhere(
              block, BuildHasUnknown(Unit(), block, Member(block, p[0], i)),
              [&](mir::Block& taken) {
                return BuildMachineBool(Unit(), taken, true);
              });
        }
        return BuildMachineBool(Unit(), block, false);
      });
}

// LRM 20.6.2: a structure holds the bits its members hold.
auto StructBodies::BitWidth() -> mir::CallableCode {
  return BodyOf(
      support::BuiltinFn::kBitstreamWidth, {WholeType()},
      Unit().builtins.int_type,
      [&](mir::CallableCode& code, mir::Block& block,
          const std::vector<mir::LocalId>& p) {
        return SumOverMembers(code, block, [&](std::size_t i) {
          return BuildBitWidth(Unit(), block, Member(block, p[0], i));
        });
      });
}

// LRM 20.9: a structure's stream is its members' laid end to end, so the count
// over it is the sum of theirs under the same control set.
auto StructBodies::CountBits() -> mir::CallableCode {
  const mir::TypeId control = BitCountControlType(Unit());
  return BodyOf(
      support::BuiltinFn::kCountBits, {WholeType(), control},
      Unit().builtins.int_type,
      [&](mir::CallableCode& code, mir::Block& block,
          const std::vector<mir::LocalId>& p) {
        return SumOverMembers(code, block, [&](std::size_t i) {
          return BuildBitCount(
              Unit(), block, Member(block, p[0], i),
              Referent(block, p[1], control));
        });
      });
}

// LRM 6.24.3: the first member's stream most significant, each after it
// concatenated below. A stream is only built where the type fixes its width, so
// no member can refuse.
auto StructBodies::ToBitstream() -> mir::CallableCode {
  const StreamShape shape = Stream();
  const mir::TypeId stream =
      mir::PackedVectorOf(Unit().types, shape.width, shape.state_kind);
  return BodyOf(
      support::BuiltinFn::kToBitstream, {WholeType()}, stream,
      [&](mir::CallableCode& code, mir::Block& block,
          const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> streams;
        for (std::size_t i = 0; i < Members().size(); ++i) {
          auto member = BuildToBitstream(
              Unit(), block, Member(block, p[0], i), diag::SourceSpan{});
          if (!member) {
            throw InternalError(
                "StructBodies::ToBitstream: a member of a structure whose "
                "stream is fixed has none");
          }
          streams.push_back(block.AsValue(Unit().types, *member));
        }
        return BuildBitsJoin(
            Unit(), block,
            code.AddLocal(JoinedBitsType(Unit(), block, streams)), streams);
      });
}

// The inverse: each member reads its own width off the stream, the first from
// the most significant end (LRM 11.4.14.3). The type fixes every member's
// width, so where each one starts is known here.
auto StructBodies::FromBitstream() -> mir::CallableCode {
  const StreamShape shape = Stream();
  const mir::TypeId stream =
      mir::PackedVectorOf(Unit().types, shape.width, shape.state_kind);
  return Body(
      support::BuiltinFn::kFromBitstream, {stream}, WholeType(),
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> members;
        std::uint64_t taken = 0;
        for (const mir::TypeId member : Members()) {
          const std::uint64_t width = FixedStreamShapeOf(Unit(), member)->width;
          taken += width;
          const mir::ExprId bits = block.exprs.Add(BuildPackedBitsRead(
              Lowerer(), block, Referent(block, p[0], stream),
              shape.width - taken,
              mir::PackedVectorOf(Unit().types, width, shape.state_kind)));
          members.push_back(BuildFromBitstream(Unit(), block, bits, member));
        }
        return Built(block, std::move(members));
      });
}

// LRM 6.24.3: where a member's width is the running program's, the members'
// streams laid end to end into a run of bits, the first most significant.
auto StructBodies::ToBitVec() -> mir::CallableCode {
  const mir::TypeId run = Unit().builtins.bit_vec;
  return BodyOf(
      support::BuiltinFn::kToBitVec, {WholeType()}, run,
      [&](mir::CallableCode& code, mir::Block& block,
          const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> streams;
        for (std::size_t i = 0; i < Members().size(); ++i) {
          auto member = BuildToBitstream(
              Unit(), block, Member(block, p[0], i), diag::SourceSpan{});
          if (!member) {
            throw InternalError(
                "StructBodies::ToBitVec: a member of a structure holding a "
                "stream has none");
          }
          streams.push_back(block.AsValue(Unit().types, *member));
        }
        return BuildRunJoin(Unit(), block, code.AddLocal(run), streams);
      });
}

// The inverse, out of a run holding exactly the structure's bits: a member
// whose type fixes its width takes that many, and one whose width is the
// program's takes what the members after it leave where they are empty, so
// the first such member takes the rest (LRM 11.4.14.4).
auto StructBodies::FromBitVec() -> mir::CallableCode {
  const mir::TypeId run = Unit().builtins.bit_vec;
  return BodyOf(
      support::BuiltinFn::kFromBitVec, {run}, WholeType(),
      [&](mir::CallableCode& code, mir::Block& block,
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
            members.push_back(Held(
                code, block, member,
                BuildFromBitstream(
                    Unit(), block,
                    BuildRunLoad(
                        Unit(), block, Referent(block, p[0], run),
                        CountNow(block, at), *fixed),
                    member)));
            SetCount(
                block, at,
                mir::BuildMachineIntBinary(
                    Unit(), block, mir::BinaryOp::kAdd, CountNow(block, at),
                    CountLiteral(block, fixed->width)));
            continue;
          }
          const mir::LocalId allotted = PartAllotment(
              code, block, length, at, CountLiteral(block, after));
          members.push_back(Held(
              code, block, member,
              BuildFromBitVec(
                  Unit(), block,
                  BuildRunExtract(
                      Unit(), block, Referent(block, p[0], run),
                      CountNow(block, at), CountNow(block, allotted)),
                  member)));
          SetCount(
              block, at,
              mir::BuildMachineIntBinary(
                  Unit(), block, mir::BinaryOp::kAdd, CountNow(block, at),
                  CountNow(block, allotted)));
        }
        return Built(block, std::move(members));
      });
}

// Two contributions folded member by member (LRM 6.7.1 composes a net of a
// structure out of its members' bits).
auto StructBodies::Fold(support::BuiltinFn fold) -> mir::CallableCode {
  return Body(
      fold, {WholeType(), WholeType()}, WholeType(),
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> members;
        for (std::size_t i = 0; i < Members().size(); ++i) {
          members.push_back(BuildValueOperation(
              Unit(), block, fold, Member(block, p[0], i),
              {Member(block, p[1], i)}, Members()[i]));
        }
        return Built(block, std::move(members));
      });
}

auto StructBodies::FilledLike() -> mir::CallableCode {
  const mir::TypeId fill = FillType(Unit());
  return Body(
      support::BuiltinFn::kFilledLike, {WholeType(), fill}, WholeType(),
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> members;
        for (std::size_t i = 0; i < Members().size(); ++i) {
          members.push_back(BuildFilled(
              Unit(), block, Member(block, p[0], i), Members()[i],
              Referent(block, p[1], fill)));
        }
        return Built(block, std::move(members));
      });
}

// What a variable of the struct starts as: each member at what the struct's
// declaration gives it (LRM 7.2.2), the rest at their type's default.
auto StructBodies::DeclaredDefault() -> mir::CallableCode {
  return Body(
      support::BuiltinFn::kDeclaredDefault, {}, WholeType(),
      [&](mir::Block& block, const std::vector<mir::LocalId>&) {
        return block.exprs.Add(
            BuildDeclaredStructValue(Lowerer(), block, *source_, WholeType()));
      });
}

// A structure's positions are its members', the first member's first.
auto StructBodies::PositionCount() -> mir::CallableCode {
  return Body(
      support::BuiltinFn::kPositionCount, {}, Unit().builtins.machine_int64,
      [&](mir::Block& block, const std::vector<mir::LocalId>&) {
        return CountLiteral(block, PositionCountOf(Unit(), WholeType()));
      });
}

auto StructBodies::TakePositions() -> mir::CallableCode {
  const mir::TypeId number = Unit().builtins.machine_int64;
  return BodyOf(
      support::BuiltinFn::kTakePositions,
      {WholeType(), WholeType(), number, number}, WholeType(),
      [&](mir::CallableCode& code, mir::Block& block,
          const std::vector<mir::LocalId>& p) {
        const HeldRun run = HoldRun(code, block, p[2], p[3]);
        std::vector<mir::ExprId> members;
        std::uint64_t at = 0;
        for (std::size_t i = 0; i < Members().size(); ++i) {
          members.push_back(PartTaken(
              code, block, Members()[i], Member(block, p[0], i),
              Member(block, p[1], i), mir::BuildMachineCount(Unit(), block, at),
              run));
          at += PositionCountOf(Unit(), Members()[i]);
        }
        return Built(block, std::move(members));
      });
}

// Builds the operations of one untagged union (LRM 7.3). Its members overlay
// one storage, so every operation on the whole value is its first member's,
// read as that member's type holds a value (LRM 7.3.2), as LRM 21.2.1.6 and
// 11.4.14.1 have printing and streaming read that member; one answering a
// union answers one holding the first member. Whether a union changed is
// whether a read through any of its members would see a change.
class UnionBodies : public OperationBodies {
 public:
  UnionBodies(
      UnitLowerer& lowerer, const mir::TypeDeclarationRef& declaration,
      mir::TypeId overlay, std::vector<mir::TypeId> members)
      : OperationBodies(
            lowerer, overlay, members,
            EqualityAnswerOver(
                lowerer.Unit(), mir::RecordKind::kUnion, members),
            MethodsOf(declaration)) {
  }

  auto Equality() -> mir::CallableCode;
  auto CaseEqual() -> mir::CallableCode;
  auto BitIdentical() -> mir::CallableCode;
  auto HasUnknown() -> mir::CallableCode;
  auto BitWidth() -> mir::CallableCode;
  auto CountBits() -> mir::CallableCode;
  auto ToBitstream() -> mir::CallableCode;
  auto FromBitstream() -> mir::CallableCode;
  auto Fold(support::BuiltinFn fold) -> mir::CallableCode;
  auto FilledLike() -> mir::CallableCode;
  auto DeclaredDefault() -> mir::CallableCode {
    return FirstMemberDefault();
  }
  // A union is one storage its members read differently (LRM 7.3), so it is
  // held whole.
  auto TakePositions() -> mir::CallableCode {
    return PositionsTakenWhole();
  }
  using OperationBodies::Inequality;
  using OperationBodies::IsUnknown;

 private:
  // Member `i` of the union the parameter `local` refers to, read as its type
  // holds a value.
  auto Read(mir::Block& block, mir::LocalId local, std::size_t i)
      -> mir::ExprId {
    return mir::ReadIfOverlaid(Unit(), block, Member(block, local, i));
  }
  auto First(mir::Block& block, mir::LocalId local) -> mir::ExprId {
    return Read(block, local, 0);
  }
  [[nodiscard]] auto FirstType() const -> mir::TypeId {
    return Members().front();
  }
  // The union holding `value` as its first member.
  auto Holding(mir::Block& block, mir::ExprId value) -> mir::ExprId {
    return block.exprs.Add(
        mir::Expr{
            .data =
                mir::UnionExpr{
                    .member = base::ComponentIndex{0}, .value = value},
            .type = WholeType()});
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
      answer_type, [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        return block.exprs.Add(BuildMirBinaryExpr(
            Unit(), block, hir::BinaryOp::kEquality, First(block, p[0]),
            First(block, p[1]), answer_type));
      });
}

auto UnionBodies::CaseEqual() -> mir::CallableCode {
  return Body(
      support::BuiltinFn::kCaseEqual, {WholeType(), WholeType()},
      Unit().builtins.bit1,
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        return BuildCaseEquality(
            Unit(), block, First(block, p[0]), First(block, p[1]));
      });
}

auto UnionBodies::BitIdentical() -> mir::CallableCode {
  return Body(
      support::BuiltinFn::kBitIdentical, {WholeType(), WholeType()},
      Unit().builtins.machine_bool,
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        for (std::size_t i = 0; i < Members().size(); ++i) {
          const mir::ExprId identical = BuildBitIdentity(
              Unit(), block, Read(block, p[0], i), Read(block, p[1], i));
          ReturnWhere(
              block, BuildConditionNot(Unit(), block, identical),
              [&](mir::Block& taken) {
                return BuildMachineBool(Unit(), taken, false);
              });
        }
        return BuildMachineBool(Unit(), block, true);
      });
}

auto UnionBodies::HasUnknown() -> mir::CallableCode {
  return Body(
      support::BuiltinFn::kHasUnknown, {WholeType()},
      Unit().builtins.machine_bool,
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        return BuildHasUnknown(Unit(), block, First(block, p[0]));
      });
}

auto UnionBodies::BitWidth() -> mir::CallableCode {
  return Body(
      support::BuiltinFn::kBitstreamWidth, {WholeType()},
      Unit().builtins.int_type,
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        return BuildBitWidth(Unit(), block, First(block, p[0]));
      });
}

auto UnionBodies::CountBits() -> mir::CallableCode {
  const mir::TypeId control = BitCountControlType(Unit());
  return Body(
      support::BuiltinFn::kCountBits, {WholeType(), control},
      Unit().builtins.int_type,
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        return BuildBitCount(
            Unit(), block, First(block, p[0]), Referent(block, p[1], control));
      });
}

auto UnionBodies::ToBitstream() -> mir::CallableCode {
  return Body(
      support::BuiltinFn::kToBitstream, {WholeType()}, Stream(),
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
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
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        const mir::ExprId bits = block.exprs.Add(BuildPackedBitsRead(
            Lowerer(), block, Referent(block, p[0], stream), 0, stream));
        return Holding(
            block, BuildFromBitstream(Unit(), block, bits, FirstType()));
      });
}

auto UnionBodies::Fold(support::BuiltinFn fold) -> mir::CallableCode {
  return Body(
      fold, {WholeType(), WholeType()}, WholeType(),
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        return Holding(
            block, BuildValueOperation(
                       Unit(), block, fold, First(block, p[0]),
                       {First(block, p[1])}, FirstType()));
      });
}

auto UnionBodies::FilledLike() -> mir::CallableCode {
  const mir::TypeId fill = FillType(Unit());
  return Body(
      support::BuiltinFn::kFilledLike, {WholeType(), fill}, WholeType(),
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        return Holding(
            block, BuildFilled(
                       Unit(), block, First(block, p[0]), FirstType(),
                       Referent(block, p[1], fill)));
      });
}

// Builds the operations of one tagged union (LRM 7.3.2, 11.9): each is the
// operation of the member the tag names, which two values compared have to
// name alike, and two values holding different members are unequal.
class TaggedUnionBodies : public OperationBodies {
 public:
  TaggedUnionBodies(
      UnitLowerer& lowerer, const mir::TypeDeclarationRef& declaration,
      mir::TypeId tagged, std::vector<mir::TypeId> members)
      : OperationBodies(
            lowerer, tagged, members,
            EqualityAnswerOver(
                lowerer.Unit(), mir::RecordKind::kTaggedUnion, members),
            MethodsOf(declaration)) {
  }

  auto Equality() -> mir::CallableCode;
  auto CaseEqual() -> mir::CallableCode;
  auto BitIdentical() -> mir::CallableCode;
  auto HasUnknown() -> mir::CallableCode;
  auto DeclaredDefault() -> mir::CallableCode {
    return FirstMemberDefault();
  }
  // Held whole, as an untagged union is.
  auto TakePositions() -> mir::CallableCode {
    return PositionsTakenWhole();
  }
  using OperationBodies::Inequality;
  using OperationBodies::IsUnknown;

 private:
  // What `held(taken, i)` answers where every one of the values `locals`
  // refers to holds member `i`, returned by the member they hold, and what
  // `neither(taken)` builds where they hold no one member alike. Each value's
  // tag is read once, and two tags are compared before any member is, as
  // rustc's derived comparison of an enum compares the discriminants first. A
  // `void` member carries no information (LRM 7.3.2), so values holding it are
  // alike in every respect and answer what `alike(taken)` builds. Each answer
  // is built in the scope that returns it, so a member is read only where it
  // is held.
  template <typename Held, typename Alike, typename Neither>
  auto ByHeldMember(
      mir::CallableCode& code, mir::Block& block,
      std::span<const mir::LocalId> locals, Held held, Alike alike,
      Neither neither) -> mir::ExprId {
    const mir::TypeId tag_type = Unit().builtins.machine_int64;
    const mir::TypeId boolean = Unit().builtins.machine_bool;
    std::vector<mir::LocalId> tags;
    tags.reserve(locals.size());
    for (const mir::LocalId local : locals) {
      const mir::LocalId tag = code.AddLocal(tag_type);
      block.AppendStmt(
          mir::LocalDeclStmt{
              .target = tag,
              .init = block.exprs.Add(
                  mir::MakeDiscriminantExpr(Whole(block, local), tag_type))});
      tags.push_back(tag);
    }
    for (const mir::LocalId other : std::span{tags}.subspan(1)) {
      ReturnWhere(
          block,
          block.exprs.Add(
              mir::MakeBinary(
                  Unit(), block, mir::BinaryOp::kInequality,
                  block.ReadLocal(tags.front(), tag_type),
                  block.ReadLocal(other, tag_type), boolean)),
          neither);
    }
    for (std::size_t i = 0; i < Members().size(); ++i) {
      const bool carries_nothing =
          Unit().types.Get(Members()[i]).Is<mir::EmptyType>();
      ReturnWhere(
          block,
          block.exprs.Add(
              mir::MakeBinary(
                  Unit(), block, mir::BinaryOp::kEquality,
                  block.ReadLocal(tags.front(), tag_type),
                  mir::BuildMachineIntLiteral(
                      Unit(), block, static_cast<std::int64_t>(i)),
                  boolean)),
          [&](mir::Block& taken) {
            return carries_nothing ? alike(taken) : held(taken, i);
          });
    }
    return neither(block);
  }
};

// LRM 11.4.5: equal where both hold one member and that member's values are,
// unequal where they hold different members.
auto TaggedUnionBodies::Equality() -> mir::CallableCode {
  const mir::TypeId answer_type = EqualityAnswer();
  return BodyOf(
      support::ValueOperator::kEquality, {WholeType(), WholeType()},
      answer_type,
      [&](mir::CallableCode& code, mir::Block& block,
          const std::vector<mir::LocalId>& p) {
        return ByHeldMember(
            code, block, p,
            [&](mir::Block& taken, std::size_t i) {
              return PartEqual(
                  taken, Member(taken, p[0], i), Member(taken, p[1], i),
                  Members()[i]);
            },
            [&](mir::Block& taken) {
              return KnownBit(taken, answer_type, true);
            },
            [&](mir::Block& taken) {
              return KnownBit(taken, answer_type, false);
            });
      });
}

auto TaggedUnionBodies::CaseEqual() -> mir::CallableCode {
  const mir::TypeId bit = Unit().builtins.bit1;
  return BodyOf(
      support::BuiltinFn::kCaseEqual, {WholeType(), WholeType()}, bit,
      [&](mir::CallableCode& code, mir::Block& block,
          const std::vector<mir::LocalId>& p) {
        return ByHeldMember(
            code, block, p,
            [&](mir::Block& taken, std::size_t i) {
              return BuildCaseEquality(
                  Unit(), taken, Member(taken, p[0], i),
                  Member(taken, p[1], i));
            },
            [&](mir::Block& taken) {
              return mir::BuildBit1Literal(Unit(), taken, true);
            },
            [&](mir::Block& taken) {
              return mir::BuildBit1Literal(Unit(), taken, false);
            });
      });
}

auto TaggedUnionBodies::BitIdentical() -> mir::CallableCode {
  const mir::TypeId boolean = Unit().builtins.machine_bool;
  return BodyOf(
      support::BuiltinFn::kBitIdentical, {WholeType(), WholeType()}, boolean,
      [&](mir::CallableCode& code, mir::Block& block,
          const std::vector<mir::LocalId>& p) {
        return ByHeldMember(
            code, block, p,
            [&](mir::Block& taken, std::size_t i) {
              return BuildBitIdentity(
                  Unit(), taken, Member(taken, p[0], i),
                  Member(taken, p[1], i));
            },
            [&](mir::Block& taken) {
              return BuildMachineBool(Unit(), taken, true);
            },
            [&](mir::Block& taken) {
              return BuildMachineBool(Unit(), taken, false);
            });
      });
}

auto TaggedUnionBodies::HasUnknown() -> mir::CallableCode {
  const mir::TypeId boolean = Unit().builtins.machine_bool;
  return BodyOf(
      support::BuiltinFn::kHasUnknown, {WholeType()}, boolean,
      [&](mir::CallableCode& code, mir::Block& block,
          const std::vector<mir::LocalId>& p) {
        return ByHeldMember(
            code, block, std::span{p}.first(1),
            [&](mir::Block& taken, std::size_t i) {
              return BuildHasUnknown(Unit(), taken, Member(taken, p[0], i));
            },
            [&](mir::Block& taken) {
              return BuildMachineBool(Unit(), taken, false);
            },
            [&](mir::Block& taken) {
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
      UnitLowerer& lowerer, mir::TypeId array, mir::TypeId element,
      std::uint64_t count)
      : OperationBodies(
            lowerer, array, {element},
            EqualityAnswerType(lowerer.Unit(), array),
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
  auto Fold(support::BuiltinFn fold) -> mir::CallableCode;
  auto FilledLike() -> mir::CallableCode;
  auto TakePositions() -> mir::CallableCode;
  using OperationBodies::DeclaredDefault;
  using OperationBodies::Inequality;
  using OperationBodies::IsUnknown;

 private:
  [[nodiscard]] auto ElementType() const -> mir::TypeId {
    return Members().front();
  }

  // The element at the position `position` of the array the parameter
  // `local` refers to, where it lies.
  auto Element(mir::Block& block, mir::LocalId local, mir::ExprId position)
      -> mir::ExprId {
    return block.exprs.Add(StepForRead(
        Unit(), block,
        DescentStep{
            .to = StepToElement{.position = position},
            .part_type = ElementType()},
        Whole(block, local)));
  }

  // A loop over every position of the array, after what `block` holds, whose
  // body `each(body, position)` states.
  template <typename Each>
  void ForEachPosition(mir::CallableCode& code, mir::Block& block, Each each) {
    const mir::TypeId position_type = mir::PositionType(Unit().types);
    const mir::LocalId position = code.AddLocal(position_type);
    mir::Block body;
    each(body, body.ReadLocal(position, position_type));
    const mir::BlockId scope = block.child_scopes.Add(std::move(body));
    const mir::ExprId within = block.exprs.Add(
        mir::MakeBinary(
            Unit(), block, mir::BinaryOp::kLessThan,
            block.ReadLocal(position, position_type),
            BuildConstantPosition(
                Unit(), block, static_cast<std::int64_t>(count_)),
            OneBitAnswerType(Unit(), std::array{position_type})));
    const mir::ExprId next = block.exprs.Add(
        mir::MakeBinary(
            Unit(), block, mir::BinaryOp::kAdd,
            block.ReadLocal(position, position_type),
            BuildConstantPosition(Unit(), block, 1), position_type));
    std::vector<mir::ForInit> init;
    init.emplace_back(
        mir::ForInitDecl{
            .induction_var = position,
            .init = BuildConstantPosition(Unit(), block, 0)});
    block.AppendStmt(
        mir::ForStmt{
            .init = std::move(init),
            .condition = ReduceToCondition(Unit(), block, within),
            .step = {block.exprs.Add(
                mir::MakeAssignExpr(
                    Unit().builtins, block.LocalPlace(position, position_type),
                    next))},
            .scope = scope});
  }

  // What `combine(body, kept, each)` makes of every element's `each(body,
  // position)`, kept in a local of `type` that starts as `first` builds it.
  template <typename First, typename Each, typename Combine>
  auto Reduced(
      mir::CallableCode& code, mir::Block& block, mir::TypeId type, First first,
      Each each, Combine combine) -> mir::ExprId {
    const mir::LocalId kept = code.AddLocal(type);
    block.AppendStmt(mir::LocalDeclStmt{.target = kept, .init = first(block)});
    ForEachPosition(code, block, [&](mir::Block& body, mir::ExprId position) {
      const mir::ExprId combined =
          combine(body, body.ReadLocal(kept, type), each(body, position));
      body.AppendStmt(
          mir::ExprStmt{
              .expr = body.exprs.Add(
                  mir::MakeAssignExpr(
                      Unit().builtins, body.LocalPlace(kept, type),
                      combined))});
    });
    return block.ReadLocal(kept, type);
  }

  // The array whose element at every position `each(body, position)` builds.
  template <typename Each>
  auto Built(mir::CallableCode& code, mir::Block& block, Each each)
      -> mir::ExprId {
    const mir::LocalId built = code.AddLocal(WholeType());
    block.AppendStmt(
        mir::LocalDeclStmt{
            .target = built,
            .init = block.exprs.Add(
                BuildDefaultValueExpr(Unit(), block, WholeType()))});
    ForEachPosition(code, block, [&](mir::Block& body, mir::ExprId position) {
      const mir::ExprId element = each(body, position);
      body.AppendStmt(
          mir::ExprStmt{
              .expr = body.exprs.Add(BuildStoreExpr(
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
  auto ElementStart(mir::Block& block, mir::ExprId position) -> mir::ExprId {
    const mir::TypeId position_type = mir::PositionType(Unit().types);
    const auto width = static_cast<std::int64_t>(ElementStream().width);
    return block.exprs.Add(
        mir::MakeBinary(
            Unit(), block, mir::BinaryOp::kSub,
            BuildConstantPosition(
                Unit(), block, (static_cast<std::int64_t>(count_) - 1) * width),
            block.exprs.Add(
                mir::MakeBinary(
                    Unit(), block, mir::BinaryOp::kMul, position,
                    BuildConstantPosition(Unit(), block, width),
                    position_type)),
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
      [&](mir::CallableCode& code, mir::Block& block,
          const std::vector<mir::LocalId>& p) {
        return Reduced(
            code, block, answer_type,
            [&](mir::Block& at) { return KnownBit(at, answer_type, true); },
            [&](mir::Block& body, mir::ExprId position) {
              return PartEqual(
                  body, Element(body, p[0], position),
                  Element(body, p[1], position), ElementType());
            },
            [&](mir::Block& body, mir::ExprId kept, mir::ExprId each) {
              return BuildMirLogicalAnd(
                  Unit(), body, answer_type, std::vector{kept, each});
            });
      });
}

auto ArrayBodies::CaseEqual() -> mir::CallableCode {
  const mir::TypeId bit = Unit().builtins.bit1;
  return BodyOf(
      support::BuiltinFn::kCaseEqual, {WholeType(), WholeType()}, bit,
      [&](mir::CallableCode& code, mir::Block& block,
          const std::vector<mir::LocalId>& p) {
        return Reduced(
            code, block, bit,
            [&](mir::Block& at) {
              return mir::BuildBit1Literal(Unit(), at, true);
            },
            [&](mir::Block& body, mir::ExprId position) {
              return BuildCaseEquality(
                  Unit(), body, Element(body, p[0], position),
                  Element(body, p[1], position));
            },
            [&](mir::Block& body, mir::ExprId kept, mir::ExprId each) {
              return BuildMirLogicalAnd(
                  Unit(), body, bit, std::vector{kept, each});
            });
      });
}

auto ArrayBodies::BitIdentical() -> mir::CallableCode {
  const mir::TypeId boolean = Unit().builtins.machine_bool;
  return BodyOf(
      support::BuiltinFn::kBitIdentical, {WholeType(), WholeType()}, boolean,
      [&](mir::CallableCode& code, mir::Block& block,
          const std::vector<mir::LocalId>& p) {
        return Reduced(
            code, block, boolean,
            [&](mir::Block& at) { return BuildMachineBool(Unit(), at, true); },
            [&](mir::Block& body, mir::ExprId position) {
              return BuildBitIdentity(
                  Unit(), body, Element(body, p[0], position),
                  Element(body, p[1], position));
            },
            [&](mir::Block& body, mir::ExprId kept, mir::ExprId each) {
              return AllHold(Unit(), body, std::vector{kept, each});
            });
      });
}

auto ArrayBodies::HasUnknown() -> mir::CallableCode {
  const mir::TypeId boolean = Unit().builtins.machine_bool;
  return BodyOf(
      support::BuiltinFn::kHasUnknown, {WholeType()}, boolean,
      [&](mir::CallableCode& code, mir::Block& block,
          const std::vector<mir::LocalId>& p) {
        return Reduced(
            code, block, boolean,
            [&](mir::Block& at) { return BuildMachineBool(Unit(), at, false); },
            [&](mir::Block& body, mir::ExprId position) {
              return BuildHasUnknown(
                  Unit(), body, Element(body, p[0], position));
            },
            [&](mir::Block& body, mir::ExprId kept, mir::ExprId each) {
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
      [&](mir::CallableCode& code, mir::Block& block,
          const std::vector<mir::LocalId>& p) {
        return Reduced(
            code, block, count,
            [&](mir::Block& at) { return mir::BuildIntLiteral(Unit(), at, 0); },
            [&](mir::Block& body, mir::ExprId position) {
              return BuildBitWidth(Unit(), body, Element(body, p[0], position));
            },
            [&](mir::Block& body, mir::ExprId kept, mir::ExprId each) {
              return body.exprs.Add(
                  mir::MakeBinary(
                      Unit(), body, mir::BinaryOp::kAdd, kept, each, count));
            });
      });
}

auto ArrayBodies::CountBits() -> mir::CallableCode {
  const mir::TypeId count = Unit().builtins.int_type;
  const mir::TypeId control = BitCountControlType(Unit());
  return BodyOf(
      support::BuiltinFn::kCountBits, {WholeType(), control}, count,
      [&](mir::CallableCode& code, mir::Block& block,
          const std::vector<mir::LocalId>& p) {
        return Reduced(
            code, block, count,
            [&](mir::Block& at) { return mir::BuildIntLiteral(Unit(), at, 0); },
            [&](mir::Block& body, mir::ExprId position) {
              return BuildBitCount(
                  Unit(), body, Element(body, p[0], position),
                  Referent(body, p[1], control));
            },
            [&](mir::Block& body, mir::ExprId kept, mir::ExprId each) {
              return body.exprs.Add(
                  mir::MakeBinary(
                      Unit(), body, mir::BinaryOp::kAdd, kept, each, count));
            });
      });
}

// LRM 6.24.3: the elements' own streams laid end to end, the one at position
// 0 most significant.
auto ArrayBodies::ToBitstream() -> mir::CallableCode {
  const mir::TypeId stream = Stream();
  return BodyOf(
      support::BuiltinFn::kToBitstream, {WholeType()}, stream,
      [&](mir::CallableCode& code, mir::Block& block,
          const std::vector<mir::LocalId>& p) {
        const mir::LocalId bits = code.AddLocal(stream);
        block.AppendStmt(
            mir::LocalDeclStmt{
                .target = bits,
                .init = mir::BuildIntegralLiteral(
                    Unit(), block, stream, mir::IntegralConstant{})});
        ForEachPosition(
            code, block, [&](mir::Block& body, mir::ExprId position) {
              auto element = BuildToBitstream(
                  Unit(), body, Element(body, p[0], position), {});
              if (!element) {
                throw InternalError(
                    "ArrayBodies::ToBitstream: an element of an array whose "
                    "stream is fixed has none");
              }
              body.AppendStmt(
                  mir::ExprStmt{
                      .expr = body.exprs.Add(BuildStoreExpr(
                          Unit(), body,
                          AccessPath{
                              .owner = body.LocalPlace(bits, stream),
                              .descent = {DescentStep{
                                  .to =
                                      StepToBits{
                                          .start =
                                              ElementStart(body, position)},
                                  .part_type = ElementBits()}}},
                          body.AsValue(Unit().types, *element)))});
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
      [&](mir::CallableCode& code, mir::Block& block,
          const std::vector<mir::LocalId>& p) {
        return Built(code, block, [&](mir::Block& body, mir::ExprId position) {
          const mir::ExprId bits = body.exprs.Add(StepRead(
              Unit(), body,
              DescentStep{
                  .to = StepToBits{.start = ElementStart(body, position)},
                  .part_type = ElementBits()},
              Referent(body, p[0], stream)));
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
      [&](mir::CallableCode& code, mir::Block& block,
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
            code, block, [&](mir::Block& body, mir::ExprId position) {
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
              InsertIntoRun(
                  Unit(), body, bits, CountNow(body, at),
                  body.ReadLocal(part, run));
              SetCount(
                  body, at,
                  mir::BuildMachineIntBinary(
                      Unit(), body, mir::BinaryOp::kAdd, CountNow(body, at),
                      BuildRunLength(Unit(), body, body.ReadLocal(part, run))));
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
      [&](mir::CallableCode& code, mir::Block& block,
          const std::vector<mir::LocalId>& p) {
        const mir::LocalId length = DeclaredCount(
            code, block,
            BuildRunLength(Unit(), block, Referent(block, p[0], run)));
        const mir::LocalId at =
            DeclaredCount(code, block, CountLiteral(block, 0));
        const mir::LocalId after = DeclaredCount(
            code, block, CountLiteral(block, (count_ - 1) * empty));
        return Built(code, block, [&](mir::Block& body, mir::ExprId) {
          const mir::LocalId allotted =
              PartAllotment(code, body, length, at, CountNow(body, after));
          const mir::ExprId element = Held(
              code, body, ElementType(),
              BuildFromBitVec(
                  Unit(), body,
                  BuildRunExtract(
                      Unit(), body, Referent(body, p[0], run),
                      CountNow(body, at), CountNow(body, allotted)),
                  ElementType()));
          SetCount(
              body, at,
              mir::BuildMachineIntBinary(
                  Unit(), body, mir::BinaryOp::kAdd, CountNow(body, at),
                  CountNow(body, allotted)));
          SetCount(
              body, after,
              mir::BuildMachineIntBinary(
                  Unit(), body, mir::BinaryOp::kSub, CountNow(body, after),
                  CountLiteral(body, empty)));
          return element;
        });
      });
}

// Two contributions folded element by element (LRM 6.7.1).
auto ArrayBodies::Fold(support::BuiltinFn fold) -> mir::CallableCode {
  return BodyOf(
      fold, {WholeType(), WholeType()}, WholeType(),
      [&](mir::CallableCode& code, mir::Block& block,
          const std::vector<mir::LocalId>& p) {
        return Built(code, block, [&](mir::Block& body, mir::ExprId position) {
          return BuildValueOperation(
              Unit(), body, fold, Element(body, p[0], position),
              {Element(body, p[1], position)}, ElementType());
        });
      });
}

auto ArrayBodies::FilledLike() -> mir::CallableCode {
  const mir::TypeId fill = FillType(Unit());
  return BodyOf(
      support::BuiltinFn::kFilledLike, {WholeType(), fill}, WholeType(),
      [&](mir::CallableCode& code, mir::Block& block,
          const std::vector<mir::LocalId>& p) {
        return Built(code, block, [&](mir::Block& body, mir::ExprId position) {
          return BuildFilled(
              Unit(), body, Element(body, p[0], position), ElementType(),
              Referent(body, p[1], fill));
        });
      });
}

// An array's positions are its elements', the one at position 0 first.
auto ArrayBodies::TakePositions() -> mir::CallableCode {
  const mir::TypeId number = Unit().builtins.machine_int64;
  return BodyOf(
      support::BuiltinFn::kTakePositions,
      {WholeType(), WholeType(), number, number}, WholeType(),
      [&](mir::CallableCode& code, mir::Block& block,
          const std::vector<mir::LocalId>& p) {
        const HeldRun run = HoldRun(code, block, p[2], p[3]);
        const std::uint64_t per_element =
            PositionCountOf(Unit(), ElementType());
        return Built(code, block, [&](mir::Block& body, mir::ExprId position) {
          return PartTaken(
              code, body, ElementType(), Element(body, p[0], position),
              Element(body, p[1], position),
              mir::BuildMachineIntBinary(
                  Unit(), body, mir::BinaryOp::kMul,
                  BuildToInt64Call(Unit(), body, position),
                  CountLiteral(body, per_element)),
              run);
        });
      });
}

// Which of the operations a type may lack it has: a stream, one of fixed width,
// and, where its width is the running program's, a stream written out where
// the type holds no class handle, whose object's stream is not yet built (LRM
// 11.4.14.1), and one read back where the type may be a destination (LRM
// 6.24.3); a net's folds (LRM 6.7.1).
struct OperationsHad {
  bool stream;
  bool fixed_stream;
  bool run_written;
  bool run_read;
  bool net_folds;
};

// Hands `add` every body `bodies` builds for an operation on the whole value
// every such type has, in one order for every kind of type.
template <typename Bodies, typename Add>
void StateWholeValue(Bodies& bodies, Add add) {
  add(support::BuiltinFn::kDeclaredDefault, bodies.DeclaredDefault());
  add(support::ValueOperator::kEquality, bodies.Equality());
  add(support::ValueOperator::kInequality, bodies.Inequality());
  add(support::BuiltinFn::kCaseEqual, bodies.CaseEqual());
  add(support::BuiltinFn::kBitIdentical, bodies.BitIdentical());
  add(support::BuiltinFn::kHasUnknown, bodies.HasUnknown());
  add(support::BuiltinFn::kIsUnknown, bodies.IsUnknown());
  add(support::BuiltinFn::kTakePositions, bodies.TakePositions());
}

// Hands `add` the bodies of a stream (LRM 6.24.3) and a net's folds (LRM
// 6.7.1) where `had` says the type has them. A tagged union has neither, so it
// is asked for none.
template <typename Bodies, typename Add>
void StateStreamAndFolds(Bodies& bodies, const OperationsHad& had, Add add) {
  if (had.stream) {
    add(support::BuiltinFn::kBitstreamWidth, bodies.BitWidth());
    add(support::BuiltinFn::kCountBits, bodies.CountBits());
    if (had.fixed_stream) {
      add(support::BuiltinFn::kToBitstream, bodies.ToBitstream());
      add(support::BuiltinFn::kFromBitstream, bodies.FromBitstream());
    }
  }
  if (had.net_folds) {
    for (const support::BuiltinFn fold : kNetFolds) {
      add(fold, bodies.Fold(fold));
    }
    add(support::BuiltinFn::kFilledLike, bodies.FilledLike());
  }
}

// Hands `add` the bodies of a stream as a run of bits where `had` says the
// type holds a stream whose width the running program decides (LRM 6.24.3):
// a structure or an array holding a dynamic array, a queue, an associative
// array or a string somewhere.
template <typename Bodies, typename Add>
void StateRunBodies(Bodies& bodies, const OperationsHad& had, Add add) {
  if (had.run_written) {
    add(support::BuiltinFn::kToBitVec, bodies.ToBitVec());
  }
  if (had.run_read) {
    add(support::BuiltinFn::kFromBitVec, bodies.FromBitVec());
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
      .net_folds = IsValidForNetOver(unit, mir::RecordKind::kUnion, members)};
}

}  // namespace

auto BuildBitIdentity(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId lhs,
    mir::ExprId rhs) -> mir::ExprId {
  return BuildValueOperation(
      unit, block, support::BuiltinFn::kBitIdentical, lhs, {rhs},
      unit.builtins.machine_bool);
}

auto StructMethodsOf(
    UnitLowerer& lowerer, const hir::UnpackedStructType& source,
    const mir::TypeDeclarationRef& declaration, mir::TypeId structure,
    std::span<const mir::TypeId> members) -> std::vector<mir::RecordMethod> {
  const mir::CompilationUnit& unit = lowerer.Unit();
  StructBodies bodies(
      lowerer, source, declaration, structure,
      {members.begin(), members.end()});
  std::vector<mir::RecordMethod> methods;
  const auto add = [&](support::ValueOperation answers,
                       mir::CallableCode code) {
    methods.push_back(
        mir::RecordMethod{.answers = answers, .code = std::move(code)});
  };
  const bool stream = EveryPart(unit, members, HasBitStream);
  const bool fixed_stream =
      IsFixedStream(FixedStreamShapeOfParts(unit, members));
  const OperationsHad had{
      .stream = stream,
      .fixed_stream = fixed_stream,
      .run_written = stream && !fixed_stream &&
                     std::ranges::none_of(
                         members,
                         [&](mir::TypeId member) {
                           return HoldsClassHandle(unit, member);
                         }),
      .run_read = stream && !fixed_stream &&
                  std::ranges::all_of(
                      members,
                      [&](mir::TypeId member) {
                        return IsStreamDestination(unit, member);
                      }),
      .net_folds = EveryPart(unit, members, IsValidForNet)};
  StateWholeValue(bodies, add);
  // What the library holding an array of the structure asks of it, where it
  // takes positions element by element; a union, held whole, needs none.
  add(support::BuiltinFn::kPositionCount, bodies.PositionCount());
  StateStreamAndFolds(bodies, had, add);
  StateRunBodies(bodies, had, add);
  return methods;
}

auto UnionMethodsOf(
    UnitLowerer& lowerer, const mir::TypeDeclarationRef& declaration,
    mir::TypeId overlay, std::span<const mir::TypeId> members)
    -> std::vector<mir::RecordMethod> {
  std::vector<mir::RecordMethod> methods;
  const auto add = [&](support::ValueOperation answers,
                       mir::CallableCode code) {
    methods.push_back(
        mir::RecordMethod{.answers = answers, .code = std::move(code)});
  };
  const OperationsHad had = OperationsHadByUnion(lowerer.Unit(), members);
  UnionBodies bodies(
      lowerer, declaration, overlay, {members.begin(), members.end()});
  StateWholeValue(bodies, add);
  StateStreamAndFolds(bodies, had, add);
  return methods;
}

auto TaggedUnionMethodsOf(
    UnitLowerer& lowerer, const mir::TypeDeclarationRef& declaration,
    mir::TypeId overlay, std::span<const mir::TypeId> members)
    -> std::vector<mir::RecordMethod> {
  std::vector<mir::RecordMethod> methods;
  const auto add = [&](support::ValueOperation answers,
                       mir::CallableCode code) {
    methods.push_back(
        mir::RecordMethod{.answers = answers, .code = std::move(code)});
  };
  TaggedUnionBodies bodies(
      lowerer, declaration, overlay, {members.begin(), members.end()});
  StateWholeValue(bodies, add);
  return methods;
}

auto ArrayOperationsOf(UnitLowerer& lowerer, mir::TypeId type)
    -> std::vector<mir::TypeOperation> {
  const mir::CompilationUnit& unit = lowerer.Unit();
  std::vector<mir::TypeOperation> operations;
  const auto add = [&](support::ValueOperation answers,
                       mir::CallableCode code) {
    operations.push_back(
        mir::TypeOperation{
            .type = type, .answers = answers, .code = std::move(code)});
  };
  // Read off the type itself, which is interned whole before any body is
  // built.
  const bool stream = HasBitStream(unit, type);
  const bool fixed_stream = IsFixedStream(FixedStreamShapeOf(unit, type));
  const OperationsHad had{
      .stream = stream,
      .fixed_stream = fixed_stream,
      .run_written = stream && !fixed_stream && !HoldsClassHandle(unit, type),
      .run_read = stream && !fixed_stream && IsStreamDestination(unit, type),
      .net_folds = IsValidForNet(unit, type)};
  const mir::UnpackedArrayType array =
      unit.types.Get(type).Get<mir::UnpackedArrayType>();
  ArrayBodies bodies(lowerer, type, array.element_type, array.count);
  StateWholeValue(bodies, add);
  StateStreamAndFolds(bodies, had, add);
  StateRunBodies(bodies, had, add);
  return operations;
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
  if (const auto* array = t.As<mir::UnpackedArrayType>()) {
    return array->count * PositionCountOf(unit, array->element_type);
  }
  if (mir::RecordKindOf(unit, type) == mir::RecordKind::kStruct) {
    std::uint64_t count = 0;
    for (const mir::TypeId member : PartTypes(unit, type)) {
      count += PositionCountOf(unit, member);
    }
    return count;
  }
  return 1;
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
    const mir::CompilationUnit& unit, mir::Block& block,
    support::BuiltinFn entry, std::optional<mir::ExprId> receiver,
    std::vector<mir::ExprId> operands, mir::TypeId result) -> mir::ExprId {
  const mir::TypeId deciding =
      receiver.has_value() ? block.exprs.Get(*receiver).type : result;
  if (std::optional<mir::DirectTarget> own =
          OwnOperationTarget(unit, deciding, entry)) {
    return OwnOperationCall(
        unit, block, *std::move(own), receiver, std::move(operands), result);
  }
  for (mir::ExprId& operand : operands) {
    operand = block.AsValue(unit.types, operand);
  }
  const std::optional<mir::ExprId> object =
      receiver.transform([&](mir::ExprId named) {
        return mir::ReadIfOverlaid(unit, block, named);
      });
  return block.exprs.Add(
      mir::MakeBuiltinCall(
          unit, block, entry, object, std::move(operands), result));
}

auto BuildEquality(
    const mir::CompilationUnit& unit, mir::Block& block,
    support::ValueOperator comparison, mir::ExprId lhs, mir::ExprId rhs,
    mir::TypeId result) -> mir::ExprId {
  const mir::TypeId type = block.exprs.Get(lhs).type;
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
  return block.exprs.Add(
      mir::MakeBinary(
          unit, block, operator_of(), block.AsValue(unit.types, lhs),
          block.AsValue(unit.types, rhs), result));
}

auto BuildCaseEquality(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId lhs,
    mir::ExprId rhs) -> mir::ExprId {
  return BuildValueOperation(
      unit, block, support::BuiltinFn::kCaseEqual, lhs, {rhs},
      unit.builtins.bit1);
}

auto BuildCaseComparison(
    const mir::CompilationUnit& unit, mir::Block& block,
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
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId value)
    -> mir::ExprId {
  return BuildValueOperation(
      unit, block, support::BuiltinFn::kIsUnknown, value, {},
      unit.builtins.bit1);
}

auto BuildBitCount(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId value,
    mir::ExprId control) -> mir::ExprId {
  return BuildValueOperation(
      unit, block, support::BuiltinFn::kCountBits, value, {control},
      unit.builtins.int_type);
}

auto BuildBitWidth(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId value)
    -> mir::ExprId {
  // An integral value is as many bits as its type is wide.
  const mir::Type& type = unit.types.Get(block.exprs.Get(value).type);
  if (type.IsIntegral()) {
    return mir::BuildIntLiteral(
        unit, block, static_cast<std::int64_t>(type.Integral().bit_width));
  }
  return BuildValueOperation(
      unit, block, support::BuiltinFn::kBitstreamWidth, value, {},
      unit.builtins.int_type);
}

}  // namespace lyra::lowering::hir_to_mir
