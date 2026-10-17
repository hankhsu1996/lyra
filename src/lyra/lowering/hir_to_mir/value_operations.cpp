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
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/bitstream.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/expression/selects.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/bit_vector.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/predicate.hpp"
#include "lyra/lowering/hir_to_mir/select_position.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/expr.hpp"
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
// all (LRM 11.4.14.1); a net admits a structure or an untagged union of what a
// net admits, and no tagged union (LRM 6.7.1); and comparing two untagged
// unions compares their first members (LRM 7.3.2).
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

// The reference such a body takes `operand` by: to the storage it names where
// it names storage a value is read from as it lies, and otherwise to its value,
// held for the full-expression as a C++ reference binds a temporary.
auto Borrow(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId operand)
    -> mir::ExprId {
  const mir::ExprId object = mir::ReadIfOverlaid(unit, block, operand);
  return block.exprs.Add(
      mir::Expr{
          .data = mir::BorrowExpr{.operand = object},
          .type = ReferenceTo(unit, block.exprs.Get(object).type)});
}

// A call of the body `target` names, borrowing what it is asked about.
auto OwnOperationCall(
    const mir::CompilationUnit& unit, mir::Block& block,
    mir::DirectTarget target, std::optional<mir::ExprId> receiver,
    std::vector<mir::ExprId> operands, mir::TypeId result) -> mir::ExprId {
  for (mir::ExprId& operand : operands) {
    operand = Borrow(unit, block, operand);
  }
  return CallOf(
      block,
      mir::Direct{
          .target = std::move(target),
          .receiver = receiver.transform(
              [&](mir::ExprId object) { return Borrow(unit, block, object); })},
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
    support::BuiltinFn::kFromBitstream,   support::BuiltinFn::kResolveTriState,
    support::BuiltinFn::kResolveWiredAnd, support::BuiltinFn::kResolveWiredOr,
    support::BuiltinFn::kDominating,      support::BuiltinFn::kFilledLike,
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

// Whether two values are the same bits, as a machine boolean.
auto BuildBitIdentity(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId lhs,
    mir::ExprId rhs) -> mir::ExprId {
  return BuildValueOperation(
      unit, block, support::BuiltinFn::kBitIdentical, lhs, {rhs},
      unit.builtins.machine_bool);
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
  const mir::ExprId filled = block.exprs.Add(MakeBuiltinCall(
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
    return BuildIntegralLiteral(
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
          return block.exprs.Add(MakeBuiltinCall(
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
  auto Fold(support::BuiltinFn fold) -> mir::CallableCode;
  auto FilledLike() -> mir::CallableCode;
  auto DeclaredDefault() -> mir::CallableCode;
  using OperationBodies::Inequality;
  using OperationBodies::IsUnknown;

 private:
  // The stream this struct makes, which every method over one has.
  auto Stream() -> StreamShape {
    return *FixedStreamShapeOfParts(Unit(), Members());
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
  return Body(
      support::ValueOperator::kEquality, {WholeType(), WholeType()},
      answer_type, [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> equal;
        equal.reserve(Members().size());
        for (std::size_t i = 0; i < Members().size(); ++i) {
          equal.push_back(PartEqual(
              block, Member(block, p[0], i), Member(block, p[1], i),
              Members()[i]));
        }
        return BuildMirLogicalAnd(Unit(), block, answer_type, equal);
      });
}

auto StructBodies::CaseEqual() -> mir::CallableCode {
  const mir::TypeId bit = Unit().builtins.bit1;
  return Body(
      support::BuiltinFn::kCaseEqual, {WholeType(), WholeType()}, bit,
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> equal;
        equal.reserve(Members().size());
        for (std::size_t i = 0; i < Members().size(); ++i) {
          equal.push_back(BuildCaseEquality(
              Unit(), block, Member(block, p[0], i), Member(block, p[1], i)));
        }
        return BuildMirLogicalAnd(Unit(), block, bit, equal);
      });
}

auto StructBodies::BitIdentical() -> mir::CallableCode {
  const mir::TypeId boolean = Unit().builtins.machine_bool;
  return Body(
      support::BuiltinFn::kBitIdentical, {WholeType(), WholeType()}, boolean,
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> identical;
        identical.reserve(Members().size());
        for (std::size_t i = 0; i < Members().size(); ++i) {
          identical.push_back(BuildBitIdentity(
              Unit(), block, Member(block, p[0], i), Member(block, p[1], i)));
        }
        return AllHold(Unit(), block, identical);
      });
}

auto StructBodies::HasUnknown() -> mir::CallableCode {
  const mir::TypeId boolean = Unit().builtins.machine_bool;
  return Body(
      support::BuiltinFn::kHasUnknown, {WholeType()}, boolean,
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> unknown;
        unknown.reserve(Members().size());
        for (std::size_t i = 0; i < Members().size(); ++i) {
          unknown.push_back(
              BuildHasUnknown(Unit(), block, Member(block, p[0], i)));
        }
        return AnyHolds(Unit(), block, unknown);
      });
}

// LRM 20.6.2: a structure holds the bits its members hold.
auto StructBodies::BitWidth() -> mir::CallableCode {
  const mir::TypeId count = Unit().builtins.int_type;
  return Body(
      support::BuiltinFn::kBitstreamWidth, {WholeType()}, count,
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        mir::ExprId answer = BuildIntLiteral(Unit(), block, 0);
        for (std::size_t i = 0; i < Members().size(); ++i) {
          answer = block.exprs.Add(MakeBinary(
              Unit(), block, mir::BinaryOp::kAdd, answer,
              BuildBitWidth(Unit(), block, Member(block, p[0], i)), count));
        }
        return answer;
      });
}

// LRM 20.9: a structure's stream is its members' laid end to end, so the count
// over it is the sum of theirs under the same control set.
auto StructBodies::CountBits() -> mir::CallableCode {
  const mir::TypeId count = Unit().builtins.int_type;
  const mir::TypeId control = BitCountControlType(Unit());
  return Body(
      support::BuiltinFn::kCountBits, {WholeType(), control}, count,
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        mir::ExprId answer = BuildIntLiteral(Unit(), block, 0);
        for (std::size_t i = 0; i < Members().size(); ++i) {
          answer = block.exprs.Add(MakeBinary(
              Unit(), block, mir::BinaryOp::kAdd, answer,
              BuildBitCount(
                  Unit(), block, Member(block, p[0], i),
                  Referent(block, p[1], control)),
              count));
        }
        return answer;
      });
}

// LRM 6.24.3: the first member's stream most significant, each after it
// concatenated below. A stream is only built where the type fixes its width, so
// no member can refuse.
auto StructBodies::ToBitstream() -> mir::CallableCode {
  const StreamShape shape = Stream();
  const mir::TypeId stream =
      mir::PackedVectorOf(Unit().types, shape.width, shape.state_kind);
  return Body(
      support::BuiltinFn::kToBitstream, {WholeType()}, stream,
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
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
        // A stream of no bits is no value, so a fixed stream has a first
        // member to start from rather than an empty join.
        mir::ExprId answer = streams.front();
        for (std::size_t i = 1; i < streams.size(); ++i) {
          const StreamShape built =
              *FixedStreamShapeOfParts(Unit(), Members().first(i + 1));
          answer = block.exprs.Add(MakeBuiltinCall(
              Unit(), block, support::BuiltinFn::kConcatBits, answer,
              {streams[i]},
              mir::PackedVectorOf(
                  Unit().types, built.width, built.state_kind)));
        }
        return answer;
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
          auto read = BuildFromBitstream(
              Unit(), block, bits, member, diag::SourceSpan{});
          if (!read) {
            throw InternalError(
                "StructBodies::FromBitstream: a member of a structure whose "
                "stream is fixed has none");
          }
          members.push_back(*read);
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
        auto first = BuildFromBitstream(Unit(), block, bits, FirstType(), {});
        if (!first) {
          throw InternalError(
              "UnionBodies::FromBitstream: the first member of a union whose "
              "stream is fixed has none");
        }
        return Holding(block, *first);
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
          block.exprs.Add(MakeBinary(
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
          block.exprs.Add(MakeBinary(
              Unit(), block, mir::BinaryOp::kEquality,
              block.ReadLocal(tags.front(), tag_type),
              BuildMachineIntLiteral(
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
              return BuildBit1Literal(Unit(), taken, true);
            },
            [&](mir::Block& taken) {
              return BuildBit1Literal(Unit(), taken, false);
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
  auto Fold(support::BuiltinFn fold) -> mir::CallableCode;
  auto FilledLike() -> mir::CallableCode;
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
    const mir::ExprId within = block.exprs.Add(MakeBinary(
        Unit(), block, mir::BinaryOp::kLessThan,
        block.ReadLocal(position, position_type),
        BuildConstantPosition(Unit(), block, static_cast<std::int64_t>(count_)),
        OneBitAnswerType(Unit(), std::array{position_type})));
    const mir::ExprId next = block.exprs.Add(MakeBinary(
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
    return block.exprs.Add(MakeBinary(
        Unit(), block, mir::BinaryOp::kSub,
        BuildConstantPosition(
            Unit(), block, (static_cast<std::int64_t>(count_) - 1) * width),
        block.exprs.Add(MakeBinary(
            Unit(), block, mir::BinaryOp::kMul, position,
            BuildConstantPosition(Unit(), block, width), position_type)),
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
            [&](mir::Block& at) { return BuildBit1Literal(Unit(), at, true); },
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
            [&](mir::Block& at) { return BuildIntLiteral(Unit(), at, 0); },
            [&](mir::Block& body, mir::ExprId position) {
              return BuildBitWidth(Unit(), body, Element(body, p[0], position));
            },
            [&](mir::Block& body, mir::ExprId kept, mir::ExprId each) {
              return body.exprs.Add(MakeBinary(
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
            [&](mir::Block& at) { return BuildIntLiteral(Unit(), at, 0); },
            [&](mir::Block& body, mir::ExprId position) {
              return BuildBitCount(
                  Unit(), body, Element(body, p[0], position),
                  Referent(body, p[1], control));
            },
            [&](mir::Block& body, mir::ExprId kept, mir::ExprId each) {
              return body.exprs.Add(MakeBinary(
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
                .init = BuildIntegralLiteral(
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
          auto element =
              BuildFromBitstream(Unit(), body, bits, ElementType(), {});
          if (!element) {
            throw InternalError(
                "ArrayBodies::FromBitstream: an element of an array whose "
                "stream is fixed has none");
          }
          return *element;
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

// Which of the operations a type may lack it has: a stream and one of fixed
// width (LRM 6.24.3), a net's folds (LRM 6.7.1).
struct OperationsHad {
  bool stream;
  bool fixed_stream;
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

// Whether a stream of `shape` is one a value is written into and read out of,
// which takes a fixed width of at least one bit.
auto IsFixedStream(const std::optional<StreamShape>& shape) -> bool {
  return shape.has_value() && shape->width != 0;
}

}  // namespace

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
  const OperationsHad had{
      .stream = EveryPart(unit, members, HasBitStream),
      .fixed_stream = IsFixedStream(FixedStreamShapeOfParts(unit, members)),
      .net_folds = EveryPart(unit, members, IsValidForNet)};
  StateWholeValue(bodies, add);
  StateStreamAndFolds(bodies, had, add);
  return methods;
}

// What an array has of the operations a type may lack, read off the type
// itself, which is interned whole before any body is built.
auto OperationsHadBy(const mir::CompilationUnit& unit, mir::TypeId type)
    -> OperationsHad {
  return OperationsHad{
      .stream = HasBitStream(unit, type),
      .fixed_stream = IsFixedStream(FixedStreamShapeOf(unit, type)),
      .net_folds = IsValidForNet(unit, type)};
}

auto UnionMethodsOf(
    UnitLowerer& lowerer, const mir::TypeDeclarationRef& declaration,
    mir::TypeId overlay, mir::RecordKind kind,
    std::span<const mir::TypeId> members) -> std::vector<mir::RecordMethod> {
  const mir::CompilationUnit& unit = lowerer.Unit();
  std::vector<mir::RecordMethod> methods;
  const auto add = [&](support::ValueOperation answers,
                       mir::CallableCode code) {
    methods.push_back(
        mir::RecordMethod{.answers = answers, .code = std::move(code)});
  };
  const OperationsHad had{
      .stream = HasBitStreamOver(unit, kind, members),
      .fixed_stream = IsFixedStream(FixedStreamShapeOf(unit, members.front())),
      .net_folds = IsValidForNetOver(unit, kind, members)};
  switch (kind) {
    case mir::RecordKind::kUnion: {
      UnionBodies bodies(
          lowerer, declaration, overlay, {members.begin(), members.end()});
      StateWholeValue(bodies, add);
      StateStreamAndFolds(bodies, had, add);
      return methods;
    }
    case mir::RecordKind::kTaggedUnion: {
      TaggedUnionBodies bodies(
          lowerer, declaration, overlay, {members.begin(), members.end()});
      StateWholeValue(bodies, add);
      return methods;
    }
    case mir::RecordKind::kStruct:
      break;
  }
  throw InternalError("UnionMethodsOf: a structure states no union's methods");
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
  const OperationsHad had = OperationsHadBy(unit, type);
  const mir::UnpackedArrayType array =
      unit.types.Get(type).Get<mir::UnpackedArrayType>();
  ArrayBodies bodies(
      lowerer, type, array.element_type, array.dim.ElementCount());
  StateWholeValue(bodies, add);
  StateStreamAndFolds(bodies, had, add);
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
      MakeBuiltinCall(unit, block, entry, object, std::move(operands), result));
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
  return block.exprs.Add(MakeBinary(
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
    return BuildIntLiteral(
        unit, block, static_cast<std::int64_t>(type.Integral().bit_width));
  }
  return BuildValueOperation(
      unit, block, support::BuiltinFn::kBitstreamWidth, value, {},
      unit.builtins.int_type);
}

}  // namespace lyra::lowering::hir_to_mir
