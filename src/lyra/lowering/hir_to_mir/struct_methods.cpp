#include "lyra/lowering/hir_to_mir/struct_methods.hpp"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/bitstream.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/expression/selects.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/bit_vector.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/predicate.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/storage_part.hpp"
#include "lyra/mir/struct_decl.hpp"
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
  const mir::Type& t = unit.types.Get(type);
  if (const auto* u = t.As<mir::UnionType>()) {
    return u->members;
  }
  if (const auto* u = t.As<mir::TaggedUnionType>()) {
    return u->members;
  }
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

// LRM 6.24.3: neither a real nor a chandle is a bit-stream type, and a value
// holding one has no stream either. A stream takes an untagged union as its
// first member and a tagged one not at all (LRM 11.4.14.1).
auto HasBitStream(const mir::CompilationUnit& unit, mir::TypeId type) -> bool {
  const mir::Type& t = unit.types.Get(type);
  if (const auto* overlay = t.As<mir::UnionType>()) {
    return HasBitStream(unit, overlay->members.front());
  }
  return !t.IsRealFamily() && !t.Is<mir::ChandleType>() &&
         !t.Is<mir::TaggedUnionType>() &&
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
  if (t.Is<mir::StructType>() || t.Is<mir::UnionType>() ||
      t.Is<mir::UnpackedArrayType>()) {
    return EveryPart(unit, PartTypes(unit, type), IsValidForNet);
  }
  return false;
}

// A net's own contribution is a value of its type every bit of which is one
// fill (LRM 6.7.1).
auto FillType(const mir::CompilationUnit& unit) -> mir::TypeId {
  return mir::PackedVectorOf(unit.types, 1, mir::IntegralStateKind::kFourState);
}

// The declaration of the struct `type` is, or nothing for a type that is none.
auto DeclarationOfStruct(const mir::CompilationUnit& unit, mir::TypeId type)
    -> std::optional<mir::TypeDeclarationRef> {
  const auto* structure = unit.types.Get(type).As<mir::StructType>();
  if (structure == nullptr) {
    return std::nullopt;
  }
  return mir::StructDeclarationOf(unit, *structure);
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

// How a struct's method takes each value it is asked about, the struct itself
// and every other operand: by a read-only reference, as Rust's comparison
// operators borrow theirs, so a value is read where it lies rather than copied
// in.
auto ReferenceTo(const mir::CompilationUnit& unit, mir::TypeId type)
    -> mir::TypeId {
  return unit.types.Intern(
      mir::Type{mir::ReferenceType{
          .referent = type, .mutability = mir::Mutability::kReadOnly}});
}

// The reference a method takes `operand` by: to the storage it names where it
// names storage a value is read from as it lies, and otherwise to its value,
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

// A call of the method the struct `declaration` names answers `answers` with.
auto StructMethodCall(
    const mir::CompilationUnit& unit, mir::Block& block,
    mir::TypeDeclarationRef declaration, support::ValueOperation answers,
    std::optional<mir::ExprId> receiver, std::vector<mir::ExprId> operands,
    mir::TypeId result) -> mir::ExprId {
  for (mir::ExprId& operand : operands) {
    operand = Borrow(unit, block, operand);
  }
  return CallOf(
      block,
      mir::Direct{
          .target =
              mir::StructMethodTarget{
                  .declaration = std::move(declaration), .answers = answers},
          .receiver = receiver.transform(
              [&](mir::ExprId object) { return Borrow(unit, block, object); })},
      std::move(operands), result);
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

// A net's two-contribution folds: each truth table, and what a stronger
// contribution leaves a weaker one.
constexpr std::array<support::BuiltinFn, 4> kNetFolds{
    support::BuiltinFn::kResolveTriState,
    support::BuiltinFn::kResolveWiredAnd,
    support::BuiltinFn::kResolveWiredOr,
    support::BuiltinFn::kDominating,
};

// Builds the methods of one struct while its declaration is being made, so
// every fact about the struct itself is read off `members` rather than off a
// declaration that does not exist yet. Each method takes the parameters its
// operation takes of any value, and returns one expression built member by
// member.
class Synthesizer {
 public:
  Synthesizer(
      UnitLowerer& lowerer, const hir::UnpackedStructType& source,
      mir::TypeDeclarationRef declaration, mir::TypeId structure,
      std::vector<mir::TypeId> members)
      : lowerer_(&lowerer),
        source_(&source),
        structure_(structure),
        declaration_(std::move(declaration)),
        members_(std::move(members)) {
  }

  auto Equality() -> mir::StructMethod;
  auto Inequality() -> mir::StructMethod;
  auto CaseEqual() -> mir::StructMethod;
  auto BitIdentical() -> mir::StructMethod;
  auto HasUnknown() -> mir::StructMethod;
  auto IsUnknown() -> mir::StructMethod;
  auto BitWidth() -> mir::StructMethod;
  auto CountBits() -> mir::StructMethod;
  auto ToBitstream() -> mir::StructMethod;
  auto FromBitstream() -> mir::StructMethod;
  auto Fold(support::BuiltinFn fold) -> mir::StructMethod;
  auto FilledLike() -> mir::StructMethod;
  auto DeclaredDefault() -> mir::StructMethod;

 private:
  auto FilledMember(
      mir::Block& block, const std::vector<mir::LocalId>& p, std::size_t i,
      mir::TypeId fill) -> mir::ExprId;
  auto Unit() -> mir::CompilationUnit& {
    return lowerer_->Unit();
  }
  // The one-bit answer `==` gives over two of this struct.
  auto EqualityType() -> mir::TypeId {
    return OneBitAnswerType(Unit(), members_);
  }

  // The stream this struct makes, which every method over one has.
  auto Stream() -> StreamShape {
    return *FixedStreamShapeOfParts(Unit(), members_);
  }

  // The method answering `answers`, taking parameters of `params` and
  // answering `result`, whose one statement returns what `answer` builds from
  // the parameters' locals. Where the operation is asked of a value, the first
  // parameter is that value. Each is taken by reference and read through it.
  template <typename Answer>
  auto Method(
      support::ValueOperation answers, std::vector<mir::TypeId> params,
      mir::TypeId result, Answer answer) -> mir::StructMethod {
    mir::CallableCode code = mir::CallableCode::Defined();
    for (const mir::TypeId param : params) {
      code.params.push_back(code.AddLocal(ReferenceTo(Unit(), param)));
    }
    if (support::ReachesAReceiver(answers)) {
      code.receiver = code.params.front();
    }
    code.result_type = result;
    const std::vector<mir::LocalId> locals = code.params;
    mir::Block& block = code.Body();
    block.AppendStmt(mir::ReturnStmt{.value = answer(block, locals)});
    return mir::StructMethod{.answers = answers, .code = std::move(code)};
  }

  // A call of another of this struct's own methods, named by the declaration
  // being made rather than looked up from the struct's type, which has no
  // declaration to read until every method is built.
  auto OwnMethod(
      mir::Block& block, support::ValueOperation answers, mir::ExprId receiver,
      std::vector<mir::ExprId> operands, mir::TypeId result) -> mir::ExprId {
    return StructMethodCall(
        Unit(), block, declaration_, answers, receiver, std::move(operands),
        result);
  }

  // What the parameter `local` refers to, of `type`, as the place it is.
  auto Referent(mir::Block& block, mir::LocalId local, mir::TypeId type)
      -> mir::ExprId {
    return block.Deref(
        Unit().types, block.LocalPlace(local, ReferenceTo(Unit(), type)), type);
  }

  // The struct the parameter `local` refers to.
  auto Whole(mir::Block& block, mir::LocalId local) -> mir::ExprId {
    return Referent(block, local, structure_);
  }

  // Member `i` of the struct parameter `local`, where it lies: each
  // operation asked of it settles whether it reads it or borrows it.
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

  // The struct built from one value per member.
  auto Built(mir::Block& block, std::vector<mir::ExprId> members)
      -> mir::ExprId {
    return block.exprs.Add(
        mir::Expr{
            .data = mir::CompositeExpr{.parts = std::move(members)},
            .type = structure_});
  }

  UnitLowerer* lowerer_;
  const hir::UnpackedStructType* source_;
  mir::TypeId structure_;
  mir::TypeDeclarationRef declaration_;
  std::vector<mir::TypeId> members_;
};

// LRM 11.4.5: equal when every pair of members is, and unknown where no pair is
// unequal and some pair is unknown -- which is the logical AND of the members'
// own answers, each taken to the structure's own answer type first.
auto Synthesizer::Equality() -> mir::StructMethod {
  const mir::TypeId answer_type = EqualityType();
  return Method(
      support::ValueOperator::kEquality, {structure_, structure_}, answer_type,
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> equal;
        equal.reserve(members_.size());
        for (std::size_t i = 0; i < members_.size(); ++i) {
          const mir::ExprId lhs = Member(block, p[0], i);
          const mir::ExprId rhs = Member(block, p[1], i);
          equal.push_back(ConvertToType(
              Unit(), block,
              block.exprs.Add(BuildMirBinaryExpr(
                  Unit(), block, hir::BinaryOp::kEquality, lhs, rhs,
                  OneBitAnswerType(Unit(), {&members_[i], 1}))),
              answer_type));
        }
        return BuildMirLogicalAnd(Unit(), block, answer_type, equal);
      });
}

// LRM 11.4.5: `!=` is the negation of `==`, unknown where that is.
auto Synthesizer::Inequality() -> mir::StructMethod {
  const mir::TypeId answer_type = EqualityType();
  return Method(
      support::ValueOperator::kInequality, {structure_, structure_},
      answer_type, [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        const mir::ExprId equal = OwnMethod(
            block, support::ValueOperator::kEquality, Whole(block, p[0]),
            {Whole(block, p[1])}, answer_type);
        return BuildLogicalNot(Unit(), block, equal);
      });
}

auto Synthesizer::CaseEqual() -> mir::StructMethod {
  const mir::TypeId bit = Unit().builtins.bit1;
  return Method(
      support::BuiltinFn::kCaseEqual, {structure_, structure_}, bit,
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> equal;
        equal.reserve(members_.size());
        for (std::size_t i = 0; i < members_.size(); ++i) {
          equal.push_back(BuildCaseEquality(
              Unit(), block, Member(block, p[0], i), Member(block, p[1], i)));
        }
        return BuildMirLogicalAnd(Unit(), block, bit, equal);
      });
}

auto Synthesizer::BitIdentical() -> mir::StructMethod {
  const mir::TypeId boolean = Unit().builtins.machine_bool;
  return Method(
      support::BuiltinFn::kBitIdentical, {structure_, structure_}, boolean,
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> identical;
        identical.reserve(members_.size());
        for (std::size_t i = 0; i < members_.size(); ++i) {
          identical.push_back(BuildBitIdentity(
              Unit(), block, Member(block, p[0], i), Member(block, p[1], i)));
        }
        return AllHold(Unit(), block, identical);
      });
}

auto Synthesizer::HasUnknown() -> mir::StructMethod {
  const mir::TypeId boolean = Unit().builtins.machine_bool;
  return Method(
      support::BuiltinFn::kHasUnknown, {structure_}, boolean,
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> unknown;
        unknown.reserve(members_.size());
        for (std::size_t i = 0; i < members_.size(); ++i) {
          unknown.push_back(
              BuildHasUnknown(Unit(), block, Member(block, p[0], i)));
        }
        return AnyHolds(Unit(), block, unknown);
      });
}

// LRM 20.9: `$isunknown` is the bit the question above answers.
auto Synthesizer::IsUnknown() -> mir::StructMethod {
  const mir::TypeId bit = Unit().builtins.bit1;
  return Method(
      support::BuiltinFn::kIsUnknown, {structure_}, bit,
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        const mir::ExprId holds_unknown = OwnMethod(
            block, support::BuiltinFn::kHasUnknown, Whole(block, p[0]), {},
            Unit().builtins.machine_bool);
        return block.exprs.Add(MakeBuiltinCall(
            Unit(), block, support::BuiltinFn::kFromBool, std::nullopt,
            {holds_unknown}, bit));
      });
}

// LRM 20.6.2: a structure holds the bits its members hold.
auto Synthesizer::BitWidth() -> mir::StructMethod {
  const mir::TypeId count = Unit().builtins.int_type;
  return Method(
      support::BuiltinFn::kBitstreamWidth, {structure_}, count,
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        mir::ExprId answer = BuildIntLiteral(Unit(), block, 0);
        for (std::size_t i = 0; i < members_.size(); ++i) {
          answer = block.exprs.Add(MakeBinary(
              Unit(), block, mir::BinaryOp::kAdd, answer,
              BuildBitWidth(Unit(), block, Member(block, p[0], i)), count));
        }
        return answer;
      });
}

// LRM 20.9: a structure's stream is its members' laid end to end, so the count
// over it is the sum of theirs under the same control set.
auto Synthesizer::CountBits() -> mir::StructMethod {
  const mir::TypeId count = Unit().builtins.int_type;
  const mir::TypeId control = BitCountControlType(Unit());
  return Method(
      support::BuiltinFn::kCountBits, {structure_, control}, count,
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        mir::ExprId answer = BuildIntLiteral(Unit(), block, 0);
        for (std::size_t i = 0; i < members_.size(); ++i) {
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
auto Synthesizer::ToBitstream() -> mir::StructMethod {
  const StreamShape shape = Stream();
  const mir::TypeId stream =
      mir::PackedVectorOf(Unit().types, shape.width, shape.state_kind);
  return Method(
      support::BuiltinFn::kToBitstream, {structure_}, stream,
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> streams;
        for (std::size_t i = 0; i < members_.size(); ++i) {
          auto member = BuildToBitstream(
              Unit(), block, Member(block, p[0], i), diag::SourceSpan{});
          if (!member) {
            throw InternalError(
                "Synthesizer::ToBitstream: a member of a structure whose "
                "stream is fixed has none");
          }
          streams.push_back(block.AsValue(Unit().types, *member));
        }
        // A stream of no bits is no value, so a fixed stream has a first
        // member to start from rather than an empty join.
        mir::ExprId answer = streams.front();
        for (std::size_t i = 1; i < streams.size(); ++i) {
          const StreamShape built = *FixedStreamShapeOfParts(
              Unit(), std::span<const mir::TypeId>{members_}.first(i + 1));
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
auto Synthesizer::FromBitstream() -> mir::StructMethod {
  const StreamShape shape = Stream();
  const mir::TypeId stream =
      mir::PackedVectorOf(Unit().types, shape.width, shape.state_kind);
  return Method(
      support::BuiltinFn::kFromBitstream, {stream}, structure_,
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> members;
        std::uint64_t taken = 0;
        for (const mir::TypeId member : members_) {
          const std::uint64_t width = FixedStreamShapeOf(Unit(), member)->width;
          taken += width;
          const mir::ExprId bits = block.exprs.Add(BuildPackedBitsRead(
              *lowerer_, block, Referent(block, p[0], stream),
              shape.width - taken,
              mir::PackedVectorOf(Unit().types, width, shape.state_kind)));
          auto read = BuildFromBitstream(
              Unit(), block, bits, member, diag::SourceSpan{});
          if (!read) {
            throw InternalError(
                "Synthesizer::FromBitstream: a member of a structure whose "
                "stream is fixed has none");
          }
          members.push_back(*read);
        }
        return Built(block, std::move(members));
      });
}

// Two contributions folded member by member (LRM 6.7.1 composes a net of a
// structure out of its members' bits).
auto Synthesizer::Fold(support::BuiltinFn fold) -> mir::StructMethod {
  return Method(
      fold, {structure_, structure_}, structure_,
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> members;
        for (std::size_t i = 0; i < members_.size(); ++i) {
          members.push_back(BuildValueOperation(
              Unit(), block, fold, Member(block, p[0], i),
              {Member(block, p[1], i)}, members_[i]));
        }
        return Built(block, std::move(members));
      });
}

auto Synthesizer::FilledLike() -> mir::StructMethod {
  const mir::TypeId fill = FillType(Unit());
  return Method(
      support::BuiltinFn::kFilledLike, {structure_, fill}, structure_,
      [&](mir::Block& block, const std::vector<mir::LocalId>& p) {
        std::vector<mir::ExprId> members;
        for (std::size_t i = 0; i < members_.size(); ++i) {
          members.push_back(FilledMember(block, p, i, fill));
        }
        return Built(block, std::move(members));
      });
}

auto Synthesizer::DeclaredDefault() -> mir::StructMethod {
  return Method(
      support::BuiltinFn::kDeclaredDefault, {}, structure_,
      [&](mir::Block& block, const std::vector<mir::LocalId>&) {
        return block.exprs.Add(
            BuildDeclaredStructValue(*lowerer_, block, *source_, structure_));
      });
}

// Member `i` with every bit the fill: a packed member is the fill repeated
// across its width (LRM 11.4.12.1), at the signedness it declares, and any
// other member answers for itself.
auto Synthesizer::FilledMember(
    mir::Block& block, const std::vector<mir::LocalId>& p, std::size_t i,
    mir::TypeId fill) -> mir::ExprId {
  const mir::Type& member = Unit().types.Get(members_[i]);
  if (!member.IsIntegral()) {
    return BuildValueOperation(
        Unit(), block, support::BuiltinFn::kFilledLike, std::nullopt,
        {Member(block, p[0], i), Referent(block, p[1], fill)}, members_[i]);
  }
  const mir::TypeId copies = mir::PackedVectorOf(
      Unit().types, member.Integral().bit_width,
      Unit().types.Get(fill).Integral().state_kind);
  const mir::ExprId run = Referent(block, p[1], fill);
  const mir::ExprId filled = block.exprs.Add(MakeBuiltinCall(
      Unit(), block, support::BuiltinFn::kReplicateBits, run, {}, copies));
  return ConvertToType(Unit(), block, filled, members_[i]);
}

}  // namespace

auto StructMethodsOf(
    UnitLowerer& lowerer, const hir::UnpackedStructType& source,
    const mir::TypeDeclarationRef& declaration, mir::TypeId structure,
    std::span<const mir::TypeId> members) -> std::vector<mir::StructMethod> {
  const mir::CompilationUnit& unit = lowerer.Unit();
  Synthesizer synthesize(
      lowerer, source, declaration, structure,
      {members.begin(), members.end()});
  std::vector<mir::StructMethod> methods;
  methods.push_back(synthesize.DeclaredDefault());
  methods.push_back(synthesize.Equality());
  methods.push_back(synthesize.Inequality());
  methods.push_back(synthesize.CaseEqual());
  methods.push_back(synthesize.BitIdentical());
  methods.push_back(synthesize.HasUnknown());
  methods.push_back(synthesize.IsUnknown());
  if (EveryPart(unit, members, HasBitStream)) {
    methods.push_back(synthesize.BitWidth());
    methods.push_back(synthesize.CountBits());
    const std::optional<StreamShape> fixed =
        FixedStreamShapeOfParts(unit, members);
    if (fixed.has_value() && fixed->width != 0) {
      methods.push_back(synthesize.ToBitstream());
      methods.push_back(synthesize.FromBitstream());
    }
  }
  if (EveryPart(unit, members, IsValidForNet)) {
    for (const support::BuiltinFn fold : kNetFolds) {
      methods.push_back(synthesize.Fold(fold));
    }
    methods.push_back(synthesize.FilledLike());
  }
  return methods;
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

auto BuildValueOperation(
    const mir::CompilationUnit& unit, mir::Block& block,
    support::BuiltinFn entry, std::optional<mir::ExprId> receiver,
    std::vector<mir::ExprId> operands, mir::TypeId result) -> mir::ExprId {
  const mir::TypeId deciding =
      receiver.has_value() ? block.exprs.Get(*receiver).type : result;
  if (std::optional<mir::TypeDeclarationRef> structure =
          DeclarationOfStruct(unit, deciding)) {
    return StructMethodCall(
        unit, block, *std::move(structure), entry, receiver,
        std::move(operands), result);
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

auto BuildStructComparison(
    const mir::CompilationUnit& unit, mir::Block& block,
    support::ValueOperator comparison, mir::ExprId lhs, mir::ExprId rhs)
    -> mir::ExprId {
  const mir::TypeId type = block.exprs.Get(lhs).type;
  std::optional<mir::TypeDeclarationRef> structure =
      DeclarationOfStruct(unit, type);
  if (!structure.has_value()) {
    throw InternalError(
        "BuildStructComparison: an operand is no struct the source declared");
  }
  return StructMethodCall(
      unit, block, *std::move(structure), comparison, lhs, {rhs},
      OneBitAnswerType(unit, *mir::ProductElements(unit, type)));
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
