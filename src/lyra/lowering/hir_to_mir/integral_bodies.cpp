#include "lyra/lowering/hir_to_mir/integral_bodies.hpp"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <format>
#include <functional>
#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/hir_to_mir/operation_bodies.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/unary_op.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/integral_operation.hpp"
#include "lyra/support/value_operation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

using support::IntegralOp;

// Where a word lies in a plane: a machine index, stated anew wherever it is
// read, since a node is evaluated where it stands and so stands once.
using Where = std::function<mir::ExprId(mir::Builder)>;

// The words a value of an integral type lies in, as a body reads and writes
// them: through a pointer to a machine array of the integer a plane is held
// in, the value plane's words and then, where the type holds x or z, the
// unknown plane's -- as a C program reads a value through
// `(uint64_t(*)[n])&v`. The pointer is held in a local, read where each word
// is.
struct PlaneWords {
  mir::LocalId pointer;
  mir::TypeId pointer_type;
  mir::TypeId array;
  std::uint64_t width;
  std::size_t words;
  bool four_state;
  mir::TypeId unit;
};

// Which plane of a value a word is read from.
enum class Plane : std::uint8_t { kValue, kUnknown };

// One word of each plane of two operands, at one index, each held in a local.
struct Bits {
  mir::LocalId av;
  mir::LocalId au;
  mir::LocalId bv;
  mir::LocalId bu;
};

// A word of each plane of an answer.
struct AnswerWord {
  mir::ExprId value;
  mir::ExprId unknown;
};

// What an arithmetic body carries from word to word: a sum's carry, or a
// difference's borrow.
enum class Carried : std::uint8_t { kSum, kDifference };

// What an arithmetic body is over: two operands, or one subtracted from zero.
enum class Operands : std::uint8_t { kTwo, kFromZero };

// What a match answers where no compared position disagrees: x where the first
// operand holds x or z at a compared position, or a match outright.
enum class Matched : std::uint8_t { kUnknownWhereTheFirstIs, kExactly };

// What a value made of a machine value is made of: a number, or a predicate.
enum class MachineOperand : std::uint8_t { kNumber, kPredicate };

// The bodies of an integral type laid out as words: each operator a word at a
// time over the planes its operands lie in, as the library's own operations
// apply it to a value's words (LRM 11.4).
class IntegralBodies : public OperationBodies {
 public:
  IntegralBodies(const mir::CompilationUnit& unit, mir::TypeId type)
      : OperationBodies(
            unit, type, {},
            [type](support::ValueOperation answers) -> mir::DirectTarget {
              return mir::TypeOperationTarget{.type = type, .answers = answers};
            }) {
  }

  auto Build(const mir::TypeOperationSource& source) -> mir::CallableCode {
    const auto op = std::get<IntegralOp>(source.answers);
    switch (op) {
      case IntegralOp::kBitwiseAnd:
        return Bitwise(
            op, [this](mir::CallableCode& c, mir::Builder b, const Bits& x) {
              const mir::ExprId zero =
                  Or(b, KnownAs(b, x.av, x.au, false),
                     KnownAs(b, x.bv, x.bu, false));
              const mir::ExprId one =
                  And(b, KnownAs(b, x.av, x.au, true),
                      KnownAs(b, x.bv, x.bu, true));
              return UnknownUnless(c, b, x, zero, one);
            });
      case IntegralOp::kBitwiseOr:
        return Bitwise(
            op, [this](mir::CallableCode& c, mir::Builder b, const Bits& x) {
              const mir::ExprId zero =
                  And(b, KnownAs(b, x.av, x.au, false),
                      KnownAs(b, x.bv, x.bu, false));
              const mir::ExprId one =
                  Or(b, KnownAs(b, x.av, x.au, true),
                     KnownAs(b, x.bv, x.bu, true));
              return UnknownUnless(c, b, x, zero, one);
            });
      case IntegralOp::kBitwiseXor:
        return Bitwise(
            op, [this](mir::CallableCode&, mir::Builder b, const Bits& x) {
              return Differing(
                  b, x,
                  Op(b, mir::BinaryOp::kBitwiseXor, Now(b, x.av),
                     Now(b, x.bv)));
            });
      case IntegralOp::kBitwiseXnor:
        return Bitwise(
            op, [this](mir::CallableCode&, mir::Builder b, const Bits& x) {
              return Differing(
                  b, x,
                  Not(b, Op(b, mir::BinaryOp::kBitwiseXor, Now(b, x.av),
                            Now(b, x.bv))));
            });
      case IntegralOp::kBitwiseNot:
        return BitwiseNot();
      case IntegralOp::kAdd:
        return Arithmetic(op, Carried::kSum, Operands::kTwo);
      case IntegralOp::kSubtract:
        return Arithmetic(op, Carried::kDifference, Operands::kTwo);
      case IntegralOp::kNegate:
        return Arithmetic(op, Carried::kDifference, Operands::kFromZero);
      case IntegralOp::kEqual:
        return Equality(op, value::FourStateBit::kOne);
      case IntegralOp::kNotEqual:
        return Equality(op, value::FourStateBit::kZero);
      case IntegralOp::kCaseEqual:
        return CaseEqual();
      case IntegralOp::kBitIdentical:
        return BitIdentical();
      case IntegralOp::kHasUnknown:
        return HasUnknown();
      case IntegralOp::kConvert:
        return Convert(source);
      case IntegralOp::kFromInt:
        return FromMachine(op, MachineOperand::kNumber);
      case IntegralOp::kFromBool:
        return FromMachine(op, MachineOperand::kPredicate);
      case IntegralOp::kToInt64:
        return ToInt64();
      case IntegralOp::kToPosition:
        return ToPosition();
      case IntegralOp::kLess:
        return Relation(op, 0b010U);
      case IntegralOp::kLessEqual:
        return Relation(op, 0b011U);
      case IntegralOp::kGreater:
        return Relation(op, 0b100U);
      case IntegralOp::kGreaterEqual:
        return Relation(op, 0b101U);
      case IntegralOp::kReductionAnd:
        return Reduction(op, value::ReductionOp::kAnd);
      case IntegralOp::kReductionOr:
        return Reduction(op, value::ReductionOp::kOr);
      case IntegralOp::kReductionXor:
        return Reduction(op, value::ReductionOp::kXor);
      case IntegralOp::kReductionNand:
        return Reduction(op, value::ReductionOp::kNand);
      case IntegralOp::kReductionNor:
        return Reduction(op, value::ReductionOp::kNor);
      case IntegralOp::kReductionXnor:
        return Reduction(op, value::ReductionOp::kXnor);
      case IntegralOp::kIsTrue:
        return IsTrue();
      case IntegralOp::kMergeConditional:
        return MergeConditional();
      case IntegralOp::kShiftLeft:
        return ShiftLeft(source);
      case IntegralOp::kLogicalShiftRight:
        return ShiftRight(source, false);
      case IntegralOp::kArithmeticShiftRight:
        return ShiftRight(source, true);
      case IntegralOp::kReplicate:
        return Replicate(source);
      case IntegralOp::kMultiply:
        return Multiply();
      case IntegralOp::kDivide:
        return Division(op, false);
      case IntegralOp::kModulo:
        return Division(op, true);
      case IntegralOp::kPower:
        return Power(source);
      case IntegralOp::kCountBits:
        return CountBits(source);
      case IntegralOp::kCeilLog2:
        return CeilLog2();
      case IntegralOp::kReverseBlocks:
        return ReverseBlocks();
      case IntegralOp::kTakePositions:
        return TakePositions();
      // LRM 11.4.6: an x or z in the second operand is a wildcard.
      case IntegralOp::kWildcardEqual:
        return MatchBody(
            op, Matched::kUnknownWhereTheFirstIs,
            [this](mir::Builder b, const Bits& x) {
              return Not(b, Now(b, x.bu));
            },
            [this](mir::Builder b, const Bits& x) {
              return And(
                  b,
                  Op(b, mir::BinaryOp::kBitwiseXor, Now(b, x.av), Now(b, x.bv)),
                  Not(b, Now(b, x.au)));
            });
      // LRM 12.5.1 `casez`: z on either side is a wildcard, every other
      // position matched on both planes.
      case IntegralOp::kCasezMatch:
        return MatchBody(
            op, Matched::kExactly,
            [this](mir::Builder b, const Bits& x) {
              return Not(
                  b, Or(b, And(b, Now(b, x.au), Not(b, Now(b, x.av))),
                        And(b, Now(b, x.bu), Not(b, Now(b, x.bv)))));
            },
            [this](mir::Builder b, const Bits& x) {
              return Or(
                  b,
                  Op(b, mir::BinaryOp::kBitwiseXor, Now(b, x.av), Now(b, x.bv)),
                  Op(b, mir::BinaryOp::kBitwiseXor, Now(b, x.au),
                     Now(b, x.bu)));
            });
      // LRM 12.5.1 `casex`: x or z on either side is a wildcard.
      case IntegralOp::kCasexMatch:
        return MatchBody(
            op, Matched::kExactly,
            [this](mir::Builder b, const Bits& x) {
              return Not(b, Or(b, Now(b, x.au), Now(b, x.bu)));
            },
            [this](mir::Builder b, const Bits& x) {
              return Op(
                  b, mir::BinaryOp::kBitwiseXor, Now(b, x.av), Now(b, x.bv));
            });
      case IntegralOp::kLogicalAnd:
      case IntegralOp::kLogicalOr:
      case IntegralOp::kLogicalEquivalence:
      case IntegralOp::kLogicalNot:
      case IntegralOp::kSlice:
      case IntegralOp::kWithSlice:
      case IntegralOp::kWithPositions:
      case IntegralOp::kFromText:
      case IntegralOp::kReadCanonicalBits:
      case IntegralOp::kReadCanonicalLogic:
      case IntegralOp::kFromSvLogic:
        break;
    }
    // A logical operator reads its operands' truths, and the rest are handed
    // what no type lays out at its width: no type answers them with a body.
    throw InternalError(
        std::format(
            "hir_to_mir: the integral operation \"{}\" is asked of a type as "
            "a body, and no type answers it with one -- please report this as "
            "a bug",
            support::IntegralOperationOf(op).name));
  }

 private:
  [[nodiscard]] auto Word() const -> mir::TypeId {
    return Unit().builtins.machine_word;
  }
  [[nodiscard]] auto Machine() const -> mir::TypeId {
    return Unit().builtins.machine_int64;
  }
  [[nodiscard]] auto Integral(mir::TypeId type) const
      -> const mir::IntegralType& {
    return Unit().types.Get(type).Integral();
  }

  auto WordLiteral(mir::Builder block, std::uint64_t value) -> mir::ExprId {
    return block.Add(
        mir::Expr{
            .data =
                mir::MachineIntLiteral{
                    .value = static_cast<std::int64_t>(value)},
            .type = Word()});
  }
  auto Op(
      mir::Builder block, mir::BinaryOp op, mir::ExprId lhs, mir::ExprId rhs)
      -> mir::ExprId {
    return block.Add(
        mir::MakeBinary(Unit(), block.Body(), op, lhs, rhs, Word()));
  }
  auto And(mir::Builder block, mir::ExprId lhs, mir::ExprId rhs)
      -> mir::ExprId {
    return Op(block, mir::BinaryOp::kBitwiseAnd, lhs, rhs);
  }
  auto Or(mir::Builder block, mir::ExprId lhs, mir::ExprId rhs) -> mir::ExprId {
    return Op(block, mir::BinaryOp::kBitwiseOr, lhs, rhs);
  }
  auto Not(mir::Builder block, mir::ExprId word) -> mir::ExprId {
    return block.Add(
        mir::MakeUnary(
            Unit(), block.Body(), mir::UnaryOp::kBitwiseNot, word, Word()));
  }
  auto Holds(
      mir::Builder block, mir::BinaryOp op, mir::ExprId lhs, mir::ExprId rhs)
      -> mir::ExprId {
    return block.Add(
        mir::MakeBinary(
            Unit(), block.Body(), op, lhs, rhs, Unit().builtins.machine_bool));
  }
  auto WordWhere(
      mir::Builder block, mir::ExprId condition, mir::ExprId then_word,
      mir::ExprId else_word) -> mir::ExprId {
    return Chosen(block, condition, then_word, else_word, Word());
  }
  // 1 where `condition` holds and 0 where it does not.
  auto WordOf(mir::Builder block, mir::ExprId condition) -> mir::ExprId {
    return WordWhere(
        block, condition, WordLiteral(block, 1), WordLiteral(block, 0));
  }
  auto IndexLiteral(mir::Builder block, std::int64_t index) -> mir::ExprId {
    return mir::BuildMachineIntLiteral(Unit(), block, index);
  }

  // A local of `code` holding `word`, what it holds now, and it moved to
  // `word`.
  auto Hold(mir::CallableCode& code, mir::Builder block, mir::ExprId word)
      -> mir::LocalId {
    return Declared(code, block, Word(), word);
  }
  auto Now(mir::Builder block, mir::LocalId local) -> mir::ExprId {
    return block.ReadLocal(local, Word());
  }
  void Set(mir::Builder block, mir::LocalId local, mir::ExprId word) {
    Statement(
        block,
        block.Add(
            mir::MakeAssignExpr(
                Unit().builtins, block.LocalPlace(local, Word()), word)));
  }

  // The fixed index `index`, and the index a loop's local holds.
  auto At(std::int64_t index) -> Where {
    return [this, index](mir::Builder block) {
      return IndexLiteral(block, index);
    };
  }
  auto AtLocal(mir::LocalId local) -> Where {
    return [this, local](mir::Builder block) {
      return block.ReadLocal(local, Machine());
    };
  }

  // The integer a plane of a value `width` bits wide is held in: a 64-bit
  // word above 64 bits, and the smallest that holds the width below.
  auto PlaneUnitOf(std::uint64_t width) -> mir::TypeId {
    if (width > 64U) {
      return Word();
    }
    mir::MachineIntWidth unit_width = mir::MachineIntWidth::k64;
    switch (value::PlaneBytesFor(width)) {
      case 1U:
        unit_width = mir::MachineIntWidth::k8;
        break;
      case 2U:
        unit_width = mir::MachineIntWidth::k16;
        break;
      case 4U:
        unit_width = mir::MachineIntWidth::k32;
        break;
      default:
        break;
    }
    return Unit().types.Intern(
        mir::Type{mir::MachineIntType{
            .width = unit_width, .signedness = mir::Signedness::kUnsigned}});
  }

  // The words the value the place `place` names lies in, to read or to write.
  auto ViewOf(
      mir::CallableCode& code, mir::Builder block, mir::ExprId place,
      mir::TypeId type, mir::Mutability mutability) -> PlaneWords {
    const mir::IntegralType& integral = Integral(type);
    const std::size_t words = value::WordCountForBits(integral.bit_width);
    const bool four_state =
        integral.state_kind == mir::IntegralStateKind::kFourState;
    const mir::TypeId unit = PlaneUnitOf(integral.bit_width);
    const mir::TypeId array = mir::MachineArrayOf(
        Unit().types, unit, four_state ? words * 2U : words);
    const mir::TypeId pointer_type =
        mir::BorrowedPointerTo(Unit().types, array, mutability);
    const mir::ExprId address = block.Add(
        mir::Expr{
            .data = mir::AddressOfExpr{.operand = place},
            .type = mir::BorrowedPointerTo(Unit().types, type, mutability)});
    const mir::LocalId pointer = Declared(
        code, block, pointer_type, CastTo(block, address, pointer_type));
    return PlaneWords{
        .pointer = pointer,
        .pointer_type = pointer_type,
        .array = array,
        .width = integral.bit_width,
        .words = words,
        .four_state = four_state,
        .unit = unit};
  }

  // The cell of `plane` at `where` of the value `view` names.
  auto Cell(
      mir::Builder block, const PlaneWords& view, const Where& where,
      Plane plane) -> mir::ExprId {
    mir::ExprId index = where(block);
    switch (plane) {
      case Plane::kValue:
        break;
      case Plane::kUnknown:
        index = mir::BuildMachineIntBinary(
            Unit(), block, mir::BinaryOp::kAdd, index,
            IndexLiteral(block, static_cast<std::int64_t>(view.words)));
        break;
    }
    const mir::ExprId array = block.Add(
        mir::MakeDerefExpr(
            block.ReadLocal(view.pointer, view.pointer_type), view.array));
    return block.Add(mir::MakeIndexExpr(array, index, view.unit));
  }
  // The word of `plane` at `where`, read as a 64-bit word; a value holding no
  // x or z has an unknown plane of zeros.
  auto PlaneWord(
      mir::Builder block, const PlaneWords& view, const Where& where,
      Plane plane) -> mir::ExprId {
    switch (plane) {
      case Plane::kValue:
        break;
      case Plane::kUnknown:
        if (!view.four_state) {
          return WordLiteral(block, 0);
        }
        break;
    }
    const mir::ExprId read = block.Read(Cell(block, view, where, plane));
    return view.unit == Word() ? read : CastTo(block, read, Word());
  }
  auto ValueWord(mir::Builder block, const PlaneWords& view, const Where& where)
      -> mir::ExprId {
    return PlaneWord(block, view, where, Plane::kValue);
  }
  auto UnknownWord(
      mir::Builder block, const PlaneWords& view, const Where& where)
      -> mir::ExprId {
    return PlaneWord(block, view, where, Plane::kUnknown);
  }
  void SetPlaneWord(
      mir::Builder block, const PlaneWords& view, const Where& where,
      Plane plane, mir::ExprId word) {
    switch (plane) {
      case Plane::kValue:
        break;
      case Plane::kUnknown:
        if (!view.four_state) {
          return;
        }
        break;
    }
    const mir::ExprId cell = Cell(block, view, where, plane);
    const mir::ExprId held =
        view.unit == Word() ? word : CastTo(block, word, view.unit);
    Statement(
        block, block.Add(mir::MakeAssignExpr(Unit().builtins, cell, held)));
  }
  void SetValueWord(
      mir::Builder block, const PlaneWords& view, const Where& where,
      mir::ExprId word) {
    SetPlaneWord(block, view, where, Plane::kValue, word);
  }
  void SetUnknownWord(
      mir::Builder block, const PlaneWords& view, const Where& where,
      mir::ExprId word) {
    SetPlaneWord(block, view, where, Plane::kUnknown, word);
  }

  // The answer of a body answering a value of this type, held in a local
  // every position of which the body writes: a value of all zeros to start.
  auto Answer(mir::CallableCode& code, mir::Builder block) -> mir::LocalId {
    return Declared(
        code, block, WholeType(),
        block.Add(
            mir::MakeIntegralLiteral(
                Unit(), WholeType(),
                mir::IntegralConstant{.value_words = {0}, .state_words = {}})));
  }
  auto AnswerView(
      mir::CallableCode& code, mir::Builder block, mir::LocalId answer)
      -> PlaneWords {
    return ViewOf(
        code, block, block.LocalPlace(answer, WholeType()), WholeType(),
        mir::Mutability::kMutable);
  }
  auto Answered(mir::Builder block, mir::LocalId answer) -> mir::ExprId {
    return block.Read(block.LocalPlace(answer, WholeType()));
  }
  auto Operand(
      mir::CallableCode& code, mir::Builder block, mir::LocalId param,
      mir::TypeId type) -> PlaneWords {
    return ViewOf(
        code, block, Referent(block, param, type), type,
        mir::Mutability::kReadOnly);
  }

  // A loop over the words of a plane `words` long, `each(body, where)` for
  // each.
  template <typename Each>
  void EachWord(
      mir::CallableCode& code, mir::Builder block, std::size_t words,
      Each each) {
    ForEachIndex(
        code, block, CountLiteral(block, words),
        [&](mir::Builder body, mir::LocalId at) { each(body, AtLocal(at)); });
  }

  // The top word of `plane` of a value left clear above its width, as every
  // value is held (LRM 6.11).
  void ClearAboveWidth(
      mir::Builder block, const PlaneWords& view, Plane plane) {
    const std::uint64_t top = value::ValidBitsMask(view.words - 1U, view.width);
    if (top == ~std::uint64_t{0}) {
      return;
    }
    const Where last = At(static_cast<std::int64_t>(view.words - 1U));
    SetPlaneWord(
        block, view, last, plane,
        And(block, PlaneWord(block, view, last, plane),
            WordLiteral(block, top)));
  }
  void ClearAboveWidth(mir::Builder block, const PlaneWords& view) {
    ClearAboveWidth(block, view, Plane::kValue);
    ClearAboveWidth(block, view, Plane::kUnknown);
  }

  // A word that is zero where no position of the values `views` names holds x
  // or z.
  auto AnyUnknown(
      mir::CallableCode& code, mir::Builder block,
      std::span<const PlaneWords> views) -> mir::LocalId {
    const mir::LocalId any = Hold(code, block, WordLiteral(block, 0));
    for (const PlaneWords& view : views) {
      if (!view.four_state) {
        continue;
      }
      EachWord(code, block, view.words, [&](mir::Builder body, const Where& i) {
        Set(body, any, Or(body, Now(body, any), UnknownWord(body, view, i)));
      });
    }
    return any;
  }

  // The positions a value's words `v`, `u` know to hold 1, or 0.
  auto KnownAs(mir::Builder block, mir::LocalId v, mir::LocalId u, bool one)
      -> mir::ExprId {
    return And(
        block, Not(block, Now(block, u)),
        one ? Now(block, v) : Not(block, Now(block, v)));
  }
  // Where the positions `zero` and `one` settle the answer, and every other
  // position an operand holds x or z at is x.
  auto UnknownUnless(
      mir::CallableCode& code, mir::Builder block, const Bits& x,
      mir::ExprId zero, mir::ExprId one) -> AnswerWord {
    const mir::LocalId held_one = Hold(code, block, one);
    const mir::LocalId unknown = Hold(
        code, block,
        And(block, Not(block, Or(block, zero, Now(block, held_one))),
            Or(block, Now(block, x.au), Now(block, x.bu))));
    return AnswerWord{
        .value = Or(block, Now(block, held_one), Now(block, unknown)),
        .unknown = Now(block, unknown)};
  }
  // The word `differ` makes of the known positions, x wherever an operand
  // holds x or z.
  auto Differing(mir::Builder block, const Bits& x, mir::ExprId differ)
      -> AnswerWord {
    return AnswerWord{
        .value =
            Or(block, Or(block, Now(block, x.au), Now(block, x.bu)), differ),
        .unknown = Or(block, Now(block, x.au), Now(block, x.bu))};
  }

  // LRM 11.4.8 Tables 11-11 to 11-15: per position, z reads as x, each word
  // of the answer the one `word` makes of the operands' words there.
  template <typename WordOfOperands>
  auto Bitwise(IntegralOp op, WordOfOperands word) -> mir::CallableCode {
    return BodyOf(
        op, [&](mir::CallableCode& code, mir::Builder block,
                const std::vector<mir::LocalId>& p) {
          const PlaneWords a = Operand(code, block, p[0], WholeType());
          const PlaneWords b = Operand(code, block, p[1], WholeType());
          const mir::LocalId answer = Answer(code, block);
          const PlaneWords out = AnswerView(code, block, answer);
          EachWord(
              code, block, a.words, [&](mir::Builder body, const Where& i) {
                const Bits bits{
                    .av = Hold(code, body, ValueWord(body, a, i)),
                    .au = Hold(code, body, UnknownWord(body, a, i)),
                    .bv = Hold(code, body, ValueWord(body, b, i)),
                    .bu = Hold(code, body, UnknownWord(body, b, i))};
                const AnswerWord made = word(code, body, bits);
                SetValueWord(body, out, i, made.value);
                SetUnknownWord(body, out, i, made.unknown);
              });
          ClearAboveWidth(block, out);
          return Answered(block, answer);
        });
  }

  auto BitwiseNot() -> mir::CallableCode {
    return BodyOf(
        IntegralOp::kBitwiseNot,
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const PlaneWords a = Operand(code, block, p[0], WholeType());
          const mir::LocalId answer = Answer(code, block);
          const PlaneWords out = AnswerView(code, block, answer);
          EachWord(
              code, block, a.words, [&](mir::Builder body, const Where& i) {
                const mir::LocalId unknown =
                    Hold(code, body, UnknownWord(body, a, i));
                SetValueWord(
                    body, out, i,
                    Or(body, Not(body, ValueWord(body, a, i)),
                       Now(body, unknown)));
                SetUnknownWord(body, out, i, Now(body, unknown));
              });
          ClearAboveWidth(block, out);
          return Answered(block, answer);
        });
  }

  // LRM 11.4.3: x or z anywhere in an operand makes the whole result x, and
  // otherwise the value planes' sum, difference or negation modulo 2^width.
  auto Arithmetic(IntegralOp op, Carried carried, Operands operands)
      -> mir::CallableCode {
    return BodyOf(
        op, [&](mir::CallableCode& code, mir::Builder block,
                const std::vector<mir::LocalId>& p) {
          std::vector<PlaneWords> read{Operand(code, block, p[0], WholeType())};
          switch (operands) {
            case Operands::kTwo:
              read.push_back(Operand(code, block, p[1], WholeType()));
              break;
            case Operands::kFromZero:
              break;
          }
          const mir::LocalId answer = Answer(code, block);
          const PlaneWords out = AnswerView(code, block, answer);
          const mir::LocalId carry = Hold(code, block, WordLiteral(block, 0));
          EachWord(
              code, block, out.words, [&](mir::Builder body, const Where& i) {
                mir::LocalId lhs{};
                mir::LocalId rhs{};
                switch (operands) {
                  case Operands::kTwo:
                    lhs = Hold(code, body, ValueWord(body, read[0], i));
                    rhs = Hold(code, body, ValueWord(body, read[1], i));
                    break;
                  case Operands::kFromZero:
                    lhs = Hold(code, body, WordLiteral(body, 0));
                    rhs = Hold(code, body, ValueWord(body, read[0], i));
                    break;
                }
                const auto below = [&](mir::LocalId l, mir::LocalId r) {
                  return WordOf(
                      body, Holds(
                                body, mir::BinaryOp::kLessThan, Now(body, l),
                                Now(body, r)));
                };
                switch (carried) {
                  case Carried::kSum: {
                    const mir::LocalId s1 = Hold(
                        code, body,
                        Op(body, mir::BinaryOp::kAdd, Now(body, lhs),
                           Now(body, rhs)));
                    const mir::LocalId s2 = Hold(
                        code, body,
                        Op(body, mir::BinaryOp::kAdd, Now(body, s1),
                           Now(body, carry)));
                    Set(body, carry,
                        Op(body, mir::BinaryOp::kAdd, below(s1, lhs),
                           below(s2, s1)));
                    SetValueWord(body, out, i, Now(body, s2));
                    break;
                  }
                  case Carried::kDifference: {
                    const mir::LocalId d1 = Hold(
                        code, body,
                        Op(body, mir::BinaryOp::kSub, Now(body, lhs),
                           Now(body, rhs)));
                    const mir::LocalId d2 = Hold(
                        code, body,
                        Op(body, mir::BinaryOp::kSub, Now(body, d1),
                           Now(body, carry)));
                    Set(body, carry,
                        Op(body, mir::BinaryOp::kAdd, below(lhs, rhs),
                           below(d1, carry)));
                    SetValueWord(body, out, i, Now(body, d2));
                    break;
                  }
                }
              });
          if (out.four_state) {
            const mir::LocalId any = AnyUnknown(code, block, read);
            const mir::LocalId fill = Hold(
                code, block,
                WordWhere(
                    block,
                    Holds(
                        block, mir::BinaryOp::kInequality, Now(block, any),
                        WordLiteral(block, 0)),
                    WordLiteral(block, ~std::uint64_t{0}),
                    WordLiteral(block, 0)));
            EachWord(
                code, block, out.words, [&](mir::Builder body, const Where& i) {
                  SetValueWord(
                      body, out, i,
                      Or(body, ValueWord(body, out, i), Now(body, fill)));
                  SetUnknownWord(body, out, i, Now(body, fill));
                });
          }
          ClearAboveWidth(block, out);
          return Answered(block, answer);
        });
  }

  // A one-bit value of `type`: 0, 1, or x, which a type holding no x holds as
  // 0, as every x written into it is.
  auto Scalar(mir::Builder block, mir::TypeId type, value::FourStateBit bit)
      -> mir::ExprId {
    if (Integral(type).state_kind == mir::IntegralStateKind::kTwoState &&
        value::UnknownBitOf(bit)) {
      bit = value::FourStateBit::kZero;
    }
    return mir::BuildIntegralLiteral(
        Unit(), block, type,
        mir::IntegralConstant{
            .value_words = {value::ValueBitOf(bit) ? 1U : 0U},
            .state_words = {value::UnknownBitOf(bit) ? 1U : 0U}});
  }

  // LRM 11.4.5 logical equality: a position both operands know and disagree on
  // settles them unequal; agreement everywhere both are known leaves the
  // answer to the unknown positions. `agreement` is what agreeing at every
  // known position answers: 1 for `==`, 0 for `!=`.
  auto Equality(IntegralOp op, value::FourStateBit agreement)
      -> mir::CallableCode {
    return BodyOf(
        op, [&](mir::CallableCode& code, mir::Builder block,
                const std::vector<mir::LocalId>& p) {
          const PlaneWords a = Operand(code, block, p[0], WholeType());
          const PlaneWords b = Operand(code, block, p[1], WholeType());
          const mir::LocalId differing =
              Hold(code, block, WordLiteral(block, 0));
          const mir::LocalId unknown = Hold(code, block, WordLiteral(block, 0));
          EachWord(
              code, block, a.words, [&](mir::Builder body, const Where& i) {
                const mir::LocalId unk = Hold(
                    code, body,
                    Or(body, UnknownWord(body, a, i), UnknownWord(body, b, i)));
                Set(body, differing,
                    Or(body, Now(body, differing),
                       And(body,
                           Op(body, mir::BinaryOp::kBitwiseXor,
                              ValueWord(body, a, i), ValueWord(body, b, i)),
                           Not(body, Now(body, unk)))));
                Set(body, unknown,
                    Or(body, Now(body, unknown), Now(body, unk)));
              });
          const mir::TypeId answer_type = code.result_type;
          mir::ExprId when_agree = Scalar(block, answer_type, agreement);
          if (a.four_state) {
            when_agree = Chosen(
                block,
                Holds(
                    block, mir::BinaryOp::kInequality, Now(block, unknown),
                    WordLiteral(block, 0)),
                Scalar(block, answer_type, value::FourStateBit::kUnknown),
                when_agree, answer_type);
          }
          return Chosen(
              block,
              Holds(
                  block, mir::BinaryOp::kEquality, Now(block, differing),
                  WordLiteral(block, 0)),
              when_agree,
              Scalar(block, answer_type, value::Inverted(agreement)),
              answer_type);
        });
  }

  // Whether every word of both planes of the two values is the same.
  auto SamePlanes(
      mir::CallableCode& code, mir::Builder block,
      const std::vector<mir::LocalId>& p) -> mir::ExprId {
    const PlaneWords a = Operand(code, block, p[0], WholeType());
    const PlaneWords b = Operand(code, block, p[1], WholeType());
    const mir::LocalId differing = Hold(code, block, WordLiteral(block, 0));
    EachWord(code, block, a.words, [&](mir::Builder body, const Where& i) {
      Set(body, differing,
          Or(body, Now(body, differing),
             Or(body,
                Op(body, mir::BinaryOp::kBitwiseXor, ValueWord(body, a, i),
                   ValueWord(body, b, i)),
                Op(body, mir::BinaryOp::kBitwiseXor, UnknownWord(body, a, i),
                   UnknownWord(body, b, i)))));
    });
    return Holds(
        block, mir::BinaryOp::kEquality, Now(block, differing),
        WordLiteral(block, 0));
  }

  // LRM 11.4.5 case equality: both planes identical, never unknown.
  auto CaseEqual() -> mir::CallableCode {
    return BodyOf(
        IntegralOp::kCaseEqual, [&](mir::CallableCode& code, mir::Builder block,
                                    const std::vector<mir::LocalId>& p) {
          return Chosen(
              block, SamePlanes(code, block, p),
              Scalar(block, code.result_type, value::FourStateBit::kOne),
              Scalar(block, code.result_type, value::FourStateBit::kZero),
              code.result_type);
        });
  }

  // Whether a write left a value as it was (LRM 9.4.2): the same bits.
  auto BitIdentical() -> mir::CallableCode {
    return BodyOf(
        IntegralOp::kBitIdentical,
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          return SamePlanes(code, block, p);
        });
  }

  // LRM 20.9 `$isunknown`: whether any position holds x or z.
  auto HasUnknown() -> mir::CallableCode {
    return BodyOf(
        IntegralOp::kHasUnknown,
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const PlaneWords a = Operand(code, block, p[0], WholeType());
          const mir::LocalId any = AnyUnknown(code, block, std::span(&a, 1));
          return Holds(
              block, mir::BinaryOp::kInequality, Now(block, any),
              WordLiteral(block, 0));
        });
  }

  // The word of the low `count` bits set, all of them from 64 up, `count` a
  // word.
  auto LowBitsOf(mir::CallableCode& code, mir::Builder block, mir::ExprId count)
      -> mir::ExprId {
    const mir::LocalId held = Hold(code, block, count);
    const mir::ExprId short_mask =
        Op(block, mir::BinaryOp::kSub,
           Op(block, mir::BinaryOp::kShl, WordLiteral(block, 1),
              And(block, Now(block, held), WordLiteral(block, 63))),
           WordLiteral(block, 1));
    return WordWhere(
        block,
        Holds(
            block, mir::BinaryOp::kGreaterEqual, Now(block, held),
            WordLiteral(block, 64)),
        WordLiteral(block, ~std::uint64_t{0}), short_mask);
  }

  // The positions of word `i` below `width`, both words: the low
  // `width - 64 i` bits of it.
  auto ValidBits(
      mir::CallableCode& code, mir::Builder block, const Where& i,
      mir::ExprId width) -> mir::ExprId {
    const mir::LocalId low = Hold(
        code, block,
        Op(block, mir::BinaryOp::kMul, CastTo(block, i(block), Word()),
           WordLiteral(block, 64)));
    const mir::LocalId held_width = Hold(code, block, width);
    return LowBitsOf(
        code, block,
        WordWhere(
            block,
            Holds(
                block, mir::BinaryOp::kGreaterThan, Now(block, held_width),
                Now(block, low)),
            Op(block, mir::BinaryOp::kSub, Now(block, held_width),
               Now(block, low)),
            WordLiteral(block, 0)));
  }

  // The 64 bits of `plane` of `view` from the bit `at`, a word, reading 0
  // past the value's words.
  auto BitsFrom(
      mir::CallableCode& code, mir::Builder block, const PlaneWords& view,
      mir::ExprId at, Plane plane) -> mir::ExprId {
    const mir::LocalId held = Hold(code, block, at);
    const mir::TypeId machine = Machine();
    const mir::LocalId index = Declared(
        code, block, machine,
        CastTo(
            block,
            Op(block, mir::BinaryOp::kShr, Now(block, held),
               WordLiteral(block, 6)),
            machine));
    const mir::LocalId bit =
        Hold(code, block, And(block, Now(block, held), WordLiteral(block, 63)));
    const auto word_at = [&](std::int64_t plus) {
      return Hold(
          code, block,
          WordOrZero(
              code, block, view,
              [this, index, plus](mir::Builder b) {
                return mir::BuildMachineIntBinary(
                    Unit(), b, mir::BinaryOp::kAdd,
                    b.ReadLocal(index, Machine()), IndexLiteral(b, plus));
              },
              plane));
    };
    const mir::LocalId low = word_at(0);
    const mir::LocalId high = word_at(1);
    const mir::ExprId above = WordWhere(
        block,
        Holds(
            block, mir::BinaryOp::kEquality, Now(block, bit),
            WordLiteral(block, 0)),
        WordLiteral(block, 0),
        Op(block, mir::BinaryOp::kShl, Now(block, high),
           Op(block, mir::BinaryOp::kSub, WordLiteral(block, 64),
              Now(block, bit))));
    return Or(
        block, Op(block, mir::BinaryOp::kShr, Now(block, low), Now(block, bit)),
        above);
  }

  // The word of `plane` at the machine index `where`, which may lie below 0
  // or past the value's words, reading 0 there.
  auto WordOrZero(
      mir::CallableCode& code, mir::Builder block, const PlaneWords& view,
      const Where& where, Plane plane) -> mir::ExprId {
    const mir::TypeId machine = Machine();
    const mir::LocalId index = Declared(code, block, machine, where(block));
    const auto last = static_cast<std::int64_t>(view.words - 1U);
    const mir::ExprId within = Holds(
        block, mir::BinaryOp::kEquality,
        And(block,
            WordOf(
                block,
                mir::BuildMachineIntComparison(
                    Unit(), block, mir::BinaryOp::kGreaterEqual,
                    block.ReadLocal(index, machine), IndexLiteral(block, 0))),
            WordOf(
                block, mir::BuildMachineIntComparison(
                           Unit(), block, mir::BinaryOp::kLessEqual,
                           block.ReadLocal(index, machine),
                           IndexLiteral(block, last)))),
        WordLiteral(block, 1));
    const mir::LocalId held_within =
        Declared(code, block, Unit().builtins.machine_bool, within);
    // An index outside the words reads word 0, whose value the condition then
    // passes over.
    const mir::LocalId clamped = Declared(
        code, block, machine,
        Chosen(
            block, block.ReadLocal(held_within, Unit().builtins.machine_bool),
            block.ReadLocal(index, machine), IndexLiteral(block, 0), machine));
    return WordWhere(
        block, block.ReadLocal(held_within, Unit().builtins.machine_bool),
        PlaneWord(block, view, AtLocal(clamped), plane), WordLiteral(block, 0));
  }

  // The order of two numbers of this type's signedness (LRM 11.4.4), from the
  // most significant word down: 0 where they are the same, 1 where the first
  // lies below and 2 where above, a signed value's top position weighing
  // negative. `holding` has bit `order` set for each order the relation
  // holds at.
  auto Relation(IntegralOp op, std::uint64_t holding) -> mir::CallableCode {
    return BodyOf(
        op, [&](mir::CallableCode& code, mir::Builder block,
                const std::vector<mir::LocalId>& p) {
          const PlaneWords a = Operand(code, block, p[0], WholeType());
          const PlaneWords b = Operand(code, block, p[1], WholeType());
          const std::uint64_t sign =
              Integral(WholeType()).signedness == mir::Signedness::kSigned
                  ? std::uint64_t{1} << ((a.width - 1U) % 64U)
                  : 0U;
          const auto top = static_cast<std::int64_t>(a.words - 1U);
          const mir::LocalId order = Hold(code, block, WordLiteral(block, 0));
          EachWord(
              code, block, a.words, [&](mir::Builder body, const Where& k) {
                const mir::TypeId machine = Machine();
                const mir::LocalId from_top = Declared(
                    code, body, machine,
                    mir::BuildMachineIntBinary(
                        Unit(), body, mir::BinaryOp::kSub,
                        IndexLiteral(body, top), k(body)));
                const Where i = AtLocal(from_top);
                const mir::LocalId inverted = Hold(
                    code, body,
                    WordWhere(
                        body,
                        mir::BuildMachineIntComparison(
                            Unit(), body, mir::BinaryOp::kEquality,
                            body.ReadLocal(from_top, machine),
                            IndexLiteral(body, top)),
                        WordLiteral(body, sign), WordLiteral(body, 0)));
                const mir::LocalId aw = Hold(
                    code, body,
                    Op(body, mir::BinaryOp::kBitwiseXor, ValueWord(body, a, i),
                       Now(body, inverted)));
                const mir::LocalId bw = Hold(
                    code, body,
                    Op(body, mir::BinaryOp::kBitwiseXor, ValueWord(body, b, i),
                       Now(body, inverted)));
                const mir::ExprId undecided = Holds(
                    body, mir::BinaryOp::kEquality, Now(body, order),
                    WordLiteral(body, 0));
                const mir::ExprId differ = Holds(
                    body, mir::BinaryOp::kInequality, Now(body, aw),
                    Now(body, bw));
                const mir::ExprId decided_here = Holds(
                    body, mir::BinaryOp::kEquality,
                    And(body, WordOf(body, undecided), WordOf(body, differ)),
                    WordLiteral(body, 1));
                Set(body, order,
                    WordWhere(
                        body, decided_here,
                        WordWhere(
                            body,
                            Holds(
                                body, mir::BinaryOp::kLessThan, Now(body, aw),
                                Now(body, bw)),
                            WordLiteral(body, 1), WordLiteral(body, 2)),
                        Now(body, order)));
              });
          const mir::TypeId answer_type = code.result_type;
          const mir::ExprId holds = Holds(
              block, mir::BinaryOp::kInequality,
              And(block,
                  Op(block, mir::BinaryOp::kShr, WordLiteral(block, holding),
                     Now(block, order)),
                  WordLiteral(block, 1)),
              WordLiteral(block, 0));
          mir::ExprId known = Chosen(
              block, holds,
              Scalar(block, answer_type, value::FourStateBit::kOne),
              Scalar(block, answer_type, value::FourStateBit::kZero),
              answer_type);
          if (a.four_state) {
            const std::array both{a, b};
            const mir::LocalId any = AnyUnknown(code, block, both);
            known = Chosen(
                block,
                Holds(
                    block, mir::BinaryOp::kInequality, Now(block, any),
                    WordLiteral(block, 0)),
                Scalar(block, answer_type, value::FourStateBit::kUnknown),
                known, answer_type);
          }
          return known;
        });
  }

  // What every position of a value reduces to (LRM 11.4.9): a known 0 settles
  // an AND and a known 1 an OR whatever else the value holds, and an x or z
  // leaves either unknown only where nothing settles it; an XOR is the parity
  // of the known positions, unknown where any is not. `inverted` is the NAND,
  // NOR or XNOR of the same.
  auto Reduction(IntegralOp op, value::ReductionOp reduced)
      -> mir::CallableCode {
    return BodyOf(
        op, [&](mir::CallableCode& code, mir::Builder block,
                const std::vector<mir::LocalId>& p) {
          const PlaneWords a = Operand(code, block, p[0], WholeType());
          const mir::LocalId any_zero =
              Hold(code, block, WordLiteral(block, 0));
          const mir::LocalId any_one = Hold(code, block, WordLiteral(block, 0));
          const mir::LocalId parity = Hold(code, block, WordLiteral(block, 0));
          const std::uint64_t top = value::ValidBitsMask(a.words - 1U, a.width);
          const auto last = static_cast<std::int64_t>(a.words - 1U);
          EachWord(
              code, block, a.words, [&](mir::Builder body, const Where& i) {
                const mir::LocalId valid = Hold(
                    code, body,
                    WordWhere(
                        body,
                        mir::BuildMachineIntComparison(
                            Unit(), body, mir::BinaryOp::kEquality, i(body),
                            IndexLiteral(body, last)),
                        WordLiteral(body, top),
                        WordLiteral(body, ~std::uint64_t{0})));
                const mir::LocalId known = Hold(
                    code, body,
                    And(body, Not(body, UnknownWord(body, a, i)),
                        Now(body, valid)));
                const mir::LocalId v = Hold(code, body, ValueWord(body, a, i));
                Set(body, any_zero,
                    Or(body, Now(body, any_zero),
                       And(body, Not(body, Now(body, v)), Now(body, known))));
                Set(body, any_one,
                    Or(body, Now(body, any_one),
                       And(body, Now(body, v), Now(body, known))));
                Set(body, parity,
                    Op(body, mir::BinaryOp::kBitwiseXor, Now(body, parity),
                       And(body, Now(body, v), Now(body, known))));
              });
          const mir::LocalId any_unknown =
              AnyUnknown(code, block, std::span(&a, 1));
          for (const std::uint64_t by : {32U, 16U, 8U, 4U, 2U, 1U}) {
            Set(block, parity,
                Op(block, mir::BinaryOp::kBitwiseXor, Now(block, parity),
                   Op(block, mir::BinaryOp::kShr, Now(block, parity),
                      WordLiteral(block, by))));
          }
          const mir::TypeId answer_type = code.result_type;
          const auto is_set = [&](mir::LocalId word) {
            return Holds(
                block, mir::BinaryOp::kInequality, Now(block, word),
                WordLiteral(block, 0));
          };
          const auto bit = [&](value::FourStateBit b, bool invert) {
            return Scalar(block, answer_type, invert ? value::Inverted(b) : b);
          };
          // The answer: `settled` where `settles` holds, else x where any
          // position is unknown, else `otherwise`.
          const auto answered =
              [&](mir::ExprId settles, value::FourStateBit settled,
                  value::FourStateBit otherwise, bool invert) {
                return Chosen(
                    block, settles, bit(settled, invert),
                    Chosen(
                        block, is_set(any_unknown),
                        bit(value::FourStateBit::kUnknown, invert),
                        bit(otherwise, invert), answer_type),
                    answer_type);
              };
          bool invert = false;
          switch (reduced) {
            case value::ReductionOp::kNand:
              invert = true;
              [[fallthrough]];
            case value::ReductionOp::kAnd:
              return answered(
                  is_set(any_zero), value::FourStateBit::kZero,
                  value::FourStateBit::kOne, invert);
            case value::ReductionOp::kNor:
              invert = true;
              [[fallthrough]];
            case value::ReductionOp::kOr:
              return answered(
                  is_set(any_one), value::FourStateBit::kOne,
                  value::FourStateBit::kZero, invert);
            case value::ReductionOp::kXnor:
              invert = true;
              [[fallthrough]];
            case value::ReductionOp::kXor:
              return Chosen(
                  block, is_set(any_unknown),
                  bit(value::FourStateBit::kUnknown, invert),
                  Chosen(
                      block,
                      Holds(
                          block, mir::BinaryOp::kInequality,
                          And(block, Now(block, parity), WordLiteral(block, 1)),
                          WordLiteral(block, 0)),
                      bit(value::FourStateBit::kOne, invert),
                      bit(value::FourStateBit::kZero, invert), answer_type),
                  answer_type);
          }
          throw InternalError("hir_to_mir: unknown reduction");
        });
  }

  // Whether some position holds a known 1, which is what makes a value true
  // (LRM 11.4.7).
  auto KnownOne(
      mir::CallableCode& code, mir::Builder block, const PlaneWords& a)
      -> mir::LocalId {
    const mir::LocalId one = Hold(code, block, WordLiteral(block, 0));
    EachWord(code, block, a.words, [&](mir::Builder body, const Where& i) {
      Set(body, one,
          Or(body, Now(body, one),
             And(body, ValueWord(body, a, i),
                 Not(body, UnknownWord(body, a, i)))));
    });
    return one;
  }

  // Whether a value is known true (LRM 11.4.7, 12.4), as a condition reads it.
  auto IsTrue() -> mir::CallableCode {
    return BodyOf(
        IntegralOp::kIsTrue, [&](mir::CallableCode& code, mir::Builder block,
                                 const std::vector<mir::LocalId>& p) {
          const PlaneWords a = Operand(code, block, p[0], WholeType());
          const mir::LocalId one = KnownOne(code, block, a);
          return Holds(
              block, mir::BinaryOp::kInequality, Now(block, one),
              WordLiteral(block, 0));
        });
  }

  // LRM 11.4.11 Table 11-20: the arms of a conditional whose condition is
  // ambiguous, combined -- a position both arms know and agree on survives,
  // and every other is x.
  auto MergeConditional() -> mir::CallableCode {
    return BodyOf(
        IntegralOp::kMergeConditional,
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const PlaneWords a = Operand(code, block, p[0], WholeType());
          const PlaneWords b = Operand(code, block, p[1], WholeType());
          const mir::LocalId answer = Answer(code, block);
          const PlaneWords out = AnswerView(code, block, answer);
          EachWord(
              code, block, a.words, [&](mir::Builder body, const Where& i) {
                const mir::LocalId av = Hold(code, body, ValueWord(body, a, i));
                const mir::LocalId agreed = Hold(
                    code, body,
                    And(body,
                        Not(body, Op(body, mir::BinaryOp::kBitwiseXor,
                                     Now(body, av), ValueWord(body, b, i))),
                        Not(body, Or(body, UnknownWord(body, a, i),
                                     UnknownWord(body, b, i)))));
                const mir::LocalId unknown =
                    Hold(code, body, Not(body, Now(body, agreed)));
                const mir::ExprId value =
                    And(body, Now(body, av), Now(body, agreed));
                SetValueWord(
                    body, out, i,
                    out.four_state ? Or(body, value, Now(body, unknown))
                                   : value);
                SetUnknownWord(body, out, i, Now(body, unknown));
              });
          ClearAboveWidth(block, out);
          return Answered(block, answer);
        });
  }

  // How far a shift amount moves a value (LRM 11.4.10): its number,
  // saturated past every width where a word above its first is set; and
  // whether it holds x or z, which makes every position of the answer x.
  struct ShiftAmount {
    mir::LocalId by;
    mir::LocalId unknown;
  };
  auto AmountOf(
      mir::CallableCode& code, mir::Builder block, mir::LocalId param,
      mir::TypeId type) -> ShiftAmount {
    const PlaneWords amount = Operand(code, block, param, type);
    const mir::LocalId above = Hold(code, block, WordLiteral(block, 0));
    ForEachIndexBetween(
        code, block, IndexLiteral(block, 1), CountLiteral(block, amount.words),
        [&](mir::Builder body, mir::LocalId at) {
          Set(body, above,
              Or(body, Now(body, above), ValueWord(body, amount, AtLocal(at))));
        });
    const mir::LocalId by = Hold(
        code, block,
        WordWhere(
            block,
            Holds(
                block, mir::BinaryOp::kInequality, Now(block, above),
                WordLiteral(block, 0)),
            WordLiteral(block, ~std::uint64_t{0}),
            ValueWord(block, amount, At(0))));
    return ShiftAmount{
        .by = by, .unknown = AnyUnknown(code, block, std::span(&amount, 1))};
  }

  // Every position of the answer x, as a shift by an unknown amount makes it
  // (LRM 11.4.10), where `unknown` is set; 0 in a type holding no x.
  void UnknownWhere(
      mir::CallableCode& code, mir::Builder block, const PlaneWords& out,
      mir::LocalId unknown) {
    const mir::LocalId fill = Hold(
        code, block,
        WordWhere(
            block,
            Holds(
                block, mir::BinaryOp::kInequality, Now(block, unknown),
                WordLiteral(block, 0)),
            WordLiteral(block, ~std::uint64_t{0}), WordLiteral(block, 0)));
    EachWord(code, block, out.words, [&](mir::Builder body, const Where& i) {
      const mir::LocalId v = Hold(code, body, ValueWord(body, out, i));
      if (out.four_state) {
        SetValueWord(body, out, i, Or(body, Now(body, v), Now(body, fill)));
        SetUnknownWord(
            body, out, i, Or(body, UnknownWord(body, out, i), Now(body, fill)));
      } else {
        SetValueWord(
            body, out, i, And(body, Now(body, v), Not(body, Now(body, fill))));
      }
    });
    ClearAboveWidth(block, out);
  }

  // LRM 11.4.10 `<<`: each plane moved toward the most significant end, the
  // vacated positions 0; an amount at or past the width moves every bit out.
  auto ShiftLeft(const mir::TypeOperationSource& source) -> mir::CallableCode {
    const mir::TypeId amount_type = source.arguments.front();
    return Defined(
        Declaration(source), [&](mir::CallableCode& code, mir::Builder block,
                                 const std::vector<mir::LocalId>& p) {
          const PlaneWords a = Operand(code, block, p[0], WholeType());
          const ShiftAmount amount = AmountOf(code, block, p[1], amount_type);
          const mir::LocalId answer = Answer(code, block);
          const PlaneWords out = AnswerView(code, block, answer);
          const mir::LocalId within = Hold(
              code, block,
              WordOf(
                  block,
                  Holds(
                      block, mir::BinaryOp::kLessThan, Now(block, amount.by),
                      WordLiteral(block, a.width))));
          const mir::LocalId by = Hold(
              code, block,
              WordWhere(
                  block,
                  Holds(
                      block, mir::BinaryOp::kInequality, Now(block, within),
                      WordLiteral(block, 0)),
                  Now(block, amount.by), WordLiteral(block, 0)));
          EachWord(
              code, block, out.words, [&](mir::Builder body, const Where& i) {
                // Word `i` of the answer is the source's bits from `64 i - by`.
                const mir::LocalId from = Hold(
                    code, body,
                    Op(body, mir::BinaryOp::kSub,
                       Op(body, mir::BinaryOp::kMul,
                          CastTo(body, i(body), Word()), WordLiteral(body, 64)),
                       Now(body, by)));
                for (const Plane plane : {Plane::kValue, Plane::kUnknown}) {
                  const mir::ExprId moved = WordWhere(
                      body,
                      Holds(
                          body, mir::BinaryOp::kInequality, Now(body, within),
                          WordLiteral(body, 0)),
                      SignedBitsFrom(code, body, a, from, plane),
                      WordLiteral(body, 0));
                  SetPlaneWord(body, out, i, plane, moved);
                }
              });
          ClearAboveWidth(block, out);
          UnknownWhere(code, block, out, amount.unknown);
          block.AppendStmt(mir::ReturnStmt{.value = Answered(block, answer)});
        });
  }

  // The 64 bits of `plane` from the bit the word `from` holds as a signed
  // number, reading 0 below the value and past it.
  auto SignedBitsFrom(
      mir::CallableCode& code, mir::Builder block, const PlaneWords& view,
      mir::LocalId from, Plane plane) -> mir::ExprId {
    // A start below 0 by `d` bits reads the word at 0 shifted up by `d`, which
    // is the 64 bits from 0 moved left; past -64 reads nothing.
    const mir::TypeId machine = Machine();
    const mir::LocalId start = Declared(
        code, block, machine, CastTo(block, Now(block, from), machine));
    const mir::ExprId negative = mir::BuildMachineIntComparison(
        Unit(), block, mir::BinaryOp::kLessThan,
        block.ReadLocal(start, machine), IndexLiteral(block, 0));
    const mir::LocalId below =
        Declared(code, block, Unit().builtins.machine_bool, negative);
    const mir::LocalId shortfall = Hold(
        code, block,
        Op(block, mir::BinaryOp::kSub, WordLiteral(block, 0),
           Now(block, from)));
    const mir::ExprId from_zero = WordWhere(
        block,
        Holds(
            block, mir::BinaryOp::kGreaterEqual, Now(block, shortfall),
            WordLiteral(block, 64)),
        WordLiteral(block, 0),
        Op(block, mir::BinaryOp::kShl,
           BitsFrom(code, block, view, WordLiteral(block, 0), plane),
           Now(block, shortfall)));
    const mir::ExprId from_start = BitsFrom(
        code, block, view,
        WordWhere(
            block, block.ReadLocal(below, Unit().builtins.machine_bool),
            WordLiteral(block, 0), Now(block, from)),
        plane);
    return WordWhere(
        block, block.ReadLocal(below, Unit().builtins.machine_bool), from_zero,
        from_start);
  }

  // LRM 11.4.10 `>>` and `>>>`: each plane moved toward the least significant
  // end, the vacated positions filled with 0, or, for `>>>` of a signed value,
  // each plane's own top position, so an x at the top extends down.
  auto ShiftRight(const mir::TypeOperationSource& source, bool arithmetic)
      -> mir::CallableCode {
    const mir::TypeId amount_type = source.arguments.front();
    return Defined(
        Declaration(source), [&](mir::CallableCode& code, mir::Builder block,
                                 const std::vector<mir::LocalId>& p) {
          const PlaneWords a = Operand(code, block, p[0], WholeType());
          const ShiftAmount amount = AmountOf(code, block, p[1], amount_type);
          const mir::LocalId answer = Answer(code, block);
          const PlaneWords out = AnswerView(code, block, answer);
          const bool fills = arithmetic && Integral(WholeType()).signedness ==
                                               mir::Signedness::kSigned;
          const mir::LocalId vacated = Hold(
              code, block,
              WordWhere(
                  block,
                  Holds(
                      block, mir::BinaryOp::kLessThan, Now(block, amount.by),
                      WordLiteral(block, a.width)),
                  Now(block, amount.by), WordLiteral(block, a.width)));
          const auto top = static_cast<std::int64_t>(a.words - 1U);
          const std::uint64_t sign_bit = (a.width - 1U) % 64U;
          EachWord(
              code, block, out.words, [&](mir::Builder body, const Where& i) {
                const mir::LocalId low = Hold(
                    code, body,
                    Op(body, mir::BinaryOp::kMul, CastTo(body, i(body), Word()),
                       WordLiteral(body, 64)));
                const mir::LocalId valid = Hold(
                    code, body,
                    ValidBits(code, body, i, WordLiteral(body, a.width)));
                const mir::LocalId above = Hold(
                    code, body,
                    And(body, Now(body, valid),
                        Not(body, ValidBits(
                                      code, body, i,
                                      Op(body, mir::BinaryOp::kSub,
                                         WordLiteral(body, a.width),
                                         Now(body, vacated))))));
                for (const Plane plane : {Plane::kValue, Plane::kUnknown}) {
                  const mir::ExprId moved = BitsFrom(
                      code, body, a,
                      Op(body, mir::BinaryOp::kAdd, Now(body, low),
                         Now(body, vacated)),
                      plane);
                  const mir::ExprId fill =
                      fills ? And(body,
                                  Repeated(
                                      body, PlaneWord(body, a, At(top), plane),
                                      sign_bit),
                                  Now(body, above))
                            : WordLiteral(body, 0);
                  SetPlaneWord(body, out, i, plane, Or(body, moved, fill));
                }
              });
          ClearAboveWidth(block, out);
          UnknownWhere(code, block, out, amount.unknown);
          block.AppendStmt(mir::ReturnStmt{.value = Answered(block, answer)});
        });
  }

  // LRM 11.4.12.1: the operand's bits repeated to fill this type, the first
  // copy at the least significant end.
  auto Replicate(const mir::TypeOperationSource& source) -> mir::CallableCode {
    const mir::TypeId from = source.arguments.front();
    return Defined(
        Declaration(source), [&](mir::CallableCode& code, mir::Builder block,
                                 const std::vector<mir::LocalId>& p) {
          const PlaneWords src = Operand(code, block, p[0], from);
          const mir::LocalId answer = Answer(code, block);
          const PlaneWords out = AnswerView(code, block, answer);
          const std::uint64_t copies = out.width / src.width;
          ForEachIndex(
              code, block, CountLiteral(block, copies),
              [&](mir::Builder body, mir::LocalId copy) {
                EachWord(
                    code, body, src.words,
                    [&](mir::Builder inner, const Where& j) {
                      const mir::LocalId at = Hold(
                          code, inner,
                          Op(inner, mir::BinaryOp::kAdd,
                             Op(inner, mir::BinaryOp::kMul,
                                CastTo(
                                    inner, inner.ReadLocal(copy, Machine()),
                                    Word()),
                                WordLiteral(inner, src.width)),
                             Op(inner, mir::BinaryOp::kMul,
                                CastTo(inner, j(inner), Word()),
                                WordLiteral(inner, 64))));
                      for (const Plane plane :
                           {Plane::kValue, Plane::kUnknown}) {
                        OrBitsAt(
                            code, inner, out, at,
                            PlaneWord(inner, src, j, plane), plane);
                      }
                    });
              });
          ClearAboveWidth(block, out);
          block.AppendStmt(mir::ReturnStmt{.value = Answered(block, answer)});
        });
  }

  // `word` written over the 64 bits of `plane` of `out` from the bit the word
  // `at` holds, by OR, what lies past the answer's words dropped.
  void OrBitsAt(
      mir::CallableCode& code, mir::Builder block, const PlaneWords& out,
      mir::LocalId at, mir::ExprId word, Plane plane) {
    const mir::LocalId held = Hold(code, block, word);
    const mir::TypeId machine = Machine();
    const mir::LocalId index = Declared(
        code, block, machine,
        CastTo(
            block,
            Op(block, mir::BinaryOp::kShr, Now(block, at),
               WordLiteral(block, 6)),
            machine));
    const mir::LocalId bit =
        Hold(code, block, And(block, Now(block, at), WordLiteral(block, 63)));
    const auto lands = [&](std::int64_t plus, const auto& part_of) {
      const Where where = [this, index, plus](mir::Builder b) {
        return mir::BuildMachineIntBinary(
            Unit(), b, mir::BinaryOp::kAdd, b.ReadLocal(index, Machine()),
            IndexLiteral(b, plus));
      };
      mir::Block inside;
      mir::Builder then_block = block.Into(inside);
      SetPlaneWord(
          then_block, out, where, plane,
          Or(then_block, PlaneWord(then_block, out, where, plane),
             part_of(then_block)));
      const mir::ExprId within = mir::BuildMachineIntComparison(
          Unit(), block, mir::BinaryOp::kLessThan,
          mir::BuildMachineIntBinary(
              Unit(), block, mir::BinaryOp::kAdd,
              block.ReadLocal(index, Machine()), IndexLiteral(block, plus)),
          IndexLiteral(block, static_cast<std::int64_t>(out.words)));
      block.AppendStmt(
          mir::IfStmt{
              .condition = within,
              .then_scope = block.AddScope(std::move(inside)),
              .else_scope = std::nullopt});
    };
    lands(0, [&](mir::Builder b) {
      return Op(b, mir::BinaryOp::kShl, Now(b, held), Now(b, bit));
    });
    lands(1, [&](mir::Builder b) {
      return WordWhere(
          b, Holds(b, mir::BinaryOp::kEquality, Now(b, bit), WordLiteral(b, 0)),
          WordLiteral(b, 0),
          Op(b, mir::BinaryOp::kShr, Now(b, held),
             Op(b, mir::BinaryOp::kSub, WordLiteral(b, 64), Now(b, bit))));
    });
  }

  // The two-word machine integer a word's product and a two-word quotient are
  // taken in.
  auto Wide() -> mir::TypeId {
    return Unit().types.Intern(
        mir::Type{mir::MachineIntType{
            .width = mir::MachineIntWidth::k128,
            .signedness = mir::Signedness::kUnsigned}});
  }
  auto WideOp(
      mir::Builder block, mir::BinaryOp op, mir::ExprId lhs, mir::ExprId rhs)
      -> mir::ExprId {
    return block.Add(
        mir::MakeBinary(Unit(), block.Body(), op, lhs, rhs, Wide()));
  }
  auto WideLiteral(mir::Builder block, std::uint64_t value) -> mir::ExprId {
    return block.Add(
        mir::Expr{
            .data =
                mir::MachineIntLiteral{
                    .value = static_cast<std::int64_t>(value)},
            .type = Wide()});
  }
  auto Widened(mir::Builder block, mir::ExprId word) -> mir::ExprId {
    return CastTo(block, word, Wide());
  }
  auto LowWord(mir::Builder block, mir::ExprId wide) -> mir::ExprId {
    return CastTo(block, wide, Word());
  }
  auto HighWord(mir::Builder block, mir::ExprId wide) -> mir::ExprId {
    return LowWord(
        block,
        WideOp(block, mir::BinaryOp::kShr, wide, WideLiteral(block, 64)));
  }
  auto HoldWide(mir::CallableCode& code, mir::Builder block, mir::ExprId wide)
      -> mir::LocalId {
    return Declared(code, block, Wide(), wide);
  }
  auto NowWide(mir::Builder block, mir::LocalId local) -> mir::ExprId {
    return block.ReadLocal(local, Wide());
  }
  void SetWide(mir::Builder block, mir::LocalId local, mir::ExprId wide) {
    Statement(
        block,
        block.Add(
            mir::MakeAssignExpr(
                Unit().builtins, block.LocalPlace(local, Wide()), wide)));
  }
  auto HoldIndex(mir::CallableCode& code, mir::Builder block, mir::ExprId index)
      -> mir::LocalId {
    return Declared(code, block, Machine(), index);
  }
  // The index a local holds, plus `plus`.
  auto AtPlus(mir::LocalId local, std::int64_t plus) -> Where {
    return [this, local, plus](mir::Builder block) {
      return mir::BuildMachineIntBinary(
          Unit(), block, mir::BinaryOp::kAdd, block.ReadLocal(local, Machine()),
          IndexLiteral(block, plus));
    };
  }
  auto IndexSum(mir::Builder block, mir::ExprId lhs, mir::ExprId rhs)
      -> mir::ExprId {
    return mir::BuildMachineIntBinary(
        Unit(), block, mir::BinaryOp::kAdd, lhs, rhs);
  }
  auto IndexDifference(mir::Builder block, mir::ExprId lhs, mir::ExprId rhs)
      -> mir::ExprId {
    return mir::BuildMachineIntBinary(
        Unit(), block, mir::BinaryOp::kSub, lhs, rhs);
  }
  // What runs where `condition` holds: the statements `then` states.
  template <typename Then>
  void When(mir::Builder block, mir::ExprId condition, Then then) {
    mir::Block inside;
    then(block.Into(inside));
    block.AppendStmt(
        mir::IfStmt{
            .condition = condition,
            .then_scope = block.AddScope(std::move(inside)),
            .else_scope = std::nullopt});
  }
  // The same, with what runs where it does not.
  template <typename Then, typename Else>
  void Choose(
      mir::Builder block, mir::ExprId condition, Then then, Else otherwise) {
    mir::Block then_block;
    then(block.Into(then_block));
    mir::Block else_block;
    otherwise(block.Into(else_block));
    block.AppendStmt(
        mir::IfStmt{
            .condition = condition,
            .then_scope = block.AddScope(std::move(then_block)),
            .else_scope = block.AddScope(std::move(else_block))});
  }

  // A value of two states and `words` words, all zero, which a body works a
  // magnitude in.
  auto Scratch(mir::CallableCode& code, mir::Builder block, std::size_t words)
      -> std::pair<mir::LocalId, mir::TypeId> {
    const mir::TypeId type = mir::PackedVectorOf(
        Unit().types, words * 64U, mir::IntegralStateKind::kTwoState);
    const mir::LocalId local = Declared(
        code, block, type,
        block.Add(
            mir::MakeIntegralLiteral(
                Unit(), type,
                mir::IntegralConstant{.value_words = {0}, .state_words = {}})));
    return {local, type};
  }
  auto ScratchView(
      mir::CallableCode& code, mir::Builder block,
      const std::pair<mir::LocalId, mir::TypeId>& scratch) -> PlaneWords {
    return ViewOf(
        code, block, block.LocalPlace(scratch.first, scratch.second),
        scratch.second, mir::Mutability::kMutable);
  }

  // The number of leading zero bits of `word`, not zero, by halving.
  auto LeadingZeros(
      mir::CallableCode& code, mir::Builder block, mir::ExprId word)
      -> mir::LocalId {
    const mir::LocalId x = Hold(code, block, word);
    const mir::LocalId zeros = Hold(code, block, WordLiteral(block, 0));
    for (const std::uint64_t half : {32U, 16U, 8U, 4U, 2U, 1U}) {
      const mir::LocalId empty = Declared(
          code, block, Unit().builtins.machine_bool,
          Holds(
              block, mir::BinaryOp::kEquality,
              Op(block, mir::BinaryOp::kShr, Now(block, x),
                 WordLiteral(block, 64U - half)),
              WordLiteral(block, 0)));
      const auto is_empty = [&] {
        return block.ReadLocal(empty, Unit().builtins.machine_bool);
      };
      Set(block, zeros,
          Op(block, mir::BinaryOp::kAdd, Now(block, zeros),
             WordWhere(
                 block, is_empty(), WordLiteral(block, half),
                 WordLiteral(block, 0))));
      Set(block, x,
          WordWhere(
              block, is_empty(),
              Op(block, mir::BinaryOp::kShl, Now(block, x),
                 WordLiteral(block, half)),
              Now(block, x)));
    }
    return zeros;
  }

  // The bits of word `i` of `a` known to be 1, none past its width.
  auto KnownOnes(
      mir::CallableCode& code, mir::Builder block, const PlaneWords& a,
      const Where& i) -> mir::ExprId {
    return And(
        block,
        And(block, ValueWord(block, a, i),
            Not(block, UnknownWord(block, a, i))),
        ValidBits(code, block, i, WordLiteral(block, a.width)));
  }

  // One more than the index of the highest bit of `a` known to be 1, as a
  // word; 0 where none is.
  auto BitLength(
      mir::CallableCode& code, mir::Builder block, const PlaneWords& a)
      -> mir::LocalId {
    const mir::LocalId above = Hold(code, block, WordLiteral(block, 0));
    EachWord(code, block, a.words, [&](mir::Builder body, const Where& i) {
      const mir::LocalId word = Hold(code, body, KnownOnes(code, body, a, i));
      When(
          body,
          Holds(
              body, mir::BinaryOp::kInequality, Now(body, word),
              WordLiteral(body, 0)),
          [&](mir::Builder then) {
            const mir::LocalId zeros =
                LeadingZeros(code, then, Now(then, word));
            Set(then, above,
                Op(then, mir::BinaryOp::kSub,
                   Op(then, mir::BinaryOp::kAdd,
                      Op(then, mir::BinaryOp::kMul,
                         CastTo(then, i(then), Word()), WordLiteral(then, 64)),
                      WordLiteral(then, 64)),
                   Now(then, zeros)));
          });
    });
    return above;
  }

  // The words of `view` below `words` made their two's complement negation in
  // place, the top word kept to `width`.
  void NegateWords(
      mir::CallableCode& code, mir::Builder block, const PlaneWords& view,
      std::size_t words, std::uint64_t width) {
    const mir::LocalId borrow = Hold(code, block, WordLiteral(block, 0));
    EachWord(code, block, words, [&](mir::Builder body, const Where& i) {
      const mir::LocalId x = Hold(code, body, ValueWord(body, view, i));
      const mir::LocalId d1 = Hold(
          code, body,
          Op(body, mir::BinaryOp::kSub, WordLiteral(body, 0), Now(body, x)));
      SetValueWord(
          body, view, i,
          Op(body, mir::BinaryOp::kSub, Now(body, d1), Now(body, borrow)));
      Set(body, borrow,
          Op(body, mir::BinaryOp::kAdd,
             WordOf(
                 body, Holds(
                           body, mir::BinaryOp::kInequality, Now(body, x),
                           WordLiteral(body, 0))),
             WordOf(
                 body, Holds(
                           body, mir::BinaryOp::kLessThan, Now(body, d1),
                           Now(body, borrow)))));
    });
    const std::uint64_t top = value::ValidBitsMask(words - 1U, width);
    if (top != ~std::uint64_t{0}) {
      const Where last = At(static_cast<std::int64_t>(words - 1U));
      SetValueWord(
          block, view, last,
          And(block, ValueWord(block, view, last), WordLiteral(block, top)));
    }
  }

  // LRM 11.4.3: the product modulo 2^width, schoolbook, each pair of words'
  // product taken whole in two words; x or z anywhere makes the whole x.
  auto Multiply() -> mir::CallableCode {
    return BodyOf(
        IntegralOp::kMultiply, [&](mir::CallableCode& code, mir::Builder block,
                                   const std::vector<mir::LocalId>& p) {
          const PlaneWords a = Operand(code, block, p[0], WholeType());
          const PlaneWords b = Operand(code, block, p[1], WholeType());
          const mir::LocalId answer = Answer(code, block);
          const PlaneWords out = AnswerView(code, block, answer);
          const auto n = static_cast<std::int64_t>(out.words);
          ForEachIndex(
              code, block, CountLiteral(block, out.words),
              [&](mir::Builder row, mir::LocalId i) {
                const mir::LocalId carry = Hold(code, row, WordLiteral(row, 0));
                const mir::LocalId factor =
                    Hold(code, row, ValueWord(row, a, AtLocal(i)));
                ForEachIndex(
                    code, row,
                    IndexDifference(
                        row, IndexLiteral(row, n), row.ReadLocal(i, Machine())),
                    [&](mir::Builder cell, mir::LocalId j) {
                      const mir::LocalId k = HoldIndex(
                          code, cell,
                          IndexSum(
                              cell, cell.ReadLocal(i, Machine()),
                              cell.ReadLocal(j, Machine())));
                      const mir::LocalId product = HoldWide(
                          code, cell,
                          WideOp(
                              cell, mir::BinaryOp::kAdd,
                              WideOp(
                                  cell, mir::BinaryOp::kAdd,
                                  WideOp(
                                      cell, mir::BinaryOp::kMul,
                                      Widened(cell, Now(cell, factor)),
                                      Widened(
                                          cell,
                                          ValueWord(cell, b, AtLocal(j)))),
                                  Widened(
                                      cell, ValueWord(cell, out, AtLocal(k)))),
                              Widened(cell, Now(cell, carry))));
                      SetValueWord(
                          cell, out, AtLocal(k),
                          LowWord(cell, NowWide(cell, product)));
                      Set(cell, carry, HighWord(cell, NowWide(cell, product)));
                    });
              });
          ClearAboveWidth(block, out);
          if (out.four_state) {
            const std::array both{a, b};
            UnknownWhere(code, block, out, AnyUnknown(code, block, both));
          }
          return Answered(block, answer);
        });
  }

  // LRM 11.4.3: a quotient truncates toward zero, so it is negative where the
  // operands' signs differ, and a remainder takes the dividend's sign; a zero
  // divisor, or x or z anywhere, makes the whole x. The magnitudes are divided
  // by Knuth's algorithm D (The Art of Computer Programming, vol. 2, 4.3.1)
  // over 64-bit digits, as the library divides them.
  auto Division(IntegralOp op, bool remainder) -> mir::CallableCode {
    return BodyOf(
        op, [&](mir::CallableCode& code, mir::Builder block,
                const std::vector<mir::LocalId>& p) {
          const PlaneWords a = Operand(code, block, p[0], WholeType());
          const PlaneWords b = Operand(code, block, p[1], WholeType());
          const std::size_t words = a.words;
          const auto n = static_cast<std::int64_t>(words);
          const bool is_signed =
              Integral(WholeType()).signedness == mir::Signedness::kSigned;
          const std::uint64_t sign_bit = (a.width - 1U) % 64U;
          const auto sign_of = [&](const PlaneWords& view) {
            return is_signed ? And(block,
                                   Op(block, mir::BinaryOp::kShr,
                                      ValueWord(block, view, At(n - 1)),
                                      WordLiteral(block, sign_bit)),
                                   WordLiteral(block, 1))
                             : WordLiteral(block, 0);
          };
          const mir::LocalId dividend_negative = Hold(code, block, sign_of(a));
          const mir::LocalId divisor_negative = Hold(code, block, sign_of(b));
          // The magnitudes, a word to spare above each for the normalized
          // dividend's carry.
          const auto u_scratch = Scratch(code, block, words + 2U);
          const auto v_scratch = Scratch(code, block, words + 1U);
          const auto q_scratch = Scratch(code, block, words + 1U);
          const PlaneWords u = ScratchView(code, block, u_scratch);
          const PlaneWords v = ScratchView(code, block, v_scratch);
          const PlaneWords q = ScratchView(code, block, q_scratch);
          EachWord(code, block, words, [&](mir::Builder body, const Where& i) {
            SetValueWord(body, u, i, ValueWord(body, a, i));
            SetValueWord(body, v, i, ValueWord(body, b, i));
          });
          When(
              block,
              Holds(
                  block, mir::BinaryOp::kInequality,
                  Now(block, dividend_negative), WordLiteral(block, 0)),
              [&](mir::Builder then) {
                NegateWords(code, then, u, words, a.width);
              });
          When(
              block,
              Holds(
                  block, mir::BinaryOp::kInequality,
                  Now(block, divisor_negative), WordLiteral(block, 0)),
              [&](mir::Builder then) {
                NegateWords(code, then, v, words, a.width);
              });
          // How many words each magnitude has below its top zeros.
          const auto used = [&](const PlaneWords& view) {
            const mir::LocalId count =
                HoldIndex(code, block, IndexLiteral(block, 0));
            EachWord(
                code, block, words, [&](mir::Builder body, const Where& i) {
                  const mir::ExprId set = Holds(
                      body, mir::BinaryOp::kInequality,
                      ValueWord(body, view, i), WordLiteral(body, 0));
                  body.AppendStmt(
                      mir::ExprStmt{
                          .expr = body.Add(
                              mir::MakeAssignExpr(
                                  Unit().builtins,
                                  body.LocalPlace(count, Machine()),
                                  Chosen(
                                      body, set,
                                      IndexSum(
                                          body, i(body), IndexLiteral(body, 1)),
                                      body.ReadLocal(count, Machine()),
                                      Machine())))});
                });
            return count;
          };
          const mir::LocalId vn_used = used(v);
          const mir::LocalId un_used = used(u);
          const mir::ExprId single = mir::BuildMachineIntComparison(
              Unit(), block, mir::BinaryOp::kLessEqual,
              block.ReadLocal(vn_used, Machine()), IndexLiteral(block, 1));
          // The remainder, a word of the divisor's size, is left in `u`.
          Choose(
              block, single,
              [&](mir::Builder one) {
                const mir::LocalId divisor =
                    Hold(code, one, ValueWord(one, v, At(0)));
                // A zero divisor answers x, below; it divides by one here.
                const mir::LocalId by = HoldWide(
                    code, one,
                    Widened(
                        one, WordWhere(
                                 one,
                                 Holds(
                                     one, mir::BinaryOp::kEquality,
                                     Now(one, divisor), WordLiteral(one, 0)),
                                 WordLiteral(one, 1), Now(one, divisor))));
                const mir::LocalId carried =
                    HoldWide(code, one, WideLiteral(one, 0));
                ForEachIndex(
                    code, one, one.ReadLocal(un_used, Machine()),
                    [&](mir::Builder body, mir::LocalId k) {
                      const mir::LocalId j = HoldIndex(
                          code, body,
                          IndexDifference(
                              body,
                              IndexDifference(
                                  body, body.ReadLocal(un_used, Machine()),
                                  IndexLiteral(body, 1)),
                              body.ReadLocal(k, Machine())));
                      const mir::LocalId current = HoldWide(
                          code, body,
                          WideOp(
                              body, mir::BinaryOp::kBitwiseOr,
                              WideOp(
                                  body, mir::BinaryOp::kShl,
                                  NowWide(body, carried),
                                  WideLiteral(body, 64)),
                              Widened(body, ValueWord(body, u, AtLocal(j)))));
                      SetValueWord(
                          body, q, AtLocal(j),
                          LowWord(
                              body,
                              WideOp(
                                  body, mir::BinaryOp::kDiv,
                                  NowWide(body, current), NowWide(body, by))));
                      SetWide(
                          body, carried,
                          WideOp(
                              body, mir::BinaryOp::kMod, NowWide(body, current),
                              NowWide(body, by)));
                    });
                EachWord(
                    code, one, words + 2U,
                    [&](mir::Builder body, const Where& i) {
                      SetValueWord(body, u, i, WordLiteral(body, 0));
                    });
                SetValueWord(
                    one, u, At(0), LowWord(one, NowWide(one, carried)));
              },
              [&](mir::Builder many) {
                DivideLong(code, many, u, v, q, un_used, vn_used);
              });
          const mir::LocalId answer = Answer(code, block);
          const PlaneWords out = AnswerView(code, block, answer);
          const PlaneWords& magnitude = remainder ? u : q;
          EachWord(
              code, block, out.words, [&](mir::Builder body, const Where& i) {
                SetValueWord(body, out, i, ValueWord(body, magnitude, i));
              });
          const mir::ExprId negative =
              remainder ? Now(block, dividend_negative)
                        : Op(block, mir::BinaryOp::kBitwiseXor,
                             Now(block, dividend_negative),
                             Now(block, divisor_negative));
          When(
              block,
              Holds(
                  block, mir::BinaryOp::kInequality, negative,
                  WordLiteral(block, 0)),
              [&](mir::Builder then) {
                NegateWords(code, then, out, out.words, out.width);
              });
          ClearAboveWidth(block, out);
          // A zero divisor answers x, as an unknown operand does.
          const mir::LocalId zero_divisor = Hold(
              code, block,
              WordWhere(
                  block,
                  mir::BuildMachineIntComparison(
                      Unit(), block, mir::BinaryOp::kEquality,
                      block.ReadLocal(vn_used, Machine()),
                      IndexLiteral(block, 0)),
                  WordLiteral(block, 1), WordLiteral(block, 0)));
          if (out.four_state) {
            const std::array both{a, b};
            const mir::LocalId any = AnyUnknown(code, block, both);
            Set(block, any,
                Or(block, Now(block, any), Now(block, zero_divisor)));
            UnknownWhere(code, block, out, any);
          } else {
            UnknownWhere(code, block, out, zero_divisor);
          }
          return Answered(block, answer);
        });
  }

  // Knuth's algorithm D over the magnitudes `u` (`m` words used) and `v` (`n`
  // words used, at least two): the quotient into `q`, the remainder left in
  // `u`'s low words. Each quotient digit is estimated from the remainder's top
  // two digits and the divisor's top one, which normalizing both until the
  // divisor's top bit is set makes at most two too large, and corrected by
  // adding the divisor back where the estimate overshot.
  void DivideLong(
      mir::CallableCode& code, mir::Builder block, const PlaneWords& u,
      const PlaneWords& v, const PlaneWords& q, mir::LocalId m,
      mir::LocalId n) {
    const auto index = [&](mir::Builder b, mir::LocalId local) {
      return b.ReadLocal(local, Machine());
    };
    const mir::LocalId top = HoldIndex(
        code, block,
        IndexDifference(block, index(block, n), IndexLiteral(block, 1)));
    const mir::LocalId shift =
        LeadingZeros(code, block, ValueWord(block, v, AtLocal(top)));
    // The bits a word passes up to the next when shifted by `shift`.
    const auto passed_up = [&](mir::Builder b, mir::ExprId below) {
      return WordWhere(
          b,
          Holds(b, mir::BinaryOp::kEquality, Now(b, shift), WordLiteral(b, 0)),
          WordLiteral(b, 0),
          Op(b, mir::BinaryOp::kShr, below,
             Op(b, mir::BinaryOp::kSub, WordLiteral(b, 64), Now(b, shift))));
    };
    // Each normalized in place, from its top word down: `v`'s `n` words and
    // `u`'s `m` words and the one above them.
    const auto normalize = [&](const PlaneWords& view, mir::LocalId count,
                               bool carry_out) {
      if (carry_out) {
        const mir::LocalId at = HoldIndex(code, block, index(block, count));
        const mir::ExprId below = WordWhere(
            block,
            mir::BuildMachineIntComparison(
                Unit(), block, mir::BinaryOp::kGreaterThan, index(block, at),
                IndexLiteral(block, 0)),
            ValueWord(block, view, AtPlus(at, -1)), WordLiteral(block, 0));
        SetValueWord(block, view, AtLocal(at), passed_up(block, below));
      }
      ForEachIndex(
          code, block, index(block, count),
          [&](mir::Builder body, mir::LocalId k) {
            const mir::LocalId i = HoldIndex(
                code, body,
                IndexDifference(
                    body,
                    IndexDifference(
                        body, index(body, count), IndexLiteral(body, 1)),
                    index(body, k)));
            const mir::ExprId below = WordWhere(
                body,
                mir::BuildMachineIntComparison(
                    Unit(), body, mir::BinaryOp::kGreaterThan, index(body, i),
                    IndexLiteral(body, 0)),
                ValueWord(
                    body, view,
                    [this, i, &code](mir::Builder b) {
                      // Index 0 reads its own word where it has none below,
                      // which the condition passes over.
                      const mir::LocalId below_index = Declared(
                          code, b, Machine(),
                          IndexDifference(
                              b, b.ReadLocal(i, Machine()),
                              IndexLiteral(b, 1)));
                      return mir::BuildMachineIntMax(
                          Unit(), b, b.ReadLocal(below_index, Machine()),
                          IndexLiteral(b, 0));
                    }),
                WordLiteral(body, 0));
            SetValueWord(
                body, view, AtLocal(i),
                Or(body,
                   Op(body, mir::BinaryOp::kShl,
                      ValueWord(body, view, AtLocal(i)), Now(body, shift)),
                   passed_up(body, below)));
          });
    };
    normalize(v, n, false);
    normalize(u, m, true);
    const mir::LocalId vtop = HoldWide(
        code, block, Widened(block, ValueWord(block, v, AtLocal(top))));
    const mir::LocalId vnext = HoldWide(
        code, block, Widened(block, ValueWord(block, v, AtPlus(top, -1))));
    // One quotient digit per position from `m - n` down to 0.
    ForEachIndex(
        code, block,
        IndexSum(
            block, IndexDifference(block, index(block, m), index(block, n)),
            IndexLiteral(block, 1)),
        [&](mir::Builder body, mir::LocalId k) {
          const mir::LocalId j = HoldIndex(
              code, body,
              IndexDifference(
                  body, IndexDifference(body, index(body, m), index(body, n)),
                  index(body, k)));
          const mir::LocalId jn = HoldIndex(
              code, body, IndexSum(body, index(body, j), index(body, n)));
          const mir::LocalId estimate = HoldWide(
              code, body,
              WideOp(
                  body, mir::BinaryOp::kBitwiseOr,
                  WideOp(
                      body, mir::BinaryOp::kShl,
                      Widened(body, ValueWord(body, u, AtLocal(jn))),
                      WideLiteral(body, 64)),
                  Widened(body, ValueWord(body, u, AtPlus(jn, -1)))));
          const mir::LocalId rest = HoldWide(
              code, body,
              WideOp(
                  body, mir::BinaryOp::kMod, NowWide(body, estimate),
                  NowWide(body, vtop)));
          SetWide(
              body, estimate,
              WideOp(
                  body, mir::BinaryOp::kDiv, NowWide(body, estimate),
                  NowWide(body, vtop)));
          // At most two corrections, each where the estimate is too large.
          for (int correction = 0; correction < 2; ++correction) {
            const mir::ExprId over_word = WideOp(
                body, mir::BinaryOp::kShr, NowWide(body, estimate),
                WideLiteral(body, 64));
            const mir::ExprId rest_fits = Holds(
                body, mir::BinaryOp::kEquality,
                LowWord(
                    body, WideOp(
                              body, mir::BinaryOp::kShr, NowWide(body, rest),
                              WideLiteral(body, 64))),
                WordLiteral(body, 0));
            const mir::ExprId too_large = body.Add(
                mir::MakeBinary(
                    Unit(), body.Body(), mir::BinaryOp::kGreaterThan,
                    WideOp(
                        body, mir::BinaryOp::kMul, NowWide(body, estimate),
                        NowWide(body, vnext)),
                    WideOp(
                        body, mir::BinaryOp::kBitwiseOr,
                        WideOp(
                            body, mir::BinaryOp::kShl, NowWide(body, rest),
                            WideLiteral(body, 64)),
                        Widened(body, ValueWord(body, u, AtPlus(jn, -2)))),
                    Unit().builtins.machine_bool));
            const mir::ExprId corrects = Holds(
                body, mir::BinaryOp::kInequality,
                And(body, WordOf(body, rest_fits),
                    Or(body,
                       WordOf(
                           body,
                           Holds(
                               body, mir::BinaryOp::kInequality,
                               LowWord(body, over_word), WordLiteral(body, 0))),
                       WordOf(body, too_large))),
                WordLiteral(body, 0));
            When(body, corrects, [&](mir::Builder then) {
              SetWide(
                  then, estimate,
                  WideOp(
                      then, mir::BinaryOp::kSub, NowWide(then, estimate),
                      WideLiteral(then, 1)));
              SetWide(
                  then, rest,
                  WideOp(
                      then, mir::BinaryOp::kAdd, NowWide(then, rest),
                      NowWide(then, vtop)));
            });
          }
          // The divisor times the digit taken from the remainder's words.
          const mir::LocalId carry = Hold(code, body, WordLiteral(body, 0));
          const mir::LocalId borrow = Hold(code, body, WordLiteral(body, 0));
          ForEachIndex(
              code, body, index(body, n),
              [&](mir::Builder step, mir::LocalId i) {
                const mir::LocalId at = HoldIndex(
                    code, step, IndexSum(step, index(step, i), index(step, j)));
                const mir::LocalId product = HoldWide(
                    code, step,
                    WideOp(
                        step, mir::BinaryOp::kAdd,
                        WideOp(
                            step, mir::BinaryOp::kMul, NowWide(step, estimate),
                            Widened(step, ValueWord(step, v, AtLocal(i)))),
                        Widened(step, Now(step, carry))));
                Set(step, carry, HighWord(step, NowWide(step, product)));
                const mir::LocalId low =
                    Hold(code, step, LowWord(step, NowWide(step, product)));
                const mir::LocalId was =
                    Hold(code, step, ValueWord(step, u, AtLocal(at)));
                const mir::LocalId d1 = Hold(
                    code, step,
                    Op(step, mir::BinaryOp::kSub, Now(step, was),
                       Now(step, low)));
                SetValueWord(
                    step, u, AtLocal(at),
                    Op(step, mir::BinaryOp::kSub, Now(step, d1),
                       Now(step, borrow)));
                Set(step, borrow,
                    Op(step, mir::BinaryOp::kAdd,
                       WordOf(
                           step, Holds(
                                     step, mir::BinaryOp::kLessThan,
                                     Now(step, was), Now(step, low))),
                       WordOf(
                           step, Holds(
                                     step, mir::BinaryOp::kLessThan,
                                     Now(step, d1), Now(step, borrow)))));
              });
          const mir::LocalId owed = HoldWide(
              code, body,
              WideOp(
                  body, mir::BinaryOp::kAdd, Widened(body, Now(body, carry)),
                  Widened(body, Now(body, borrow))));
          const mir::LocalId was_top = HoldWide(
              code, body, Widened(body, ValueWord(body, u, AtLocal(jn))));
          const mir::LocalId overshot = Declared(
              code, body, Unit().builtins.machine_bool,
              body.Add(
                  mir::MakeBinary(
                      Unit(), body.Body(), mir::BinaryOp::kLessThan,
                      NowWide(body, was_top), NowWide(body, owed),
                      Unit().builtins.machine_bool)));
          SetValueWord(
              body, u, AtLocal(jn),
              LowWord(
                  body, WideOp(
                            body, mir::BinaryOp::kSub, NowWide(body, was_top),
                            NowWide(body, owed))));
          SetValueWord(
              body, q, AtLocal(j), LowWord(body, NowWide(body, estimate)));
          When(
              body, body.ReadLocal(overshot, Unit().builtins.machine_bool),
              [&](mir::Builder back) {
                SetValueWord(
                    back, q, AtLocal(j),
                    Op(back, mir::BinaryOp::kSub,
                       ValueWord(back, q, AtLocal(j)), WordLiteral(back, 1)));
                const mir::LocalId added =
                    Hold(code, back, WordLiteral(back, 0));
                ForEachIndex(
                    code, back, index(back, n),
                    [&](mir::Builder step, mir::LocalId i) {
                      const mir::LocalId at = HoldIndex(
                          code, step,
                          IndexSum(step, index(step, i), index(step, j)));
                      const mir::LocalId sum = HoldWide(
                          code, step,
                          WideOp(
                              step, mir::BinaryOp::kAdd,
                              WideOp(
                                  step, mir::BinaryOp::kAdd,
                                  Widened(
                                      step, ValueWord(step, u, AtLocal(at))),
                                  Widened(
                                      step, ValueWord(step, v, AtLocal(i)))),
                              Widened(step, Now(step, added))));
                      SetValueWord(
                          step, u, AtLocal(at),
                          LowWord(step, NowWide(step, sum)));
                      Set(step, added, HighWord(step, NowWide(step, sum)));
                    });
                SetValueWord(
                    back, u, AtLocal(jn),
                    Op(back, mir::BinaryOp::kAdd,
                       ValueWord(back, u, AtLocal(jn)), Now(back, added)));
              });
        });
    // The remainder, its normalization undone, in `u`'s low `n` words, every
    // word above them clear.
    ForEachIndex(
        code, block, index(block, n), [&](mir::Builder body, mir::LocalId i) {
          const mir::ExprId above = ValueWord(body, u, AtPlus(i, 1));
          SetValueWord(
              body, u, AtLocal(i),
              Or(body,
                 Op(body, mir::BinaryOp::kShr, ValueWord(body, u, AtLocal(i)),
                    Now(body, shift)),
                 WordWhere(
                     body,
                     Holds(
                         body, mir::BinaryOp::kEquality, Now(body, shift),
                         WordLiteral(body, 0)),
                     WordLiteral(body, 0),
                     Op(body, mir::BinaryOp::kShl, above,
                        Op(body, mir::BinaryOp::kSub, WordLiteral(body, 64),
                           Now(body, shift))))));
        });
    ForEachIndexBetween(
        code, block, index(block, n), CountLiteral(block, u.words),
        [&](mir::Builder body, mir::LocalId i) {
          SetValueWord(body, u, AtLocal(i), WordLiteral(body, 0));
        });
  }

  // LRM 11.4.3 power: x or z in either operand makes the whole x; a negative
  // exponent is a reciprocal, 1 for a base of 1, +-1 for -1 as the exponent is
  // even or odd, x for 0 and 0 for any other (Table 11-4); and any other
  // exponent multiplies the base that many times, by squaring, each product
  // this type's own.
  auto Power(const mir::TypeOperationSource& source) -> mir::CallableCode {
    const mir::TypeId exponent_type = source.arguments.front();
    return Defined(
        Declaration(source), [&](mir::CallableCode& code, mir::Builder block,
                                 const std::vector<mir::LocalId>& p) {
          const PlaneWords base = Operand(code, block, p[0], WholeType());
          const PlaneWords exponent = Operand(code, block, p[1], exponent_type);
          const mir::LocalId answer =
              Declared(code, block, WholeType(), FromNumber(block, 1));
          const mir::LocalId factor = Declared(
              code, block, WholeType(),
              block.Read(Referent(block, p[0], WholeType())));
          const auto multiplied = [&](mir::Builder b, mir::LocalId by) {
            return Own(
                b, IntegralOp::kMultiply,
                {b.LocalPlace(answer, WholeType()),
                 b.LocalPlace(by, WholeType())},
                WholeType());
          };
          const auto squared = [&](mir::Builder b) {
            return Own(
                b, IntegralOp::kMultiply,
                {b.LocalPlace(factor, WholeType()),
                 b.LocalPlace(factor, WholeType())},
                WholeType());
          };
          // The exponent's bits past its highest 1 multiply nothing.
          const mir::LocalId length = BitLength(code, block, exponent);
          ForEachIndex(
              code, block, CastTo(block, Now(block, length), Machine()),
              [&](mir::Builder body, mir::LocalId at) {
                const mir::LocalId bit = Hold(
                    code, body,
                    And(body,
                        Op(body, mir::BinaryOp::kShr,
                           ValueWord(
                               body, exponent,
                               [this, at](mir::Builder b) {
                                 return mir::BuildMachineIntBinary(
                                     Unit(), b, mir::BinaryOp::kDiv,
                                     b.ReadLocal(at, Machine()),
                                     IndexLiteral(b, 64));
                               }),
                           And(body,
                               CastTo(
                                   body, body.ReadLocal(at, Machine()), Word()),
                               WordLiteral(body, 63))),
                        WordLiteral(body, 1)));
                When(
                    body,
                    Holds(
                        body, mir::BinaryOp::kInequality, Now(body, bit),
                        WordLiteral(body, 0)),
                    [&](mir::Builder then) {
                      Statement(
                          then, then.Add(
                                    mir::MakeAssignExpr(
                                        Unit().builtins,
                                        then.LocalPlace(answer, WholeType()),
                                        multiplied(then, factor))));
                    });
                Statement(
                    body, body.Add(
                              mir::MakeAssignExpr(
                                  Unit().builtins,
                                  body.LocalPlace(factor, WholeType()),
                                  squared(body))));
              });
          const PlaneWords out = AnswerView(code, block, answer);
          // A negative exponent: 1 for a base of 1, and for -1 the sign the
          // exponent's evenness gives; x for 0; 0 otherwise.
          if (Integral(exponent_type).signedness == mir::Signedness::kSigned) {
            const std::uint64_t top_bit = (exponent.width - 1U) % 64U;
            const mir::ExprId negative = Holds(
                block, mir::BinaryOp::kInequality,
                And(block,
                    Op(block, mir::BinaryOp::kShr,
                       ValueWord(
                           block, exponent,
                           At(static_cast<std::int64_t>(exponent.words - 1U))),
                       WordLiteral(block, top_bit)),
                    WordLiteral(block, 1)),
                WordLiteral(block, 0));
            When(block, negative, [&](mir::Builder then) {
              ReciprocalPower(code, then, base, exponent, answer);
            });
          }
          if (out.four_state) {
            const std::array both{base, exponent};
            UnknownWhere(code, block, out, AnyUnknown(code, block, both));
          }
          block.AppendStmt(mir::ReturnStmt{.value = Answered(block, answer)});
        });
  }

  // This type's value of the machine number `number`, its sign filling every
  // word.
  auto FromNumber(mir::Builder block, std::int64_t number) -> mir::ExprId {
    const std::size_t words =
        value::WordCountForBits(Integral(WholeType()).bit_width);
    std::vector<std::uint64_t> value_words(
        words, number < 0 ? ~std::uint64_t{0} : 0U);
    value_words.front() = static_cast<std::uint64_t>(number);
    return mir::BuildIntegralLiteral(
        Unit(), block, WholeType(),
        mir::IntegralConstant{
            .value_words = std::move(value_words), .state_words = {}});
  }

  // LRM 11.4.3 Table 11-4, a negative exponent: 1 for a base of 1, +-1 for a
  // signed base of -1 as the exponent is even or odd, x for 0, 0 otherwise.
  void ReciprocalPower(
      mir::CallableCode& code, mir::Builder block, const PlaneWords& base,
      const PlaneWords& exponent, mir::LocalId answer) {
    const bool is_signed =
        Integral(WholeType()).signedness == mir::Signedness::kSigned;
    const mir::LocalId zero = Hold(code, block, WordLiteral(block, 0));
    const mir::LocalId ones = Hold(code, block, WordLiteral(block, 0));
    const std::uint64_t top = value::ValidBitsMask(base.words - 1U, base.width);
    const auto last = static_cast<std::int64_t>(base.words - 1U);
    EachWord(code, block, base.words, [&](mir::Builder body, const Where& i) {
      const mir::LocalId w = Hold(code, body, ValueWord(body, base, i));
      Set(body, zero, Or(body, Now(body, zero), Now(body, w)));
      const mir::ExprId full = WordWhere(
          body,
          mir::BuildMachineIntComparison(
              Unit(), body, mir::BinaryOp::kEquality, i(body),
              IndexLiteral(body, last)),
          WordLiteral(body, top), WordLiteral(body, ~std::uint64_t{0}));
      Set(body, ones,
          Or(body, Now(body, ones),
             Op(body, mir::BinaryOp::kBitwiseXor, Now(body, w), full)));
    });
    // Whether the base is 1: its low word 1 and every other 0.
    const mir::LocalId high = Hold(code, block, WordLiteral(block, 0));
    ForEachIndexBetween(
        code, block, IndexLiteral(block, 1), CountLiteral(block, base.words),
        [&](mir::Builder body, mir::LocalId at) {
          Set(body, high,
              Or(body, Now(body, high), ValueWord(body, base, AtLocal(at))));
        });
    const mir::ExprId is_one = Holds(
        block, mir::BinaryOp::kEquality,
        Or(block, Now(block, high),
           Op(block, mir::BinaryOp::kBitwiseXor, ValueWord(block, base, At(0)),
              WordLiteral(block, 1))),
        WordLiteral(block, 0));
    const mir::ExprId is_minus_one =
        is_signed ? Holds(
                        block, mir::BinaryOp::kEquality, Now(block, ones),
                        WordLiteral(block, 0))
                  : Holds(
                        block, mir::BinaryOp::kInequality,
                        WordLiteral(block, 0), WordLiteral(block, 0));
    const mir::ExprId odd = Holds(
        block, mir::BinaryOp::kInequality,
        And(block, ValueWord(block, exponent, At(0)), WordLiteral(block, 1)),
        WordLiteral(block, 0));
    const mir::TypeId type = WholeType();
    // x, which a type holding no x holds as 0.
    const std::size_t words = value::WordCountForBits(base.width);
    const bool holds_x =
        Integral(type).state_kind == mir::IntegralStateKind::kFourState;
    const mir::ExprId x = mir::BuildIntegralLiteral(
        Unit(), block, type,
        mir::IntegralConstant{
            .value_words = std::vector(words, holds_x ? ~std::uint64_t{0} : 0U),
            .state_words = holds_x ? std::vector(words, ~std::uint64_t{0})
                                   : std::vector<std::uint64_t>{}});
    const mir::ExprId chosen = Chosen(
        block,
        Holds(
            block, mir::BinaryOp::kEquality, Now(block, zero),
            WordLiteral(block, 0)),
        x,
        Chosen(
            block, is_one, FromNumber(block, 1),
            Chosen(
                block, is_minus_one,
                Chosen(
                    block, odd, FromNumber(block, -1), FromNumber(block, 1),
                    type),
                FromNumber(block, 0), type),
            type),
        type);
    Statement(
        block,
        block.Add(
            mir::MakeAssignExpr(
                Unit().builtins, block.LocalPlace(answer, type), chosen)));
  }

  // How many bits of `word` are set, by summing them in ever wider fields.
  auto PopCount(mir::CallableCode& code, mir::Builder block, mir::ExprId word)
      -> mir::ExprId {
    const mir::LocalId x = Hold(code, block, word);
    Set(block, x,
        Op(block, mir::BinaryOp::kSub, Now(block, x),
           And(block,
               Op(block, mir::BinaryOp::kShr, Now(block, x),
                  WordLiteral(block, 1)),
               WordLiteral(block, 0x5555'5555'5555'5555U))));
    Set(block, x,
        Op(block, mir::BinaryOp::kAdd,
           And(block, Now(block, x),
               WordLiteral(block, 0x3333'3333'3333'3333U)),
           And(block,
               Op(block, mir::BinaryOp::kShr, Now(block, x),
                  WordLiteral(block, 2)),
               WordLiteral(block, 0x3333'3333'3333'3333U))));
    Set(block, x,
        And(block,
            Op(block, mir::BinaryOp::kAdd, Now(block, x),
               Op(block, mir::BinaryOp::kShr, Now(block, x),
                  WordLiteral(block, 4))),
            WordLiteral(block, 0x0F0F'0F0F'0F0F'0F0FU)));
    return Op(
        block, mir::BinaryOp::kShr,
        Op(block, mir::BinaryOp::kMul, Now(block, x),
           WordLiteral(block, 0x0101'0101'0101'0101U)),
        WordLiteral(block, 56));
  }

  // A value of the integral type `type` holding the machine number the word
  // local `number` holds.
  auto NumberOf(mir::Builder block, mir::LocalId number, mir::TypeId type)
      -> mir::ExprId {
    return block.Add(
        mir::MakeBuiltinCall(
            Unit(), block.Body(), support::BuiltinFn::kIntegralFromInt,
            std::nullopt, {CastTo(block, Now(block, number), Machine())},
            type));
  }

  // LRM 20.9 `$countbits`: how many positions hold one of the scalars the
  // control bits name, a scalar several of them name counting once.
  auto CountBits(const mir::TypeOperationSource& source) -> mir::CallableCode {
    const mir::TypeId control_type = source.arguments.front();
    return Defined(
        Declaration(source), [&](mir::CallableCode& code, mir::Builder block,
                                 const std::vector<mir::LocalId>& p) {
          const PlaneWords a = Operand(code, block, p[0], WholeType());
          const PlaneWords control = Operand(code, block, p[1], control_type);
          // Each scalar the control names, as a word all of whose bits are
          // set where it is named.
          const mir::LocalId zero = Hold(code, block, WordLiteral(block, 0));
          const mir::LocalId one = Hold(code, block, WordLiteral(block, 0));
          const mir::LocalId high = Hold(code, block, WordLiteral(block, 0));
          const mir::LocalId unknown = Hold(code, block, WordLiteral(block, 0));
          EachWord(
              code, block, control.words,
              [&](mir::Builder body, const Where& i) {
                const mir::LocalId valid = Hold(
                    code, body,
                    ValidBits(code, body, i, WordLiteral(body, control.width)));
                const mir::LocalId v =
                    Hold(code, body, ValueWord(body, control, i));
                const mir::LocalId u =
                    Hold(code, body, UnknownWord(body, control, i));
                const auto named = [&](mir::LocalId flag, mir::ExprId where) {
                  Set(body, flag, Or(body, Now(body, flag), where));
                };
                named(
                    zero, And(body,
                              And(body, Not(body, Now(body, v)),
                                  Not(body, Now(body, u))),
                              Now(body, valid)));
                named(one, And(body, Now(body, v), Not(body, Now(body, u))));
                named(high, And(body, Not(body, Now(body, v)), Now(body, u)));
                named(unknown, And(body, Now(body, v), Now(body, u)));
              });
          const auto every_bit = [&](mir::LocalId flag) {
            return Hold(
                code, block,
                WordWhere(
                    block,
                    Holds(
                        block, mir::BinaryOp::kInequality, Now(block, flag),
                        WordLiteral(block, 0)),
                    WordLiteral(block, ~std::uint64_t{0}),
                    WordLiteral(block, 0)));
          };
          const mir::LocalId counts_zero = every_bit(zero);
          const mir::LocalId counts_one = every_bit(one);
          const mir::LocalId counts_high = every_bit(high);
          const mir::LocalId counts_unknown = every_bit(unknown);
          const mir::LocalId count = Hold(code, block, WordLiteral(block, 0));
          EachWord(
              code, block, a.words, [&](mir::Builder body, const Where& i) {
                const mir::LocalId valid = Hold(
                    code, body,
                    ValidBits(code, body, i, WordLiteral(body, a.width)));
                const mir::LocalId v = Hold(code, body, ValueWord(body, a, i));
                const mir::LocalId u =
                    Hold(code, body, UnknownWord(body, a, i));
                const mir::ExprId counted = Or(
                    body,
                    Or(body,
                       And(body, Now(body, counts_zero),
                           And(body,
                               And(body, Not(body, Now(body, v)),
                                   Not(body, Now(body, u))),
                               Now(body, valid))),
                       And(body, Now(body, counts_one),
                           And(body, Now(body, v), Not(body, Now(body, u))))),
                    Or(body,
                       And(body, Now(body, counts_high),
                           And(body, Not(body, Now(body, v)), Now(body, u))),
                       And(body, Now(body, counts_unknown),
                           And(body, Now(body, v), Now(body, u)))));
                Set(body, count,
                    Op(body, mir::BinaryOp::kAdd, Now(body, count),
                       PopCount(code, body, counted)));
              });
          block.AppendStmt(
              mir::ReturnStmt{
                  .value = NumberOf(block, count, code.result_type)});
        });
  }

  // LRM 20.8.1 `$clog2`: the operand read as unsigned, 0 answering 0, an x or
  // z bit reading 0 -- the index of its highest set bit, one more where any
  // other is set.
  auto CeilLog2() -> mir::CallableCode {
    return BodyOf(
        IntegralOp::kCeilLog2, [&](mir::CallableCode& code, mir::Builder block,
                                   const std::vector<mir::LocalId>& p) {
          const PlaneWords a = Operand(code, block, p[0], WholeType());
          const mir::LocalId set = Hold(code, block, WordLiteral(block, 0));
          EachWord(
              code, block, a.words, [&](mir::Builder body, const Where& i) {
                Set(body, set,
                    Op(body, mir::BinaryOp::kAdd, Now(body, set),
                       PopCount(code, body, KnownOnes(code, body, a, i))));
              });
          const mir::LocalId above = BitLength(code, block, a);
          // The highest set bit's index is `above - 1`; one more where another
          // is set.
          const mir::LocalId answer = Hold(
              code, block,
              WordWhere(
                  block,
                  Holds(
                      block, mir::BinaryOp::kGreaterThan, Now(block, set),
                      WordLiteral(block, 1)),
                  Now(block, above),
                  WordWhere(
                      block,
                      Holds(
                          block, mir::BinaryOp::kEquality, Now(block, above),
                          WordLiteral(block, 0)),
                      WordLiteral(block, 0),
                      Op(block, mir::BinaryOp::kSub, Now(block, above),
                         WordLiteral(block, 1)))));
          return NumberOf(block, answer, code.result_type);
        });
  }

  // LRM 11.4.14.2: the value's blocks of `block` positions in reversed order,
  // counted from its least significant bit, the bits inside each left where
  // they are; the last block is what the width leaves over. Where the block
  // divides 64 and the width, each word of the answer is a run of 64 of the
  // value's positions with its blocks reversed by halving swaps; otherwise each
  // block is moved whole.
  auto ReverseBlocks() -> mir::CallableCode {
    return BodyOf(
        IntegralOp::kReverseBlocks,
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const PlaneWords a = Operand(code, block, p[0], WholeType());
          const mir::LocalId size = Hold(
              code, block,
              CastTo(block, block.ReadLocal(p[1], Machine()), Word()));
          const mir::LocalId answer = Answer(code, block);
          const PlaneWords out = AnswerView(code, block, answer);
          const auto divides = [&](std::uint64_t whole) {
            return WordOf(
                block, Holds(
                           block, mir::BinaryOp::kEquality,
                           Op(block, mir::BinaryOp::kMod,
                              WordLiteral(block, whole), Now(block, size)),
                           WordLiteral(block, 0)));
          };
          const mir::ExprId by_words = Holds(
              block, mir::BinaryOp::kInequality,
              And(block, divides(64), divides(out.width)),
              WordLiteral(block, 0));
          Choose(
              block, by_words,
              [&](mir::Builder words) {
                EachWord(
                    code, words, out.words,
                    [&](mir::Builder body, const Where& k) {
                      const mir::LocalId low = Hold(
                          code, body,
                          Op(body, mir::BinaryOp::kMul,
                             CastTo(body, k(body), Word()),
                             WordLiteral(body, 64)));
                      const mir::LocalId count = Hold(
                          code, body,
                          WordWhere(
                              body,
                              Holds(
                                  body, mir::BinaryOp::kLessThan,
                                  Op(body, mir::BinaryOp::kSub,
                                     WordLiteral(body, out.width),
                                     Now(body, low)),
                                  WordLiteral(body, 64)),
                              Op(body, mir::BinaryOp::kSub,
                                 WordLiteral(body, out.width), Now(body, low)),
                              WordLiteral(body, 64)));
                      const mir::LocalId from = Hold(
                          code, body,
                          Op(body, mir::BinaryOp::kSub,
                             Op(body, mir::BinaryOp::kSub,
                                WordLiteral(body, out.width), Now(body, low)),
                             Now(body, count)));
                      for (const Plane plane :
                           {Plane::kValue, Plane::kUnknown}) {
                        const mir::LocalId word = Hold(
                            code, body,
                            And(body,
                                BitsFrom(code, body, a, Now(body, from), plane),
                                LowBitsOf(code, body, Now(body, count))));
                        for (const auto& [half, low_half] : std::array<
                                 std::pair<std::uint64_t, std::uint64_t>, 6>{
                                 {{32U, 0x0000'0000'FFFF'FFFFU},
                                  {16U, 0x0000'FFFF'0000'FFFFU},
                                  {8U, 0x00FF'00FF'00FF'00FFU},
                                  {4U, 0x0F0F'0F0F'0F0F'0F0FU},
                                  {2U, 0x3333'3333'3333'3333U},
                                  {1U, 0x5555'5555'5555'5555U}}}) {
                          const mir::ExprId swapped = Or(
                              body,
                              And(body,
                                  Op(body, mir::BinaryOp::kShr, Now(body, word),
                                     WordLiteral(body, half)),
                                  WordLiteral(body, low_half)),
                              Op(body, mir::BinaryOp::kShl,
                                 And(body, Now(body, word),
                                     WordLiteral(body, low_half)),
                                 WordLiteral(body, half)));
                          Set(body, word,
                              WordWhere(
                                  body,
                                  Holds(
                                      body, mir::BinaryOp::kGreaterEqual,
                                      WordLiteral(body, half), Now(body, size)),
                                  swapped, Now(body, word)));
                        }
                        SetPlaneWord(
                            body, out, k, plane,
                            Op(body, mir::BinaryOp::kShr, Now(body, word),
                               Op(body, mir::BinaryOp::kSub,
                                  WordLiteral(body, 64), Now(body, count))));
                      }
                    });
              },
              [&](mir::Builder blocks) {
                // Each block moved whole, 64 positions at a time.
                const mir::LocalId low =
                    Hold(code, blocks, WordLiteral(blocks, 0));
                mir::Block step;
                mir::Builder body = blocks.Into(step);
                const mir::LocalId count = Hold(
                    code, body,
                    WordWhere(
                        body,
                        Holds(
                            body, mir::BinaryOp::kLessThan,
                            Op(body, mir::BinaryOp::kSub,
                               WordLiteral(body, out.width), Now(body, low)),
                            Now(body, size)),
                        Op(body, mir::BinaryOp::kSub,
                           WordLiteral(body, out.width), Now(body, low)),
                        Now(body, size)));
                const mir::LocalId to = Hold(
                    code, body,
                    Op(body, mir::BinaryOp::kSub,
                       Op(body, mir::BinaryOp::kSub,
                          WordLiteral(body, out.width), Now(body, low)),
                       Now(body, count)));
                const mir::LocalId moved =
                    Hold(code, body, WordLiteral(body, 0));
                mir::Block chunk;
                mir::Builder inner = body.Into(chunk);
                const mir::LocalId run = Hold(
                    code, inner,
                    WordWhere(
                        inner,
                        Holds(
                            inner, mir::BinaryOp::kLessThan,
                            Op(inner, mir::BinaryOp::kSub, Now(inner, count),
                               Now(inner, moved)),
                            WordLiteral(inner, 64)),
                        Op(inner, mir::BinaryOp::kSub, Now(inner, count),
                           Now(inner, moved)),
                        WordLiteral(inner, 64)));
                const mir::LocalId at = Hold(
                    code, inner,
                    Op(inner, mir::BinaryOp::kAdd, Now(inner, to),
                       Now(inner, moved)));
                for (const Plane plane : {Plane::kValue, Plane::kUnknown}) {
                  OrBitsAt(
                      code, inner, out, at,
                      And(inner,
                          BitsFrom(
                              code, inner, a,
                              Op(inner, mir::BinaryOp::kAdd, Now(inner, low),
                                 Now(inner, moved)),
                              plane),
                          LowBitsOf(code, inner, Now(inner, run))),
                      plane);
                }
                Set(inner, moved,
                    Op(inner, mir::BinaryOp::kAdd, Now(inner, moved),
                       Now(inner, run)));
                body.AppendStmt(
                    mir::WhileStmt{
                        .condition = Holds(
                            body, mir::BinaryOp::kLessThan, Now(body, moved),
                            Now(body, count)),
                        .scope = body.AddScope(std::move(chunk))});
                Set(body, low,
                    Op(body, mir::BinaryOp::kAdd, Now(body, low),
                       Now(body, count)));
                blocks.AppendStmt(
                    mir::WhileStmt{
                        .condition = Holds(
                            blocks, mir::BinaryOp::kLessThan, Now(blocks, low),
                            WordLiteral(blocks, out.width)),
                        .scope = blocks.AddScope(std::move(step))});
              });
          ClearAboveWidth(block, out);
          return Answered(block, answer);
        });
  }

  // A value whose positions from `start`, `count` of them, are `from`'s and
  // every other `into`'s (LRM 7.6), a run of both machine numbers.
  auto TakePositions() -> mir::CallableCode {
    return BodyOf(
        IntegralOp::kTakePositions,
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const PlaneWords into = Operand(code, block, p[0], WholeType());
          const PlaneWords from = Operand(code, block, p[1], WholeType());
          const mir::LocalId start = Hold(
              code, block,
              CastTo(block, block.ReadLocal(p[2], Machine()), Word()));
          const mir::LocalId end = Hold(
              code, block,
              Op(block, mir::BinaryOp::kAdd, Now(block, start),
                 CastTo(block, block.ReadLocal(p[3], Machine()), Word())));
          const mir::LocalId answer = Answer(code, block);
          const PlaneWords out = AnswerView(code, block, answer);
          EachWord(
              code, block, out.words, [&](mir::Builder body, const Where& i) {
                const mir::LocalId taken = Hold(
                    code, body,
                    And(body, ValidBits(code, body, i, Now(body, end)),
                        Not(body, ValidBits(code, body, i, Now(body, start)))));
                for (const Plane plane : {Plane::kValue, Plane::kUnknown}) {
                  SetPlaneWord(
                      body, out, i, plane,
                      Or(body,
                         And(body, PlaneWord(body, into, i, plane),
                             Not(body, Now(body, taken))),
                         And(body, PlaneWord(body, from, i, plane),
                             Now(body, taken))));
                }
              });
          return Answered(block, answer);
        });
  }

  // The positions of each word where the operands' planes disagree, among
  // those `compared` keeps, accumulated; a match where none do.
  template <typename Compared, typename Mismatched>
  auto MatchBody(
      IntegralOp op, Matched matched, Compared compared, Mismatched mismatched)
      -> mir::CallableCode {
    return BodyOf(
        op, [&](mir::CallableCode& code, mir::Builder block,
                const std::vector<mir::LocalId>& p) {
          const PlaneWords a = Operand(code, block, p[0], WholeType());
          const PlaneWords b = Operand(code, block, p[1], WholeType());
          const mir::LocalId differing =
              Hold(code, block, WordLiteral(block, 0));
          const mir::LocalId unknown = Hold(code, block, WordLiteral(block, 0));
          EachWord(
              code, block, a.words, [&](mir::Builder body, const Where& i) {
                const Bits x{
                    .av = Hold(code, body, ValueWord(body, a, i)),
                    .au = Hold(code, body, UnknownWord(body, a, i)),
                    .bv = Hold(code, body, ValueWord(body, b, i)),
                    .bu = Hold(code, body, UnknownWord(body, b, i))};
                const mir::LocalId kept = Hold(code, body, compared(body, x));
                Set(body, differing,
                    Or(body, Now(body, differing),
                       And(body, mismatched(body, x), Now(body, kept))));
                Set(body, unknown,
                    Or(body, Now(body, unknown),
                       And(body, Now(body, x.au), Now(body, kept))));
              });
          const mir::TypeId answer_type = code.result_type;
          const mir::ExprId agree = Holds(
              block, mir::BinaryOp::kEquality, Now(block, differing),
              WordLiteral(block, 0));
          return Chosen(
              block, agree, MatchedAnswer(block, matched, unknown, answer_type),
              Scalar(block, answer_type, value::FourStateBit::kZero),
              answer_type);
        });
  }
  // What a match answers where nothing disagrees: a wildcard equality is x
  // where the first operand holds x or z at a compared position (LRM 11.4.6);
  // a case match is a match.
  auto MatchedAnswer(
      mir::Builder block, Matched matched, mir::LocalId unknown,
      mir::TypeId answer_type) -> mir::ExprId {
    switch (matched) {
      case Matched::kUnknownWhereTheFirstIs:
        return Chosen(
            block,
            Holds(
                block, mir::BinaryOp::kInequality, Now(block, unknown),
                WordLiteral(block, 0)),
            Scalar(block, answer_type, value::FourStateBit::kUnknown),
            Scalar(block, answer_type, value::FourStateBit::kOne), answer_type);
      case Matched::kExactly:
        return Scalar(block, answer_type, value::FourStateBit::kOne);
    }
    throw InternalError("hir_to_mir: unknown match");
  }

  // A word every bit of which is bit `bit` of `word`.
  auto Repeated(mir::Builder block, mir::ExprId word, std::uint64_t bit)
      -> mir::ExprId {
    return Op(
        block, mir::BinaryOp::kSub, WordLiteral(block, 0),
        And(block,
            Op(block, mir::BinaryOp::kShr, word, WordLiteral(block, bit)),
            WordLiteral(block, 1)));
  }

  // LRM 6.24.1, 11.8.2: a value carried into this type keeps the bits both
  // types have, and the bits this one has above the source's are 0, or the
  // source's sign where the source is signed -- an x or z sign filling with
  // itself; carried into a type holding no x or z, an x or z bit is 0.
  auto Convert(const mir::TypeOperationSource& source) -> mir::CallableCode {
    const mir::TypeId from = source.arguments.front();
    return Defined(
        Declaration(source), [&](mir::CallableCode& code, mir::Builder block,
                                 const std::vector<mir::LocalId>& p) {
          const PlaneWords src = Operand(code, block, p[0], from);
          const mir::LocalId answer = Answer(code, block);
          const PlaneWords out = AnswerView(code, block, answer);
          const bool extends =
              Integral(from).signedness == mir::Signedness::kSigned &&
              out.width > src.width;
          const auto top = static_cast<std::int64_t>(src.words - 1U);
          const std::uint64_t sign_bit = (src.width - 1U) % 64U;
          const auto fill_of = [&](Plane plane) {
            return extends ? Repeated(
                                 block, PlaneWord(block, src, At(top), plane),
                                 sign_bit)
                           : WordLiteral(block, 0);
          };
          const mir::LocalId value_above =
              Hold(code, block, fill_of(Plane::kValue));
          const mir::LocalId unknown_above =
              Hold(code, block, fill_of(Plane::kUnknown));
          // The bits of the source's top word that are the source's.
          const std::uint64_t inside =
              value::ValidBitsMask(src.words - 1U, src.width);
          // The words both values have, each the source's where the source
          // has the bit and the fill above.
          const std::size_t shared = std::min(src.words, out.words);
          EachWord(code, block, shared, [&](mir::Builder body, const Where& i) {
            const mir::LocalId kept = Hold(
                code, body,
                WordWhere(
                    body,
                    mir::BuildMachineIntComparison(
                        Unit(), body, mir::BinaryOp::kEquality, i(body),
                        IndexLiteral(body, top)),
                    WordLiteral(body, inside),
                    WordLiteral(body, ~std::uint64_t{0})));
            const auto made = [&](Plane plane, mir::LocalId above) {
              return Or(
                  body,
                  And(body, PlaneWord(body, src, i, plane), Now(body, kept)),
                  And(body, Now(body, above), Not(body, Now(body, kept))));
            };
            const mir::LocalId unknown =
                Hold(code, body, made(Plane::kUnknown, unknown_above));
            const mir::ExprId value = made(Plane::kValue, value_above);
            SetValueWord(
                body, out, i,
                out.four_state
                    ? value
                    : And(body, value, Not(body, Now(body, unknown))));
            SetUnknownWord(body, out, i, Now(body, unknown));
          });
          // The words above the source's are its fill.
          ForEachIndexBetween(
              code, block,
              IndexLiteral(block, static_cast<std::int64_t>(shared)),
              CountLiteral(block, out.words),
              [&](mir::Builder body, mir::LocalId at) {
                const Where i = AtLocal(at);
                SetValueWord(
                    body, out, i,
                    out.four_state ? Now(body, value_above)
                                   : And(body, Now(body, value_above),
                                         Not(body, Now(body, unknown_above))));
                SetUnknownWord(body, out, i, Now(body, unknown_above));
              });
          ClearAboveWidth(block, out);
          block.AppendStmt(mir::ReturnStmt{.value = Answered(block, answer)});
        });
  }

  // A number carried into this type keeps its low bits, its sign filling the
  // rest (LRM 6.24.1); a predicate is 1 where it holds and 0 where not.
  auto FromMachine(IntegralOp op, MachineOperand operand) -> mir::CallableCode {
    return BodyOf(
        op, [&](mir::CallableCode& code, mir::Builder block,
                const std::vector<mir::LocalId>& p) {
          const mir::LocalId answer = Answer(code, block);
          const PlaneWords out = AnswerView(code, block, answer);
          mir::LocalId low{};
          mir::ExprId fill{};
          switch (operand) {
            case MachineOperand::kNumber:
              low = Hold(
                  code, block,
                  CastTo(block, block.ReadLocal(p[0], Machine()), Word()));
              fill = Repeated(block, Now(block, low), 63);
              break;
            case MachineOperand::kPredicate:
              low = Hold(
                  code, block,
                  WordOf(
                      block,
                      block.ReadLocal(p[0], Unit().builtins.machine_bool)));
              fill = WordLiteral(block, 0);
              break;
          }
          const mir::LocalId held_fill = Hold(code, block, fill);
          EachWord(
              code, block, out.words, [&](mir::Builder body, const Where& i) {
                SetValueWord(body, out, i, Now(body, held_fill));
              });
          SetValueWord(block, out, At(0), Now(block, low));
          ClearAboveWidth(block, out);
          return Answered(block, answer);
        });
  }

  // LRM 6.12.1: the low 64 bits as a number, an x or z bit reading 0.
  auto ToInt64() -> mir::CallableCode {
    return BodyOf(
        IntegralOp::kToInt64, [&](mir::CallableCode& code, mir::Builder block,
                                  const std::vector<mir::LocalId>& p) {
          const PlaneWords a = Operand(code, block, p[0], WholeType());
          return CastTo(
              block,
              And(block, ValueWord(block, a, At(0)),
                  Not(block, UnknownWord(block, a, At(0)))),
              Machine());
        });
  }

  // LRM 11.5.1, 7.4.5: the position an index names, all x where it names
  // none: where it holds x or z, a number no machine word holds, or a
  // magnitude past every value.
  auto ToPosition() -> mir::CallableCode {
    return BodyOf(
        IntegralOp::kToPosition,
        [&](mir::CallableCode& code, mir::Builder block,
            const std::vector<mir::LocalId>& p) {
          const PlaneWords a = Operand(code, block, p[0], WholeType());
          const mir::LocalId low =
              Hold(code, block, ValueWord(block, a, At(0)));
          const bool is_signed =
              Integral(WholeType()).signedness == mir::Signedness::kSigned;
          const mir::LocalId sign = Hold(
              code, block,
              is_signed ? Repeated(
                              block,
                              ValueWord(
                                  block, a,
                                  At(static_cast<std::int64_t>(a.words - 1U))),
                              (a.width - 1U) % 64U)
                        : WordLiteral(block, 0));
          // Every bit above the low 63 repeats the sign: the low word's top
          // bit, and each higher word the sign's fill up to the width.
          const mir::LocalId differing = Hold(
              code, block,
              Op(block, mir::BinaryOp::kBitwiseXor,
                 Op(block, mir::BinaryOp::kShr, Now(block, low),
                    WordLiteral(block, 63)),
                 And(block, Now(block, sign), WordLiteral(block, 1))));
          ForEachIndexBetween(
              code, block, IndexLiteral(block, 1), CountLiteral(block, a.words),
              [&](mir::Builder body, mir::LocalId at) {
                Set(body, differing,
                    Or(body, Now(body, differing),
                       Op(body, mir::BinaryOp::kBitwiseXor,
                          ValueWord(body, a, AtLocal(at)), Now(body, sign))));
              });
          const std::uint64_t top = value::ValidBitsMask(a.words - 1U, a.width);
          if (top != ~std::uint64_t{0} && a.words > 1U) {
            Set(block, differing,
                Op(block, mir::BinaryOp::kBitwiseXor, Now(block, differing),
                   And(block, Now(block, sign), WordLiteral(block, ~top))));
          }
          const mir::LocalId any = AnyUnknown(code, block, std::span(&a, 1));
          const mir::LocalId at = Declared(
              code, block, Machine(),
              CastTo(block, Now(block, low), Machine()));
          const auto beyond = [&](mir::BinaryOp op, std::int64_t bound) {
            return WordOf(
                block, mir::BuildMachineIntComparison(
                           Unit(), block, op, block.ReadLocal(at, Machine()),
                           IndexLiteral(block, bound)));
          };
          const mir::ExprId outside = Or(
              block, beyond(mir::BinaryOp::kGreaterThan, value::kPositionLimit),
              beyond(mir::BinaryOp::kLessThan, -value::kPositionLimit));
          const mir::ExprId named = Holds(
              block, mir::BinaryOp::kEquality,
              Or(block, Or(block, Now(block, any), Now(block, differing)),
                 outside),
              WordLiteral(block, 0));
          const mir::TypeId position = code.result_type;
          const mir::ExprId as_position = block.Add(
              mir::MakeBuiltinCall(
                  Unit(), block.Body(), support::BuiltinFn::kIntegralFromInt,
                  std::nullopt, {block.ReadLocal(at, Machine())}, position));
          const mir::ExprId none = mir::BuildIntegralLiteral(
              Unit(), block, position,
              mir::IntegralConstant{
                  .value_words = {~std::uint64_t{0}},
                  .state_words = {~std::uint64_t{0}}});
          return Chosen(block, named, as_position, none, position);
        });
  }
};

}  // namespace

auto BuildIntegralOperationBody(
    const mir::CompilationUnit& unit, const mir::TypeOperationSource& source)
    -> mir::CallableCode {
  return IntegralBodies(unit, source.type).Build(source);
}

}  // namespace lyra::lowering::hir_to_mir
