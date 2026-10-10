#include "lyra/backend/llvm/integral_one_word.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <utility>

#include <llvm/ADT/APInt.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Casting.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/support/integral_operation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::backend::llvm_backend {

namespace {

using support::IntegralOp;

// An operand read as a truth value (LRM 11.4.7): whether a bit of it is
// definitely 1, and whether, none being so, an x or z leaves that open.
struct Truth {
  llvm::Value* nonzero;
  llvm::Value* unknown;
};

// The position an index names (LRM 11.5.1, 7.4.5) as a 64-bit number, and
// whether it names one at all: an index holding x or z, or a magnitude past
// every value, names none. An index of an unsigned type names no negative
// position, which settles the direction a part moves in while compiling.
struct NamedPosition {
  llvm::Value* at;
  llvm::Value* named;
  bool can_be_negative;
};

enum class ShiftKind : std::uint8_t { kLeft, kLogicalRight, kArithmeticRight };

auto IsClear(const llvm::Value* plane) -> bool {
  const auto* constant = llvm::dyn_cast<llvm::Constant>(plane);
  return constant != nullptr && constant->isNullValue();
}

auto IsFull(const llvm::Value* plane) -> bool {
  const auto* constant = llvm::dyn_cast<llvm::Constant>(plane);
  return constant != nullptr && constant->isAllOnesValue();
}

auto Clear(llvm::Type* type) -> llvm::Value* {
  return llvm::Constant::getNullValue(type);
}

auto Full(llvm::Type* type) -> llvm::Value* {
  return llvm::Constant::getAllOnesValue(type);
}

// A value with no x or z in it.
auto Known(llvm::Value* value) -> OneWord {
  return OneWord{.value = value, .unknown = Clear(value->getType())};
}

// Each operation over integral values as instructions on one word per plane:
// what the value layer states for the operation over planes of any width,
// written for planes of a single word, where it is a handful of instructions
// and no call.
class OneWordArithmetic {
 public:
  explicit OneWordArithmetic(llvm::IRBuilderBase& builder) : b_(&builder) {
  }

  [[nodiscard]] auto AnySet(llvm::Value* plane) const -> llvm::Value* {
    return b_->CreateIsNotNull(plane);
  }

  [[nodiscard]] auto Not(llvm::Value* plane) const -> llvm::Value* {
    return b_->CreateNot(plane);
  }

  // A predicate's answer held by a type that has no x: x is 0 there (LRM
  // 6.11.2).
  [[nodiscard]] auto Settled(OneWord bit, bool holds_unknown) const -> OneWord {
    if (holds_unknown) {
      return bit;
    }
    return Known(KnownBits(bit));
  }

  // `answer`, or x in every position where `when` holds -- which a type with
  // no x holds as 0.
  [[nodiscard]] auto AllXWhere(
      llvm::Value* when, OneWord answer, bool holds_unknown) const -> OneWord {
    llvm::Value* const every = b_->CreateSExt(when, answer.value->getType());
    if (!holds_unknown) {
      return Known(And(answer.value, Not(every)));
    }
    return OneWord{
        .value = Or(answer.value, every), .unknown = Or(answer.unknown, every)};
  }

  // LRM 11.4.3: x or z anywhere in either operand makes the whole result x.
  [[nodiscard]] auto Arithmetic(
      llvm::Instruction::BinaryOps op, OneWord a, OneWord b,
      bool holds_unknown) const -> OneWord {
    return AllXWhere(
        AnySet(Or(a.unknown, b.unknown)),
        Known(b_->CreateBinOp(op, a.value, b.value)), holds_unknown);
  }

  [[nodiscard]] auto Negated(OneWord a, bool holds_unknown) const -> OneWord {
    return AllXWhere(
        AnySet(a.unknown), Known(b_->CreateNeg(a.value)), holds_unknown);
  }

  // LRM 11.4.3: division truncates toward zero, a remainder takes the
  // dividend's sign, and a zero divisor makes the result x. The magnitudes
  // divide unsigned, so the most negative value over -1 is the wrapped
  // quotient the language gives it and no machine division overflows.
  [[nodiscard]] auto Divided(
      OneWord a, OneWord b, bool is_signed, bool remainder,
      bool holds_unknown) const -> OneWord {
    llvm::Type* const type = a.value->getType();
    llvm::Value* const no_answer =
        Or(AnySet(Or(a.unknown, b.unknown)), b_->CreateIsNull(b.value));
    llvm::Value* a_negative = b_->getFalse();
    llvm::Value* b_negative = b_->getFalse();
    llvm::Value* a_magnitude = a.value;
    llvm::Value* b_magnitude = b.value;
    if (is_signed) {
      a_negative = b_->CreateICmpSLT(a.value, Clear(type));
      b_negative = b_->CreateICmpSLT(b.value, Clear(type));
      a_magnitude =
          b_->CreateSelect(a_negative, b_->CreateNeg(a.value), a.value);
      b_magnitude =
          b_->CreateSelect(b_negative, b_->CreateNeg(b.value), b.value);
    }
    llvm::Value* const divisor = b_->CreateSelect(
        no_answer, llvm::ConstantInt::get(type, 1), b_magnitude);
    llvm::Value* const magnitude = remainder
                                       ? b_->CreateURem(a_magnitude, divisor)
                                       : b_->CreateUDiv(a_magnitude, divisor);
    llvm::Value* const negative =
        remainder ? a_negative : Xor(a_negative, b_negative);
    return AllXWhere(
        no_answer,
        Known(
            is_signed ? b_->CreateSelect(
                            negative, b_->CreateNeg(magnitude), magnitude)
                      : magnitude),
        holds_unknown);
  }

  // LRM 11.4.8 Tables 11-11 to 11-15, per position, z reading as x. A
  // position of an `&` is 0 where either operand is known 0, and otherwise x
  // where either is unknown; an `|` likewise around a known 1.
  [[nodiscard]] auto BitwiseAnd(OneWord a, OneWord b) const -> OneWord {
    llvm::Value* const value =
        And(Or(a.value, a.unknown), Or(b.value, b.unknown));
    return OneWord{
        .value = value, .unknown = And(value, Or(a.unknown, b.unknown))};
  }

  [[nodiscard]] auto BitwiseOr(OneWord a, OneWord b) const -> OneWord {
    llvm::Value* const either = Or(a.unknown, b.unknown);
    llvm::Value* unknown = either;
    if (!IsClear(either)) {
      unknown = And(
          either,
          Not(Or(And(a.value, Not(a.unknown)), And(b.value, Not(b.unknown)))));
    }
    return OneWord{
        .value = Or(Or(a.value, b.value), either), .unknown = unknown};
  }

  [[nodiscard]] auto BitwiseXor(OneWord a, OneWord b) const -> OneWord {
    llvm::Value* const unknown = Or(a.unknown, b.unknown);
    return OneWord{
        .value = Or(unknown, Xor(a.value, b.value)), .unknown = unknown};
  }

  [[nodiscard]] auto BitwiseXnor(OneWord a, OneWord b) const -> OneWord {
    llvm::Value* const unknown = Or(a.unknown, b.unknown);
    return OneWord{
        .value = Or(unknown, Not(Xor(a.value, b.value))), .unknown = unknown};
  }

  [[nodiscard]] auto BitwiseNot(OneWord a) const -> OneWord {
    return OneWord{.value = Or(Not(a.value), a.unknown), .unknown = a.unknown};
  }

  // LRM 11.4.7 logical negation of one scalar: an x stays x.
  [[nodiscard]] auto Inverted(OneWord bit) const -> OneWord {
    return OneWord{
        .value = Or(Not(bit.value), bit.unknown), .unknown = bit.unknown};
  }

  // LRM 11.4.5 logical equality: a position both operands know and disagree on
  // settles them unequal, and only agreement everywhere both are known leaves
  // the answer to the unknown bits.
  [[nodiscard]] auto Equality(OneWord a, OneWord b) const -> OneWord {
    llvm::Value* const unknown = Or(a.unknown, b.unknown);
    llvm::Value* const agree =
        b_->CreateIsNull(And(Xor(a.value, b.value), Not(unknown)));
    return OneWord{.value = agree, .unknown = And(agree, AnySet(unknown))};
  }

  // LRM 11.4.4: a relation with an x or z anywhere in either operand is x.
  [[nodiscard]] auto Relation(
      llvm::CmpInst::Predicate holds, OneWord a, OneWord b) const -> OneWord {
    llvm::Value* const unknown = AnySet(Or(a.unknown, b.unknown));
    return OneWord{
        .value = Or(b_->CreateICmp(holds, a.value, b.value), unknown),
        .unknown = unknown};
  }

  // LRM 11.4.5 case equality: both planes identical.
  [[nodiscard]] auto CaseEquality(OneWord a, OneWord b) const -> llvm::Value* {
    return And(
        b_->CreateICmpEQ(a.value, b.value),
        b_->CreateICmpEQ(a.unknown, b.unknown));
  }

  // LRM 11.4.6: an x or z in `b` is a wildcard; one in `a` where `b` compares
  // leaves the answer unknown unless a known position already disagrees.
  [[nodiscard]] auto WildcardEquality(OneWord a, OneWord b) const -> OneWord {
    llvm::Value* const compared = Not(b.unknown);
    llvm::Value* const agree = b_->CreateIsNull(
        And(And(Xor(a.value, b.value), Not(a.unknown)), compared));
    return OneWord{
        .value = agree,
        .unknown = And(agree, AnySet(And(a.unknown, compared)))};
  }

  // LRM 12.5.1 `casez`: z on either side is a wildcard, and every other
  // position matches exactly on both planes.
  [[nodiscard]] auto CasezMatch(OneWord a, OneWord b) const -> llvm::Value* {
    llvm::Value* const wildcard =
        Or(And(a.unknown, Not(a.value)), And(b.unknown, Not(b.value)));
    return b_->CreateIsNull(And(
        Or(Xor(a.value, b.value), Xor(a.unknown, b.unknown)), Not(wildcard)));
  }

  // LRM 12.5.1 `casex`: x or z on either side is a wildcard.
  [[nodiscard]] auto CasexMatch(OneWord a, OneWord b) const -> llvm::Value* {
    return b_->CreateIsNull(
        And(Xor(a.value, b.value), Not(Or(a.unknown, b.unknown))));
  }

  // LRM 11.4.7: a definitively-one bit settles the value as nonzero however
  // many unknown bits sit beside it.
  [[nodiscard]] auto TruthOf(OneWord a) const -> Truth {
    llvm::Value* const nonzero = AnySet(And(a.value, Not(a.unknown)));
    return Truth{
        .nonzero = nonzero, .unknown = And(Not(nonzero), AnySet(a.unknown))};
  }

  // LRM 11.4.7: the answer is unknown only where an operand's truth is and
  // the other operand does not settle it.
  [[nodiscard]] auto LogicalAnd(Truth a, Truth b) const -> OneWord {
    llvm::Value* const one = And(a.nonzero, b.nonzero);
    llvm::Value* const not_zero =
        And(Or(a.nonzero, a.unknown), Or(b.nonzero, b.unknown));
    return OneWord{.value = not_zero, .unknown = And(not_zero, Not(one))};
  }

  [[nodiscard]] auto LogicalOr(Truth a, Truth b) const -> OneWord {
    llvm::Value* const one = Or(a.nonzero, b.nonzero);
    llvm::Value* const not_zero =
        Or(Or(a.nonzero, a.unknown), Or(b.nonzero, b.unknown));
    return OneWord{.value = not_zero, .unknown = And(not_zero, Not(one))};
  }

  [[nodiscard]] auto LogicalNot(Truth a) const -> OneWord {
    return OneWord{.value = Not(a.nonzero), .unknown = a.unknown};
  }

  [[nodiscard]] auto LogicalEquivalence(Truth a, Truth b) const -> OneWord {
    llvm::Value* const unknown = Or(a.unknown, b.unknown);
    return OneWord{
        .value = Or(unknown, b_->CreateICmpEQ(a.nonzero, b.nonzero)),
        .unknown = unknown};
  }

  // LRM 11.4.9: a known 0 settles an and-reduction and a known 1 an
  // or-reduction whatever else the operand holds; an x or z anywhere makes an
  // xor-reduction x.
  [[nodiscard]] auto Reduced(OneWord a, value::ReductionOp op) const
      -> OneWord {
    llvm::Value* const known = Not(a.unknown);
    llvm::Value* const any_unknown = AnySet(a.unknown);
    const auto and_bit = [&] {
      llvm::Value* const no_zero = b_->CreateIsNull(And(Not(a.value), known));
      return OneWord{.value = no_zero, .unknown = And(no_zero, any_unknown)};
    };
    const auto or_bit = [&] {
      llvm::Value* const any_one = AnySet(And(a.value, known));
      return OneWord{
          .value = Or(any_one, any_unknown),
          .unknown = And(Not(any_one), any_unknown)};
    };
    const auto xor_bit = [&] {
      llvm::Value* const parity = b_->CreateTrunc(
          b_->CreateUnaryIntrinsic(llvm::Intrinsic::ctpop, And(a.value, known)),
          b_->getInt1Ty());
      return OneWord{.value = Or(any_unknown, parity), .unknown = any_unknown};
    };
    switch (op) {
      case value::ReductionOp::kAnd:
        return and_bit();
      case value::ReductionOp::kOr:
        return or_bit();
      case value::ReductionOp::kXor:
        return xor_bit();
      case value::ReductionOp::kNand:
        return Inverted(and_bit());
      case value::ReductionOp::kNor:
        return Inverted(or_bit());
      case value::ReductionOp::kXnor:
        return Inverted(xor_bit());
    }
    throw InternalError("llvm codegen: unknown reduction");
  }

  // LRM 11.4.10: an amount at or past the width moves every bit out, an
  // arithmetic shift fills each plane from its own top position, and an x or
  // z in the amount makes the whole result x. The amount is read unsigned.
  [[nodiscard]] auto Shifted(
      OneWord a, std::uint64_t width, OneWord amount, ShiftKind kind,
      bool holds_unknown) const -> OneWord {
    llvm::Type* const type = a.value->getType();
    llvm::Value* const by = b_->CreateZExt(amount.value, b_->getInt64Ty());
    llvm::Value* const past = b_->CreateICmpUGE(by, b_->getInt64(width));
    const auto within = [&](std::uint64_t saturated) {
      return b_->CreateZExtOrTrunc(
          b_->CreateSelect(past, b_->getInt64(saturated), by), type);
    };
    const auto moved_out = [&](llvm::Instruction::BinaryOps op) {
      llvm::Value* const amount_within = within(0);
      return [this, op, past, amount_within](llvm::Value* plane) {
        if (IsClear(plane)) {
          return plane;
        }
        return b_->CreateSelect(
            past, Clear(plane->getType()),
            b_->CreateBinOp(op, plane, amount_within));
      };
    };
    const auto shifted = [&](auto plane) {
      return AllXWhere(
          AnySet(amount.unknown),
          OneWord{.value = plane(a.value), .unknown = plane(a.unknown)},
          holds_unknown);
    };
    switch (kind) {
      case ShiftKind::kLeft:
        return shifted(moved_out(llvm::Instruction::Shl));
      case ShiftKind::kLogicalRight:
        return shifted(moved_out(llvm::Instruction::LShr));
      case ShiftKind::kArithmeticRight: {
        llvm::Value* const amount_within = within(width - 1);
        return shifted([this, amount_within](llvm::Value* plane) {
          return IsClear(plane) ? plane : b_->CreateAShr(plane, amount_within);
        });
      }
    }
    throw InternalError("llvm codegen: unknown shift");
  }

  // LRM 11.4.12: `high` in the most significant positions and `low` below it.
  [[nodiscard]] auto Concat(
      OneWord high, OneWord low, std::uint64_t low_width,
      std::uint64_t answer_width) const -> OneWord {
    llvm::Type* const type = b_->getIntNTy(Bits(answer_width));
    const auto joined = [&](llvm::Value* high_plane, llvm::Value* low_plane) {
      llvm::Value* const raised = b_->CreateZExt(high_plane, type);
      return Or(
          IsClear(raised)
              ? raised
              : b_->CreateShl(raised, llvm::ConstantInt::get(type, low_width)),
          b_->CreateZExt(low_plane, type));
    };
    return OneWord{
        .value = joined(high.value, low.value),
        .unknown = joined(high.unknown, low.unknown)};
  }

  // LRM 11.4.12.1: as many copies of the operand as fill the answer, laid end
  // to end. Copies of a value narrower than their spacing never overlap, so one
  // multiplication lays them all down. A two-state answer takes every x or z
  // as 0.
  [[nodiscard]] auto Replicated(
      OneWord a, std::uint64_t width, std::uint64_t answer_width,
      bool holds_unknown) const -> OneWord {
    llvm::Type* const type = b_->getIntNTy(Bits(answer_width));
    llvm::APInt copies(Bits(answer_width), 0);
    for (std::uint64_t at = 0; at < answer_width; at += width) {
      copies.setBit(Bits(at));
    }
    const auto laid = [&](llvm::Value* plane) {
      llvm::Value* const one = b_->CreateZExtOrTrunc(plane, type);
      if (IsClear(one) || copies.isOne()) {
        return one;
      }
      return b_->CreateMul(one, llvm::ConstantInt::get(type, copies));
    };
    if (!holds_unknown) {
      return Known(laid(KnownBits(a)));
    }
    return OneWord{.value = laid(a.value), .unknown = laid(a.unknown)};
  }

  [[nodiscard]] auto PositionNamed(
      OneWord index, const value::IntegralShape& shape) const -> NamedPosition {
    const bool is_signed = shape.signedness == value::Signedness::kSigned;
    llvm::Type* const word = b_->getInt64Ty();
    llvm::Value* const at = is_signed ? b_->CreateSExt(index.value, word)
                                      : b_->CreateZExt(index.value, word);
    llvm::Value* named = b_->CreateIsNull(index.unknown);
    if (!is_signed && shape.width == 64) {
      named = And(named, b_->CreateICmpSGE(at, Clear(word)));
    }
    // An index no wider than this cannot hold a magnitude past the limit.
    if (shape.width > 32) {
      named =
          And(named, And(b_->CreateICmpSGE(
                             at, llvm::ConstantInt::getSigned(
                                     word, -value::kPositionLimit)),
                         b_->CreateICmpSLE(
                             at, llvm::ConstantInt::getSigned(
                                     word, value::kPositionLimit))));
    }
    return NamedPosition{
        .at = at, .named = named, .can_be_negative = is_signed};
  }

  // LRM 11.5.1: as many bits as the answer is wide, from the position named,
  // counted from the least significant bit. A position outside the value
  // reads x, or 0 in a two-state answer, and so does every bit where the
  // position names none or the part lies wholly outside.
  [[nodiscard]] auto Slice(
      OneWord source, std::uint64_t source_width, const NamedPosition& position,
      std::uint64_t answer_width, bool holds_unknown) const -> OneWord {
    llvm::Type* const word = b_->getInt64Ty();
    llvm::Type* const type = b_->getIntNTy(Bits(answer_width));
    const auto taken = [&](llvm::Value* plane) {
      return b_->CreateTrunc(
          Moved(
              plane, position, llvm::Instruction::LShr, llvm::Instruction::Shl),
          type);
    };
    llvm::Value* reaches =
        And(position.named,
            b_->CreateICmpSLT(
                position.at, llvm::ConstantInt::get(word, source_width)));
    if (position.can_be_negative) {
      reaches = And(
          reaches, b_->CreateICmpSGT(
                       position.at,
                       llvm::ConstantInt::getSigned(
                           word, -static_cast<std::int64_t>(answer_width))));
    }
    if (!holds_unknown) {
      return Known(Select(
          reaches, taken(b_->CreateZExt(KnownBits(source), word)),
          Clear(type)));
    }
    llvm::Value* const outside = Not(taken(LowBits(source_width)));
    return OneWord{
        .value = Select(
            reaches, Or(taken(b_->CreateZExt(source.value, word)), outside),
            Full(type)),
        .unknown = Select(
            reaches, Or(taken(b_->CreateZExt(source.unknown, word)), outside),
            Full(type))};
  }

  // LRM 11.5.1: `bits` written into `target` at the position named, every
  // other position left as it stands; a position outside the value is not
  // written, and none is where the position names none. Bits with no x
  // settle the positions they land on, and an x or z lands in a two-state
  // target as 0.
  [[nodiscard]] auto WithSlice(
      OneWord target, std::uint64_t target_width, const NamedPosition& position,
      OneWord bits, std::uint64_t bits_width, bool holds_unknown) const
      -> OneWord {
    llvm::Type* const word = b_->getInt64Ty();
    llvm::Type* const type = target.value->getType();
    const auto placed = [&](llvm::Value* plane) {
      return Moved(
          plane, position, llvm::Instruction::Shl, llvm::Instruction::LShr);
    };
    llvm::Value* reaches =
        And(position.named,
            b_->CreateICmpSLT(
                position.at, llvm::ConstantInt::get(word, target_width)));
    if (position.can_be_negative) {
      reaches = And(
          reaches,
          b_->CreateICmpSGT(
              position.at, llvm::ConstantInt::getSigned(
                               word, -static_cast<std::int64_t>(bits_width))));
    }
    llvm::Value* const written = Select(
        reaches,
        b_->CreateTrunc(
            And(placed(LowBits(bits_width)), LowBits(target_width)), type),
        Clear(type));
    const auto replaced = [&](llvm::Value* kept, llvm::Value* plane) {
      return Or(
          And(kept, Not(written)),
          And(b_->CreateTrunc(placed(b_->CreateZExt(plane, word)), type),
              written));
    };
    if (!holds_unknown) {
      return Known(replaced(target.value, KnownBits(bits)));
    }
    return OneWord{
        .value = replaced(target.value, bits.value),
        .unknown = replaced(target.unknown, bits.unknown)};
  }

  // LRM 6.24.1, 10.7: the source's bits where it reaches and, above them, its
  // sign where a signed source widens -- an x or z sign filling with itself.
  // A two-state answer takes every x or z as 0.
  [[nodiscard]] auto Converted(
      OneWord source, bool is_signed, std::uint64_t answer_width,
      bool holds_unknown) const -> OneWord {
    llvm::Type* const type = b_->getIntNTy(Bits(answer_width));
    const auto resized = [&](llvm::Value* plane) {
      return is_signed ? b_->CreateSExtOrTrunc(plane, type)
                       : b_->CreateZExtOrTrunc(plane, type);
    };
    return Settled(
        OneWord{
            .value = resized(source.value), .unknown = resized(source.unknown)},
        holds_unknown);
  }

  // The value as a 64-bit number, sign-extended from its width when signed.
  // An x or z bit reads as 0 (LRM 6.12.1).
  [[nodiscard]] auto ToInt64(OneWord a, bool is_signed) const -> llvm::Value* {
    llvm::Value* const known = KnownBits(a);
    return is_signed ? b_->CreateSExt(known, b_->getInt64Ty())
                     : b_->CreateZExt(known, b_->getInt64Ty());
  }

  // LRM 11.4.11 Table 11-20: a position both arms know and agree on survives,
  // and every other position is x -- 0 in a type that holds none.
  [[nodiscard]] auto MergeConditional(
      OneWord a, OneWord b, bool holds_unknown) const -> OneWord {
    llvm::Value* const agreed =
        And(Not(Xor(a.value, b.value)), Not(Or(a.unknown, b.unknown)));
    llvm::Value* const kept = And(a.value, agreed);
    if (!holds_unknown) {
      return Known(kept);
    }
    return OneWord{.value = Or(kept, Not(agreed)), .unknown = Not(agreed)};
  }

  // LRM 6.6.1 Table 6-2, 6.6.3 Tables 6-3 and 6-4: z defers to the other
  // driver; where both drive, tri-state passes agreement and makes a conflict
  // x, wired-and lets any 0 win, wired-or any 1.
  [[nodiscard]] auto Resolved(
      OneWord a, OneWord b, value::NetResolution fold) const -> OneWord {
    llvm::Value* const a_z = And(Not(a.value), a.unknown);
    llvm::Value* const b_z = And(Not(b.value), b.unknown);
    llvm::Value* const take_a = And(Not(a_z), b_z);
    llvm::Value* const both = And(Not(a_z), Not(b_z));
    llvm::Value* const value = Or(And(a_z, b.value), And(take_a, a.value));
    llvm::Value* const unknown =
        Or(And(a_z, b.unknown), And(take_a, a.unknown));
    const auto any_x = [&] {
      return Or(And(a.value, a.unknown), And(b.value, b.unknown));
    };
    switch (fold) {
      case value::NetResolution::kTriState: {
        llvm::Value* const same = And(
            both,
            And(Not(Xor(a.value, b.value)), Not(Xor(a.unknown, b.unknown))));
        llvm::Value* const conflict = And(both, Not(same));
        return OneWord{
            .value = Or(value, Or(And(same, a.value), conflict)),
            .unknown = Or(unknown, Or(And(same, a.unknown), conflict))};
      }
      case value::NetResolution::kWiredAnd: {
        llvm::Value* const any_zero =
            Or(And(Not(a.value), Not(a.unknown)),
               And(Not(b.value), Not(b.unknown)));
        llvm::Value* const no_zero = And(both, Not(any_zero));
        return OneWord{
            .value = Or(value, no_zero),
            .unknown = Or(unknown, And(no_zero, any_x()))};
      }
      case value::NetResolution::kWiredOr: {
        llvm::Value* const any_one =
            Or(And(a.value, Not(a.unknown)), And(b.value, Not(b.unknown)));
        llvm::Value* const x_bits = And(And(both, Not(any_one)), any_x());
        return OneWord{
            .value = Or(value, Or(And(both, any_one), x_bits)),
            .unknown = Or(unknown, x_bits)};
      }
    }
    throw InternalError("llvm codegen: unknown net fold");
  }

  // LRM 28.12.1: the stronger contribution determines every position it
  // drives, which is every position where it is not z, and the weaker one the
  // rest.
  [[nodiscard]] auto Dominating(OneWord stronger, OneWord weaker) const
      -> OneWord {
    llvm::Value* const undriven = And(Not(stronger.value), stronger.unknown);
    return OneWord{
        .value =
            Or(And(stronger.value, Not(undriven)), And(weaker.value, undriven)),
        .unknown =
            Or(And(stronger.unknown, Not(undriven)),
               And(weaker.unknown, undriven))};
  }

 private:
  [[nodiscard]] static auto Bits(std::uint64_t count) -> unsigned {
    return static_cast<unsigned>(count);
  }

  [[nodiscard]] auto LowBits(std::uint64_t count) const -> llvm::Value* {
    return b_->getInt64(value::LowBits(count));
  }

  // The value plane as a type with no x holds it: every x or z as 0 (LRM
  // 6.11.2).
  [[nodiscard]] auto KnownBits(OneWord a) const -> llvm::Value* {
    return And(a.value, Not(a.unknown));
  }

  [[nodiscard]] auto And(llvm::Value* a, llvm::Value* b) const -> llvm::Value* {
    if (IsClear(a) || IsFull(b)) {
      return a;
    }
    if (IsClear(b) || IsFull(a)) {
      return b;
    }
    return b_->CreateAnd(a, b);
  }

  [[nodiscard]] auto Or(llvm::Value* a, llvm::Value* b) const -> llvm::Value* {
    if (IsClear(a) || IsFull(b)) {
      return b;
    }
    if (IsClear(b) || IsFull(a)) {
      return a;
    }
    return b_->CreateOr(a, b);
  }

  [[nodiscard]] auto Xor(llvm::Value* a, llvm::Value* b) const -> llvm::Value* {
    if (IsClear(a)) {
      return b;
    }
    if (IsClear(b)) {
      return a;
    }
    return b_->CreateXor(a, b);
  }

  [[nodiscard]] auto Select(
      llvm::Value* when, llvm::Value* then, llvm::Value* otherwise) const
      -> llvm::Value* {
    if (IsFull(when)) {
      return then;
    }
    if (IsClear(when)) {
      return otherwise;
    }
    return b_->CreateSelect(when, then, otherwise);
  }

  // A 64-bit plane moved by the position: by `toward` where the position is
  // not negative and by `back` its magnitude where it is. A position that
  // reaches nothing moves by an amount the caller's own test discards, so the
  // amount is only kept inside the word.
  [[nodiscard]] auto Moved(
      llvm::Value* plane, const NamedPosition& position,
      llvm::Instruction::BinaryOps toward,
      llvm::Instruction::BinaryOps back) const -> llvm::Value* {
    if (IsClear(plane)) {
      return plane;
    }
    llvm::Value* const within = b_->getInt64(63);
    llvm::Value* const forward =
        b_->CreateBinOp(toward, plane, b_->CreateAnd(position.at, within));
    if (!position.can_be_negative) {
      return forward;
    }
    return b_->CreateSelect(
        b_->CreateICmpSGE(position.at, b_->getInt64(0)), forward,
        b_->CreateBinOp(
            back, plane, b_->CreateAnd(b_->CreateNeg(position.at), within)));
  }

  llvm::IRBuilderBase* b_;
};

// Where the unknown plane of a one-bit value at `at` lies: after its value
// plane.
auto UnknownPlaneOfOneBit(llvm::IRBuilderBase& builder, llvm::Value* at)
    -> llvm::Value* {
  return builder.CreateConstInBoundsGEP1_64(
      builder.getInt8Ty(), at, value::PlaneBytesFor(1));
}

void RequireOneBit(const value::IntegralShape& answer) {
  if (answer.width != 1 || value::PlaneBytesFor(1) != 1) {
    throw InternalError(
        "llvm codegen: a comparison's answer is held as a value of a type that "
        "is not one bit -- please report this as a bug");
  }
}

}  // namespace

auto LoadComparisonAnswer(
    llvm::IRBuilderBase& builder, llvm::Value* at,
    const value::IntegralShape& answer) -> llvm::Value* {
  RequireOneBit(answer);
  llvm::Type* const byte = builder.getInt8Ty();
  llvm::Value* const bit = builder.CreateLoad(byte, at);
  if (!answer.IsFourState()) {
    return bit;
  }
  llvm::Value* const unknown =
      builder.CreateLoad(byte, UnknownPlaneOfOneBit(builder, at));
  return builder.CreateOr(
      builder.CreateShl(bit, value::kScalarValueBit),
      builder.CreateShl(unknown, value::kScalarUnknownBit));
}

void StoreComparisonAnswer(
    llvm::IRBuilderBase& builder, llvm::Value* scalar, llvm::Value* at,
    const value::IntegralShape& answer) {
  RequireOneBit(answer);
  llvm::Type* const byte = builder.getInt8Ty();
  if (!answer.IsFourState()) {
    builder.CreateStore(
        builder.CreateZExt(
            builder.CreateICmpEQ(
                scalar,
                builder.getInt8(std::to_underlying(value::FourStateBit::kOne))),
            byte),
        at);
    return;
  }
  const auto plane_bit = [&](unsigned position) {
    return builder.CreateAnd(
        builder.CreateLShr(scalar, position), builder.getInt8(1));
  };
  builder.CreateStore(plane_bit(value::kScalarValueBit), at);
  builder.CreateStore(
      plane_bit(value::kScalarUnknownBit), UnknownPlaneOfOneBit(builder, at));
}

auto LowerOnOneWord(
    llvm::IRBuilderBase& builder, IntegralOp op, const IntegralShapes& shapes,
    const OneWordOperands& operands) -> std::optional<OneWordAnswer> {
  const auto fits_one_word =
      [](const std::optional<value::IntegralShape>& shape) {
        return !shape.has_value() || shape->width <= 64;
      };
  if (!std::ranges::all_of(shapes.operands, fits_one_word) ||
      !fits_one_word(shapes.answer)) {
    return std::nullopt;
  }
  const OneWordArithmetic words(builder);

  const auto shape = [&](std::size_t index) -> const value::IntegralShape& {
    return *shapes.operands.at(index);
  };
  const auto load = [&](std::size_t index) { return operands.planes(index); };
  const auto width = [&](std::size_t index) { return shape(index).width; };
  const auto reads_signed = [&](std::size_t index) {
    return shape(index).signedness == value::Signedness::kSigned;
  };
  const auto answers_unknown = [&] { return shapes.answer->IsFourState(); };
  const auto answer_width = [&] { return shapes.answer->width; };
  // A predicate's one-bit answer, held as the answer's type holds it.
  const auto answer_bit = [&](OneWord bit) {
    return words.Settled(bit, answers_unknown());
  };
  const auto machine = [&](unsigned bits) {
    llvm::Value* const operand = operands.machine(0);
    if (!operand->getType()->isIntegerTy(bits)) {
      throw InternalError(
          "llvm codegen: an integral value is built from a machine value of "
          "another type than its operation takes -- please report this as a "
          "bug");
    }
    return operand;
  };
  const auto exact = [&] {
    return builder.getIntNTy(static_cast<unsigned>(answer_width()));
  };

  const auto arithmetic = [&](llvm::Instruction::BinaryOps instruction) {
    return words.Arithmetic(instruction, load(0), load(1), answers_unknown());
  };
  const auto divided = [&](bool remainder) {
    return words.Divided(
        load(0), load(1), reads_signed(0), remainder, answers_unknown());
  };
  const auto relation = [&](llvm::CmpInst::Predicate as_signed,
                            llvm::CmpInst::Predicate as_unsigned) {
    return answer_bit(words.Relation(
        reads_signed(0) ? as_signed : as_unsigned, load(0), load(1)));
  };
  const auto reduced = [&](value::ReductionOp reduction) {
    return answer_bit(words.Reduced(load(0), reduction));
  };
  const auto resolved = [&](value::NetResolution fold) {
    return words.Resolved(load(0), load(1), fold);
  };
  const auto shifted = [&](ShiftKind kind) {
    return words.Shifted(load(0), width(0), load(1), kind, answers_unknown());
  };

  switch (op) {
    case IntegralOp::kAdd:
      return arithmetic(llvm::Instruction::Add);
    case IntegralOp::kSubtract:
      return arithmetic(llvm::Instruction::Sub);
    case IntegralOp::kMultiply:
      return arithmetic(llvm::Instruction::Mul);
    case IntegralOp::kDivide:
      return divided(false);
    case IntegralOp::kModulo:
      return divided(true);
    case IntegralOp::kNegate:
      return words.Negated(load(0), answers_unknown());
    case IntegralOp::kBitwiseAnd:
      return words.BitwiseAnd(load(0), load(1));
    case IntegralOp::kBitwiseOr:
      return words.BitwiseOr(load(0), load(1));
    case IntegralOp::kBitwiseXor:
      return words.BitwiseXor(load(0), load(1));
    case IntegralOp::kBitwiseXnor:
      return words.BitwiseXnor(load(0), load(1));
    case IntegralOp::kBitwiseNot:
      return words.BitwiseNot(load(0));
    case IntegralOp::kEqual:
      return answer_bit(words.Equality(load(0), load(1)));
    case IntegralOp::kNotEqual:
      return answer_bit(words.Inverted(words.Equality(load(0), load(1))));
    case IntegralOp::kLess:
      return relation(llvm::CmpInst::ICMP_SLT, llvm::CmpInst::ICMP_ULT);
    case IntegralOp::kLessEqual:
      return relation(llvm::CmpInst::ICMP_SLE, llvm::CmpInst::ICMP_ULE);
    case IntegralOp::kGreater:
      return relation(llvm::CmpInst::ICMP_SGT, llvm::CmpInst::ICMP_UGT);
    case IntegralOp::kGreaterEqual:
      return relation(llvm::CmpInst::ICMP_SGE, llvm::CmpInst::ICMP_UGE);
    case IntegralOp::kCaseEqual:
      return Known(words.CaseEquality(load(0), load(1)));
    case IntegralOp::kWildcardEqual:
      return answer_bit(words.WildcardEquality(load(0), load(1)));
    case IntegralOp::kCasezMatch:
      return Known(words.CasezMatch(load(0), load(1)));
    case IntegralOp::kCasexMatch:
      return Known(words.CasexMatch(load(0), load(1)));
    case IntegralOp::kLogicalAnd:
      return answer_bit(
          words.LogicalAnd(words.TruthOf(load(0)), words.TruthOf(load(1))));
    case IntegralOp::kLogicalOr:
      return answer_bit(
          words.LogicalOr(words.TruthOf(load(0)), words.TruthOf(load(1))));
    case IntegralOp::kLogicalNot:
      return answer_bit(words.LogicalNot(words.TruthOf(load(0))));
    case IntegralOp::kLogicalEquivalence:
      return answer_bit(words.LogicalEquivalence(
          words.TruthOf(load(0)), words.TruthOf(load(1))));
    // LRM 12.4: only a value with a bit that is definitely 1 holds as a
    // condition.
    case IntegralOp::kIsTrue:
      return words.TruthOf(load(0)).nonzero;
    case IntegralOp::kReductionAnd:
      return reduced(value::ReductionOp::kAnd);
    case IntegralOp::kReductionOr:
      return reduced(value::ReductionOp::kOr);
    case IntegralOp::kReductionXor:
      return reduced(value::ReductionOp::kXor);
    case IntegralOp::kReductionNand:
      return reduced(value::ReductionOp::kNand);
    case IntegralOp::kReductionNor:
      return reduced(value::ReductionOp::kNor);
    case IntegralOp::kReductionXnor:
      return reduced(value::ReductionOp::kXnor);
    case IntegralOp::kShiftLeft:
      return shifted(ShiftKind::kLeft);
    case IntegralOp::kLogicalShiftRight:
      return shifted(ShiftKind::kLogicalRight);
    // LRM 11.4.10: `>>>` fills from the sign of a signed operand and with 0
    // from an unsigned one.
    case IntegralOp::kArithmeticShiftRight:
      return shifted(
          reads_signed(0) ? ShiftKind::kArithmeticRight
                          : ShiftKind::kLogicalRight);
    case IntegralOp::kConcat:
      return words.Concat(load(0), load(1), width(1), answer_width());
    case IntegralOp::kReplicate:
      return words.Replicated(
          load(0), width(0), answer_width(), answers_unknown());
    case IntegralOp::kSlice:
      return words.Slice(
          load(0), width(0), words.PositionNamed(load(1), shape(1)),
          answer_width(), answers_unknown());
    case IntegralOp::kWithSlice:
      return words.WithSlice(
          load(0), width(0), words.PositionNamed(load(1), shape(1)), load(2),
          width(2), answers_unknown());
    case IntegralOp::kConvert:
      return words.Converted(
          load(0), reads_signed(0), answer_width(), answers_unknown());
    // LRM 11.6.1: a number carried into the answer's type keeps its low bits.
    case IntegralOp::kFromInt:
      return Known(builder.CreateTrunc(machine(64), exact()));
    case IntegralOp::kFromBool:
      return Known(builder.CreateZExt(machine(1), exact()));
    case IntegralOp::kToInt64:
      return words.ToInt64(load(0), reads_signed(0));
    // The position an index names, in the type position arithmetic is done
    // in: all x where it names none.
    case IntegralOp::kToPosition: {
      const NamedPosition position = words.PositionNamed(load(0), shape(0));
      return words.AllXWhere(
          words.Not(position.named), Known(position.at), answers_unknown());
    }
    case IntegralOp::kIsUnknown:
      return Known(words.AnySet(load(0).unknown));
    case IntegralOp::kHasUnknown:
      return words.AnySet(load(0).unknown);
    case IntegralOp::kBitIdentical:
      return words.CaseEquality(load(0), load(1));
    case IntegralOp::kMergeConditional:
      return words.MergeConditional(load(0), load(1), answers_unknown());
    case IntegralOp::kResolveTriState:
      return resolved(value::NetResolution::kTriState);
    case IntegralOp::kResolveWiredAnd:
      return resolved(value::NetResolution::kWiredAnd);
    case IntegralOp::kResolveWiredOr:
      return resolved(value::NetResolution::kWiredOr);
    case IntegralOp::kDominate:
      return words.Dominating(load(0), load(1));
    // Long work at every width, which the library carries out: a power, a
    // count of bits, a logarithm, a reversal of blocks, and a value read out
    // of text or of what a foreign call left.
    case IntegralOp::kPower:
    case IntegralOp::kCountBits:
    case IntegralOp::kCeilLog2:
    case IntegralOp::kReverseBlocks:
    case IntegralOp::kFromText:
    case IntegralOp::kReadCanonicalBits:
    case IntegralOp::kReadCanonicalLogic:
    case IntegralOp::kFromSvLogic:
      return std::nullopt;
  }
  throw InternalError("llvm codegen: unknown integral operation");
}

}  // namespace lyra::backend::llvm_backend
