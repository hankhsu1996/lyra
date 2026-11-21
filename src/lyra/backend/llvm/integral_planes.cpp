#include "lyra/backend/llvm/integral_planes.hpp"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <format>
#include <functional>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include <llvm/ADT/APInt.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/LLVMContext.h>
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
auto Known(llvm::Value* value) -> Planes {
  return Planes{.value = value, .unknown = Clear(value->getType())};
}

// Each operation over integral values as instructions on planes held as
// integers of the values' own widths: what the value layer states for the
// operation over planes of any width, written where the width is known, so it
// is instructions over the words that width has and no call.
class PlaneArithmetic {
 public:
  explicit PlaneArithmetic(llvm::IRBuilderBase& builder) : b_(&builder) {
  }

  [[nodiscard]] auto AnySet(llvm::Value* plane) const -> llvm::Value* {
    return b_->CreateIsNotNull(plane);
  }

  [[nodiscard]] auto Not(llvm::Value* plane) const -> llvm::Value* {
    return b_->CreateNot(plane);
  }

  // A predicate's answer held by a type that has no x: x is 0 there (LRM
  // 6.11.2).
  [[nodiscard]] auto Settled(Planes bit, bool holds_unknown) const -> Planes {
    if (holds_unknown) {
      return bit;
    }
    return Known(KnownBits(bit));
  }

  // `answer`, or x in every position where `when` holds -- which a type with
  // no x holds as 0.
  [[nodiscard]] auto AllXWhere(
      llvm::Value* when, Planes answer, bool holds_unknown) const -> Planes {
    llvm::Value* const every = b_->CreateSExt(when, answer.value->getType());
    if (!holds_unknown) {
      return Known(And(answer.value, Not(every)));
    }
    return Planes{
        .value = Or(answer.value, every), .unknown = Or(answer.unknown, every)};
  }

  // LRM 11.4.3: x or z anywhere in either operand makes the whole result x.
  [[nodiscard]] auto Arithmetic(
      llvm::Instruction::BinaryOps op, Planes a, Planes b,
      bool holds_unknown) const -> Planes {
    llvm::Value* const unknown = AnySet(Or(a.unknown, b.unknown));
    llvm::Value* const answer = b_->CreateBinOp(op, a.value, b.value);
    return AllXWhere(unknown, Known(answer), holds_unknown);
  }

  [[nodiscard]] auto Negated(Planes a, bool holds_unknown) const -> Planes {
    llvm::Value* const unknown = AnySet(a.unknown);
    llvm::Value* const negated = b_->CreateNeg(a.value);
    return AllXWhere(unknown, Known(negated), holds_unknown);
  }

  // LRM 11.4.3: division truncates toward zero, a remainder takes the
  // dividend's sign, and a zero divisor makes the result x. The magnitudes
  // divide unsigned, so the most negative value over -1 is the wrapped
  // quotient the language gives it and no machine division overflows.
  [[nodiscard]] auto Divided(
      Planes a, Planes b, bool is_signed, bool remainder,
      bool holds_unknown) const -> Planes {
    llvm::Type* const type = a.value->getType();
    llvm::Value* const any_unknown = AnySet(Or(a.unknown, b.unknown));
    llvm::Value* const by_zero = b_->CreateIsNull(b.value);
    llvm::Value* const no_answer = Or(any_unknown, by_zero);
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
  [[nodiscard]] auto BitwiseAnd(Planes a, Planes b) const -> Planes {
    llvm::Value* const a_maybe_one = Or(a.value, a.unknown);
    llvm::Value* const b_maybe_one = Or(b.value, b.unknown);
    llvm::Value* const value = And(a_maybe_one, b_maybe_one);
    return Planes{
        .value = value, .unknown = And(value, Or(a.unknown, b.unknown))};
  }

  [[nodiscard]] auto BitwiseOr(Planes a, Planes b) const -> Planes {
    llvm::Value* const either = Or(a.unknown, b.unknown);
    llvm::Value* unknown = either;
    if (!IsClear(either)) {
      llvm::Value* const a_one = And(a.value, Not(a.unknown));
      llvm::Value* const b_one = And(b.value, Not(b.unknown));
      unknown = And(either, Not(Or(a_one, b_one)));
    }
    return Planes{
        .value = Or(Or(a.value, b.value), either), .unknown = unknown};
  }

  [[nodiscard]] auto BitwiseXor(Planes a, Planes b) const -> Planes {
    llvm::Value* const unknown = Or(a.unknown, b.unknown);
    return Planes{
        .value = Or(unknown, Xor(a.value, b.value)), .unknown = unknown};
  }

  [[nodiscard]] auto BitwiseXnor(Planes a, Planes b) const -> Planes {
    llvm::Value* const unknown = Or(a.unknown, b.unknown);
    return Planes{
        .value = Or(unknown, Not(Xor(a.value, b.value))), .unknown = unknown};
  }

  [[nodiscard]] auto BitwiseNot(Planes a) const -> Planes {
    return Planes{.value = Or(Not(a.value), a.unknown), .unknown = a.unknown};
  }

  // LRM 11.4.7 logical negation of one scalar: an x stays x.
  [[nodiscard]] auto Inverted(Planes bit) const -> Planes {
    return Planes{
        .value = Or(Not(bit.value), bit.unknown), .unknown = bit.unknown};
  }

  // LRM 11.4.5 logical equality: a position both operands know and disagree on
  // settles them unequal, and only agreement everywhere both are known leaves
  // the answer to the unknown bits.
  [[nodiscard]] auto Equality(Planes a, Planes b) const -> Planes {
    llvm::Value* const unknown = Or(a.unknown, b.unknown);
    llvm::Value* const differ = Xor(a.value, b.value);
    llvm::Value* const agree = b_->CreateIsNull(And(differ, Not(unknown)));
    return Planes{.value = agree, .unknown = And(agree, AnySet(unknown))};
  }

  // LRM 11.4.4: a relation with an x or z anywhere in either operand is x.
  [[nodiscard]] auto Relation(
      llvm::CmpInst::Predicate holds, Planes a, Planes b) const -> Planes {
    llvm::Value* const unknown = AnySet(Or(a.unknown, b.unknown));
    return Planes{
        .value = Or(b_->CreateICmp(holds, a.value, b.value), unknown),
        .unknown = unknown};
  }

  // LRM 11.4.5 case equality: both planes identical.
  [[nodiscard]] auto CaseEquality(Planes a, Planes b) const -> llvm::Value* {
    llvm::Value* const values_equal = b_->CreateICmpEQ(a.value, b.value);
    llvm::Value* const unknowns_equal = b_->CreateICmpEQ(a.unknown, b.unknown);
    return And(values_equal, unknowns_equal);
  }

  // LRM 11.4.6: an x or z in `b` is a wildcard; one in `a` where `b` compares
  // leaves the answer unknown unless a known position already disagrees.
  [[nodiscard]] auto WildcardEquality(Planes a, Planes b) const -> Planes {
    llvm::Value* const compared = Not(b.unknown);
    llvm::Value* const differ = Xor(a.value, b.value);
    llvm::Value* const agree =
        b_->CreateIsNull(And(And(differ, Not(a.unknown)), compared));
    return Planes{
        .value = agree,
        .unknown = And(agree, AnySet(And(a.unknown, compared)))};
  }

  // LRM 12.5.1 `casez`: z on either side is a wildcard, and every other
  // position matches exactly on both planes.
  [[nodiscard]] auto CasezMatch(Planes a, Planes b) const -> llvm::Value* {
    llvm::Value* const a_z = And(a.unknown, Not(a.value));
    llvm::Value* const b_z = And(b.unknown, Not(b.value));
    llvm::Value* const wildcard = Or(a_z, b_z);
    llvm::Value* const values_differ = Xor(a.value, b.value);
    llvm::Value* const unknowns_differ = Xor(a.unknown, b.unknown);
    llvm::Value* const differ = Or(values_differ, unknowns_differ);
    return b_->CreateIsNull(And(differ, Not(wildcard)));
  }

  // LRM 12.5.1 `casex`: x or z on either side is a wildcard.
  [[nodiscard]] auto CasexMatch(Planes a, Planes b) const -> llvm::Value* {
    llvm::Value* const differ = Xor(a.value, b.value);
    return b_->CreateIsNull(And(differ, Not(Or(a.unknown, b.unknown))));
  }

  // LRM 11.4.7: a definitively-one bit settles the value as nonzero however
  // many unknown bits sit beside it.
  [[nodiscard]] auto TruthOf(Planes a) const -> Truth {
    llvm::Value* const nonzero = AnySet(And(a.value, Not(a.unknown)));
    llvm::Value* const not_nonzero = Not(nonzero);
    return Truth{
        .nonzero = nonzero, .unknown = And(not_nonzero, AnySet(a.unknown))};
  }

  // LRM 11.4.7: the answer is unknown only where an operand's truth is and
  // the other operand does not settle it.
  [[nodiscard]] auto LogicalAnd(Truth a, Truth b) const -> Planes {
    llvm::Value* const one = And(a.nonzero, b.nonzero);
    llvm::Value* const a_not_zero = Or(a.nonzero, a.unknown);
    llvm::Value* const b_not_zero = Or(b.nonzero, b.unknown);
    llvm::Value* const not_zero = And(a_not_zero, b_not_zero);
    return Planes{.value = not_zero, .unknown = And(not_zero, Not(one))};
  }

  [[nodiscard]] auto LogicalOr(Truth a, Truth b) const -> Planes {
    llvm::Value* const one = Or(a.nonzero, b.nonzero);
    llvm::Value* const a_not_zero = Or(a.nonzero, a.unknown);
    llvm::Value* const b_not_zero = Or(b.nonzero, b.unknown);
    llvm::Value* const not_zero = Or(a_not_zero, b_not_zero);
    return Planes{.value = not_zero, .unknown = And(not_zero, Not(one))};
  }

  [[nodiscard]] auto LogicalNot(Truth a) const -> Planes {
    return Planes{.value = Not(a.nonzero), .unknown = a.unknown};
  }

  [[nodiscard]] auto LogicalEquivalence(Truth a, Truth b) const -> Planes {
    llvm::Value* const unknown = Or(a.unknown, b.unknown);
    return Planes{
        .value = Or(unknown, b_->CreateICmpEQ(a.nonzero, b.nonzero)),
        .unknown = unknown};
  }

  // LRM 11.4.9: a known 0 settles an and-reduction and a known 1 an
  // or-reduction whatever else the operand holds; an x or z anywhere makes an
  // xor-reduction x.
  [[nodiscard]] auto Reduced(Planes a, value::ReductionOp op) const -> Planes {
    llvm::Value* const known = Not(a.unknown);
    llvm::Value* const any_unknown = AnySet(a.unknown);
    const auto and_bit = [&] {
      llvm::Value* const no_zero = b_->CreateIsNull(And(Not(a.value), known));
      return Planes{.value = no_zero, .unknown = And(no_zero, any_unknown)};
    };
    const auto or_bit = [&] {
      llvm::Value* const any_one = AnySet(And(a.value, known));
      return Planes{
          .value = Or(any_one, any_unknown),
          .unknown = And(Not(any_one), any_unknown)};
    };
    const auto xor_bit = [&] {
      llvm::Value* const ones =
          b_->CreateUnaryIntrinsic(llvm::Intrinsic::ctpop, And(a.value, known));
      llvm::Value* const parity = b_->CreateTrunc(ones, b_->getInt1Ty());
      return Planes{.value = Or(any_unknown, parity), .unknown = any_unknown};
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
      Planes a, std::uint64_t width, Planes amount, ShiftKind kind,
      bool holds_unknown) const -> Planes {
    llvm::Type* const type = a.value->getType();
    // The amount is compared at a width that holds both it and the operand's
    // own width.
    llvm::Type* const counted = b_->getIntNTy(
        std::max(amount.value->getType()->getIntegerBitWidth(), 64U));
    llvm::Value* const by = b_->CreateZExt(amount.value, counted);
    llvm::Value* const past =
        b_->CreateICmpUGE(by, llvm::ConstantInt::get(counted, width));
    const auto within = [&](std::uint64_t saturated) {
      return b_->CreateZExtOrTrunc(
          b_->CreateSelect(
              past, llvm::ConstantInt::get(counted, saturated), by),
          type);
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
      llvm::Value* const unknown = AnySet(amount.unknown);
      const Planes moved{.value = plane(a.value), .unknown = plane(a.unknown)};
      return AllXWhere(unknown, moved, holds_unknown);
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

  // LRM 11.4.12.1: as many copies of the operand as fill the answer, laid end
  // to end. What is laid so far is laid again above itself until it fills the
  // answer, so the steps follow the logarithm of the count, and a copy past
  // the answer's width is moved out. A two-state answer takes every x or z as
  // 0.
  [[nodiscard]] auto Replicated(
      Planes a, std::uint64_t width, std::uint64_t answer_width,
      bool holds_unknown) const -> Planes {
    llvm::Type* const type = b_->getIntNTy(Bits(answer_width));
    const auto laid = [&](llvm::Value* plane) {
      llvm::Value* filled = b_->CreateZExtOrTrunc(plane, type);
      if (IsClear(filled)) {
        return filled;
      }
      for (std::uint64_t have = width; have < answer_width; have *= 2) {
        filled = b_->CreateOr(
            filled, b_->CreateShl(filled, llvm::ConstantInt::get(type, have)));
      }
      return filled;
    };
    if (!holds_unknown) {
      return Known(laid(KnownBits(a)));
    }
    return Planes{.value = laid(a.value), .unknown = laid(a.unknown)};
  }

  [[nodiscard]] auto PositionNamed(
      Planes index, const value::IntegralShape& shape) const -> NamedPosition {
    const bool is_signed = shape.signedness == value::Signedness::kSigned;
    llvm::Type* const word = b_->getInt64Ty();
    llvm::Type* const held = index.value->getType();
    llvm::Value* const at = is_signed
                                ? b_->CreateSExtOrTrunc(index.value, word)
                                : b_->CreateZExtOrTrunc(index.value, word);
    llvm::Value* named = b_->CreateIsNull(index.unknown);
    // An index wider than the number names a position only where the number
    // holds all of it.
    if (shape.width > 64) {
      named = And(
          named,
          b_->CreateICmpEQ(
              is_signed ? b_->CreateSExt(at, held) : b_->CreateZExt(at, held),
              index.value));
    }
    if (!is_signed && shape.width >= 64) {
      named = And(named, b_->CreateICmpSGE(at, Clear(word)));
    }
    // An index no wider than this cannot hold a magnitude past the limit.
    if (shape.width > 32) {
      llvm::Value* const not_below = b_->CreateICmpSGE(
          at, llvm::ConstantInt::getSigned(word, -value::kPositionLimit));
      llvm::Value* const not_above = b_->CreateICmpSLE(
          at, llvm::ConstantInt::getSigned(word, value::kPositionLimit));
      named = And(named, And(not_below, not_above));
    }
    return NamedPosition{
        .at = at, .named = named, .can_be_negative = is_signed};
  }

  // LRM 11.5.1: as many bits as the answer is wide, from the position named,
  // counted from the least significant bit. A position outside the value
  // reads x, or 0 in a two-state answer, and so does every bit where the
  // position names none or the part lies wholly outside.
  [[nodiscard]] auto Slice(
      Planes source, std::uint64_t source_width, const NamedPosition& position,
      std::uint64_t answer_width, bool holds_unknown) const -> Planes {
    llvm::Type* const word = b_->getInt64Ty();
    llvm::Type* const type = b_->getIntNTy(Bits(answer_width));
    // A plane is moved at a width that holds the source and the answer both.
    llvm::Type* const wide =
        b_->getIntNTy(Bits(std::max(source_width, answer_width)));
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
          reaches, taken(b_->CreateZExt(KnownBits(source), wide)),
          Clear(type)));
    }
    llvm::Value* const outside = Not(taken(LowBits(wide, source_width)));
    return Planes{
        .value = Select(
            reaches, Or(taken(b_->CreateZExt(source.value, wide)), outside),
            Full(type)),
        .unknown = Select(
            reaches, Or(taken(b_->CreateZExt(source.unknown, wide)), outside),
            Full(type))};
  }

  // LRM 11.5.1: `bits` written into `target` at the position named, every
  // other position left as it stands; a position outside the value is not
  // written, and none is where the position names none. Bits with no x
  // settle the positions they land on, and an x or z lands in a two-state
  // target as 0.
  [[nodiscard]] auto WithSlice(
      Planes target, std::uint64_t target_width, const NamedPosition& position,
      Planes bits, std::uint64_t bits_width, bool holds_unknown) const
      -> Planes {
    llvm::Type* const word = b_->getInt64Ty();
    llvm::Type* const type = target.value->getType();
    // A plane is moved at a width that holds the target and the bits both.
    llvm::Type* const wide =
        b_->getIntNTy(Bits(std::max(target_width, bits_width)));
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
            And(placed(LowBits(wide, bits_width)), LowBits(wide, target_width)),
            type),
        Clear(type));
    const auto replaced = [&](llvm::Value* kept, llvm::Value* plane) {
      llvm::Value* const kept_bits = And(kept, Not(written));
      llvm::Value* const placed_bits =
          b_->CreateTrunc(placed(b_->CreateZExt(plane, wide)), type);
      return Or(kept_bits, And(placed_bits, written));
    };
    if (!holds_unknown) {
      return Known(replaced(target.value, KnownBits(bits)));
    }
    return Planes{
        .value = replaced(target.value, bits.value),
        .unknown = replaced(target.unknown, bits.unknown)};
  }

  // LRM 6.24.1, 10.7: the source's bits where it reaches and, above them, its
  // sign where a signed source widens -- an x or z sign filling with itself.
  // A two-state answer takes every x or z as 0.
  [[nodiscard]] auto Converted(
      Planes source, bool is_signed, std::uint64_t answer_width,
      bool holds_unknown) const -> Planes {
    llvm::Type* const type = b_->getIntNTy(Bits(answer_width));
    const auto resized = [&](llvm::Value* plane) {
      return is_signed ? b_->CreateSExtOrTrunc(plane, type)
                       : b_->CreateZExtOrTrunc(plane, type);
    };
    return Settled(
        Planes{
            .value = resized(source.value), .unknown = resized(source.unknown)},
        holds_unknown);
  }

  // The value's low 64 bits as a number, sign-extended from its width when
  // signed and narrower. An x or z bit reads as 0 (LRM 6.12.1).
  [[nodiscard]] auto ToInt64(Planes a, bool is_signed) const -> llvm::Value* {
    llvm::Value* const known = KnownBits(a);
    return is_signed ? b_->CreateSExtOrTrunc(known, b_->getInt64Ty())
                     : b_->CreateZExtOrTrunc(known, b_->getInt64Ty());
  }

  // LRM 11.4.11 Table 11-20: a position both arms know and agree on survives,
  // and every other position is x -- 0 in a type that holds none.
  [[nodiscard]] auto MergeConditional(
      Planes a, Planes b, bool holds_unknown) const -> Planes {
    llvm::Value* const values_agree = Not(Xor(a.value, b.value));
    llvm::Value* const both_known = Not(Or(a.unknown, b.unknown));
    llvm::Value* const agreed = And(values_agree, both_known);
    llvm::Value* const kept = And(a.value, agreed);
    if (!holds_unknown) {
      return Known(kept);
    }
    return Planes{.value = Or(kept, Not(agreed)), .unknown = Not(agreed)};
  }

 private:
  [[nodiscard]] static auto Bits(std::uint64_t count) -> unsigned {
    return static_cast<unsigned>(count);
  }

  [[nodiscard]] static auto LowBits(llvm::Type* type, std::uint64_t count)
      -> llvm::Value* {
    return llvm::ConstantInt::get(
        type,
        llvm::APInt::getLowBitsSet(type->getIntegerBitWidth(), Bits(count)));
  }

  // The value plane as a type with no x holds it: every x or z as 0 (LRM
  // 6.11.2).
  [[nodiscard]] auto KnownBits(Planes a) const -> llvm::Value* {
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

  // A plane moved by the position: by `toward` where the position is not
  // negative and by `back` its magnitude where it is. A position that reaches
  // nothing moves by an amount the caller's own test discards, so the amount
  // is only kept inside the plane.
  [[nodiscard]] auto Moved(
      llvm::Value* plane, const NamedPosition& position,
      llvm::Instruction::BinaryOps toward,
      llvm::Instruction::BinaryOps back) const -> llvm::Value* {
    if (IsClear(plane)) {
      return plane;
    }
    llvm::Type* const type = plane->getType();
    const auto within = [&](llvm::Value* by) {
      llvm::Value* const fits =
          b_->CreateICmpULT(by, b_->getInt64(type->getIntegerBitWidth()));
      llvm::Value* const amount = b_->CreateSelect(fits, by, b_->getInt64(0));
      return b_->CreateZExtOrTrunc(amount, type);
    };
    llvm::Value* const forward =
        b_->CreateBinOp(toward, plane, within(position.at));
    if (!position.can_be_negative) {
      return forward;
    }
    llvm::Value* const ahead = b_->CreateICmpSGE(position.at, b_->getInt64(0));
    llvm::Value* const backward =
        b_->CreateBinOp(back, plane, within(b_->CreateNeg(position.at)));
    return b_->CreateSelect(ahead, forward, backward);
  }

  llvm::IRBuilderBase* b_;
};

// The low `count` positions of an integer of `type` set, for a `count` below
// the type's width, so a run of positions is two of these apart.
auto LowPositions(
    llvm::IRBuilderBase& builder, llvm::Value* count, llvm::Type* type)
    -> llvm::Value* {
  llvm::Value* const one = llvm::ConstantInt::get(type, 1);
  return builder.CreateSub(
      builder.CreateShl(one, builder.CreateZExt(count, type)), one);
}

// How many words a part of `width` bits can span: those its width fills, and
// one more where it does not start at a word's first position.
[[nodiscard]] auto WordsSpannedBy(std::uint64_t width) -> std::uint64_t {
  return value::WordCountForBits(width) + 1;
}

// The first `count` 64-bit words of `bits`, an integer of any width, low
// first, those past its width clear.
auto WordsOf(
    llvm::IRBuilderBase& builder, llvm::Value* bits, std::uint64_t count)
    -> std::vector<llvm::Value*> {
  const std::uint64_t width = bits->getType()->getIntegerBitWidth();
  llvm::Type* const word = builder.getInt64Ty();
  std::vector<llvm::Value*> words;
  words.reserve(count);
  for (std::uint64_t k = 0; k < count; ++k) {
    words.push_back(
        64 * k >= width ? Clear(word)
                        : builder.CreateZExtOrTrunc(
                              builder.CreateLShr(bits, 64 * k), word));
  }
  return words;
}

// The integer of `type` whose 64-bit words, low first, are `words`.
auto Joined(
    llvm::IRBuilderBase& builder, std::span<llvm::Value* const> words,
    llvm::Type* type) -> llvm::Value* {
  const std::uint64_t width = type->getIntegerBitWidth();
  llvm::Value* joined = Clear(type);
  for (std::uint64_t k = 0; k < words.size() && 64 * k < width; ++k) {
    llvm::Value* const piece = builder.CreateZExtOrTrunc(words[k], type);
    joined = builder.CreateOr(
        joined, k == 0 ? piece : builder.CreateShl(piece, 64 * k));
  }
  return joined;
}

// `number` held between `lowest` and `highest`, both included.
auto Clamped(
    llvm::IRBuilderBase& builder, llvm::Value* number, std::int64_t lowest,
    std::int64_t highest) -> llvm::Value* {
  llvm::Value* const low = builder.getInt64(lowest);
  llvm::Value* const high = builder.getInt64(highest);
  llvm::Value* const raised =
      builder.CreateSelect(builder.CreateICmpSLT(number, low), low, number);
  return builder.CreateSelect(
      builder.CreateICmpSGT(raised, high), high, raised);
}

// The positions of word `index` -- those from 64 times it on -- that lie in
// [from, to), as a word.
auto PositionsOfWordIn(
    llvm::IRBuilderBase& builder, llvm::Value* index, llvm::Value* from,
    llvm::Value* to) -> llvm::Value* {
  llvm::Value* const first = builder.CreateShl(index, 6);
  llvm::Value* const below =
      Clamped(builder, builder.CreateSub(from, first), 0, 64);
  llvm::Value* const until =
      Clamped(builder, builder.CreateSub(to, first), 0, 64);
  llvm::Type* const pair = builder.getInt128Ty();
  return builder.CreateTrunc(
      builder.CreateAnd(
          LowPositions(builder, until, pair),
          builder.CreateNot(LowPositions(builder, below, pair))),
      builder.getInt64Ty());
}

// Where bits reached through `parts`, outermost first, start in the value of
// `whole`, and the positions of the value they may reach: inside the value,
// and inside every part they are reached through (LRM 11.5.1). Each part's
// start is a position in what it is a part of; one naming no position names
// nothing.
struct ReachedBits {
  llvm::Value* start;
  llvm::Value* from;
  llvm::Value* to;
  llvm::Value* named;
};

auto ReachOfParts(
    llvm::IRBuilderBase& builder, const value::IntegralShape& whole,
    std::span<const PartOfValue> parts) -> ReachedBits {
  const PlaneArithmetic arithmetic(builder);
  ReachedBits reach{
      .start = builder.getInt64(0),
      .from = builder.getInt64(0),
      .to = builder.getInt64(static_cast<std::int64_t>(whole.width)),
      .named = builder.getTrue()};
  for (const PartOfValue& part : parts) {
    const NamedPosition position =
        arithmetic.PositionNamed(part.start, part.start_shape);
    reach.named = builder.CreateAnd(reach.named, position.named);
    reach.start = builder.CreateAdd(reach.start, position.at);
    llvm::Value* const end = builder.CreateAdd(
        reach.start, builder.getInt64(static_cast<std::int64_t>(part.width)));
    reach.from = builder.CreateSelect(
        builder.CreateICmpSGT(reach.start, reach.from), reach.start,
        reach.from);
    reach.to = builder.CreateSelect(
        builder.CreateICmpSLT(end, reach.to), end, reach.to);
  }
  return reach;
}

}  // namespace

// LRM 11.5.1: as many bits as `answer` is wide of the value of `whole` whose
// words are `words`, reached through `parts`, outermost first, the last of
// which is as wide as `answer`. Only the words the bits span are read, and
// each word of the answer is the two words it lies across funneled by where
// the bits start in a word, so no integer wider than a word is shifted. A
// position outside the value or outside a part the bits are reached through
// reads x, or 0 in a two-state answer, and so does every bit where a part
// names no position.
auto ReadPartOfWords(
    llvm::IRBuilderBase& builder, const ValueWords& words,
    const value::IntegralShape& whole, std::span<const PartOfValue> parts,
    const value::IntegralShape& answer) -> Planes {
  const ReachedBits reach = ReachOfParts(builder, whole, parts);
  const std::uint64_t answer_words = value::WordCountForBits(answer.width);
  llvm::Type* const word = builder.getInt64Ty();
  llvm::Type* const type =
      builder.getIntNTy(static_cast<unsigned>(answer.width));
  const auto count = static_cast<std::int64_t>(answer.width);
  llvm::Value* const low = builder.CreateAShr(reach.start, 6);
  llvm::Value* const shift = builder.CreateAnd(reach.start, 63);
  // Each word the bits can span, read once.
  const auto read = [&](Plane plane) {
    std::vector<llvm::Value*> spanned;
    for (std::uint64_t k = 0; k <= answer_words; ++k) {
      spanned.push_back(words.Read(
          plane, builder.CreateAdd(
                     low, builder.getInt64(static_cast<std::int64_t>(k)))));
    }
    return spanned;
  };
  const std::vector<llvm::Value*> value_words = read(Plane::kValue);
  const std::vector<llvm::Value*> unknown_words = read(Plane::kUnknown);
  const auto taken = [&](const std::vector<llvm::Value*>& spanned,
                         std::uint64_t k) -> llvm::Value* {
    return builder.CreateIntrinsic(
        llvm::Intrinsic::fshr, {word}, {spanned[k + 1], spanned[k], shift});
  };
  // The answer's positions the bits may reach, counted from the answer's own
  // least significant bit.
  llvm::Value* const below =
      Clamped(builder, builder.CreateSub(reach.from, reach.start), 0, count);
  llvm::Value* const until =
      Clamped(builder, builder.CreateSub(reach.to, reach.start), 0, count);
  std::vector<llvm::Value*> value_answer;
  std::vector<llvm::Value*> unknown_answer;
  for (std::uint64_t k = 0; k < answer_words; ++k) {
    llvm::Value* const inside = builder.CreateSelect(
        reach.named,
        PositionsOfWordIn(
            builder, builder.getInt64(static_cast<std::int64_t>(k)), below,
            until),
        Clear(word));
    llvm::Value* const v = taken(value_words, k);
    llvm::Value* const u = taken(unknown_words, k);
    if (!answer.IsFourState()) {
      value_answer.push_back(builder.CreateAnd(
          builder.CreateAnd(v, builder.CreateNot(u)), inside));
      continue;
    }
    llvm::Value* const outside = builder.CreateNot(inside);
    value_answer.push_back(builder.CreateOr(v, outside));
    unknown_answer.push_back(builder.CreateOr(u, outside));
  }
  if (!answer.IsFourState()) {
    return Known(Joined(builder, value_answer, type));
  }
  return Planes{
      .value = Joined(builder, value_answer, type),
      .unknown = Joined(builder, unknown_answer, type)};
}

namespace {

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
  llvm::Value* const value_bit = builder.CreateShl(bit, value::kScalarValueBit);
  llvm::Value* const unknown_bit =
      builder.CreateShl(unknown, value::kScalarUnknownBit);
  return builder.CreateOr(value_bit, unknown_bit);
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
    llvm::Value* const shifted = builder.CreateLShr(scalar, position);
    return builder.CreateAnd(shifted, builder.getInt8(1));
  };
  builder.CreateStore(plane_bit(value::kScalarValueBit), at);
  llvm::Value* const unknown_bit = plane_bit(value::kScalarUnknownBit);
  builder.CreateStore(unknown_bit, UnknownPlaneOfOneBit(builder, at));
}

auto LowerOnPlanes(
    llvm::IRBuilderBase& builder, IntegralOp op, const IntegralShapes& shapes,
    const PlaneOperands& operands) -> std::optional<PlaneAnswer> {
  const auto every_value = [&](auto holds) {
    const auto held = [&](const std::optional<value::IntegralShape>& shape) {
      return !shape.has_value() || holds(*shape);
    };
    return std::ranges::all_of(shapes.operands, held) && held(shapes.answer);
  };
  if (!every_value(IsAtMostTwoWords)) {
    return std::nullopt;
  }
  const PlaneArithmetic words(builder);

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
  const auto answer_bit = [&](Planes bit) {
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
  // An operation of the two operands, loaded first to last.
  const auto binary = [&](auto operation) {
    const Planes lhs = load(0);
    const Planes rhs = load(1);
    return std::invoke(operation, words, lhs, rhs);
  };

  const auto arithmetic = [&](llvm::Instruction::BinaryOps instruction) {
    const Planes lhs = load(0);
    const Planes rhs = load(1);
    const bool holds_unknown = answers_unknown();
    return words.Arithmetic(instruction, lhs, rhs, holds_unknown);
  };
  const auto divided = [&](bool remainder) {
    const Planes dividend = load(0);
    const Planes divisor = load(1);
    const bool is_signed = reads_signed(0);
    const bool holds_unknown = answers_unknown();
    return words.Divided(
        dividend, divisor, is_signed, remainder, holds_unknown);
  };
  const auto relation = [&](llvm::CmpInst::Predicate as_signed,
                            llvm::CmpInst::Predicate as_unsigned) {
    const llvm::CmpInst::Predicate predicate =
        reads_signed(0) ? as_signed : as_unsigned;
    const Planes lhs = load(0);
    const Planes rhs = load(1);
    return answer_bit(words.Relation(predicate, lhs, rhs));
  };
  const auto reduced = [&](value::ReductionOp reduction) {
    return answer_bit(words.Reduced(load(0), reduction));
  };
  const auto shifted = [&](ShiftKind kind) {
    const Planes operand = load(0);
    const std::uint64_t operand_width = width(0);
    const Planes amount = load(1);
    const bool holds_unknown = answers_unknown();
    return words.Shifted(operand, operand_width, amount, kind, holds_unknown);
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
    case IntegralOp::kNegate: {
      const Planes operand = load(0);
      const bool holds_unknown = answers_unknown();
      return words.Negated(operand, holds_unknown);
    }
    case IntegralOp::kBitwiseAnd:
      return binary(&PlaneArithmetic::BitwiseAnd);
    case IntegralOp::kBitwiseOr:
      return binary(&PlaneArithmetic::BitwiseOr);
    case IntegralOp::kBitwiseXor:
      return binary(&PlaneArithmetic::BitwiseXor);
    case IntegralOp::kBitwiseXnor:
      return binary(&PlaneArithmetic::BitwiseXnor);
    case IntegralOp::kBitwiseNot:
      return words.BitwiseNot(load(0));
    case IntegralOp::kEqual:
      return answer_bit(binary(&PlaneArithmetic::Equality));
    case IntegralOp::kNotEqual:
      return answer_bit(words.Inverted(binary(&PlaneArithmetic::Equality)));
    case IntegralOp::kLess:
      return relation(llvm::CmpInst::ICMP_SLT, llvm::CmpInst::ICMP_ULT);
    case IntegralOp::kLessEqual:
      return relation(llvm::CmpInst::ICMP_SLE, llvm::CmpInst::ICMP_ULE);
    case IntegralOp::kGreater:
      return relation(llvm::CmpInst::ICMP_SGT, llvm::CmpInst::ICMP_UGT);
    case IntegralOp::kGreaterEqual:
      return relation(llvm::CmpInst::ICMP_SGE, llvm::CmpInst::ICMP_UGE);
    case IntegralOp::kCaseEqual:
      return Known(binary(&PlaneArithmetic::CaseEquality));
    case IntegralOp::kWildcardEqual:
      return answer_bit(binary(&PlaneArithmetic::WildcardEquality));
    case IntegralOp::kCasezMatch:
      return Known(binary(&PlaneArithmetic::CasezMatch));
    case IntegralOp::kCasexMatch:
      return Known(binary(&PlaneArithmetic::CasexMatch));
    case IntegralOp::kLogicalAnd: {
      const Truth lhs = words.TruthOf(load(0));
      const Truth rhs = words.TruthOf(load(1));
      return answer_bit(words.LogicalAnd(lhs, rhs));
    }
    case IntegralOp::kLogicalOr: {
      const Truth lhs = words.TruthOf(load(0));
      const Truth rhs = words.TruthOf(load(1));
      return answer_bit(words.LogicalOr(lhs, rhs));
    }
    case IntegralOp::kLogicalNot:
      return answer_bit(words.LogicalNot(words.TruthOf(load(0))));
    case IntegralOp::kLogicalEquivalence: {
      const Truth lhs = words.TruthOf(load(0));
      const Truth rhs = words.TruthOf(load(1));
      return answer_bit(words.LogicalEquivalence(lhs, rhs));
    }
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
    case IntegralOp::kReplicate: {
      const Planes operand = load(0);
      const std::uint64_t operand_width = width(0);
      const std::uint64_t replicated_width = answer_width();
      const bool holds_unknown = answers_unknown();
      return words.Replicated(
          operand, operand_width, replicated_width, holds_unknown);
    }
    case IntegralOp::kSlice: {
      if (!IsSingleWord(shape(0))) {
        const std::array parts{PartOfValue{
            .start = load(1),
            .start_shape = shape(1),
            .width = shapes.answer->width}};
        return ReadPartOfWords(
            builder, operands.words(0), shape(0), parts, *shapes.answer);
      }
      const Planes source = load(0);
      const std::uint64_t source_width = width(0);
      const Planes index = load(1);
      const value::IntegralShape& index_shape = shape(1);
      const NamedPosition position = words.PositionNamed(index, index_shape);
      const std::uint64_t part_width = answer_width();
      const bool holds_unknown = answers_unknown();
      return words.Slice(
          source, source_width, position, part_width, holds_unknown);
    }
    case IntegralOp::kWithSlice: {
      const Planes target = load(0);
      const std::uint64_t target_width = width(0);
      const Planes index = load(1);
      const value::IntegralShape& index_shape = shape(1);
      const NamedPosition position = words.PositionNamed(index, index_shape);
      const Planes bits = load(2);
      const std::uint64_t bits_width = width(2);
      const bool holds_unknown = answers_unknown();
      return words.WithSlice(
          target, target_width, position, bits, bits_width, holds_unknown);
    }
    case IntegralOp::kConvert: {
      const Planes source = load(0);
      const bool is_signed = reads_signed(0);
      const std::uint64_t converted_width = answer_width();
      const bool holds_unknown = answers_unknown();
      return words.Converted(source, is_signed, converted_width, holds_unknown);
    }
    // LRM 11.6.1: a number carried into the answer's type keeps its low bits,
    // and its sign above them where the type is wider.
    case IntegralOp::kFromInt: {
      llvm::Value* const number = machine(64);
      llvm::Type* const type = exact();
      return Known(builder.CreateSExtOrTrunc(number, type));
    }
    case IntegralOp::kFromBool: {
      llvm::Value* const bit = machine(1);
      llvm::Type* const type = exact();
      return Known(builder.CreateZExt(bit, type));
    }
    case IntegralOp::kToInt64: {
      const Planes operand = load(0);
      const bool is_signed = reads_signed(0);
      return words.ToInt64(operand, is_signed);
    }
    // The position an index names, in the type position arithmetic is done
    // in: all x where it names none.
    case IntegralOp::kToPosition: {
      const Planes index = load(0);
      const value::IntegralShape& index_shape = shape(0);
      const NamedPosition position = words.PositionNamed(index, index_shape);
      llvm::Value* const names_none = words.Not(position.named);
      const bool holds_unknown = answers_unknown();
      return words.AllXWhere(names_none, Known(position.at), holds_unknown);
    }
    case IntegralOp::kHasUnknown:
      return words.AnySet(load(0).unknown);
    case IntegralOp::kBitIdentical:
      return binary(&PlaneArithmetic::CaseEquality);
    case IntegralOp::kMergeConditional: {
      const Planes lhs = load(0);
      const Planes rhs = load(1);
      const bool holds_unknown = answers_unknown();
      return words.MergeConditional(lhs, rhs, holds_unknown);
    }
    // LRM 20.9 `$countbits`: the positions of the first operand holding one of
    // the scalars some position of the control names, each scalar counted
    // once however many positions name it.
    case IntegralOp::kCountBits: {
      const Planes a = load(0);
      const Planes control = load(1);
      const auto names = [&](llvm::Value* positions) {
        return builder.CreateICmpNE(positions, Clear(positions->getType()));
      };
      const auto counted_where = [&](llvm::Value* control_positions,
                                     llvm::Value* positions) {
        return builder.CreateSelect(
            names(control_positions), positions, Clear(positions->getType()));
      };
      llvm::Value* const v = a.value;
      llvm::Value* const u = a.unknown;
      llvm::Value* const cv = control.value;
      llvm::Value* const cu = control.unknown;
      llvm::Value* const not_v = builder.CreateNot(v);
      llvm::Value* const not_u = builder.CreateNot(u);
      llvm::Value* const not_cv = builder.CreateNot(cv);
      llvm::Value* const not_cu = builder.CreateNot(cu);
      llvm::Value* const counted = builder.CreateOr(
          builder.CreateOr(
              counted_where(
                  builder.CreateAnd(not_cv, not_cu),
                  builder.CreateAnd(not_v, not_u)),
              counted_where(
                  builder.CreateAnd(cv, not_cu), builder.CreateAnd(v, not_u))),
          builder.CreateOr(
              counted_where(
                  builder.CreateAnd(not_cv, cu), builder.CreateAnd(not_v, u)),
              counted_where(
                  builder.CreateAnd(cv, cu), builder.CreateAnd(v, u))));
      llvm::Value* const count =
          builder.CreateUnaryIntrinsic(llvm::Intrinsic::ctpop, counted);
      llvm::Type* const type = exact();
      return Known(builder.CreateZExtOrTrunc(count, type));
    }
    // LRM 20.8.1 `$clog2`: the operand read as unsigned, an x or z bit read as
    // 0, and 0 answering 0.
    case IntegralOp::kCeilLog2: {
      const Planes a = load(0);
      llvm::Value* const known =
          builder.CreateAnd(a.value, builder.CreateNot(a.unknown));
      llvm::Type* const number = builder.getInt64Ty();
      llvm::Value* const leading = builder.CreateZExtOrTrunc(
          builder.CreateBinaryIntrinsic(
              llvm::Intrinsic::ctlz, known, builder.getFalse()),
          number);
      llvm::Value* const set = builder.CreateZExtOrTrunc(
          builder.CreateUnaryIntrinsic(llvm::Intrinsic::ctpop, known), number);
      llvm::Value* const highest = builder.CreateSub(
          builder.getInt64(static_cast<std::int64_t>(width(0)) - 1), leading);
      llvm::Value* const rounded_up = builder.CreateAdd(
          highest,
          builder.CreateZExt(
              builder.CreateICmpUGT(set, builder.getInt64(1)), number));
      llvm::Value* const answer = builder.CreateSelect(
          builder.CreateICmpEQ(set, builder.getInt64(0)), builder.getInt64(0),
          rounded_up);
      llvm::Type* const type = exact();
      return Known(builder.CreateTrunc(answer, type));
    }
    // Long work at every width, which the library carries out: a power, a
    // reversal of blocks of a size the program computes, and a value read out
    // of text or of what a foreign call left. The positions a procedural
    // continuous assignment shows are the library's too, since only a write it
    // overrides asks for them, and so are the positions a net resolved, which
    // only a net showing them asks to have laid in.
    case IntegralOp::kTakePositions:
    case IntegralOp::kWithPositions:
    case IntegralOp::kPower:
    case IntegralOp::kReverseBlocks:
    case IntegralOp::kFromText:
    case IntegralOp::kReadCanonicalBits:
    case IntegralOp::kReadCanonicalLogic:
    case IntegralOp::kFromSvLogic:
      return std::nullopt;
  }
  throw InternalError("llvm codegen: unknown integral operation");
}

WordsLyingAt::WordsLyingAt(
    llvm::IRBuilderBase& builder, llvm::Value* at,
    const value::IntegralShape& shape)
    : builder_(&builder), at_(at), shape_(shape) {
}

auto WordsLyingAt::Holds(llvm::Value* index) const -> llvm::Value* {
  return builder_->CreateICmpULT(
      index, builder_->getInt64(value::WordCountForBits(shape_.width)));
}

auto WordsLyingAt::PlaneAt(Plane plane) const -> llvm::Value* {
  switch (plane) {
    case Plane::kValue:
      return at_;
    case Plane::kUnknown:
      return shape_.IsFourState() ? builder_->CreateConstInBoundsGEP1_64(
                                        builder_->getInt8Ty(), at_,
                                        value::PlaneBytesFor(shape_.width))
                                  : nullptr;
  }
  throw InternalError("llvm codegen: unknown plane");
}

// Where the word `index` names lies, or the first word where it names none,
// which is read and written back unchanged.
auto WordsLyingAt::WordAt(Plane plane, llvm::Value* index) const
    -> llvm::Value* {
  llvm::Value* const within =
      builder_->CreateSelect(Holds(index), index, builder_->getInt64(0));
  return builder_->CreateInBoundsGEP(
      builder_->getInt64Ty(), PlaneAt(plane), within);
}

auto WordsLyingAt::Read(Plane plane, llvm::Value* index) const -> llvm::Value* {
  llvm::Type* const word = builder_->getInt64Ty();
  if (PlaneAt(plane) == nullptr) {
    return Clear(word);
  }
  llvm::Value* const loaded = builder_->CreateAlignedLoad(
      word, WordAt(plane, index),
      llvm::Align(value::IntegralAlignFor(shape_.width)));
  return builder_->CreateSelect(Holds(index), loaded, Clear(word));
}

void WordsLyingAt::Replace(
    Plane plane, llvm::Value* index, llvm::Value* bits,
    llvm::Value* mask) const {
  if (PlaneAt(plane) == nullptr) {
    return;
  }
  llvm::Type* const word = builder_->getInt64Ty();
  llvm::Value* const reached =
      builder_->CreateAnd(mask, builder_->CreateSExt(Holds(index), word));
  ReplaceAt(WordAt(plane, index), bits, reached);
}

void WordsLyingAt::ReplaceHeld(
    Plane plane, llvm::Value* index, llvm::Value* bits,
    llvm::Value* mask) const {
  if (PlaneAt(plane) == nullptr) {
    return;
  }
  ReplaceAt(
      builder_->CreateInBoundsGEP(
          builder_->getInt64Ty(), PlaneAt(plane), index),
      bits, mask);
}

void WordsLyingAt::ReplaceAt(
    llvm::Value* at, llvm::Value* bits, llvm::Value* mask) const {
  llvm::Type* const word = builder_->getInt64Ty();
  const llvm::Align align(value::IntegralAlignFor(shape_.width));
  llvm::Value* const kept = builder_->CreateAnd(
      builder_->CreateAlignedLoad(word, at, align), builder_->CreateNot(mask));
  builder_->CreateAlignedStore(
      builder_->CreateOr(kept, builder_->CreateAnd(bits, mask)), at, align);
}

void WritePartOfWords(
    llvm::IRBuilderBase& builder, const MutableValueWords& words,
    const value::IntegralShape& whole, std::span<const PartOfValue> parts,
    Planes bits, const value::IntegralShape& bits_shape) {
  const std::uint64_t spanned = WordsSpannedBy(bits_shape.width);
  const std::uint64_t source_words = value::WordCountForBits(bits_shape.width);
  llvm::Type* const word = builder.getInt64Ty();
  const ReachedBits reached = ReachOfParts(builder, whole, parts);
  llvm::Value* const start = reached.start;
  llvm::Value* const from = reached.from;
  llvm::Value* const to = reached.to;
  llvm::Value* const named = reached.named;
  // The bits as the words of the value they land in, each the two words of
  // the bits it lies across funneled by where they start in a word, so no
  // integer wider than a word is shifted.
  llvm::Value* const shift = builder.CreateAnd(start, 63);
  const auto placed = [&](llvm::Value* plane) {
    const std::vector<llvm::Value*> source =
        WordsOf(builder, plane, source_words);
    std::vector<llvm::Value*> landed;
    for (std::uint64_t k = 0; k < spanned; ++k) {
      llvm::Value* const high = k < source_words ? source[k] : Clear(word);
      llvm::Value* const below = k > 0 ? source[k - 1] : Clear(word);
      landed.push_back(builder.CreateIntrinsic(
          llvm::Intrinsic::fshl, {word}, {high, below, shift}));
    }
    return landed;
  };
  const std::vector<llvm::Value*> reach = placed(
      llvm::ConstantInt::get(
          builder.getIntNTy(static_cast<unsigned>(bits_shape.width)),
          llvm::APInt::getAllOnes(static_cast<unsigned>(bits_shape.width))));
  // Bits with no x settle where they land; a two-state value takes an x or z
  // as 0.
  const std::vector<llvm::Value*> value_landed = placed(
      whole.IsFourState()
          ? bits.value
          : builder.CreateAnd(bits.value, builder.CreateNot(bits.unknown)));
  const std::vector<llvm::Value*> unknown_landed = placed(bits.unknown);
  llvm::Value* const low = builder.CreateAShr(start, 6);
  const auto index_at = [&](std::int64_t offset) {
    return builder.CreateAdd(low, builder.getInt64(offset));
  };
  const auto replace = [&](std::int64_t offset, llvm::Value* mask,
                           const auto& replacing) {
    llvm::Value* const index = index_at(offset);
    const auto at = static_cast<std::size_t>(offset);
    (words.*replacing)(Plane::kValue, index, value_landed[at], mask);
    (words.*replacing)(Plane::kUnknown, index, unknown_landed[at], mask);
  };
  // Bits lying wholly inside the value and every part they are reached
  // through are written by the masks their width and start give, as a
  // bit-field is: every word they span is one the value has, and so is the
  // last word of the run where they reach it. Bits partly outside are written
  // word by word, each word asked whether the value has it and which of its
  // positions the bits may land on.
  llvm::Value* const end = builder.CreateAdd(
      start, builder.getInt64(static_cast<std::int64_t>(bits_shape.width)));
  llvm::Value* const inside = builder.CreateAnd(
      named,
      builder.CreateAnd(
          builder.CreateICmpSGE(start, from), builder.CreateICmpSLE(end, to)));
  llvm::Function* const function = builder.GetInsertBlock()->getParent();
  llvm::LLVMContext& context = builder.getContext();
  llvm::BasicBlock* const held =
      llvm::BasicBlock::Create(context, "", function);
  llvm::BasicBlock* const clipped =
      llvm::BasicBlock::Create(context, "", function);
  llvm::BasicBlock* const written =
      llvm::BasicBlock::Create(context, "", function);
  builder.CreateCondBr(inside, held, clipped);
  const auto last = static_cast<std::int64_t>(spanned) - 1;
  builder.SetInsertPoint(held);
  for (std::int64_t offset = 0; offset < last; ++offset) {
    replace(
        offset, reach[static_cast<std::size_t>(offset)],
        &MutableValueWords::ReplaceHeld);
  }
  replace(
      last, reach[static_cast<std::size_t>(last)], &MutableValueWords::Replace);
  builder.CreateBr(written);
  builder.SetInsertPoint(clipped);
  for (std::int64_t offset = 0; offset <= last; ++offset) {
    replace(
        offset,
        builder.CreateSelect(
            named,
            builder.CreateAnd(
                reach[static_cast<std::size_t>(offset)],
                PositionsOfWordIn(builder, index_at(offset), from, to)),
            builder.getInt64(0)),
        &MutableValueWords::Replace);
  }
  builder.CreateBr(written);
  builder.SetInsertPoint(written);
}

auto LoadPlanes(
    llvm::IRBuilderBase& builder, llvm::Value* at,
    const value::IntegralShape& shape) -> Planes {
  const std::size_t plane_bytes = value::PlaneBytesFor(shape.width);
  llvm::Type* const unit =
      builder.getIntNTy(static_cast<unsigned>(8 * plane_bytes));
  const llvm::Align align(value::IntegralAlignFor(shape.width));
  llvm::Type* const exact =
      builder.getIntNTy(static_cast<unsigned>(shape.width));
  const auto plane = [&](llvm::Value* plane_at) {
    return builder.CreateTrunc(
        builder.CreateAlignedLoad(unit, plane_at, align), exact);
  };
  llvm::Value* const value = plane(at);
  return Planes{
      .value = value,
      .unknown = shape.IsFourState()
                     ? plane(builder.CreateConstInBoundsGEP1_64(
                           builder.getInt8Ty(), at, plane_bytes))
                     : llvm::Constant::getNullValue(exact)};
}

void StorePlanes(
    llvm::IRBuilderBase& builder, Planes planes, llvm::Value* at,
    const value::IntegralShape& shape) {
  if (planes.value->getType()->getIntegerBitWidth() != shape.width) {
    throw InternalError(
        std::format(
            "llvm codegen: an operation on planes answers {} bits where "
            "its answer's type has {} -- please report this as a bug",
            planes.value->getType()->getIntegerBitWidth(), shape.width));
  }
  const std::size_t plane_bytes = value::PlaneBytesFor(shape.width);
  llvm::Type* const unit =
      builder.getIntNTy(static_cast<unsigned>(8 * plane_bytes));
  const llvm::Align align(value::IntegralAlignFor(shape.width));
  const auto plane = [&](llvm::Value* bits, llvm::Value* plane_at) {
    builder.CreateAlignedStore(builder.CreateZExt(bits, unit), plane_at, align);
  };
  plane(planes.value, at);
  if (shape.IsFourState()) {
    plane(
        planes.unknown, builder.CreateConstInBoundsGEP1_64(
                            builder.getInt8Ty(), at, plane_bytes));
  }
}

}  // namespace lyra::backend::llvm_backend
