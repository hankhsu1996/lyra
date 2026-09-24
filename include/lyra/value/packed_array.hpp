#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <string_view>

#include "lyra/value/concepts.hpp"
#include "lyra/value/packed.hpp"
#include "lyra/value/packed_type.hpp"

namespace lyra::value {

class PackedArrayRef;
class String;

// LRM 11.4.7 truth value of an integral. A definitively-one bit settles the
// value as nonzero however many unknown bits sit beside it, so `(1, x, x, x)`
// is known nonzero while `(x, x, x, x)` is unknown.
enum class Truthiness : std::uint8_t { kKnownZero, kKnownNonzero, kUnknown };

// Unified integral value: one type for every SystemVerilog integral -- `byte`,
// `shortint`, `int`, `longint`, `integer`, `time`, every `bit [N:0]` /
// `logic [N:0]` / `reg [N:0]`, and the multi-dimensional packed forms.
//
// A value carries what every operation on it needs: how many bits, whether they
// are read as signed, whether a position may hold x or z, and the bits. How a
// declaration divides those bits is one fact per declaration rather than one
// per value, so an operation that names a position inside a value takes it as
// an argument. That is what lets one run of bits answer as `bit [7:0]` at one
// site and as `bit [3:0][1:0]` at another, which is what the two declarations
// mean. Storage layout is private to this class and disjoint from the API
// contract.
class PackedArray {
 public:
  // Default constructor: a 0-bit empty value, not a valid SystemVerilog value.
  // It is the unestablished state of a freshly value-initialized cell and of
  // the default-constructed internal slots STL and the container wrappers
  // require; a later operation installs the declared representation before any
  // read.
  PackedArray();

  // A default value of a declared type, which is what generated code names at
  // a declaration. Only the width, signedness and state domain are read; how
  // the type divides those bits is the type's own business and reaches an
  // access as an operand.
  explicit PackedArray(PackedType type);

  // The same, where the caller holds the three facts rather than a declared
  // type -- an internal result, or a shape computed while running.
  PackedArray(std::uint64_t bit_width, bool is_signed, bool is_four_state);

  // Convenience factory for the default int shape (32-bit, signed, 2-state).
  // Matches SystemVerilog's default int literal type, so `int x = 5;` lowers
  // to `PackedArray x = PackedArray::Int(5);`.
  [[nodiscard]] static auto Int(std::int32_t value) -> PackedArray;

  // Default `int unsigned` shape (32-bit, unsigned, 2-state) per LRM 6.11
  // Table 6-8. A system function whose own prototype states this type rather
  // than leaving it to context answers with it (LRM 18.13.1).
  [[nodiscard]] static auto IntUnsigned(std::uint32_t value) -> PackedArray;

  // Default byte shape (8-bit, signed, 2-state) per LRM 6.11 Table 6-8.
  [[nodiscard]] static auto Byte(std::int8_t value) -> PackedArray;

  // Default integer shape (32-bit, signed, 4-state) per LRM 6.11 Table 6-8.
  [[nodiscard]] static auto Integer(std::int32_t value) -> PackedArray;

  // 1-bit unsigned 2-state shape. Used to lift a host `bool` (e.g. from
  // `HasUnknown()`) into the SV `bit` type at runtime-surface boundaries
  // such as `IsUnknown()`. Sibling of `Int(int32_t)` / `Byte`.
  [[nodiscard]] static auto Bit(bool value) -> PackedArray;

  // Same shape as `Bit`, but named after the call-site role: shapes the
  // host-bool result of a real / string comparison or logical operator into
  // the LRM 11.3 / 11.4 1-bit integral result. HIR-to-MIR emits this via
  // `BuiltinFn::kFromBool`; renamed at the call site so reading the emitted
  // C++ keeps the SV-level reason visible.
  [[nodiscard]] static auto FromBool(bool value) -> PackedArray;

  // 1-bit unsigned 4-state shape holding high impedance. The scalar a net's
  // positions take where nothing drives them (LRM 6.6), which is also what a
  // fold treats as its identity.
  [[nodiscard]] static auto HighImpedanceScalar() -> PackedArray;

  // Constructs a narrow PackedArray (bit_width <= 64) from an integer value:
  // bits above the width are masked out and the unknown plane is left at zero.
  // The result shape comes from a `PackedType` -- the destination's declared
  // representation -- or from a single bit width, an internal caller's
  // computed flat width.
  [[nodiscard]] static auto FromInt(std::int64_t value, const PackedType& type)
      -> PackedArray;
  [[nodiscard]] static auto FromInt(
      std::int64_t value, std::uint64_t bit_width, bool is_signed,
      bool is_four_state) -> PackedArray;

  // Construct a PackedArray of the declared type from raw word planes, for a
  // literal that does not fit a single int64 carrier: widths > 64 bits, or
  // 4-state literals carrying X/Z bits. `value_words` must have
  // ceil(bit_width / 64) entries; bits above `bit_width` in the top word are
  // masked. For 2-state shapes `unknown_words` must be empty; for 4-state
  // shapes it must either be empty (no X/Z) or match `value_words` in size.
  // Generated code passes the word planes as `std::array` spans and the shape
  // as a `PackedType`.
  [[nodiscard]] static auto FromWords(
      std::span<const std::uint64_t> value_words,
      std::span<const std::uint64_t> unknown_words, const PackedType& type)
      -> PackedArray;

  // Width-shorthand twin of FromWords for callers that assemble word planes
  // dynamically against a 1-D flat width (e.g. the $sscanf scanner
  // accumulating an arbitrary number of bits). Same word-plane contract.
  [[nodiscard]] static auto FromWords(
      std::span<const std::uint64_t> value_words,
      std::span<const std::uint64_t> unknown_words, std::uint64_t bit_width,
      bool is_signed, bool is_four_state) -> PackedArray;

  // LRM 5.9 / 21.3.4.4 byte-stream-to-packed convention: the first byte
  // fills the destination's most significant location. Used wherever the
  // SystemVerilog spec defines a byte order against a packed bit-vector
  // (`$fread` of an integral variable, `$fread` of an unpacked-array
  // element, `"abc"` packed-string literal). Shortfalls (`bytes` carrying
  // fewer than `bit_width` bits) zero-pad the LSBs; excess input bits are
  // silently truncated. The unknown plane is left zero (the SV spec treats
  // file / byte-array bytes as 2-state).
  [[nodiscard]] static auto FromBytes(
      std::span<const char> bytes, std::uint64_t bit_width, bool is_signed,
      bool is_four_state) -> PackedArray;

  // Converts `src` to the declared representation `type` names: a fresh value
  // of that shape carrying `src`'s bits, sign- or zero-extended when widening
  // and truncated when narrowing per `src`'s signedness, with the unknown plane
  // adjusted across the 2-state / 4-state boundary (widening adds a zero plane,
  // narrowing collapses X/Z to 0). The single-bit-width form is an internal
  // shorthand for a computed flat width.
  [[nodiscard]] static auto ConvertFrom(
      const PackedArray& src, const PackedType& type) -> PackedArray;
  [[nodiscard]] static auto ConvertFrom(
      const PackedArray& src, std::uint64_t dst_bit_width, bool dst_is_signed,
      bool dst_is_four_state) -> PackedArray;

  // LRM 5.9: a string value assigned to an integral variable is right-justified
  // -- a destination wider than the text pads its leftmost bits with zeros, and
  // a narrower one truncates the leftmost characters. The text is first taken
  // at its own width (its first character the most significant byte), so
  // conforming that value to the representation `type` names is what applies
  // the justification rule. An empty string yields zero.
  [[nodiscard]] static auto FromString(
      const String& text, const PackedType& type) -> PackedArray;

  // LRM 21.4 memory-load digit parse: builds a value of the given shape from a
  // radix-`base` digit string (base 2 / 8 / 16). Each digit contributes
  // log2(base) bits MSB-first, so the string is right-justified into the target
  // -- a shorter string zero-fills the leading bits and a longer one drops
  // them. An `x` / `X` digit contributes that many x bits and a `z` / `Z` / `?`
  // digit that many z bits; a 2-state target collapses both to 0. `_`
  // separators are ignored. Returns nullopt if `digits` (after removing `_`)
  // is empty or holds a character that is not a base-`base` digit, x, z, or ?.
  [[nodiscard]] static auto FromDigits(
      std::string_view digits, unsigned base, std::uint64_t bit_width,
      bool is_signed, bool is_four_state) -> std::optional<PackedArray>;

  // LRM 11.4.12: `{*this, rhs}`. This value occupies the result's MSBs and
  // `rhs` its LSBs; the result width is the sum, unsigned (LRM 11.8.1), and
  // 4-state iff either operand is. A source-level join of more operands
  // composes as the left-to-right chain of these, which holds the same value
  // because composing is associative over both the bit plane and the state
  // domain. Neither operand is copied to describe it.
  [[nodiscard]] auto Concat(const PackedArray& rhs) const -> PackedArray;

  // LRM 11.4.12.1: `{count{*this}}`. Result bit width is BitWidth() * count,
  // unsigned, 4-state iff this value is. Throws InternalError if the result
  // would be zero-width -- a packed multiplier is a constant expression the
  // front end has already checked, so a count that is not positive by the time
  // it reaches here indicates a frontend / lowering bug.
  [[nodiscard]] auto Replicate(std::int64_t count) const -> PackedArray;

  [[nodiscard]] auto BitWidth() const -> std::uint64_t;
  [[nodiscard]] auto IsSigned() const -> bool;
  [[nodiscard]] auto IsFourState() const -> bool;

  // LRM 20.6.2 `$bits`: a packed value occupies its declared bit width. It is
  // the leaf of the recursive bit count a dynamically sized container reports
  // over its elements.
  [[nodiscard]] auto BitstreamWidth() const -> PackedArray {
    return PackedArray::Int(static_cast<std::int32_t>(BitWidth()));
  }

  // LRM 6.24.3: the bits this value contributes to a stream, as an unsigned
  // vector of its own width. It is the leaf of the recursion an aggregate
  // performs, and the identity for a value that is already one bit plane --
  // what changes is only that a stream carries no signedness.
  [[nodiscard]] auto ToBitstream() const -> PackedArray;

  // The inverse: `bits`, which the caller has already brought to the width
  // `prototype` reports, read back at the prototype's declared representation.
  // A 4-state stream reaching a 2-state prototype collapses its unknown bits,
  // which is the cast LRM 11.4.14.3 names.
  [[nodiscard]] static auto FromBitstream(
      const PackedArray& bits, const PackedArray& prototype) -> PackedArray;

  // LRM 11.4.14.2: this value divided into `block_bits`-wide blocks from its
  // least significant bit up, with the block order reversed and the bits inside
  // each block left where they are. A leftover top block keeps whatever width
  // is left; nothing is padded or dropped, so the result has this value's own
  // width.
  [[nodiscard]] auto ReverseBlocks(std::int64_t block_bits) const
      -> PackedArray;

  // Restore the value to its shape's canonical default in place: all-zero for
  // 2-state, all-X for 4-state (LRM Table 6-7 / Table 7-1). The width,
  // signedness and state domain are preserved; only the bits are reset.
  // Container shield slots call this on OOB access so the slot mirrors what
  // a freshly-defaulted element would read as.
  auto ResetToDefault() -> void;

  // `prototype`'s declared type with every bit set to `fill`'s low bit: what a
  // net shows where nothing drives it, and what a driver contributes where it
  // is not driving (LRM 6.6, 6.7.1). The prototype's contents are unused; only
  // its declared type (width, signedness, state domain) is read, and an unknown
  // or high-impedance fill collapses to zero on a 2-state shape, which has
  // neither state.
  [[nodiscard]] static auto FilledLike(
      const PackedArray& prototype, const PackedArray& fill) -> PackedArray;

  // LRM 9.4.2 update event predicate (engine change-detection hook): are the
  // two values bit-identical, considering both the value plane and the
  // unknown plane. Distinct from the SV `===` operator (`CaseEqual`) only in
  // role -- the algorithm is the same, but this is the host-bool form the
  // runtime uses, named after what it answers rather than after the operator.
  [[nodiscard]] auto IsBitIdentical(const PackedArray& other) const -> bool;

  // LRM 9.4.2 LSB: edge transitions are detected only on bit 0. Returns the
  // bit as a 4-state code so the caller can apply Table 9-2 (0/x/z to 1
  // posedge, 1/x/z to 0 negedge).
  [[nodiscard]] auto Lsb() const -> FourStateBit;

  // LRM 11.4.5 `===` operator form (1-bit PackedArray); `==` returns 4-state
  // and propagates X, this returns deterministic 0 or 1.
  [[nodiscard]] auto CaseEqual(const PackedArray& other) const -> PackedArray;

  // Mirror of `PackedArrayRef::ToOwned`. A chain that ends on a const path
  // (e.g. `var.Get().Slice(...)`) materialises to `PackedArray` directly, so
  // the emit-side `.ToOwned()` wrap needs to compile on both sides; here it
  // is just an explicit copy. Mirrors Rust's `<T as ToOwned>::to_owned()`
  // for `T = PackedArray`, where `&T -> T` is a copy.
  [[nodiscard]] auto ToOwned() const -> PackedArray {
    return *this;
  }

  // Word-level access for `Formatter<PackedArray>` construction and
  // intra-runtime interop. The spans alias the PackedArray's storage and stay
  // valid for the object's lifetime.
  [[nodiscard]] auto ValueWords() const -> std::span<const std::uint64_t>;
  [[nodiscard]] auto UnknownWords() const -> std::span<const std::uint64_t>;

  // View the bit vector as a byte sequence, most significant byte first. The
  // value is right-justified -- its least significant bit is the low bit of the
  // last byte -- and a width that is not a multiple of 8 is zero-filled on the
  // left. A byte with any x or z bit yields `0x00`. The shared producer for the
  // LRM 6.16 string lift and the LRM 21.2.1.7 `%s` formatter, which agree on
  // that layout and differ only in what they do with a NUL byte.
  [[nodiscard]] auto ByteString() const -> std::string;

  // Typed view accessors. The state domain selects which
  // overload is callable: 2-state stores expose Bit views, 4-state stores
  // expose Logic views. Calling the wrong one throws InternalError.
  [[nodiscard]] auto AsBitView() -> BitView;
  [[nodiscard]] auto AsBitView() const -> ConstBitView;
  [[nodiscard]] auto AsLogicView() -> LogicView;
  [[nodiscard]] auto AsLogicView() const -> ConstLogicView;

  // A pure value: copy, move, and assignment all replace the whole value --
  // declared type and contents together. A SystemVerilog variable keeps its
  // declared type across assignment, but that is enforced at the store boundary
  // (which converts the right-hand side to the destination's declared type
  // before the store) and the variable cell, not by the value's own assignment.
  // With no shape-preserving assignment, the type relocates correctly inside
  // STL containers through the defaulted move, with no special handling.
  PackedArray(const PackedArray&) = default;
  PackedArray(PackedArray&&) noexcept = default;
  auto operator=(const PackedArray&) -> PackedArray& = default;
  auto operator=(PackedArray&&) noexcept -> PackedArray& = default;
  ~PackedArray() = default;

  // True for the 0-bit empty state a value-initialized cell holds before its
  // declared representation is installed. The representation is installed once,
  // before any store; every store is then asserted against it. This predicate
  // distinguishes the not-yet-installed state from an installed one.
  [[nodiscard]] auto IsUninitialized() const -> bool;

  // True when `other` is held the same way -- same width, same signedness,
  // same state domain. Once established, a cell asserts this on every store:
  // the right-hand side must already be at the cell's declared type (the store
  // boundary is what converts it there), and the cell never reshapes. A
  // mismatch reaching a store is a conversion the lowering owed and did not
  // emit -- a compiler bug.
  [[nodiscard]] auto SameRepresentation(const PackedArray& other) const -> bool;

  // Extract the value as a 64-bit signed integer. Sign-extends from
  // bit_width when the value is signed. X/Z bits map to 0. bit_width must
  // be <= 64.
  [[nodiscard]] auto ToInt64() const -> std::int64_t;

  // SystemVerilog "non-zero" interpretation: any value bit set with the
  // matching unknown-plane bit clear makes the result true. X/Z bits do not
  // count as truthy. `operator bool` is explicit so it only fires in boolean
  // contexts (`if`, `while`, ternary cond, `&&`, `||`, `!`) and never as an
  // implicit conversion that would shadow the operator overloads.
  [[nodiscard]] auto IsTruthy() const -> bool;
  [[nodiscard]] explicit operator bool() const {
    return IsTruthy();
  }

  // Drives LRM 11.4 X/Z propagation: arithmetic, comparison, shift, and
  // power return all-X (or 1-bit X) when any operand has HasUnknown().
  [[nodiscard]] auto HasUnknown() const -> bool;

  [[nodiscard]] auto Truth() const -> Truthiness;

  // LRM 20.9 `$isunknown` -- the SV-surface query. Returns a 1-bit, 2-state
  // `PackedArray` so the MIR call's result type matches the C++ return type
  // with no backend lift. The host-bool `HasUnknown()` above is the
  // internal X-check the operator implementations use.
  [[nodiscard]] auto IsUnknown() const -> PackedArray {
    return Bit(HasUnknown());
  }

  // LRM 20.8.1 `$clog2` -- the SV-surface query. ceil(log2) of the operand
  // read as unsigned (X/Z bits collapse to 0), with $clog2(0) == 0. Returns a
  // 32-bit `integer` so the MIR call's result type matches the C++ return type
  // with no backend lift.
  [[nodiscard]] auto Clog2() const -> PackedArray;

  // LRM 20.9 `$countbits` -- how many of this value's bits carry one of the
  // control-bit values. `control_bits` holds those values one per bit position;
  // a bit value named more than once still counts once, so the result never
  // exceeds the width. A 2-state receiver has no x or z bits to match. Returns
  // a 32-bit `int` so the MIR call's result type matches the C++ return type
  // with no backend lift.
  [[nodiscard]] auto CountBits(const PackedArray& control_bits) const
      -> PackedArray;

  // Operands are assumed to share the same shape (slang's promotion
  // contract). Comparison / logical results are 1-bit, 4-state when any
  // operand is 4-state (so they can carry an X under LRM 11.4
  // propagation). Shift amounts are self-determined per LRM 11.6.
  [[nodiscard]] auto operator+(const PackedArray& other) const -> PackedArray;
  [[nodiscard]] auto operator-(const PackedArray& other) const -> PackedArray;
  [[nodiscard]] auto operator*(const PackedArray& other) const -> PackedArray;
  [[nodiscard]] auto operator/(const PackedArray& other) const -> PackedArray;
  [[nodiscard]] auto operator%(const PackedArray& other) const -> PackedArray;

  [[nodiscard]] auto operator&(const PackedArray& other) const -> PackedArray;
  [[nodiscard]] auto operator|(const PackedArray& other) const -> PackedArray;
  [[nodiscard]] auto operator^(const PackedArray& other) const -> PackedArray;

  // LRM 11.4 compound assignments. Each is `*this = *this op rhs`; the
  // binary `op` already enforces operand-shape compatibility, so frontends
  // must hand both operands at the same shape (typically via a Conversion
  // on the rhs to lhs.type at HIR construction). Shift compounds are
  // method-style because the binary form is method-style too.
  auto operator+=(const PackedArray& rhs) -> PackedArray& {
    return *this = *this + rhs;
  }
  auto operator-=(const PackedArray& rhs) -> PackedArray& {
    return *this = *this - rhs;
  }
  auto operator*=(const PackedArray& rhs) -> PackedArray& {
    return *this = *this * rhs;
  }
  auto operator/=(const PackedArray& rhs) -> PackedArray& {
    return *this = *this / rhs;
  }
  auto operator%=(const PackedArray& rhs) -> PackedArray& {
    return *this = *this % rhs;
  }
  auto operator&=(const PackedArray& rhs) -> PackedArray& {
    return *this = *this & rhs;
  }
  auto operator|=(const PackedArray& rhs) -> PackedArray& {
    return *this = *this | rhs;
  }
  auto operator^=(const PackedArray& rhs) -> PackedArray& {
    return *this = *this ^ rhs;
  }
  auto ShiftLeftAssign(const PackedArray& rhs) -> PackedArray& {
    return *this = ShiftLeft(rhs);
  }
  auto LogicalShiftRightAssign(const PackedArray& rhs) -> PackedArray& {
    return *this = LogicalShiftRight(rhs);
  }
  auto ArithmeticShiftRightAssign(const PackedArray& rhs) -> PackedArray& {
    return *this = ArithmeticShiftRight(rhs);
  }

  // LRM 11.4.2 inc/dec on a 1D integer value: prefix mutates in place and
  // returns the new value (standard C++ idiom); postfix snapshots the old
  // value, mutates, returns the snapshot.
  auto operator++() -> PackedArray& {
    return *this = *this + FromInt(1, bit_width_, is_signed_, is_four_state_);
  }
  auto operator++(int) -> PackedArray {
    PackedArray prior = *this;
    ++*this;
    return prior;
  }
  auto operator--() -> PackedArray& {
    return *this = *this - FromInt(1, bit_width_, is_signed_, is_four_state_);
  }
  auto operator--(int) -> PackedArray {
    PackedArray prior = *this;
    --*this;
    return prior;
  }

  [[nodiscard]] auto operator==(const PackedArray& other) const -> PackedArray;
  [[nodiscard]] auto operator!=(const PackedArray& other) const -> PackedArray;
  // LRM 11.4.6: X/Z in `other` are wildcards; X/Z in `*this` are not.
  [[nodiscard]] auto WildcardEquals(const PackedArray& other) const
      -> PackedArray;
  // LRM 12.5.1 casez per-label compare: Z bits on either operand are
  // wildcards (do-not-care); remaining bits must match exactly on both
  // planes (X must still match X). A label either matches or it does not, so
  // the 1-bit answer is two-state whatever the operands are.
  [[nodiscard]] auto CasezEquals(const PackedArray& other) const -> PackedArray;
  // LRM 12.5.1 casex per-label compare: any unknown bit (X or Z) on either
  // operand is a wildcard; remaining bits must match on the value plane. Its
  // answer is two-state for the same reason.
  [[nodiscard]] auto CasexEquals(const PackedArray& other) const -> PackedArray;
  // LRM 11.4.11 Table 11-20: the two arms of a conditional operator whose
  // condition is ambiguous, combined bit by bit -- a bit both arms know and
  // agree on survives, and every other bit becomes x.
  [[nodiscard]] auto MergeConditional(const PackedArray& other) const
      -> PackedArray;
  // Resolution of two driver contributions under the truth table `fold` names:
  // tri-state (LRM 6.6.1 Table 6-2), wired-and (LRM 6.6.3 Table 6-3), or
  // wired-or (Table 6-4). Z is every fold's identity and defers to the other
  // driver; tri-state passes equal drivers through and yields X on a 0/1
  // conflict, wired-and lets any 0 win, wired-or any 1. Associative and
  // commutative, so a net folds its drivers in any order.
  [[nodiscard]] auto ResolveNet(
      const PackedArray& other, NetResolution fold) const -> PackedArray;
  // The result of this value meeting a weaker one: it determines every position
  // it drives, and `weaker` determines the rest (LRM 28.12.1). A position is
  // driven wherever this value is not high-impedance there, so nothing is
  // folded -- a stronger contribution replaces a weaker one rather than
  // combining with it.
  [[nodiscard]] auto Dominating(const PackedArray& weaker) const -> PackedArray;

  // LRM 11.5.1 bit-select and part-select, and a packed aggregate's member
  // (LRM 7.2.1), which are one operation: a run of `width` bits starting at
  // `position`, counted from this value's least significant bit. What the
  // source wrote to name that run has been read before it gets here, so the
  // start arrives as a position and the width as a count.
  //
  // A read materializes the run as an owned value, unsigned as a run taken out
  // of a value is. Bits the run reaches for outside this value read x, or 0 in
  // a two-state value, and a start that names no position reads all of them
  // that way. A write lands on the bits inside this value and leaves the rest
  // alone, so a start that names no position writes nothing. The functional
  // form is that write applied to a copy, for a value reached by an opaque
  // handle that cannot be written in place.
  [[nodiscard]] auto Slice(
      const PackedArray& position, std::int64_t width) const -> PackedArray;
  [[nodiscard]] auto SliceRef(const PackedArray& position, std::int64_t width)
      -> PackedArrayRef;
  [[nodiscard]] auto WithSlice(
      const PackedArray& position, std::int64_t width,
      const PackedArray& value) const -> PackedArray;

  // The same run where the caller holds its start as a machine offset rather
  // than as a value the program computed, so there is no unknown start to
  // answer for.
  [[nodiscard]] auto ExtractRun(std::int64_t start, std::uint64_t width) const
      -> PackedArray;
  auto AssignRun(
      std::int64_t start, std::uint64_t width, const PackedArray& value)
      -> void;

  // The position an index names once arithmetic has to be done on it: the same
  // integer as a 64-bit signed four-state value, which no shift of a declared
  // range can wrap, and all x where the index names no position at all. A
  // select that reads an index as it stands takes the index itself.
  [[nodiscard]] static auto ToPosition(const PackedArray& index) -> PackedArray;
  [[nodiscard]] auto operator<(const PackedArray& other) const -> PackedArray;
  [[nodiscard]] auto operator<=(const PackedArray& other) const -> PackedArray;
  [[nodiscard]] auto operator>(const PackedArray& other) const -> PackedArray;
  [[nodiscard]] auto operator>=(const PackedArray& other) const -> PackedArray;

  [[nodiscard]] auto operator-() const -> PackedArray;
  [[nodiscard]] auto operator~() const -> PackedArray;

  // `&&` / `||` lose C++ short-circuit semantics when overloaded; that is
  // acceptable here because callers (cpp emit, ad-hoc runtime use) work with
  // already-materialized PackedArray values without side effects, and any
  // optimizer pass that cares (clang's mem2reg, LLVM) sees these as plain
  // member calls equivalent to the OR-chain it would see for `LogicalOr` --
  // there is no codegen difference.
  [[nodiscard]] auto operator&&(const PackedArray& other) const -> PackedArray;
  [[nodiscard]] auto operator||(const PackedArray& other) const -> PackedArray;
  [[nodiscard]] auto operator!() const -> PackedArray;

  // SV `->` and `<->` have no C++ operator counterpart; method-only.
  [[nodiscard]] auto LogicalImplication(const PackedArray& other) const
      -> PackedArray;
  [[nodiscard]] auto LogicalEquivalence(const PackedArray& other) const
      -> PackedArray;

  [[nodiscard]] auto BitwiseXnor(const PackedArray& other) const -> PackedArray;

  [[nodiscard]] auto ShiftLeft(const PackedArray& amount) const -> PackedArray;
  [[nodiscard]] auto LogicalShiftRight(const PackedArray& amount) const
      -> PackedArray;
  [[nodiscard]] auto ArithmeticShiftRight(const PackedArray& amount) const
      -> PackedArray;

  [[nodiscard]] auto Pow(const PackedArray& exponent) const -> PackedArray;

  [[nodiscard]] auto ReductionAnd() const -> PackedArray;
  [[nodiscard]] auto ReductionOr() const -> PackedArray;
  [[nodiscard]] auto ReductionXor() const -> PackedArray;
  [[nodiscard]] auto ReductionNand() const -> PackedArray;
  [[nodiscard]] auto ReductionNor() const -> PackedArray;
  [[nodiscard]] auto ReductionXnor() const -> PackedArray;

 private:
  // The shape and the words of its planes, taken as given. A constructor that
  // names a shape installs that shape's default value over the bits; this one
  // does not.
  PackedArray(
      std::uint64_t bit_width, bool is_signed, bool is_four_state,
      PackedWordArray planes);

  // A value of the given shape with every bit clear, for an operation that is
  // about to write all of them.
  [[nodiscard]] static auto Blank(
      std::uint64_t bit_width, bool is_signed, bool is_four_state)
      -> PackedArray;

  [[nodiscard]] auto WordsPerPlane() const -> std::size_t;

  // Writable planes, which is sound only while nothing else can observe this
  // value -- a result being filled, or a designated run being written through.
  [[nodiscard]] auto MutableValueWords() -> std::span<std::uint64_t>;
  [[nodiscard]] auto MutableUnknownWords() -> std::span<std::uint64_t>;

  // Takes the bits of a value whose planes are clear from words the caller
  // assembled, masking whatever sits above the declared width. An empty
  // unknown plane leaves this value's own clear one.
  auto InstallPlanes(
      std::span<const std::uint64_t> value_words,
      std::span<const std::uint64_t> unknown_words) -> void;

  // A value of a stated shape whose bits are words assembled somewhere else --
  // a literal, a byte stream, a digit string, a foreign buffer. The words are
  // checked against the shape before they are taken, which is what separates
  // this from an operation writing its own result.
  [[nodiscard]] static auto MakeFromWordPlanes(
      std::uint64_t bit_width, bool is_signed, bool is_four_state,
      std::span<const std::uint64_t> value_words,
      std::span<const std::uint64_t> unknown_words) -> PackedArray;

  // Copies `src`'s bits into `dst`, sign- or zero-extending or truncating per
  // `src`'s signedness and adjusting the unknown plane across the state-domain
  // boundary. Every one of `dst`'s bits is written, so it arrives clear rather
  // than at its shape's default.
  [[nodiscard]] static auto ConvertBitsInto(
      PackedArray dst, const PackedArray& src) -> PackedArray;

  // What every operation on this value needs and nothing else: how many bits
  // it has, whether they are read as signed, and whether a position may hold x
  // or z. How a declaration divides those bits is the declaration's own fact
  // and reaches an operation that names a position as an argument, so it is
  // not here. A width of zero is the not-yet-installed state.
  std::uint64_t bit_width_ = 0;
  bool is_signed_ = false;
  bool is_four_state_ = false;
  // Bit i of the value is bit i%64 of word i/64, in as many words as the width
  // needs. A four-state value carries a second plane the same length after the
  // first, whose set positions are the ones holding x or z; a two-state value
  // carries none, which is what having no such state means. One run holds both
  // so that a value fitting one word -- nearly every value a design computes --
  // keeps both planes in place, and making, copying or discarding it never
  // reaches the allocator.
  PackedWordArray planes_;
};

// A writable designation into a run of a `PackedArray`, named by where the run
// starts in the root and how long it is. A further step composes onto it by
// naming a run inside this one, and assigning writes the root under the LRM
// 11.5.1 partial-write rules; reading materializes a fresh value. A start that
// names no position at any step names none for the whole chain, which makes the
// final write a no-op and the read all x.
class PackedArrayRef {
 public:
  PackedArrayRef(
      PackedArray& root, std::optional<std::int64_t> start,
      std::uint64_t bit_width);

  // Move-only: a ref aliases its root by raw pointer, so duplicating the
  // handle and outliving the source would dangle. Moves are fine because the
  // descriptor is just relocated. Chain composition relies on RVO / move.
  PackedArrayRef(const PackedArrayRef&) = delete;
  auto operator=(const PackedArrayRef&) -> PackedArrayRef& = delete;
  PackedArrayRef(PackedArrayRef&&) noexcept = default;
  auto operator=(PackedArrayRef&&) noexcept -> PackedArrayRef& = default;
  ~PackedArrayRef() = default;

  // Allocate an independent `PackedArray` holding the bits this view
  // currently projects. There is no implicit conversion from ref to value;
  // every materialisation is spelled `.ToOwned()` (Rust's `ToOwned` trait
  // semantics: borrowed view -> owning value) so the allocation cost is
  // visible at the call site.
  [[nodiscard]] auto ToOwned() const -> PackedArray;

  auto operator=(const PackedArray& value) -> PackedArrayRef&;

  // LRM 11.4 compound assignments. Read the current run once, combine with
  // `rhs` (the front end converts rhs to the target's type), write it back.
  // The start and width this designation holds are the eval-once mechanism:
  // the chain is built once by the caller, the designation captures where it
  // lands, and both the read and the write here use that same place with no
  // re-evaluation of indices.
  auto operator+=(const PackedArray& rhs) -> PackedArrayRef& {
    return *this = ToOwned() + rhs;
  }
  auto operator-=(const PackedArray& rhs) -> PackedArrayRef& {
    return *this = ToOwned() - rhs;
  }
  auto operator*=(const PackedArray& rhs) -> PackedArrayRef& {
    return *this = ToOwned() * rhs;
  }
  auto operator/=(const PackedArray& rhs) -> PackedArrayRef& {
    return *this = ToOwned() / rhs;
  }
  auto operator%=(const PackedArray& rhs) -> PackedArrayRef& {
    return *this = ToOwned() % rhs;
  }
  auto operator&=(const PackedArray& rhs) -> PackedArrayRef& {
    return *this = ToOwned() & rhs;
  }
  auto operator|=(const PackedArray& rhs) -> PackedArrayRef& {
    return *this = ToOwned() | rhs;
  }
  auto operator^=(const PackedArray& rhs) -> PackedArrayRef& {
    return *this = ToOwned() ^ rhs;
  }
  auto ShiftLeftAssign(const PackedArray& rhs) -> PackedArrayRef& {
    return *this = ToOwned().ShiftLeft(rhs);
  }
  auto LogicalShiftRightAssign(const PackedArray& rhs) -> PackedArrayRef& {
    return *this = ToOwned().LogicalShiftRight(rhs);
  }
  auto ArithmeticShiftRightAssign(const PackedArray& rhs) -> PackedArrayRef& {
    return *this = ToOwned().ArithmeticShiftRight(rhs);
  }

  // LRM 11.4.2 inc/dec on a partial-write proxy. Both forms return PackedArray
  // by value -- prefix returns the new sub-slice, postfix returns the old --
  // so an outer rvalue use (`b = ++var[3:0]`) consumes a materialized value
  // instead of holding a transient proxy past the end of the full expression.
  auto operator++() -> PackedArray {
    PackedArray current = ToOwned();
    auto updated =
        current + PackedArray::FromInt(
                      1, bit_width_, current.IsSigned(), current.IsFourState());
    *this = updated;
    return updated;
  }
  auto operator++(int) -> PackedArray {
    PackedArray prior = ToOwned();
    *this = prior + PackedArray::FromInt(
                        1, bit_width_, prior.IsSigned(), prior.IsFourState());
    return prior;
  }
  auto operator--() -> PackedArray {
    PackedArray current = ToOwned();
    auto updated =
        current - PackedArray::FromInt(
                      1, bit_width_, current.IsSigned(), current.IsFourState());
    *this = updated;
    return updated;
  }
  auto operator--(int) -> PackedArray {
    PackedArray prior = ToOwned();
    *this = prior - PackedArray::FromInt(
                        1, bit_width_, prior.IsSigned(), prior.IsFourState());
    return prior;
  }

  // Chain composition: a run of this run, its start counted from this run's
  // least significant bit. The step stays a designation, so the assignment at
  // the tail writes through to the root.
  [[nodiscard]] auto SliceRef(
      const PackedArray& position, std::int64_t width) const -> PackedArrayRef;

 private:
  PackedArray* root_;
  std::optional<std::int64_t> start_;
  std::uint64_t bit_width_;
};

// The `width` bits sitting `consumed` bits in from the most significant end of
// `bits`. An unpack reads a stream from that end (LRM 11.4.14.3), so every part
// of an aggregate takes its own bits by how many the parts before it took, and
// one statement of where a part's bits are serves every aggregate that has
// parts.
[[nodiscard]] auto BitstreamSegment(
    const PackedArray& bits, std::uint64_t consumed, std::uint64_t width)
    -> PackedArray;

static_assert(LyraValue<PackedArray>);
static_assert(CaseEqualComparable<PackedArray>);
static_assert(WildcardComparable<PackedArray>);
static_assert(Ordered<PackedArray>);
static_assert(BitstreamSizable<PackedArray>);
static_assert(BitstreamConvertible<PackedArray>);
static_assert(Sliceable<PackedArray>);
static_assert(SliceableRef<PackedArray>);
static_assert(Ownable<PackedArray>);
static_assert(Defaultable<PackedArray>);
static_assert(NetResolvable<PackedArray>);
static_assert(ConditionallyMergeable<PackedArray>);

}  // namespace lyra::value
