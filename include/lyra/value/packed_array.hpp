#pragma once

#include <array>
#include <concepts>
#include <cstdint>
#include <initializer_list>
#include <span>
#include <type_traits>
#include <variant>
#include <vector>

#include "lyra/value/packed.hpp"

namespace lyra::value {

class PackedArrayRef;

// Declared range of one packed dimension. Outermost dimension is dims[0]; the
// inner element type's dim stack follows. For `bit [N-1:0]`: dims = [{N-1,
// 0}] (1D, each element is one bit). For `bit [1:0][7:0]`: dims = [{1, 0},
// {7, 0}] (2D, each outer element is `bit [7:0]`).
//
// This carries SystemVerilog's declared bounds. Storage layout is private --
// PackedArray today uses flat-bit BitValue/LogicValue, but the API contract
// is element-level (operator[] / Slice take outer-element positions), so
// future storage optimizations (canonical byte alignment, vector-of-elements
// for huge outer dims, etc.) change runtime internals without touching the
// emit or this contract.
struct PackedRange {
  std::int64_t left;
  std::int64_t right;

  [[nodiscard]] auto ElementCount() const -> std::uint64_t {
    return static_cast<std::uint64_t>(
        (left >= right ? left - right : right - left) + 1);
  }
};

// Unified integral value. Mirrors slang's `IntegralType` and the legacy
// archive's `IntegralInfo`: one type, three attributes plus a dim stack.
// `backend::cpp` emits every SystemVerilog integral (`byte`, `shortint`,
// `int`, `longint`, `integer`, `time`, `bit [N:0]`, `logic [N:0]`, `reg
// [N:0]`, and multi-dim packed forms like `bit [N:0][M:0]`) as `PackedArray`.
//
// `dims_` carries the declared structure: 1D for vectors, multi-dim for
// packed-of-packed. `operator[]` and `Slice` dispatch via `dims_` so the API
// is element-level regardless of operand dimensionality -- the emitted C++
// never bakes element-width arithmetic into call sites. Storage layout (flat
// bit planes today; potentially canonical-byte-aligned or vector-of-elements
// once optimization work begins) is private to PackedArray and disjoint from
// the API contract.
class PackedArray {
 public:
  // Primary constructor: declared dim stack + sign + 4-state. Total bit
  // width is the product of dim sizes (cached internally as `bit_width_`).
  PackedArray(
      std::initializer_list<PackedRange> dims, bool is_signed,
      bool is_four_state);

  // 1D shorthand for the common `bit [bit_width-1:0]` case. Equivalent to
  // the dim-list constructor with `{{bit_width-1, 0}}`.
  PackedArray(std::uint64_t bit_width, bool is_signed, bool is_four_state);

  // Convenience factory for the default int shape (32-bit, signed, 2-state).
  // Matches SystemVerilog's default int literal type, so `int x = 5;` lowers
  // to `PackedArray x = PackedArray::Int(5);`.
  [[nodiscard]] static auto Int(std::int32_t value) -> PackedArray;

  // Default byte shape (8-bit, signed, 2-state) per LRM 6.11 Table 6-8.
  [[nodiscard]] static auto Byte(std::int8_t value) -> PackedArray;

  // Default integer shape (32-bit, signed, 4-state) per LRM 6.11 Table 6-8.
  [[nodiscard]] static auto Integer(std::int32_t value) -> PackedArray;

  // Construct a narrow PackedArray (bit_width <= 64) from an integer value.
  // The `std::int64_t` parameter is the carrier type wide enough to cover
  // every narrow width; the resulting shape is set by `bit_width`, with bits
  // above masked out and the unknown plane left at zero.
  [[nodiscard]] static auto FromInt(
      std::int64_t value, std::uint64_t bit_width, bool is_signed,
      bool is_four_state) -> PackedArray;

  // Construct a PackedArray of arbitrary width from raw word planes. Used
  // by cpp emit for any literal that does not fit a single int64 carrier:
  // widths > 64 bits, or 4-state literals carrying X/Z bits. `value_words`
  // must have ceil(bit_width / 64) entries; bits above `bit_width` in the
  // top word are masked. For 2-state shapes `unknown_words` must be empty;
  // for 4-state shapes it must either be empty (no X/Z) or match
  // `value_words` in size.
  [[nodiscard]] static auto FromWords(
      std::initializer_list<std::uint64_t> value_words,
      std::initializer_list<std::uint64_t> unknown_words,
      std::uint64_t bit_width, bool is_signed, bool is_four_state)
      -> PackedArray;

  // Width-aware conversion. Constructs a fresh PackedArray of the
  // destination shape and copies bits from `src`, sign- or zero-extending
  // per `src`'s signedness when widening, truncating when narrowing.
  // 2-state -> 4-state leaves the unknown plane zero; 4-state -> 2-state
  // collapses any X/Z bits to 0. Both wide (>64-bit) and 4-state paths are
  // supported.
  [[nodiscard]] static auto ConvertFrom(
      const PackedArray& src, std::uint64_t dst_bit_width, bool dst_is_signed,
      bool dst_is_four_state) -> PackedArray;

  // Emit-side selector chain reads against a mutable base produce a
  // `PackedArrayRef` rvalue; this overload materializes via direct-init so
  // emit can pass the chain expression straight in.
  [[nodiscard]] static auto ConvertFrom(
      const PackedArrayRef& src, std::uint64_t dst_bit_width,
      bool dst_is_signed, bool dst_is_four_state) -> PackedArray;

  // LRM 11.4.12: `{a, b, c, ...}`. First operand occupies the result's MSBs,
  // last occupies the LSBs. Total bit width is the sum of operand widths.
  // Result is unsigned (LRM 11.8.1); 4-state iff any operand is 4-state.
  // Variadic-template wrapper so each operand binds by `const&`, accepting
  // both lvalues and prvalue temporaries (e.g., emitted literals) without
  // copy. Forwards to the span-based impl below.
  template <typename... Ops>
    requires(
        sizeof...(Ops) > 0 &&
        (std::same_as<std::remove_cvref_t<Ops>, PackedArray> && ...))
  [[nodiscard]] static auto Concat(const Ops&... ops) -> PackedArray {
    const std::array<const PackedArray*, sizeof...(Ops)> ptrs{&ops...};
    return Concat(std::span<const PackedArray* const>{ptrs});
  }

  [[nodiscard]] static auto Concat(std::span<const PackedArray* const> operands)
      -> PackedArray;

  // LRM 11.4.12.1: `{count{operand}}`. Result bit width is
  // operand.BitWidth() * count, unsigned, 4-state iff operand is 4-state.
  // Throws InternalError if the result would be zero-width -- the AST -> HIR
  // concat lowering drops `{0{...}}` operands before they reach the runtime,
  // so an in-runtime zero must indicate a frontend / lowering bug.
  [[nodiscard]] static auto Replicate(
      const PackedArray& operand, std::uint64_t count) -> PackedArray;

  [[nodiscard]] auto BitWidth() const -> std::uint64_t;
  [[nodiscard]] auto IsSigned() const -> bool;
  // Declared dim stack, outermost first. Storage is flat regardless; this is
  // an API-contract field that operator[] / Slice dispatch on.
  [[nodiscard]] auto Dims() const -> std::span<const PackedRange>;
  [[nodiscard]] auto IsFourState() const -> bool;

  // LRM 11.4.5 `===` predicate form (host bool). Operator form: `CaseEqual`.
  [[nodiscard]] auto IsCaseEqual(const PackedArray& other) const -> bool;

  // LRM 9.4.2 LSB: edge transitions are detected only on bit 0. Returns the
  // bit as a 4-state code so the caller can apply Table 9-2 (0/x/z to 1
  // posedge, 1/x/z to 0 negedge).
  [[nodiscard]] auto Lsb() const -> FourStateBit;

  // Returns one bit at the flat-storage offset, encoded as a 4-state code.
  // Used by the runtime's per-leaf edge classifier to sample the bit a waiter
  // projected onto. LRM 11.5.1 OOB rule applies: a position outside `[0,
  // BitWidth())` reads as X for 4-state and 0 for 2-state.
  [[nodiscard]] auto GetBit(std::uint64_t flat_offset) const -> FourStateBit;

  // LRM 11.4.5 `===` operator form (1-bit PackedArray); `==` returns 4-state
  // and propagates X, this returns deterministic 0 or 1.
  [[nodiscard]] auto CaseEqual(const PackedArray& other) const -> PackedArray;

  // Mirror of `PackedArrayRef::Clone`. A chain that ends on a const path
  // (e.g. `var.Get().Slice(...)`) materializes to `PackedArray` directly, so
  // the emit-side `.Clone()` wrap needs to compile on both sides; here it is
  // just an explicit copy.
  [[nodiscard]] auto Clone() const -> PackedArray { return *this; }

  // Word-level access for `RuntimeValueView` construction and intra-runtime
  // interop. The spans alias the PackedArray's storage and stay valid for
  // the object's lifetime.
  [[nodiscard]] auto ValueWords() const -> std::span<const std::uint64_t>;
  [[nodiscard]] auto UnknownWords() const -> std::span<const std::uint64_t>;

  // Typed view accessors. The atom encoded in `is_four_state_` selects which
  // overload is callable: 2-state stores expose Bit views, 4-state stores
  // expose Logic views. Calling the wrong one throws InternalError.
  [[nodiscard]] auto AsBitView() -> BitView;
  [[nodiscard]] auto AsBitView() const -> ConstBitView;
  [[nodiscard]] auto AsLogicView() -> LogicView;
  [[nodiscard]] auto AsLogicView() const -> ConstLogicView;

  // Width-aware copy. The destination's attributes drive sign-extension,
  // zero-extension, or truncation as appropriate.
  auto AssignFrom(const PackedArray& other) -> void;

  // Variable assignment in SystemVerilog preserves the destination's declared
  // shape; the producer is expected to insert any width/state conversion (the
  // cpp backend lowers slang's ConversionExpr into PackedArray::ConvertFrom).
  // Both copy- and move-assignment route through AssignFrom, which throws
  // InternalError on shape mismatch. This makes "(target = value)" the single
  // emit shape for variable assignment and surfaces any missing conversion as
  // a backend bug rather than silently relayout-ing the destination.
  // Construction (copy/move ctor) does the opposite: a freshly-constructed
  // PackedArray adopts the source's shape, since there is no declared shape
  // to preserve.
  PackedArray(const PackedArray&) = default;
  PackedArray(PackedArray&&) noexcept = default;
  auto operator=(const PackedArray& other) -> PackedArray&;
  auto operator=(PackedArray&& other) noexcept(false) -> PackedArray&;
  ~PackedArray() = default;

  // Extract the value as a 64-bit signed integer. Sign-extends from
  // bit_width when is_signed_ is true. X/Z bits map to 0. bit_width must
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

  [[nodiscard]] auto operator==(const PackedArray& other) const -> PackedArray;
  [[nodiscard]] auto operator!=(const PackedArray& other) const -> PackedArray;
  // LRM 11.4.6: X/Z in `other` are wildcards; X/Z in `*this` are not.
  [[nodiscard]] auto WildcardEquals(const PackedArray& other) const
      -> PackedArray;
  // LRM 12.5.1 casez per-label compare: Z bits on either operand are
  // wildcards (do-not-care); remaining bits must match exactly on both
  // planes (X must still match X). Returns a deterministic 1-bit result.
  [[nodiscard]] auto CasezEquals(const PackedArray& other) const -> PackedArray;
  // LRM 12.5.1 casex per-label compare: any unknown bit (X or Z) on either
  // operand is a wildcard; remaining bits must match on the value plane.
  // Returns a deterministic 1-bit result.
  [[nodiscard]] auto CasexEquals(const PackedArray& other) const -> PackedArray;

  // Low-level bit-level primitives. `ExtractBits` reads `bit_width` contiguous
  // bits starting at `lsb_bit`. `AssignSlice` writes those bits with the LRM
  // 11.5.1 corner cases (X/Z lsb, fully-OOB lsb, lsb magnitude exceeding the
  // 64-bit position carrier all collapse to a silent no-op; partial-OOB
  // affects only in-range bits). Positions are in the canonical flat-bit
  // address space, dimension-agnostic. The element-level chain methods below
  // (`operator[]`, `Slice`) compose offsets in this address space and route
  // here at the chain leaf.
  [[nodiscard]] auto ExtractBits(
      const PackedArray& lsb_bit, std::uint32_t bit_width) const -> PackedArray;
  auto AssignSlice(
      const PackedArray& lsb_bit, std::uint32_t bit_width,
      const PackedArray& value) -> void;

  // Proxy-chain entry points. Both methods take positions in the operand's
  // outer-element units; PackedArray's `dims_` decides the element bit width
  // internally. For a 1D operand (`dims_.size() == 1`), one "element" is one
  // bit, so `ElementAt` is the LRM 11.5.1 bit-select and `Slice` is the LRM
  // 11.5.1 part-select at bit level. For a multi-dim operand, one "element"
  // is the inner subtype, and the API scales positions internally.
  //
  // Non-const overloads return a `PackedArrayRef` that composes further
  // selectors and routes a final `operator=` through `AssignSlice`. Const
  // overloads materialize the sub-slice as a fresh `PackedArray` for
  // read-side use. Uniformly method-style (no `operator[]`) so a chain is
  // visually consistent: `data.ElementAt(idx).Slice(lsb, count)`.
  [[nodiscard]] auto ElementAt(const PackedArray& idx) -> PackedArrayRef;
  [[nodiscard]] auto ElementAt(const PackedArray& idx) const -> PackedArray;
  [[nodiscard]] auto Slice(
      const PackedArray& lsb_in_outer_elements,
      std::uint32_t count_in_outer_elements) -> PackedArrayRef;
  [[nodiscard]] auto Slice(
      const PackedArray& lsb_in_outer_elements,
      std::uint32_t count_in_outer_elements) const -> PackedArray;
  [[nodiscard]] auto operator<(const PackedArray& other) const -> PackedArray;
  [[nodiscard]] auto operator<=(const PackedArray& other) const -> PackedArray;
  [[nodiscard]] auto operator>(const PackedArray& other) const -> PackedArray;
  [[nodiscard]] auto operator>=(const PackedArray& other) const -> PackedArray;

  // Unary operators.
  [[nodiscard]] auto operator-() const -> PackedArray;
  [[nodiscard]] auto operator~() const -> PackedArray;

  // Logical operators. `&&` / `||` lose C++ short-circuit semantics when
  // overloaded; that is acceptable here because callers (cpp emit, ad-hoc
  // runtime use) work with already-materialized PackedArray values without
  // side effects, and any optimizer pass that cares (clang's mem2reg, LLVM)
  // sees these as plain member calls equivalent to the OR-chain it would
  // see for `LogicalOr` -- there is no codegen difference.
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

  [[nodiscard]] auto Power(const PackedArray& exponent) const -> PackedArray;

  [[nodiscard]] auto ReductionAnd() const -> PackedArray;
  [[nodiscard]] auto ReductionOr() const -> PackedArray;
  [[nodiscard]] auto ReductionXor() const -> PackedArray;
  [[nodiscard]] auto ReductionNand() const -> PackedArray;
  [[nodiscard]] auto ReductionNor() const -> PackedArray;
  [[nodiscard]] auto ReductionXnor() const -> PackedArray;

 private:
  // Single source of truth for "construct a PackedArray with a known value".
  // FromInt, FromWords, and any internal op that needs to materialize a
  // result from word planes must delegate here. The helper guarantees both
  // planes are fully written (value from `value_words`, unknown from
  // `unknown_words` or zero when empty); no caller can leak the all-X
  // residue left by LogicValue's default ctor.
  [[nodiscard]] static auto MakeFromWordPlanes(
      std::uint64_t bit_width, bool is_signed, bool is_four_state,
      std::span<const std::uint64_t> value_words,
      std::span<const std::uint64_t> unknown_words) -> PackedArray;

  std::uint64_t bit_width_;
  bool is_signed_;
  bool is_four_state_;
  std::variant<BitValue, LogicValue> storage_;
  std::vector<PackedRange> dims_;
};

// Writable reference into a sub-range of a PackedArray. Composes through
// chained `operator[]` / `Slice` and triggers the LRM 11.5.1 partial-write
// rules via `AssignSlice` on the root when assigned. Reading materializes
// back to a fresh `PackedArray`.
//
// Carries a `dims_` stack mirroring the structural shape of this sub-view:
// `dims_[0]` is the outer dim at the current chain layer; further chain
// methods dispatch on it to compute element bit width and emit a
// dimension-aware sub-Ref. The internal `bit_offset_` is kept in a canonical
// 64-bit signed 4-state shape so chained `+` / `*` between layers do not
// collide on shape, and X/Z in any layer's index/lsb propagates to the final
// write (LRM "X/Z position is a no-op").
class PackedArrayRef {
 public:
  PackedArrayRef(
      PackedArray& root, const PackedArray& bit_offset, std::uint32_t bit_width,
      std::vector<PackedRange> dims);

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
  // every materialization is spelled `.Clone()` so the allocation cost is
  // visible at the call site.
  [[nodiscard]] auto Clone() const -> PackedArray;

  // Write-side: route through AssignSlice on root.
  auto operator=(const PackedArray& value) -> PackedArrayRef&;

  // LRM 11.4 compound assignments. Read the current sub-slice once, combine
  // with `rhs` (frontend converts rhs to lhs.type), write back through
  // `AssignSlice`. The fixed `bit_offset_` / `bit_width_` on the proxy are
  // the eval-once mechanism: the lvalue chain is constructed once by the
  // caller, the proxy captures the position, and both the read and write
  // here use that same position with no re-evaluation of indices.
  auto operator+=(const PackedArray& rhs) -> PackedArrayRef& {
    return *this = Clone() + rhs;
  }
  auto operator-=(const PackedArray& rhs) -> PackedArrayRef& {
    return *this = Clone() - rhs;
  }
  auto operator*=(const PackedArray& rhs) -> PackedArrayRef& {
    return *this = Clone() * rhs;
  }
  auto operator/=(const PackedArray& rhs) -> PackedArrayRef& {
    return *this = Clone() / rhs;
  }
  auto operator%=(const PackedArray& rhs) -> PackedArrayRef& {
    return *this = Clone() % rhs;
  }
  auto operator&=(const PackedArray& rhs) -> PackedArrayRef& {
    return *this = Clone() & rhs;
  }
  auto operator|=(const PackedArray& rhs) -> PackedArrayRef& {
    return *this = Clone() | rhs;
  }
  auto operator^=(const PackedArray& rhs) -> PackedArrayRef& {
    return *this = Clone() ^ rhs;
  }
  auto ShiftLeftAssign(const PackedArray& rhs) -> PackedArrayRef& {
    return *this = Clone().ShiftLeft(rhs);
  }
  auto LogicalShiftRightAssign(const PackedArray& rhs) -> PackedArrayRef& {
    return *this = Clone().LogicalShiftRight(rhs);
  }
  auto ArithmeticShiftRightAssign(const PackedArray& rhs) -> PackedArrayRef& {
    return *this = Clone().ArithmeticShiftRight(rhs);
  }

  // Chain composition. Positions are in the current sub-view's outer-element
  // units; the proxy scales internally based on `dims_`.
  [[nodiscard]] auto ElementAt(const PackedArray& idx) const -> PackedArrayRef;
  [[nodiscard]] auto Slice(
      const PackedArray& lsb_in_outer_elements,
      std::uint32_t count_in_outer_elements) const -> PackedArrayRef;

 private:
  PackedArray* root_;
  PackedArray bit_offset_;
  std::uint32_t bit_width_;
  std::vector<PackedRange> dims_;
};

}  // namespace lyra::value
