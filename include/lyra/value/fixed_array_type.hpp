#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <vector>

#include "lyra/value/element_type.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/reduction.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

class String;

// How a fixed-size unpacked array is laid out: how far apart two neighbouring
// elements lie, and the size and alignment of the array.
struct FixedArrayLayout {
  std::size_t stride;
  std::size_t size;
  std::size_t align;
};

// The layout of `count` elements of `element_size` bytes aligned to
// `element_align`, one after another. Generated code places an element by this
// and the library walks one by it, so it is stated once for both.
[[nodiscard]] constexpr auto FixedArrayLayoutOf(
    std::size_t element_size, std::size_t element_align, std::size_t count)
    -> FixedArrayLayout {
  const std::size_t stride =
      (element_size + element_align - 1) / element_align * element_align;
  return FixedArrayLayout{
      .stride = stride, .size = stride * count, .align = element_align};
}

// The type of a fixed-size unpacked array (LRM 7.4.2) as code compiled before
// any design existed is handed it: how many elements, and their type. An array
// is its elements one after another and nothing else, so every operation on a
// whole array is that operation on each element in turn, carried out here once
// for every array type.
//
// The elements lie in the array, and an element keeps its address across every
// assignment (LRM 13.5.2 has a reference to one go on naming it).
//
// Generated code states one of these for each array type it hands a value of
// to such code, as constant data of this class, so its layout is part of what
// the two sides agree on.
class FixedArrayType final : public ValueType {
 public:
  FixedArrayType(const value::ElementType& element, std::uint32_t count);

  [[nodiscard]] auto Count() const -> std::size_t {
    return count_;
  }
  [[nodiscard]] auto ElementType() const -> const value::ElementType& {
    return *element_;
  }

  // The element at storage position `position`, counted from the left, of the
  // array lying at `array`.
  [[nodiscard]] auto ElementAt(const void* array, std::size_t position) const
      -> const void*;
  [[nodiscard]] auto ElementAt(void* array, std::size_t position) const
      -> void*;

  // LRM 7.4.5: the element `position` names in the array lying at `array` as
  // storage a write lands in, where no read reaches where it names none.
  [[nodiscard]] auto ElementMut(
      void* array, std::optional<std::int64_t> position,
      Formation& formed) const -> void*;

  // LRM 7.4.5 / 7.4.6: the `count` elements from `start`, each the element
  // default where it lies outside the array, every one of them where `start`
  // names no position.
  [[nodiscard]] auto SliceElements(
      const void* array, std::optional<std::int64_t> start,
      std::size_t count) const -> std::vector<const void*>;

  // LRM 7.6: the window takes `replacement`, element for element; an element
  // outside the array is skipped and a start naming no position writes
  // nothing. Answers whether any element took a different value (LRM 4.3).
  auto AssignSlice(
      void* array, std::optional<std::int64_t> start, std::size_t count,
      std::span<const void* const> replacement) const -> bool;

  // LRM 7.12.2: puts the value that was at `order[k]` at position `k`.
  void Permute(void* array, std::span<const std::size_t> order) const;

  // Where the type states each of its own parts, which code stating a type as
  // constant data places them by.
  [[nodiscard]] auto CountStatedAt() const -> StatedAt;
  [[nodiscard]] auto ElementStatedAt() const -> StatedAt;
  [[nodiscard]] auto PartsStatedAt() const -> StatedAt;
  [[nodiscard]] auto PartsOwnerStatedAt() const -> StatedAt;
  // The name the implementation gives the class its parts answer as, which is
  // what that class's table is found by.
  [[nodiscard]] static auto PartsClassName() -> const char*;

  void Copy(const void* value, void* out) const override;
  void Move(void* value, void* out) const noexcept override;
  void Destroy(void* value) const noexcept override;
  void Assign(void* storage, const void* value) const override;

  [[nodiscard]] auto Equal(const void* lhs, const void* rhs) const
      -> FourStateBit override;
  [[nodiscard]] auto CaseEqual(const void* lhs, const void* rhs) const
      -> bool override;
  [[nodiscard]] auto BitIdentical(const void* lhs, const void* rhs) const
      -> bool override;
  [[nodiscard]] auto HasUnknown(const void* value) const -> bool override;
  void ClearAboveWidths(void* value) const override;

  void BuildDefault(void* out) const override;

  void BitstreamWidth(const void* value, void* out) const override;
  void CountBits(
      const void* value, const ConstIntegralView& control_bits,
      void* out) const override;
  auto WriteToStream(
      const void* value, const IntegralView& stream, std::uint64_t filled) const
      -> std::uint64_t override;
  auto ReadFromStream(
      const ConstIntegralView& stream, std::uint64_t taken, void* out) const
      -> std::uint64_t override;

  void ResolveTriState(
      const void* lhs, const void* rhs, void* out) const override;
  void ResolveWiredAnd(
      const void* lhs, const void* rhs, void* out) const override;
  void ResolveWiredOr(
      const void* lhs, const void* rhs, void* out) const override;
  void Dominating(
      const void* stronger, const void* weaker, void* out) const override;
  void FilledLike(
      const void* prototype, const void* fill, void* out) const override;

  [[nodiscard]] auto OrderBefore(const void* lhs, const void* rhs) const
      -> bool override;
  [[nodiscard]] auto IsTrue(const void* value) const -> bool override;
  void Reduce(Reduction reduction, const void* lhs, const void* rhs, void* out)
      const override;

  [[nodiscard]] auto Parts() const -> const PartsByPosition* override;
  [[nodiscard]] auto AsIntegral() const -> const IntegralValueType* override;
  [[nodiscard]] auto AsFixedArray() const -> const FixedArrayType* override;

  // An array of this type built in `out`, its element at each position built
  // by `build(position, element_out)`.
  template <typename Build>
  void BuildEach(void* out, Build build) const {
    Building building(*this, out);
    for (std::size_t i = 0; i < count_; ++i) {
      build(i, building.Next());
      building.Advance();
    }
    building.Finish();
  }

 private:
  // An array's elements answered by position, for what walks a value down to
  // its leaves. It names the array type it answers for, since the questions
  // are asked of a value alone.
  class ElementsByPosition final : public PartsByPosition {
   public:
    explicit ElementsByPosition(const FixedArrayType& array) : array_(&array) {
    }

    [[nodiscard]] auto Count(const void* value) const -> std::size_t override;
    [[nodiscard]] auto Type(const void* value) const
        -> const ValueType& override;
    [[nodiscard]] auto At(const void* value, std::size_t position) const
        -> const void* override;
    [[nodiscard]] auto RefAt(void* value, std::size_t position) const
        -> void* override;

   private:
    friend class FixedArrayType;
    const FixedArrayType* array_;
  };

  // An array being built element by element in storage that holds none yet.
  // Whatever was built is ended unless the build finishes.
  class Building {
   public:
    Building(const FixedArrayType& type, void* out);
    Building(const Building&) = delete;
    auto operator=(const Building&) -> Building& = delete;
    Building(Building&&) = delete;
    auto operator=(Building&&) -> Building& = delete;
    ~Building();

    // Storage for the next element, and the caller's word that it built one
    // there.
    [[nodiscard]] auto Next() -> void*;
    void Advance();
    void Finish();

   private:
    const FixedArrayType* type_;
    void* out_;
    std::size_t built_ = 0;
    bool finished_ = false;
  };

  [[nodiscard]] auto Layout() const -> FixedArrayLayout;

  using Fold =
      void (ValueType::*)(const void* lhs, const void* rhs, void* out) const;
  void FoldedBy(Fold fold, const void* lhs, const void* rhs, void* out) const;

  std::uint32_t count_;
  const value::ElementType* element_;
  ElementsByPosition parts_;
};

// A fixed-size unpacked array lying in something else, read where it lies. It
// owns nothing, so the array has to outlive it.
struct FixedArrayView {
  const FixedArrayType* type;
  const void* bytes;

  [[nodiscard]] auto Count() const -> std::size_t {
    return type->Count();
  }
  [[nodiscard]] auto ElementType() const -> const value::ElementType& {
    return type->ElementType();
  }
  // The element at storage position `position`, counted from the left.
  [[nodiscard]] auto ElementAt(std::size_t position) const -> const void* {
    return type->ElementAt(bytes, position);
  }
};

// LRM 21.3.4.3: an array of bytes read as a contiguous character sequence in
// element order, the low byte of each element becoming one character and
// embedded NULs included -- what a scan takes as its input text.
[[nodiscard]] auto ByteStringOf(const FixedArrayView& array) -> String;

}  // namespace lyra::value
