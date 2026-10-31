#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <type_traits>
#include <vector>

#include "lyra/value/element_type.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/layout.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

class String;

// The type of a fixed-size unpacked array (LRM 7.4.2) as code compiled before
// any design existed is handed it: the numbers and the table every type
// states, then how many elements, and their type, which is what reaching an
// element, a run of them, or their order by position needs. Every operation
// on a whole array is code compiled for the array type, which the type's table
// and its operations name, as a structure's and a union's are, so nothing here
// builds one.
//
// The elements lie in the array, and an element keeps its address across every
// assignment (LRM 13.5.2 has a reference to one go on naming it).
//
// Generated code states one of these for each array type it hands a value of
// to such code, as constant data laid out as this record is.
class FixedArrayType {
 public:
  // A type of `count` elements of `element`, laid out as a sequence of them,
  // answering its whole value's operations through `witnesses`.
  FixedArrayType(
      const ValueWitnessTable& witnesses, const value::ElementType& element,
      std::uint32_t count);

  [[nodiscard]] auto Type() const -> const ValueType& {
    return type_;
  }
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

  // Where the type states its count and the address of its element's type,
  // past the numbers every type states.
  [[nodiscard]] static constexpr auto CountStatedAt() -> StatedAt {
    return {
        .offset = offsetof(FixedArrayType, count_), .bytes = sizeof(count_)};
  }
  [[nodiscard]] static constexpr auto ElementStatedAt() -> std::size_t {
    return offsetof(FixedArrayType, element_);
  }

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

  FixedArrayType(
      const ValueWitnessTable& witnesses, const value::ElementType& element,
      std::uint32_t count, SequenceLayout layout);

  // How far apart two elements of an array of this type lie.
  [[nodiscard]] auto Stride() const -> std::size_t;

  ValueType type_;
  std::uint32_t count_;
  const value::ElementType* element_;
};

static_assert(std::is_standard_layout_v<FixedArrayType>);

// Any fixed-size array's elements answered by position, for what walks a
// value down to its leaves, each answer read off the array type it is handed.
// The table of every such type names this one.
extern "C" {
extern const PartsWitnessTable lyra_rt_fixed_array_parts_witness_table;
}

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
