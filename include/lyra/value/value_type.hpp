#pragma once

#include <bit>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <type_traits>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/support/layout.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/reduction.hpp"

namespace lyra::value {

class FixedArrayType;
class IntegralValueType;
class ValueType;

// What a value whose parts are ordered by position -- a fixed-size array, a
// dynamic array, a queue (LRM 7.4, 7.5, 7.10) -- answers about them, one
// function each, handed the value's type first as every witness is: how many
// it holds, the type they are, and where the one at storage position
// `position` lies, for reading and as storage a write lands in. What walks a
// value down to its leaves -- imaging it for a foreign call (LRM H.12),
// loading a memory file into it (LRM 21.4) -- asks these of the type at each
// level. Generated code names the library's own for a fixed-size array, whose
// functions read everything off the type they are handed.
struct PartsWitnessTable {
  std::size_t (*count)(const ValueType& type, const void* value);
  const ValueType& (*type)(const ValueType& type, const void* value);
  const void* (*at)(
      const ValueType& type, const void* value, std::size_t position);
  void* (*ref_at)(const ValueType& type, void* value, std::size_t position);
};

// Where a type states one of its numbers: how far into the record the number
// lies, and how many bytes it takes.
struct StatedAt {
  std::size_t offset;
  std::size_t bytes;
};

// The storage `bytes` past `from`, where a part laid out by a type lies.
[[nodiscard]] inline auto Offset(const void* from, std::size_t bytes) -> const
    void* {
  return std::bit_cast<const void*>(
      std::bit_cast<std::uintptr_t>(from) + bytes);
}
[[nodiscard]] inline auto Offset(void* from, std::size_t bytes) -> void* {
  return std::bit_cast<void*>(std::bit_cast<std::uintptr_t>(from) + bytes);
}

// What code compiled without knowing a type needs of a value of it, one
// function each: the storage's own lifecycle, the operations the language
// defines on the whole value (LRM 11.4.5, 20.6.2, 20.9, 6.24.3, 6.6.1,
// 28.12.1) and those a container's algorithms ask of an element or of what a
// `with` clause answers (LRM 7.8, 7.12, 12.4), and the parts the value holds
// by position, where it holds any. Each function is the type's witness for one
// operation, Swift's word for the function showing that a type has it, and is
// handed the type first, as Swift hands each value witness the type's
// metadata, so one table serves every type whose functions read the rest off
// the type.
//
// The table is this declaration, which generated code and the library both
// read: generated code states one for each type it compiles the operations
// of, placing each function where this record lays it out.
//
// Every value is passed as its address. Copying and moving build in `out` a
// value the source still has to end; `assign` writes into a value already
// there, which goes on being that value. An operation answering a value builds
// it in `out`. One the language does not define for the type is never asked of
// it, since the front end rejects the program asking.
//
// An integral answer is built as the bytes of its type where the language fixes
// that type -- a width's and a count's `int`. An equality answers its 0, 1 or
// x as a scalar, and a case equality, which is never unknown (LRM 11.4.5),
// whether it holds; whoever knows the type the comparison has holds either as
// a value of it. A stream of bits is an integral value, which a value writes
// its own bits into and is read back out of (LRM 6.24.3), and the control bits
// a count is told of are one too. A fill is the bytes of a `logic`.
struct ValueWitnessTable {
  void (*copy)(const ValueType& type, const void* value, void* out);
  void (*move)(const ValueType& type, void* value, void* out) noexcept;
  void (*destroy)(const ValueType& type, void* value) noexcept;
  void (*assign)(const ValueType& type, void* storage, const void* value);

  FourStateBit (*equal)(
      const ValueType& type, const void* lhs, const void* rhs);
  bool (*case_equal)(const ValueType& type, const void* lhs, const void* rhs);
  bool (*bit_identical)(
      const ValueType& type, const void* lhs, const void* rhs);
  bool (*has_unknown)(const ValueType& type, const void* value);

  // The value storage of the type holds before anything writes it, which the
  // type alone fixes: a singular type's default (LRM 6.4, Table 6-7), an empty
  // container, and an aggregate's parts each at what its own type builds. What
  // a declaration states of its members (LRM 7.2.2) is written by its
  // initialization after this. A union holds its first member at what that
  // member's type builds, every other byte zero (LRM 7.3).
  void (*build_default)(const ValueType& type, void* out);

  void (*bitstream_width)(const ValueType& type, const void* value, void* out);
  void (*count_bits)(
      const ValueType& type, const void* value,
      const ConstIntegralView& control_bits, void* out);
  // The value's bits written into a stream below its `filled` most
  // significant positions, and the value of this type the stream holds below
  // its `taken` most significant ones, built in `out`. The type fixes how
  // many positions a value of it takes and what each part of it holds, which
  // is everything a sequence of bits carries nothing of. Each answers how many
  // positions are filled, or taken, after it.
  std::uint64_t (*write_to_stream)(
      const ValueType& type, const void* value, const IntegralView& stream,
      std::uint64_t filled);
  std::uint64_t (*read_from_stream)(
      const ValueType& type, const ConstIntegralView& stream,
      std::uint64_t taken, void* out);

  void (*resolve_tri_state)(
      const ValueType& type, const void* lhs, const void* rhs, void* out);
  void (*resolve_wired_and)(
      const ValueType& type, const void* lhs, const void* rhs, void* out);
  void (*resolve_wired_or)(
      const ValueType& type, const void* lhs, const void* rhs, void* out);
  void (*dominating)(
      const ValueType& type, const void* stronger, const void* weaker,
      void* out);
  void (*filled_like)(
      const ValueType& type, const void* prototype, const void* fill,
      void* out);
  // `into` with the `count` positions from `first` taken from `from`, which
  // a procedural continuous assignment holding them shows there (LRM 10.6.2):
  // an integral value's bits, an aggregate's parts' positions one part after
  // another, and the one position of any other value, held whole. The two
  // numbers are machine integers, passed by address as every operand is.
  void (*take_positions)(
      const ValueType& type, const void* into, const void* from,
      const std::int64_t* first, const std::int64_t* count, void* out);

  // The order an associative array keeps its indices in (LRM 7.8.2, 7.8.4)
  // and the one `min`, `max` and `sort` read (LRM 7.12).
  bool (*order_before)(const ValueType& type, const void* lhs, const void* rhs);
  // Whether a value holds as a condition (LRM 12.4).
  bool (*is_true)(const ValueType& type, const void* value);
  void (*reduce)(
      const ValueType& type, Reduction reduction, const void* lhs,
      const void* rhs, void* out);

  // What a value of the type answers about its parts, where they are ordered
  // by position, and nothing for a type whose parts are not.
  const PartsWitnessTable* parts;
};

// What every witness of an operation the language does not define for its type
// does when asked, which the front end makes unreachable: generated code
// names it, through the library's entry for it, for each such entry of a
// table it states.
[[noreturn]] void LacksOperation();

// The table of the witnesses `W` states: a static member for each entry, a
// function or a lambda taking and answering what the entry does, and
// `W::kParts` for its parts. Every table the library states is made this way,
// so a witness is written once and the table only lists them.
template <typename W>
inline constexpr ValueWitnessTable kValueWitnessTableFor{
    .copy = W::Copy,
    .move = W::Move,
    .destroy = W::Destroy,
    .assign = W::Assign,
    .equal = W::Equal,
    .case_equal = W::CaseEqual,
    .bit_identical = W::BitIdentical,
    .has_unknown = W::HasUnknown,
    .build_default = W::BuildDefault,
    .bitstream_width = W::BitstreamWidth,
    .count_bits = W::CountBits,
    .write_to_stream = W::WriteToStream,
    .read_from_stream = W::ReadFromStream,
    .resolve_tri_state = W::ResolveTriState,
    .resolve_wired_and = W::ResolveWiredAnd,
    .resolve_wired_or = W::ResolveWiredOr,
    .dominating = W::Dominating,
    .filled_like = W::FilledLike,
    .take_positions = W::TakePositions,
    .order_before = W::OrderBefore,
    .is_true = W::IsTrue,
    .reduce = W::Reduce,
    .parts = W::kParts};

// Which of the records the library reads a type through this one opens, where
// it opens one: an integral type's (LRM 6.11), whose values a facility
// compiled for every type reads the bits of, and a fixed-size unpacked array
// type's (LRM 7.4.2). Every other type is its table and its numbers alone.
enum class ValueTypeKind : std::uint32_t {
  kIntegral,
  kFixedArray,
  kOther,
};

// A type as code compiled before any design existed is handed it: how much
// storage a value of it takes, its table, and which record it opens. A library
// compiled once holds a value of a type the design declares as its bytes
// together with this, the way Swift's generic code holds a value with its
// type's metadata and Rust's `dyn` reference holds one with its vtable.
//
// A value's type is exact, since nothing in the language makes the type a value
// has differ from the type it is held as, so whoever holds the value states its
// type once and the value carries none of its own. A reference and a part
// designated within a write name bytes lying in something else and hold no
// type, so an access through one of those is told the type of what it
// reaches.
//
// It is laid out as a C record, so code stating one as constant data places
// each number where this declaration does; a record adding numbers of its
// own opens with one of these, so the type's address is the record's.
class ValueType {
 public:
  constexpr ValueType(
      const ValueWitnessTable& witnesses, std::uint32_t size,
      std::uint32_t align, ValueTypeKind kind)
      : witnesses_(&witnesses), size_(size), align_(align), kind_(kind) {
  }
  ValueType(const ValueType&) = delete;
  auto operator=(const ValueType&) -> ValueType& = delete;
  ValueType(ValueType&&) = delete;
  auto operator=(ValueType&&) -> ValueType& = delete;
  ~ValueType() = default;

  [[nodiscard]] auto Size() const -> std::size_t {
    return size_;
  }
  [[nodiscard]] auto Align() const -> std::size_t {
    return align_;
  }
  [[nodiscard]] auto Layout() const -> support::Layout {
    return {.size = size_, .align = align_};
  }

  void Copy(const void* value, void* out) const {
    witnesses_->copy(*this, value, out);
  }
  void Move(void* value, void* out) const noexcept {
    witnesses_->move(*this, value, out);
  }
  void Destroy(void* value) const noexcept {
    witnesses_->destroy(*this, value);
  }
  void Assign(void* storage, const void* value) const {
    witnesses_->assign(*this, storage, value);
  }

  [[nodiscard]] auto Equal(const void* lhs, const void* rhs) const
      -> FourStateBit {
    return witnesses_->equal(*this, lhs, rhs);
  }
  [[nodiscard]] auto CaseEqual(const void* lhs, const void* rhs) const -> bool {
    return witnesses_->case_equal(*this, lhs, rhs);
  }
  [[nodiscard]] auto BitIdentical(const void* lhs, const void* rhs) const
      -> bool {
    return witnesses_->bit_identical(*this, lhs, rhs);
  }
  [[nodiscard]] auto HasUnknown(const void* value) const -> bool {
    return witnesses_->has_unknown(*this, value);
  }

  void BuildDefault(void* out) const {
    witnesses_->build_default(*this, out);
  }

  void BitstreamWidth(const void* value, void* out) const {
    witnesses_->bitstream_width(*this, value, out);
  }
  void CountBits(
      const void* value, const ConstIntegralView& control_bits,
      void* out) const {
    witnesses_->count_bits(*this, value, control_bits, out);
  }
  auto WriteToStream(
      const void* value, const IntegralView& stream, std::uint64_t filled) const
      -> std::uint64_t {
    return witnesses_->write_to_stream(*this, value, stream, filled);
  }
  auto ReadFromStream(
      const ConstIntegralView& stream, std::uint64_t taken, void* out) const
      -> std::uint64_t {
    return witnesses_->read_from_stream(*this, stream, taken, out);
  }

  void ResolveTriState(const void* lhs, const void* rhs, void* out) const {
    witnesses_->resolve_tri_state(*this, lhs, rhs, out);
  }
  void ResolveWiredAnd(const void* lhs, const void* rhs, void* out) const {
    witnesses_->resolve_wired_and(*this, lhs, rhs, out);
  }
  void ResolveWiredOr(const void* lhs, const void* rhs, void* out) const {
    witnesses_->resolve_wired_or(*this, lhs, rhs, out);
  }
  void Dominating(const void* stronger, const void* weaker, void* out) const {
    witnesses_->dominating(*this, stronger, weaker, out);
  }
  void FilledLike(const void* prototype, const void* fill, void* out) const {
    witnesses_->filled_like(*this, prototype, fill, out);
  }
  void TakePositions(
      const void* into, const void* from, std::int64_t first,
      std::int64_t count, void* out) const {
    witnesses_->take_positions(*this, into, from, &first, &count, out);
  }

  [[nodiscard]] auto OrderBefore(const void* lhs, const void* rhs) const
      -> bool {
    return witnesses_->order_before(*this, lhs, rhs);
  }
  [[nodiscard]] auto IsTrue(const void* value) const -> bool {
    return witnesses_->is_true(*this, value);
  }
  void Reduce(
      Reduction reduction, const void* lhs, const void* rhs, void* out) const {
    witnesses_->reduce(*this, reduction, lhs, rhs, out);
  }

  // The parts of the value lying at `value`, for a type whose parts are
  // ordered by position, which only a walk the compiler set out over such a
  // type asks.
  [[nodiscard]] auto PartCount(const void* value) const -> std::size_t {
    return Parts().count(*this, value);
  }
  [[nodiscard]] auto PartType(const void* value) const -> const ValueType& {
    return Parts().type(*this, value);
  }
  [[nodiscard]] auto PartAt(const void* value, std::size_t position) const
      -> const void* {
    return Parts().at(*this, value, position);
  }
  [[nodiscard]] auto PartAt(void* value, std::size_t position) const -> void* {
    return Parts().ref_at(*this, value, position);
  }

  // This type as an integral one, or as a fixed-size unpacked array type,
  // where it opens that record, and nothing otherwise.
  [[nodiscard]] auto AsIntegral() const -> const IntegralValueType* {
    switch (kind_) {
      case ValueTypeKind::kIntegral:
        return std::bit_cast<const IntegralValueType*>(this);
      case ValueTypeKind::kFixedArray:
      case ValueTypeKind::kOther:
        return nullptr;
    }
    std::unreachable();
  }
  [[nodiscard]] auto AsFixedArray() const -> const FixedArrayType* {
    switch (kind_) {
      case ValueTypeKind::kFixedArray:
        return std::bit_cast<const FixedArrayType*>(this);
      case ValueTypeKind::kIntegral:
      case ValueTypeKind::kOther:
        return nullptr;
    }
    std::unreachable();
  }

  // Where the type states the address of its table, and its size, its
  // alignment and which record it opens.
  [[nodiscard]] static constexpr auto WitnessesStatedAt() -> std::size_t {
    return offsetof(ValueType, witnesses_);
  }
  [[nodiscard]] static constexpr auto SizeStatedAt() -> StatedAt {
    return {.offset = offsetof(ValueType, size_), .bytes = sizeof(size_)};
  }
  [[nodiscard]] static constexpr auto AlignStatedAt() -> StatedAt {
    return {.offset = offsetof(ValueType, align_), .bytes = sizeof(align_)};
  }
  [[nodiscard]] static constexpr auto KindStatedAt() -> StatedAt {
    return {.offset = offsetof(ValueType, kind_), .bytes = sizeof(kind_)};
  }

 private:
  [[nodiscard]] auto Parts() const -> const PartsWitnessTable& {
    if (witnesses_->parts == nullptr) {
      throw InternalError(
          "a value is walked by position whose type orders no parts so -- "
          "please report this as a bug");
    }
    return *witnesses_->parts;
  }

  const ValueWitnessTable* witnesses_;
  std::uint32_t size_;
  std::uint32_t align_;
  ValueTypeKind kind_;
};

static_assert(std::is_standard_layout_v<PartsWitnessTable>);
static_assert(std::is_standard_layout_v<ValueWitnessTable>);
static_assert(std::is_standard_layout_v<ValueType>);

// A value the library holds of a type it was compiled without and is handed as
// bytes laid out by that type, which state nothing of it -- a tuple, a union, a
// fixed-size unpacked array: kept with its type, and compared with and taking
// the bytes of a value of that type.
template <class T>
concept LaidOutByType = TakesBytesInPlace<T> && requires(const T& read) {
  { read.Type() } -> std::convertible_to<const ValueType&>;
};

}  // namespace lyra::value
