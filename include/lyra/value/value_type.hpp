#pragma once

#include <bit>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <type_traits>

#include "lyra/base/internal_error.hpp"
#include "lyra/support/layout.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/value_type_kind.hpp"

namespace lyra::value {

class AssociativeArrayType;
class DynamicArrayType;
class FixedArrayType;
class IntegralValueType;
class QueueType;
class ValueType;

// One part of a value: where it lies and its type, no place where the value
// holds no such part.
struct ValuePart {
  const void* at;
  const ValueType* type;
};

// What a value answers about the part one step of a path into it names (LRM
// 7.2, 7.4) -- an element of a fixed-size array by its position, a component
// of a product by its index -- handed the value's type first, as Swift's
// generic code reaches a field through the offsets its type's metadata lists.
// A write asks it of the value it replaced and of the one it left, so a wait
// on a part below the one written is told whether that part changed. A type
// that states none is answered as a whole: an array whose size the run sets,
// whose elements a whole write moves and ends; a tagged union, whose member
// holds a value only under its tag; a string.
struct StepWitnessTable {
  ValuePart (*part)(
      const ValueType& type, const void* value, std::uint32_t step);
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

struct ValueOperations;

// What the language's operations on a whole value of a type the library was
// compiled without do, one function each, which a container's algorithms ask
// of an element or of what a `with` clause answers (LRM 7.8, 7.12, 12.4): an
// equality, a case equality, whether any position holds x or z, and the
// value's stream of bits (LRM 11.4.5, 20.6.2, 20.9, 6.24.3). Each is handed
// the operations it belongs to first, which say the type it acts on and, for a
// container, its element's operations.
//
// An integral answer is built as the bytes of its type where the language fixes
// that type -- a width's and a count's `int`. An equality answers its 0, 1 or
// x as a scalar, and a case equality, which is never unknown (LRM 11.4.5),
// whether it holds; whoever knows the type the comparison has holds either as
// a value of it. A stream of bits is an integral value, which a value writes
// its own bits into and is read back out of (LRM 6.24.3). The control bits a
// count is told of are a value of the one type they have.
//
// What streams a value can fail, or drop what a queue in it cannot hold. No
// function here is handed where the design wrote that: each is reached under
// the place the operation asking it bound, so a report made inside one reads
// that.
struct OperationFunctions {
  FourStateBit (*equal)(
      const ValueOperations& self, const void* lhs, const void* rhs);
  bool (*case_equal)(
      const ValueOperations& self, const void* lhs, const void* rhs);
  bool (*has_unknown)(const ValueOperations& self, const void* value);

  void (*bitstream_width)(
      const ValueOperations& self, const void* value, void* out);
  void (*count_bits)(
      const ValueOperations& self, const void* value,
      const LogicVector<4>& control_bits, void* out);
  // The value's bits written into a stream below its `filled` most
  // significant positions, and the value of this type the stream holds below
  // its `taken` most significant ones, read into the value of the type `out`
  // holds, which it replaces. The type fixes what each part of a value of it
  // holds, which is everything a sequence of bits carries nothing of; a part
  // whose type fixes its width takes that many positions, and one whose width
  // the running program decides takes every position before `until`, the
  // first such part of a value taking what the rest leave (LRM 6.24.3,
  // 11.4.14.4). Each answers how many positions are filled, or taken, after
  // it.
  std::uint64_t (*write_to_stream)(
      const ValueOperations& self, const void* value,
      const IntegralView& stream, std::uint64_t filled);
  std::uint64_t (*read_from_stream)(
      const ValueOperations& self, const ConstIntegralView& stream,
      std::uint64_t taken, std::uint64_t until, void* out);
};

// The operations of one type, which an operation comparing or streaming values
// of it is handed: the functions, which a library kind states once for every
// type of it, and what they act on -- the type, and a container's element's
// operations, as Swift instantiates a conditional conformance's witness table
// with its arguments' tables. So generated code states only data for a type of
// a library kind, a container of any depth included.
struct ValueOperations {
  const OperationFunctions* functions;
  const ValueType* type;
  const ValueOperations* element;
};

// What code compiled without knowing a type needs of a value of it wherever it
// holds one, one function each: the storage's own lifecycle, and whether a
// value written over another changed it (LRM 9.4.2), since any variable may
// be waited on. Each function is the type's witness for one operation,
// Swift's word for the function showing that a type has it, and is handed the
// type first, as Swift hands each value witness the type's metadata, so one
// table serves every type whose functions read the rest off the type.
//
// The table is this declaration, which generated code and the library both
// read: generated code states one for each type it compiles the operations
// of, placing each function where this record lays it out.
//
// Every value is passed as its address. Copying builds in `out` a second value
// equal to the source; relocating builds the value itself there and takes it
// from the source, whose storage is owed nothing afterwards; `assign` writes
// into a value already there, which goes on being that value.
struct ValueWitnessTable {
  void (*copy)(const ValueType& type, const void* value, void* out);
  void (*relocate)(const ValueType& type, void* value, void* out) noexcept;
  void (*destroy)(const ValueType& type, void* value) noexcept;
  void (*assign)(const ValueType& type, void* storage, const void* value);
  bool (*bit_identical)(
      const ValueType& type, const void* lhs, const void* rhs);

  // What a value of the type answers about the part a step of a path names,
  // and nothing for a type whose value is answered as a whole.
  const StepWitnessTable* steps;
};

// What every witness of an operation the language does not define for its type
// does when asked, which the front end makes unreachable: generated code
// names it, through the library's entry for it, for each such entry of a
// table it states.
[[noreturn]] void LacksOperation();

// What a net whose data type the library was compiled without asks of the
// type (LRM 6.6.7, 6.7.1), one function each, handed the type first: a value's
// positions, read out whole and written in part, and how many it has. Only a
// net asks these, so the net is built holding the table.
struct NetWitnessTable {
  // Every position the net resolves of a value, the first least significant,
  // written into `out`, which is as wide as the value has positions.
  void (*to_positions)(
      const ValueType& type, const void* value, const IntegralView& out);
  // How many positions a value of the type has.
  std::int64_t (*position_count)(const ValueType& type);
  // Some of a value's positions replaced in place by `in`'s, whose first
  // position stands at position `start` of the value and which holds `count`;
  // answers whether any changed (LRM 6.6.7, 9.4.2).
  bool (*write_positions)(
      const ValueType& type, void* value, const ConstIntegralView* in,
      std::int64_t start, std::int64_t count);
};

// `into` with the `count` positions from `first` taken from `from`, a value of
// `type`, which a procedural continuous assignment holding them shows there
// (LRM 10.6.2): an integral value's bits, a real's IEEE 754 bits, an
// aggregate's parts' positions one part after another, and the one position of
// any other value, held whole. The two numbers are machine integers, passed by
// address as every operand is. Only a takeover asks it, so a takeover is handed
// it where it begins.
using TakePositionsWitness = void (*)(
    const ValueType& type, const void* into, const void* from,
    const std::int64_t* first, const std::int64_t* count, void* out);

// The tables of the witnesses `W` states: a static member for each entry, a
// function or a lambda taking and answering what the entry does, and, for a
// type's table, `W::kSteps` for the parts a path's steps name. Every such
// table the library states for its own kinds is made this way, so a witness
// is written once and a table only lists them.
template <typename W>
inline constexpr OperationFunctions kOperationFunctionsFor{
    .equal = W::Equal,
    .case_equal = W::CaseEqual,
    .has_unknown = W::HasUnknown,
    .bitstream_width = W::BitstreamWidth,
    .count_bits = W::CountBits,
    .write_to_stream = W::WriteToStream,
    .read_from_stream = W::ReadFromStream};

template <typename W>
inline constexpr ValueWitnessTable kValueWitnessTableFor{
    .copy = W::Copy,
    .relocate = W::Relocate,
    .destroy = W::Destroy,
    .assign = W::Assign,
    .bit_identical = W::BitIdentical,
    .steps = W::kSteps};

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
  void Relocate(void* value, void* out) const noexcept {
    witnesses_->relocate(*this, value, out);
  }
  void Destroy(void* value) const noexcept {
    witnesses_->destroy(*this, value);
  }
  void Assign(void* storage, const void* value) const {
    witnesses_->assign(*this, storage, value);
  }

  [[nodiscard]] auto BitIdentical(const void* lhs, const void* rhs) const
      -> bool {
    return witnesses_->bit_identical(*this, lhs, rhs);
  }

  // Whether the type answers the part a step of a path names, and the part
  // step `step` names in the value at `value`, no place where it holds none.
  [[nodiscard]] auto HasSteps() const -> bool {
    return witnesses_->steps != nullptr;
  }
  [[nodiscard]] auto PartAtStep(const void* value, std::uint32_t step) const
      -> ValuePart {
    if (witnesses_->steps == nullptr) {
      throw InternalError(
          "a path steps into a value whose type states no part a path steps "
          "to -- please report this as a bug");
    }
    return witnesses_->steps->part(*this, value, step);
  }

  // Which record this type opens; and this type as an integral one, as a
  // fixed-size unpacked array type, as a dynamic array type, as a queue type
  // or as an associative array type, where it opens that record, and nothing
  // otherwise.
  [[nodiscard]] auto Kind() const -> ValueTypeKind {
    return kind_;
  }
  [[nodiscard]] auto AsIntegral() const -> const IntegralValueType* {
    return Opened<IntegralValueType, ValueTypeKind::kIntegral>();
  }
  [[nodiscard]] auto AsFixedArray() const -> const FixedArrayType* {
    return Opened<FixedArrayType, ValueTypeKind::kFixedArray>();
  }
  [[nodiscard]] auto AsDynamicArray() const -> const DynamicArrayType* {
    return Opened<DynamicArrayType, ValueTypeKind::kDynamicArray>();
  }
  [[nodiscard]] auto AsQueue() const -> const QueueType* {
    return Opened<QueueType, ValueTypeKind::kQueue>();
  }
  [[nodiscard]] auto AsAssociativeArray() const -> const AssociativeArrayType* {
    return Opened<AssociativeArrayType, ValueTypeKind::kAssociativeArray>();
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
  // This type as the record `Record` that the kind `kOpens` opens, which
  // begins with this one, where this type opens it.
  template <typename Record, ValueTypeKind kOpens>
  [[nodiscard]] auto Opened() const -> const Record* {
    return kind_ == kOpens ? std::bit_cast<const Record*>(this) : nullptr;
  }

  const ValueWitnessTable* witnesses_;
  std::uint32_t size_;
  std::uint32_t align_;
  ValueTypeKind kind_;
};

static_assert(std::is_standard_layout_v<OperationFunctions>);
static_assert(std::is_standard_layout_v<ValueOperations>);
static_assert(std::is_standard_layout_v<ValueWitnessTable>);
static_assert(std::is_standard_layout_v<NetWitnessTable>);
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
