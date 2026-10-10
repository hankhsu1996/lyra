#pragma once

#include <cstddef>
#include <cstdint>
#include <span>

#include "lyra/value/any_value.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

class TupleType;

// Where one component of a tuple sits in the tuple's storage, and the type it
// is a value of.
struct TupleComponent {
  std::uint32_t offset;
  const ValueType* type;
};

// The type of one tuple: an unpacked structure (LRM 7.2), or a call's answer,
// its result together with its `output` and `inout` arguments (LRM 13.5). Only
// the code that laid a tuple type out can state its operations, so every one
// is generated there, one per type, and nothing here builds one.
class TupleType : public ValueType {
 public:
  TupleType() = delete;

  [[nodiscard]] auto Components() const -> std::span<const TupleComponent> {
    return {components_, count_};
  }

 private:
  std::uint32_t count_;
  const TupleComponent* components_;
};

// A tuple as the runtime holds one. The execution backend's library is compiled
// once, so it cannot be instantiated for each tuple type the way the C++
// backend's `Tuple<Ts...>` is; it holds every tuple through this one type
// instead: the tuple exactly as the program lays it out, in storage of its own,
// held with its type as any value the library was compiled without is. The
// bytes open with the address of that type, so a tuple lent by its bytes alone
// still says what it is.
class RuntimeTuple {
 public:
  // Holds no tuple: a variable's storage before its declaration installs one.
  RuntimeTuple() = default;

  // A copy of the tuple laid out at `laid_out`.
  [[nodiscard]] static auto CopyOf(const void* laid_out) -> RuntimeTuple;

  // The tuple's bytes, which is what crosses as its handle, and the type they
  // open with.
  [[nodiscard]] auto Bytes() const -> const void*;
  [[nodiscard]] auto Bytes() -> void*;
  [[nodiscard]] auto Type() const -> const TupleType&;

  // The type the tuple laid out at `laid_out` opens with. The caller of a
  // library call answering a tuple knows its type and the library does not,
  // so the caller states the type in the storage it gives.
  [[nodiscard]] static auto TypeAt(const void* laid_out) -> const TupleType&;

  // What a tuple wherever it lies answers through the type its bytes open
  // with, for a tuple no object of this type holds -- one lent by reference,
  // or a component of another. Component `index` of the tuple laid out at
  // `laid_out` lies at the address this answers, which is that component's
  // handle: a tuple component's bytes are laid out there, and every other
  // component's object is.
  [[nodiscard]] static auto ComponentAt(void* laid_out, std::size_t index)
      -> void*;
  [[nodiscard]] static auto ComponentAt(const void* laid_out, std::size_t index)
      -> const void*;
  [[nodiscard]] static auto BitIdentical(const void* lhs, const void* rhs)
      -> bool;
  // Writes the tuple laid out at `value` into the one laid out at `storage`,
  // which goes on being that tuple.
  static void AssignAt(void* storage, const void* value);

  // A copy of the tuple, or the tuple itself, laid out in `out`.
  auto CopyInto(void* out) const -> void*;
  auto MoveInto(void* out) && -> void*;

  [[nodiscard]] auto operator==(const RuntimeTuple& other) const
      -> FourStateBit;
  [[nodiscard]] auto operator!=(const RuntimeTuple& other) const
      -> FourStateBit;
  [[nodiscard]] auto CaseEqual(const RuntimeTuple& other) const -> Bit;
  [[nodiscard]] auto ResolveTriState(const RuntimeTuple& other) const
      -> RuntimeTuple;
  [[nodiscard]] auto ResolveWiredAnd(const RuntimeTuple& other) const
      -> RuntimeTuple;
  [[nodiscard]] auto ResolveWiredOr(const RuntimeTuple& other) const
      -> RuntimeTuple;
  [[nodiscard]] auto Dominating(const RuntimeTuple& weaker) const
      -> RuntimeTuple;
  [[nodiscard]] static auto FilledLike(
      const RuntimeTuple& prototype, const Logic& fill) -> RuntimeTuple;
  [[nodiscard]] auto IsBitIdentical(const RuntimeTuple& other) const -> bool;
  [[nodiscard]] auto HasUnknown() const -> bool;
  [[nodiscard]] auto IsUnknown() const -> Bit;
  [[nodiscard]] auto BitstreamWidth() const -> Int;
  [[nodiscard]] auto CountBits(const ConstIntegralView& control_bits) const
      -> Int;
  auto WriteToStream(
      Planes stream, std::uint64_t stream_width, std::uint64_t filled) const
      -> std::uint64_t;
  [[nodiscard]] auto ReadFromStream(
      ConstPlanes stream, std::uint64_t stream_width, std::uint64_t taken) const
      -> std::pair<RuntimeTuple, std::uint64_t>;

 private:
  explicit RuntimeTuple(AnyValue value);

  AnyValue value_;
};

static_assert(LyraValue<RuntimeTuple>);
static_assert(NetResolvable<RuntimeTuple>);
static_assert(CaseEqualComparable<RuntimeTuple>);
static_assert(BitstreamSizable<RuntimeTuple, ConstIntegralView>);

}  // namespace lyra::value
