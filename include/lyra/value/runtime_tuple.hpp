#pragma once

#include <cstddef>
#include <cstdint>
#include <span>

#include "lyra/value/any_value.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/value_type.hpp"
#include "lyra/value/wide.hpp"

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

  // Where component `index` of a tuple of this type laid out at `laid_out`
  // lies, which is that component's handle: a tuple or a fixed-size array
  // component's bytes are laid out there, and every other component's object
  // is.
  [[nodiscard]] auto ComponentAt(void* laid_out, std::size_t index) const
      -> void*;
  [[nodiscard]] auto ComponentAt(const void* laid_out, std::size_t index) const
      -> const void*;

  // Builds at `laid_out` a tuple of this type with each component at what its
  // own type builds before anything writes it. A component that fails to
  // build ends the ones built before it.
  void BuildEachComponentDefault(void* laid_out) const;

 private:
  std::uint32_t count_;
  const TupleComponent* components_;
};

// A tuple as the runtime keeps one of its own. The execution backend's library
// is compiled once, so it cannot be instantiated for each tuple type the way
// the C++ backend's `Tuple<Ts...>` is; it holds every tuple through this one
// type instead: the tuple exactly as the program lays it out, in storage of its
// own, held with its type as any value the library was compiled without is.
// The bytes carry no type, so whoever hands a tuple in by its bytes states the
// type beside them.
class RuntimeTuple {
 public:
  // Holds no tuple: storage before whatever declares it installs one.
  RuntimeTuple() = default;

  // The tuple `value` holds, which is a value of a tuple type.
  explicit RuntimeTuple(AnyValue value);

  // A copy of the tuple of `type` laid out at `laid_out`.
  [[nodiscard]] static auto CopyOf(const TupleType& type, const void* laid_out)
      -> RuntimeTuple;

  // The tuple's bytes, which is what crosses as its handle, and its type.
  [[nodiscard]] auto Bytes() const -> const void*;
  [[nodiscard]] auto Bytes() -> void*;
  [[nodiscard]] auto Type() const -> const ValueType&;

  // The tuple of this one's type laid out at `bytes`, compared with and taken
  // into the components this holds, so a store of one obtains no storage for
  // the tuple. The bytes may be this tuple's own.
  [[nodiscard]] auto IsBitIdenticalTo(const void* bytes) const -> bool;
  void AssignBytes(const void* bytes);

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
  [[nodiscard]] auto WriteToStream(
      const IntegralView& stream, std::uint64_t filled) const -> std::uint64_t;

 private:
  AnyValue value_;
};

static_assert(LyraValue<RuntimeTuple>);
static_assert(NetResolvable<RuntimeTuple>);
static_assert(CaseEqualComparable<RuntimeTuple>);
static_assert(BitstreamSizable<RuntimeTuple, ConstIntegralView>);
static_assert(TakesBytesInPlace<RuntimeTuple>);

}  // namespace lyra::value
