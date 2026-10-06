#pragma once

#include <memory>
#include <new>

#include "lyra/value/concepts.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

// A value of a type the library was compiled without, in storage of its own,
// held with its type: what Rust's `Box<dyn Trait>` and Swift's existential
// container are. Everything it does it asks of that type, so it answers the
// language's whole-value operations as a value of the type would.
class AnyValue {
 public:
  // Holds no value: storage before whatever declares it installs one.
  AnyValue() = default;

  // A value of `type` that `build` lays out in the storage it is handed, which
  // the value then owns.
  template <typename Build>
  [[nodiscard]] static auto Built(const ValueType& type, Build build)
      -> AnyValue {
    const std::align_val_t align{type.Align()};
    std::unique_ptr<void, Deallocate> storage(
        ::operator new(type.Size(), align), Deallocate{align});
    build(storage.get());
    return {storage.release(), type};
  }

  // A copy of the value of `type` lying at `value`.
  [[nodiscard]] static auto CopyOf(const ValueType& type, const void* value)
      -> AnyValue;

  AnyValue(const AnyValue& other);
  auto operator=(const AnyValue& other) -> AnyValue&;
  AnyValue(AnyValue&&) noexcept = default;
  auto operator=(AnyValue&&) noexcept -> AnyValue& = default;
  ~AnyValue() = default;

  [[nodiscard]] auto Type() const -> const ValueType&;
  [[nodiscard]] auto Bytes() const -> const void* {
    return bytes_.get();
  }
  [[nodiscard]] auto Bytes() -> void* {
    return bytes_.get();
  }

  [[nodiscard]] auto operator==(const AnyValue& other) const -> PackedArray;
  [[nodiscard]] auto operator!=(const AnyValue& other) const -> PackedArray;
  [[nodiscard]] auto CaseEqual(const AnyValue& other) const -> PackedArray;
  [[nodiscard]] auto ResolveTriState(const AnyValue& other) const -> AnyValue;
  [[nodiscard]] auto ResolveWiredAnd(const AnyValue& other) const -> AnyValue;
  [[nodiscard]] auto ResolveWiredOr(const AnyValue& other) const -> AnyValue;
  [[nodiscard]] auto Dominating(const AnyValue& weaker) const -> AnyValue;
  [[nodiscard]] static auto FilledLike(
      const AnyValue& prototype, const PackedArray& fill) -> AnyValue;
  // Two values hold the same bits only where they are of one type; a holder
  // with no value differs from one with a value.
  [[nodiscard]] auto IsBitIdentical(const AnyValue& other) const -> bool;
  [[nodiscard]] auto HasUnknown() const -> bool;
  [[nodiscard]] auto IsUnknown() const -> PackedArray;
  [[nodiscard]] auto BitstreamWidth() const -> PackedArray;
  [[nodiscard]] auto CountBits(const PackedArray& control_bits) const
      -> PackedArray;
  [[nodiscard]] auto ToBitstream() const -> PackedArray;
  [[nodiscard]] static auto FromBitstream(
      const PackedArray& bits, const AnyValue& prototype) -> AnyValue;

 private:
  // Frees storage a value was being built in, for a build that did not finish:
  // nothing in it is known to be whole, so nothing is ended.
  struct Deallocate {
    std::align_val_t align;
    void operator()(void* storage) const noexcept {
      ::operator delete(storage, align);
    }
  };

  struct End {
    const ValueType* type;
    void operator()(void* value) const noexcept {
      type->Destroy(value);
      Deallocate{std::align_val_t{type->Align()}}(value);
    }
  };

  AnyValue(void* value, const ValueType& type) : bytes_(value, End{&type}) {
  }

  // One of the type's three folds of two net contributions, which share one
  // shape.
  using Fold =
      void (ValueType::*)(const void* lhs, const void* rhs, void* out) const;
  [[nodiscard]] auto FoldedBy(Fold fold, const AnyValue& other) const
      -> AnyValue;

  std::unique_ptr<void, End> bytes_;
};

static_assert(LyraValue<AnyValue>);
static_assert(NetResolvable<AnyValue>);
static_assert(CaseEqualComparable<AnyValue>);
static_assert(BitstreamSizable<AnyValue>);
static_assert(BitstreamConvertible<AnyValue>);

}  // namespace lyra::value
