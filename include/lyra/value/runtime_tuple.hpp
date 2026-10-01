#pragma once

#include <cstddef>
#include <memory>
#include <new>
#include <vector>

#include "lyra/support/tuple_operations.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::value {

struct RuntimeValue;

// A tuple -- an unpacked struct (LRM 7.2) among them -- as the runtime holds
// one. The execution backend's library is compiled once, so it cannot be
// instantiated for each tuple type the way the C++ backend's `Tuple<Ts...>` is;
// it holds every tuple through this one type instead. What it holds is the
// tuple exactly as the program lays it out, in storage of its own, opening with
// the table of its type's operations, so the value is the same bytes wherever
// it lies and every operation below is a call through that table.
class RuntimeTuple {
 public:
  // Holds no tuple: a variable's storage before its declaration installs one.
  RuntimeTuple() = default;
  RuntimeTuple(const RuntimeTuple& other);
  RuntimeTuple(RuntimeTuple&& other) noexcept = default;
  auto operator=(const RuntimeTuple& other) -> RuntimeTuple&;
  auto operator=(RuntimeTuple&& other) noexcept -> RuntimeTuple& = default;
  ~RuntimeTuple() = default;

  // A copy of the tuple laid out at `laid_out`.
  [[nodiscard]] static auto CopyOf(const void* laid_out) -> RuntimeTuple;

  // Takes the tuple laid out at `laid_out`, leaving what is there to be ended
  // by whoever gave it.
  [[nodiscard]] static auto MovedFrom(void* laid_out) -> RuntimeTuple;

  // A tuple of `ops`'s type that `build` lays out in the storage it is handed,
  // which the tuple then owns.
  template <typename Build>
  [[nodiscard]] static auto Built(
      const support::TupleOperations& ops, Build build) -> RuntimeTuple {
    const std::align_val_t align{ops.align};
    std::unique_ptr<void, Deallocate> storage(
        ::operator new(ops.size, align), Deallocate{align});
    build(storage.get());
    RuntimeTuple built;
    built.bytes_.reset(storage.release());
    return built;
  }

  // Lays out in `out` the tuple whose table `out` already holds, from one value
  // per component. The caller of a library call answering a tuple knows its
  // type and the library does not, so the caller states the type in the
  // storage it gives, and this builds the rest.
  static auto LayOut(void* out, std::vector<RuntimeValue> components) -> void*;

  // The tuple's bytes, which is what crosses as its handle.
  [[nodiscard]] auto Bytes() const -> const void*;
  [[nodiscard]] auto Bytes() -> void*;

  // What a tuple wherever it lies answers through the table its bytes open
  // with, for a tuple no object of this type holds -- one lent by reference,
  // or a component of another. Component `index` of the tuple laid out at
  // `laid_out` lies at the address this answers, which is that component's
  // handle: a tuple component's bytes are laid out there, and every other
  // component's object is.
  [[nodiscard]] static auto ComponentAt(void* laid_out, std::size_t index)
      -> void*;
  [[nodiscard]] static auto BitIdentical(const void* lhs, const void* rhs)
      -> bool;
  // Writes the tuple laid out at `value` into the one laid out at `storage`,
  // which goes on being that tuple.
  static void AssignAt(void* storage, const void* value);

  // A copy of the tuple, or the tuple itself, laid out in `out`.
  auto CopyInto(void* out) const -> void*;
  auto MoveInto(void* out) && -> void*;

  // How many components the tuple holds, and a copy of component `index` as a
  // value of its own domain.
  [[nodiscard]] auto RawSize() const -> std::size_t;
  [[nodiscard]] auto Component(std::size_t index) const -> RuntimeValue;

  [[nodiscard]] auto operator==(const RuntimeTuple& other) const -> PackedArray;
  [[nodiscard]] auto operator!=(const RuntimeTuple& other) const -> PackedArray;
  [[nodiscard]] auto CaseEqual(const RuntimeTuple& other) const -> PackedArray;
  [[nodiscard]] auto ResolveTriState(const RuntimeTuple& other) const
      -> RuntimeTuple;
  [[nodiscard]] auto ResolveWiredAnd(const RuntimeTuple& other) const
      -> RuntimeTuple;
  [[nodiscard]] auto ResolveWiredOr(const RuntimeTuple& other) const
      -> RuntimeTuple;
  [[nodiscard]] auto Dominating(const RuntimeTuple& weaker) const
      -> RuntimeTuple;
  [[nodiscard]] static auto FilledLike(
      const RuntimeTuple& prototype, const PackedArray& fill) -> RuntimeTuple;
  [[nodiscard]] auto IsBitIdentical(const RuntimeTuple& other) const -> bool;
  [[nodiscard]] auto HasUnknown() const -> bool;
  [[nodiscard]] auto IsUnknown() const -> PackedArray;
  [[nodiscard]] auto BitstreamWidth() const -> PackedArray;
  [[nodiscard]] auto CountBits(const PackedArray& control_bits) const
      -> PackedArray;
  [[nodiscard]] auto ToBitstream() const -> PackedArray;
  [[nodiscard]] static auto FromBitstream(
      const PackedArray& bits, const RuntimeTuple& prototype) -> RuntimeTuple;

 private:
  // Frees storage a tuple was being built in, for a build that did not finish:
  // nothing in it is known to be whole, so nothing is ended.
  struct Deallocate {
    std::align_val_t align;
    void operator()(void* storage) const noexcept {
      ::operator delete(storage, align);
    }
  };

  // Ends the tuple laid out at `laid_out` through its type's table, then frees
  // the storage at the alignment that table states.
  struct End {
    void operator()(void* laid_out) const noexcept;
  };

  [[nodiscard]] auto Ops() const -> const support::TupleOperations&;

  // One of the type's three folds of two net contributions, which the three
  // tables share the shape of.
  using Fold = void* (*)(const void* lhs, const void* rhs, void* out);
  [[nodiscard]] auto FoldedBy(Fold fold, const RuntimeTuple& other) const
      -> RuntimeTuple;

  std::unique_ptr<void, End> bytes_;
};

static_assert(LyraValue<RuntimeTuple>);
static_assert(NetResolvable<RuntimeTuple>);
static_assert(CaseEqualComparable<RuntimeTuple>);
static_assert(BitstreamSizable<RuntimeTuple>);
static_assert(BitstreamConvertible<RuntimeTuple>);

}  // namespace lyra::value
