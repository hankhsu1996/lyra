#pragma once

#include <cstdint>

#include "lyra/runtime/value_storage_core.hpp"
#include "lyra/runtime/words_holding.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/integral.hpp"

namespace lyra::runtime {

// A procedural local's value storage, owned by an activation and reached by a
// handle a suspending body holds across suspensions. It is the runtime home a
// value-typed local of such a body has on the execution backend: storage that
// lives as long as the execution, which a reference can bind.
//
// It is not observable -- a procedural local is not a signal, so a write is
// never an update event and no subscriber is woken. That non-observation is the
// whole difference from `Var<T>`; both build on `ValueStorageCore<T>`.
template <value::LyraValue T>
class ActivationValueCell : public ValueStorageCore<T> {
 public:
  // Writes the local. The first write installs the declared representation -- a
  // procedural local has no construction step separate from its declaration, so
  // its initializer is that first write -- and later writes overwrite at that
  // representation.
  void Store(const T& value) {
    if (!this->IsInstalled()) {
      this->Install(value);
      return;
    }
    this->Overwrite(value);
  }
};

// The same of an integral value wider than a word, whose words lie after the
// cell. A local has no declaration to install it, so the cell is built at its
// width holding the default of its type (LRM Table 6-7), and every store into
// it is the bytes of a value that wide.
template <value::StateDomain kDomain>
class ActivationWordsCell {
 public:
  explicit ActivationWordsCell(std::uint64_t width) : held_(width) {
    held_.Take(
        WordsHolding<kDomain>::Owned::Filled(
            width, value::FourStateBit::kUnknown));
  }

  void Store(const void* bytes) {
    held_.TakeBytes(bytes);
  }
  [[nodiscard]] auto Bytes() const -> void* {
    return held_.Bytes();
  }
  [[nodiscard]] auto BytesThrough() const -> void* const* {
    return held_.BytesThrough();
  }

 private:
  WordsHolding<kDomain> held_;
};

}  // namespace lyra::runtime
