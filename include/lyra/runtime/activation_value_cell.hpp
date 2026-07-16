#pragma once

#include "lyra/runtime/value_storage_core.hpp"
#include "lyra/value/concepts.hpp"

namespace lyra::runtime {

// A procedural local's value storage, owned by an activation and reached by a
// handle a suspending body holds across suspensions. It is the runtime home a
// value-typed local needs on the execution backend, where a value crosses the
// boundary as an opaque handle whose transient arena storage is released at
// each suspension: the handle a generated frame holds points here, into
// activation-lifetime storage, not into that per-stretch arena.
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

}  // namespace lyra::runtime
