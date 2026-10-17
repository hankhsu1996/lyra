#pragma once

#include <memory>

#include "lyra/runtime/var.hpp"
#include "lyra/value/concepts.hpp"

namespace lyra::runtime {

// A counted hold on a variable's cell, which ends with the last holder rather
// than with the scope that made it. What the generated code builds one over is
// a block's local that a branch the block spawns can outlive (LRM 6.21): the
// block holds one, each branch naming the local captures a copy, and the cell
// ends once the block and every such branch have let theirs go.
//
// Counting the holders is exact here because they are enumerable rather than
// discovered: a program cannot store a reference to an automatic, and LRM
// 9.3.2 bars a detached branch from naming a `ref` formal at all, so the only
// names into this cell are the frame that made it and the branches spawned
// under it. Both edges run one way in time, so no cycle can form and nothing
// is left for reachability to decide.
class SharedPointer {
 public:
  SharedPointer() = default;

  // The first hold and the cell come into existence together, so there is no
  // moment at which one of them is reachable without the other. A cell of a
  // value of the library's own holds nothing until the local's declaration
  // initializes it.
  template <value::LyraValue T>
  [[nodiscard]] static auto MakeCell() -> SharedPointer {
    return SharedPointer(std::make_shared<Var<T>>());
  }

  // The same of a variable whose value lies at its cell's tail, held as
  // `HeldAs` holds one and told `told`, which holds what the value's type
  // builds before anything writes it from then.
  template <class HeldAs, class Told>
  [[nodiscard]] static auto MakeTailCell(const Told& told) -> SharedPointer {
    return SharedPointer(runtime::MakeTailCell<HeldAs>(told));
  }

  // Where the held cell lies.
  [[nodiscard]] auto Pointee() const -> void*;

 private:
  explicit SharedPointer(std::shared_ptr<void> held);

  std::shared_ptr<void> held_;
};

}  // namespace lyra::runtime
