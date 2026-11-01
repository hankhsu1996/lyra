#pragma once

#include <cstddef>

namespace lyra::support {

// How many bytes a value or an object takes, and the alignment it is kept at,
// as Rust's `core::alloc::Layout` states the two. Code laying a value out,
// code stating how large one is, the holders keeping one, and the runtime
// answering how large an object of its own is all state it this way.
struct Layout {
  std::size_t size;
  std::size_t align;

  friend auto operator==(const Layout&, const Layout&) -> bool = default;
};

// The storage an object needs, and whether ending one has anything to do, as
// rustc's `needs_drop` asks of a type. The runtime answers it for each object
// it builds, read off that object's own type; a holder keeping a value at its
// tail answers it with that value's layout, so its size follows the type it
// holds.
struct ObjectLayout {
  Layout layout;
  bool needs_end;

  friend auto operator==(const ObjectLayout&, const ObjectLayout&)
      -> bool = default;
};

}  // namespace lyra::support
