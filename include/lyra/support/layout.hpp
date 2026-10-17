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

}  // namespace lyra::support
