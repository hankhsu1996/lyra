#include "lyra/value/queue_bound.hpp"

#include <utility>

namespace lyra::value {

namespace {

auto CurrentListener() -> BoundOverflowListener& {
  static BoundOverflowListener listener = nullptr;
  return listener;
}

}  // namespace

void ReportBoundOverflow() {
  if (const BoundOverflowListener listener = CurrentListener();
      listener != nullptr) {
    listener();
  }
}

auto ListenForBoundOverflow(BoundOverflowListener listener)
    -> BoundOverflowListener {
  return std::exchange(CurrentListener(), listener);
}

}  // namespace lyra::value
