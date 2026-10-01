#include "lyra/runtime/shared_pointer.hpp"

#include <memory>
#include <utility>

namespace lyra::runtime {

SharedPointer::SharedPointer(std::shared_ptr<void> held)
    : held_(std::move(held)) {
}

auto SharedPointer::Pointee() const -> void* {
  return held_.get();
}

}  // namespace lyra::runtime
