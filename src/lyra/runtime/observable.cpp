#include "lyra/runtime/observable.hpp"

#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/wait.hpp"

namespace lyra::runtime {

Observable::Observable() = default;
Observable::~Observable() = default;

auto Observable::Members() noexcept -> IntrusiveList<WaitMembership>& {
  return members_;
}

}  // namespace lyra::runtime
