#include "lyra/runtime/source_follower.hpp"

#include <utility>

#include "lyra/runtime/owned_call.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/wait.hpp"

namespace lyra::runtime {

SourceFollower::SourceFollower(const Trigger& source, OwnedCall changed)
    : changed_(std::move(changed)) {
  EnrolOnPlace(membership_, this, source);
}

SourceFollower::~SourceFollower() = default;

void SourceFollower::Reached() {
  changed_();
}

}  // namespace lyra::runtime
