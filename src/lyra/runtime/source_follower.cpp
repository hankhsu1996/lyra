#include "lyra/runtime/source_follower.hpp"

#include <utility>

#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/owned_call.hpp"
#include "lyra/runtime/trigger.hpp"

namespace lyra::runtime {

SourceFollower::SourceFollower(const Trigger& source, OwnedCall changed)
    : changed_(std::move(changed)) {
  membership_.observer = this;
  membership_.through = source.through;
  membership_.reads = source.reads;
  source.observable->MembersAt(source.part).PushBack(membership_);
}

SourceFollower::~SourceFollower() = default;

void SourceFollower::Reached() {
  changed_();
}

}  // namespace lyra::runtime
