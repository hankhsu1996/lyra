#include "lyra/value/element_type.hpp"

#include "lyra/value/element_policy.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

auto ElementType::Default() const -> const void* {
  if (scratch_->element_default == nullptr) {
    StorageFor storage(*type_);
    build_default_(storage.Get());
    scratch_->element_default = storage.Release();
  }
  return scratch_->element_default;
}

auto ElementType::DiscardedWrite() const -> void* {
  if (scratch_->discarded_write == nullptr) {
    StorageFor storage(*type_);
    type_->Copy(Default(), storage.Get());
    scratch_->discarded_write = storage.Release();
  } else {
    type_->Assign(scratch_->discarded_write, Default());
  }
  return scratch_->discarded_write;
}

}  // namespace lyra::value
