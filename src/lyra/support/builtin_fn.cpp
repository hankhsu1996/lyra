#include "lyra/support/builtin_fn.hpp"

namespace lyra::support {

auto IsMutatingBuiltinFn(BuiltinFn id) -> bool {
  switch (id) {
    case BuiltinFn::kPutc:
    case BuiltinFn::kItoa:
    case BuiltinFn::kHextoa:
    case BuiltinFn::kOcttoa:
    case BuiltinFn::kBintoa:
    case BuiltinFn::kRealtoa:
    case BuiltinFn::kDelete:
    case BuiltinFn::kReverse:
    case BuiltinFn::kSort:
    case BuiltinFn::kRsort:
    case BuiltinFn::kInsert:
    case BuiltinFn::kPopFront:
    case BuiltinFn::kPopBack:
    case BuiltinFn::kPushFront:
    case BuiltinFn::kPushBack:
      return true;
    default:
      return false;
  }
}

auto IsContainerAccessFn(BuiltinFn id) -> bool {
  switch (id) {
    case BuiltinFn::kElement:
    case BuiltinFn::kElementRef:
    case BuiltinFn::kSlice:
    case BuiltinFn::kSliceRef:
      return true;
    default:
      return false;
  }
}

}  // namespace lyra::support
