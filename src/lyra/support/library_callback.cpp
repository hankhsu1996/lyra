#include "lyra/support/library_callback.hpp"

#include <string_view>

#include "lyra/base/internal_error.hpp"

namespace lyra::support {

auto LibraryCallbackName(LibraryCallback callback) -> std::string_view {
  switch (callback) {
    case LibraryCallback::kScopeResolve:
      return "sv_resolve";
    case LibraryCallback::kScopeInitialize:
      return "sv_initialize";
    case LibraryCallback::kScopeCreateProcesses:
      return "sv_create_processes";
    case LibraryCallback::kScopeDescribe:
      return "sv_describe";
    case LibraryCallback::kClassRandVariables:
      return "sv_rand_variables";
    case LibraryCallback::kClassConstrain:
      return "sv_constrain";
    case LibraryCallback::kClassRandObjects:
      return "sv_rand_objects";
    case LibraryCallback::kClassPreRandomize:
      return "sv_pre_randomize";
    case LibraryCallback::kClassPostRandomize:
      return "sv_post_randomize";
  }
  throw InternalError("LibraryCallbackName: unknown library callback");
}

}  // namespace lyra::support
