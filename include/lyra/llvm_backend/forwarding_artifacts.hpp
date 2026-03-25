#pragma once

#include <vector>

#include "lyra/llvm_backend/forwarding_map.hpp"
#include "lyra/llvm_backend/kernel_types.hpp"

namespace lyra::lowering::mir_to_llvm {

// Pipeline-owned forwarding analysis result. Built once in lower.cpp and
// threaded unchanged into layout construction, metadata lowering, and
// validation. No second recomputation or downstream rediscovery is allowed.
struct ForwardingArtifacts {
  ForwardingMap map;
  std::vector<ConnectionKernelEntry> canonicalized_connections;
};

}  // namespace lyra::lowering::mir_to_llvm
