#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include "llvm_stats.hpp"
#include "lyra/common/source_manager.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/design.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"

namespace lyra::driver {

enum class ProcessStatsKind : uint8_t {
  kConnection,
  kInitial,
  kAlways,
  kAlwaysComb,
  kAlwaysFf,
  kAlwaysLatch,
  kFinal,
  kMirOnce,
  kMirLooping,
  kMirFinal,
};

enum class ProcessStatsStorageClass : uint8_t {
  kWrapper,
  kSharedTemplate,
  kNonSharedBody,
};

struct ProcessStatsEntry {
  uint32_t layout_index = 0;
  ProcessStatsKind kind = ProcessStatsKind::kMirOnce;
  ProcessStatsStorageClass storage_class =
      ProcessStatsStorageClass::kNonSharedBody;
  std::string source_location;
  uint64_t llvm_insts = 0;
  uint64_t llvm_bbs = 0;
  uint32_t mir_stmts = 0;
};

struct ProcessStatsData {
  std::vector<ProcessStatsEntry> entries;

  size_t num_init = 0;
  size_t num_connection = 0;
  size_t num_kernelized = 0;
  size_t num_module = 0;

  uint32_t count_initial = 0;
  uint32_t count_always = 0;
  uint32_t count_always_comb = 0;
  uint32_t count_always_ff = 0;
  uint32_t count_always_latch = 0;
  uint32_t count_final = 0;
  uint32_t count_connection = 0;

  uint64_t total_proc_insts = 0;
  uint64_t total_llvm_insts = 0;

  uint32_t template_groups = 0;
  uint64_t shared_ir_insts = 0;
  uint32_t wrapper_count = 0;
  uint32_t nonshared_count = 0;

  uint64_t median_insts = 0;
  uint64_t p90_insts = 0;
  uint64_t p99_insts = 0;
  uint64_t max_insts = 0;
};

auto CollectProcessStats(
    const mir::Design& design, const mir::Arena& design_arena,
    const lowering::OriginMap& design_origins, const hir::Design& hir_design,
    const hir::Arena& global_hir_arena, const SourceManager& source_manager,
    const LlvmStats& llvm_stats) -> ProcessStatsData;

}  // namespace lyra::driver
