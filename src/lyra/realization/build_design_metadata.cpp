#include "lyra/realization/build_design_metadata.hpp"

#include <cstdint>
#include <string>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/loop_site_meta.hpp"
#include "lyra/runtime/process_meta_abi.hpp"
#include "lyra/runtime/slot_meta_abi.hpp"

namespace lyra::realization {

namespace {

auto BuildSlotMetaWords(const std::vector<SlotMetaInput>& slots)
    -> std::vector<uint32_t> {
  if (slots.empty()) {
    return {};
  }

  std::vector<uint32_t> words;
  words.reserve(slots.size() * runtime::slot_meta_abi::kStride);

  for (const auto& slot : slots) {
    words.push_back(slot.byte_offset);
    words.push_back(slot.total_bytes);
    words.push_back(slot.storage_kind);
    words.push_back(slot.value_offset);
    words.push_back(slot.value_bytes);
    words.push_back(slot.unk_offset);
    words.push_back(slot.unk_bytes);
  }

  return words;
}

auto BuildProcessMeta(const std::vector<ScheduledProcessInput>& processes)
    -> MetaWordTable {
  if (processes.empty()) {
    return {};
  }

  std::vector<char> pool;
  pool.push_back('\0');

  auto add_string = [&](const std::string& s) -> uint32_t {
    if (s.empty()) return 0;
    auto off = static_cast<uint32_t>(pool.size());
    pool.insert(pool.end(), s.begin(), s.end());
    pool.push_back('\0');
    return off;
  };

  std::vector<uint32_t> words;
  words.reserve(processes.size() * runtime::process_meta_abi::kStride);

  for (const auto& proc : processes) {
    uint32_t inst_off = add_string(proc.instance_path);
    uint32_t file_off = add_string(proc.file);

    words.push_back(inst_off);
    words.push_back(proc.kind_packed);
    words.push_back(file_off);
    words.push_back(proc.line);
    words.push_back(proc.col);
  }

  return {.words = std::move(words), .pool = std::move(pool)};
}

auto BuildLoopSiteMeta(const std::vector<LoopSiteInput>& sites)
    -> MetaWordTable {
  if (sites.empty()) {
    return {};
  }

  std::vector<char> pool;
  pool.push_back('\0');

  auto add_string = [&](const std::string& s) -> uint32_t {
    if (s.empty()) return 0;
    auto off = static_cast<uint32_t>(pool.size());
    pool.insert(pool.end(), s.begin(), s.end());
    pool.push_back('\0');
    return off;
  };

  std::vector<uint32_t> words;
  words.reserve(sites.size() * runtime::loop_site_meta_abi::kStride);

  for (const auto& site : sites) {
    uint32_t file_off = add_string(site.file);
    words.push_back(file_off);
    words.push_back(site.line);
    words.push_back(site.col);
  }

  return {.words = std::move(words), .pool = std::move(pool)};
}

auto BuildCombKernelWords(const std::vector<CombKernelInput>& kernels)
    -> std::vector<uint32_t> {
  if (kernels.empty()) {
    return {};
  }

  std::vector<uint32_t> words;
  words.push_back(static_cast<uint32_t>(kernels.size()));

  for (const auto& ck : kernels) {
    words.push_back(ck.scheduled_process_index);
    words.push_back(static_cast<uint32_t>(ck.triggers.size()));
    for (const auto& trigger : ck.triggers) {
      words.push_back(trigger.slot_id);
      words.push_back(trigger.byte_offset);
      words.push_back(trigger.byte_size);
    }
  }

  return words;
}

}  // namespace

auto BuildDesignMetadata(const DesignMetadataInputs& input) -> DesignMetadata {
  // Verify scheduled_process_index values are dense and monotonic.
  for (uint32_t i = 0; i < input.scheduled_processes.size(); ++i) {
    if (input.scheduled_processes[i].scheduled_process_index != i) {
      throw common::InternalError(
          "BuildDesignMetadata", "scheduled_process_index not dense/monotonic");
    }
  }

  // Verify loop_site_index values are dense and monotonic.
  for (uint32_t i = 0; i < input.loop_sites.size(); ++i) {
    if (input.loop_sites[i].loop_site_index != i) {
      throw common::InternalError(
          "BuildDesignMetadata", "loop_site_index not dense/monotonic");
    }
  }

  auto slot_meta_words = BuildSlotMetaWords(input.slot_meta);

  if (!slot_meta_words.empty() &&
      slot_meta_words.size() % runtime::slot_meta_abi::kStride != 0) {
    throw common::InternalError(
        "BuildDesignMetadata", "slot_meta_words size not divisible by kStride");
  }

  auto process_meta = BuildProcessMeta(input.scheduled_processes);

  if (!process_meta.words.empty() &&
      process_meta.words.size() % runtime::process_meta_abi::kStride != 0) {
    throw common::InternalError(
        "BuildDesignMetadata",
        "process_meta words size not divisible by kStride");
  }

  auto loop_site_meta = BuildLoopSiteMeta(input.loop_sites);

  if (!loop_site_meta.words.empty() &&
      loop_site_meta.words.size() % runtime::loop_site_meta_abi::kStride != 0) {
    throw common::InternalError(
        "BuildDesignMetadata",
        "loop_site_meta words size not divisible by kStride");
  }

  return DesignMetadata{
      .slot_meta_words = std::move(slot_meta_words),
      .process_meta = std::move(process_meta),
      .loop_site_meta = std::move(loop_site_meta),
      .connection_descriptors = input.connection_descriptors,
      .comb_kernel_words = BuildCombKernelWords(input.comb_kernels),
      .instance_paths = input.instance_paths,
  };
}

}  // namespace lyra::realization
