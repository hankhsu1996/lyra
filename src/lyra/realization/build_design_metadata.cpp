#include "lyra/realization/build_design_metadata.hpp"

#include <cstdint>
#include <string>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/back_edge_site_meta.hpp"
#include "lyra/runtime/process_meta_abi.hpp"
#include "lyra/runtime/process_trigger_abi.hpp"
#include "lyra/runtime/slot_meta_abi.hpp"
#include "lyra/runtime/trace_signal_meta_abi.hpp"

namespace lyra::realization {

namespace {

auto BuildSlotMetaWords(const std::vector<metadata::SlotMetaInput>& slots)
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

auto BuildProcessMeta(
    const std::vector<metadata::ScheduledProcessInput>& processes)
    -> metadata::MetaWordTable {
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

auto BuildBackEdgeSiteMeta(
    const std::vector<metadata::BackEdgeSiteInput>& sites)
    -> metadata::MetaWordTable {
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
  words.reserve(sites.size() * runtime::back_edge_site_abi::kStride);

  for (const auto& site : sites) {
    uint32_t file_off = add_string(site.file);
    words.push_back(file_off);
    words.push_back(site.line);
    words.push_back(site.col);
  }

  return {.words = std::move(words), .pool = std::move(pool)};
}

auto BuildCombKernelWords(const std::vector<metadata::CombKernelInput>& kernels)
    -> std::vector<uint32_t> {
  if (kernels.empty()) {
    return {};
  }

  std::vector<uint32_t> words;
  words.push_back(static_cast<uint32_t>(kernels.size()));

  for (const auto& ck : kernels) {
    words.push_back(ck.scheduled_process_index);
    // Flags bitmask: bit 0 = kSelfEdge (write set overlaps trigger set).
    uint32_t flags = ck.has_self_edge ? 1U : 0U;
    words.push_back(flags);
    words.push_back(static_cast<uint32_t>(ck.triggers.size()));
    for (const auto& trigger : ck.triggers) {
      words.push_back(trigger.slot_id);
      words.push_back(trigger.byte_offset);
      words.push_back(trigger.byte_size);
    }
  }

  return words;
}

auto BuildTraceSignalMeta(
    const std::vector<metadata::TraceSignalMetaInput>& signals)
    -> metadata::MetaWordTable {
  if (signals.empty()) return {};

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
  words.reserve(signals.size() * runtime::trace_signal_meta_abi::kStride);

  for (const auto& sig : signals) {
    uint32_t name_off = add_string(sig.hierarchical_name);
    words.push_back(name_off);
    words.push_back(sig.bit_width);
    words.push_back(sig.trace_kind);
  }

  return {.words = std::move(words), .pool = std::move(pool)};
}

}  // namespace

auto BuildDesignMetadata(const metadata::DesignMetadataInputs& input)
    -> metadata::DesignMetadata {
  // Verify scheduled_process_index values are dense and monotonic.
  for (uint32_t i = 0; i < input.scheduled_processes.size(); ++i) {
    if (input.scheduled_processes[i].scheduled_process_index != i) {
      throw common::InternalError(
          "BuildDesignMetadata", "scheduled_process_index not dense/monotonic");
    }
  }

  // Verify back_edge_site_index values are dense and monotonic.
  for (uint32_t i = 0; i < input.back_edge_sites.size(); ++i) {
    if (input.back_edge_sites[i].back_edge_site_index != i) {
      throw common::InternalError(
          "BuildDesignMetadata", "back_edge_site_index not dense/monotonic");
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

  auto back_edge_site_meta = BuildBackEdgeSiteMeta(input.back_edge_sites);

  if (!back_edge_site_meta.words.empty() &&
      back_edge_site_meta.words.size() % runtime::back_edge_site_abi::kStride !=
          0) {
    throw common::InternalError(
        "BuildDesignMetadata",
        "back_edge_site_meta words size not divisible by kStride");
  }

  auto trace_signal_meta = BuildTraceSignalMeta(input.trace_signal_meta);

  if (!trace_signal_meta.words.empty() &&
      trace_signal_meta.words.size() %
              runtime::trace_signal_meta_abi::kStride !=
          0) {
    throw common::InternalError(
        "BuildDesignMetadata",
        "trace_signal_meta words size not divisible by kStride");
  }

  // Serialize process trigger metadata.
  // Row layout defined by process_trigger_abi constants.
  // Format: [num_entries, per-row fields in ABI-defined order...]
  std::vector<uint32_t> process_trigger_words;
  if (!input.process_triggers.empty()) {
    process_trigger_words.reserve(
        1 + (input.process_triggers.size() *
             runtime::process_trigger_abi::kStride));
    process_trigger_words.push_back(
        static_cast<uint32_t>(input.process_triggers.size()));
    for (const auto& pt : input.process_triggers) {
      auto base = process_trigger_words.size();
      process_trigger_words.resize(
          base + runtime::process_trigger_abi::kStride);
      process_trigger_words
          [base + runtime::process_trigger_abi::kFieldProcessIndex] =
              pt.scheduled_process_index;
      process_trigger_words[base + runtime::process_trigger_abi::kFieldSlotId] =
          pt.slot_id;
      process_trigger_words
          [base + runtime::process_trigger_abi::kFieldEdgeKind] =
              static_cast<uint32_t>(pt.edge);
      uint32_t flags = 0;
      if (pt.is_groupable)
        flags |= runtime::process_trigger_abi::kFlagGroupable;
      process_trigger_words[base + runtime::process_trigger_abi::kFieldFlags] =
          flags;
    }
  }

  return metadata::DesignMetadata{
      .slot_meta_words = std::move(slot_meta_words),
      .process_meta = std::move(process_meta),
      .back_edge_site_meta = std::move(back_edge_site_meta),
      .connection_descriptors = input.connection_descriptors,
      .comb_kernel_words = BuildCombKernelWords(input.comb_kernels),
      .process_trigger_words = std::move(process_trigger_words),
      .instance_paths = input.instance_paths,
      .trace_signal_meta = std::move(trace_signal_meta),
  };
}

}  // namespace lyra::realization
