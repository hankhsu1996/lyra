#include "lyra/runtime/activation_trace_format.hpp"

#include <format>

namespace lyra::runtime {

namespace {

auto WakeCauseLabel(WakeCause cause) -> const char* {
  switch (cause) {
    case WakeCause::kEdge:
      return "edge";
    case WakeCause::kChange:
      return "change";
    case WakeCause::kContainer:
      return "container";
    case WakeCause::kDelay:
      return "delay";
    case WakeCause::kDelayZero:
      return "delay_zero";
    case WakeCause::kInitial:
      return "initial";
    case WakeCause::kRepeat:
      return "repeat";
  }
  return "unknown";
}

}  // namespace

auto FormatActivationEvent(
    const ActivationEvent& event, const ProcessMetaRegistry& proc_meta)
    -> std::string {
  std::string proc_str;
  if (proc_meta.IsPopulated()) {
    proc_str = proc_meta.Format(event.process_id);
  } else {
    proc_str = std::format("<process {}>", event.process_id);
  }

  if (event.kind == ActivationEventKind::kWake) {
    std::string result = std::format(
        "[act] t={} d={} wake {} cause={} resume={}", event.time, event.delta,
        proc_str, WakeCauseLabel(event.cause), event.resume_block);
    if (HasTriggerSlot(event.cause)) {
      result += std::format(" slot={}", event.trigger_slot);
    }
    return result;
  }

  return std::format(
      "[act] t={} d={} run  {} dirtied={} resume={}", event.time, event.delta,
      proc_str, event.slots_dirtied, event.resume_block);
}

}  // namespace lyra::runtime
