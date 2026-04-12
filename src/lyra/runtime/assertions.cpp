#include "lyra/runtime/assertions.hpp"

#include <format>
#include <optional>

#include "lyra/common/deferred_assertion_abi.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/deferred_assertion_thunk.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/reporting.hpp"
#include "lyra/runtime/runtime_instance.hpp"

extern "C" void LyraRecordImmediateCoverHit(void* engine, uint32_t site_index) {
  static_cast<lyra::runtime::Engine*>(engine)->RecordImmediateCoverHit(
      site_index);
}

extern "C" void LyraInitDeferredAssertionSites(
    void* engine, const LyraDeferredAssertionSiteMeta* sites,
    uint32_t num_sites) {
  static_cast<lyra::runtime::Engine*>(engine)->InitDeferredAssertionSites(
      sites, num_sites);
}

extern "C" void LyraEnqueueObservedDeferredAssertion(
    void* engine, uint32_t process_id, uint32_t instance_id, uint32_t site_id,
    uint8_t disposition, const void* payload_ptr, uint32_t payload_size,
    const lyra::DeferredAssertionRefBindingAbi* ref_ptr, uint32_t ref_count) {
  static_cast<lyra::runtime::Engine*>(engine)->EnqueueDeferredAssertion(
      process_id, instance_id, site_id, disposition, payload_ptr, payload_size,
      ref_ptr, ref_count);
}

namespace lyra::runtime {

void Engine::InitDeferredAssertionSites(
    const LyraDeferredAssertionSiteMeta* sites, uint32_t count) {
  deferred_assertion_site_meta_ = sites;
  num_deferred_assertion_sites_ = count;
}

void Engine::EnqueueDeferredAssertion(
    uint32_t process_id, uint32_t instance_id, uint32_t site_id,
    uint8_t disposition, const void* payload_ptr, uint32_t payload_size,
    const DeferredAssertionRefBindingAbi* ref_ptr, uint32_t ref_count) {
  if (process_id >= deferred_assertion_states_.size()) {
    throw common::InternalError(
        "Engine::EnqueueDeferredAssertion",
        std::format(
            "process_id {} out of range (num_processes={})", process_id,
            deferred_assertion_states_.size()));
  }
  if (site_id >= num_deferred_assertion_sites_) {
    throw common::InternalError(
        "Engine::EnqueueDeferredAssertion",
        std::format(
            "site_id {} out of range (num_sites={})", site_id,
            num_deferred_assertion_sites_));
  }
  if (disposition >
      static_cast<uint8_t>(DeferredAssertionDispositionAbi::kCoverHit)) {
    throw common::InternalError(
        "Engine::EnqueueDeferredAssertion",
        std::format("unknown disposition {}", disposition));
  }

  auto& state = deferred_assertion_states_[process_id];
  DeferredAssertionRecord rec{
      .enqueue_generation = state.flush_generation,
      .site_id = site_id,
      .disposition = disposition,
      .instance_id = instance_id,
      .payload_size = payload_size,
      .payload = {},
      .ref_bindings = {},
  };
  if (payload_ptr != nullptr && payload_size > 0) {
    rec.payload.AssignCopy(payload_ptr, payload_size);
  }
  if (ref_ptr != nullptr && ref_count > 0) {
    rec.ref_bindings.assign(ref_ptr, ref_ptr + ref_count);
  }
  state.pending.push_back(std::move(rec));

  if (deferred_pending_flags_[process_id] == 0) {
    deferred_pending_flags_[process_id] = 1;
    pending_deferred_processes_.push_back(ProcessId::FromIndex(process_id));
  }
}

void Engine::MatureAndExecuteObservedDeferredAssertions() {
  using Disp = DeferredAssertionDispositionAbi;

  for (ProcessId pid : pending_deferred_processes_) {
    auto& state = deferred_assertion_states_[pid.Index()];

    for (const auto& rec : state.pending) {
      if (rec.enqueue_generation != state.flush_generation) continue;

      if (rec.site_id >= num_deferred_assertion_sites_) {
        throw common::InternalError(
            "Engine::MatureAndExecuteObservedDeferredAssertions",
            std::format("site_id {} out of range", rec.site_id));
      }
      const auto& site = deferred_assertion_site_meta_[rec.site_id];
      auto disp = static_cast<Disp>(rec.disposition);

      switch (disp) {
        case Disp::kDefaultFailReport: {
          const char* kind_str = (site.kind == 1) ? "assumption" : "assertion";
          std::string msg =
              std::format("Immediate {} failed (deferred #0)", kind_str);
          std::optional<ResolvedOrigin> origin;
          if (site.origin_file != nullptr && site.origin_line > 0) {
            origin = ResolvedOrigin{
                .file = site.origin_file,
                .line = site.origin_line,
                .col = site.origin_col};
          }
          EmitReport(
              this, ReportRequest{
                        .kind = ReportKind::kAssertionFailure,
                        .severity = Severity::kError,
                        .origin = origin,
                        .message = std::move(msg),
                        .action = ReportAction::kContinue,
                    });
          break;
        }
        case Disp::kCoverHit: {
          if (site.cover_site_id == UINT32_MAX) {
            throw common::InternalError(
                "Engine::MatureAndExecuteObservedDeferredAssertions",
                "kCoverHit disposition but no cover_site_id on site metadata");
          }
          RecordImmediateCoverHit(site.cover_site_id);
          break;
        }
        case Disp::kFailAction:
        case Disp::kPassAction: {
          DeferredAssertionThunkFn thunk =
              (disp == Disp::kPassAction) ? site.pass_thunk : site.fail_thunk;
          if (thunk == nullptr) {
            throw common::InternalError(
                "Engine::MatureAndExecuteObservedDeferredAssertions",
                std::format(
                    "thunk is null for disposition {}",
                    static_cast<uint8_t>(disp)));
          }

          // Validate payload size against site metadata.
          uint32_t expected = (disp == Disp::kPassAction)
                                  ? site.pass_payload_size
                                  : site.fail_payload_size;
          if (rec.payload_size != expected) {
            throw common::InternalError(
                "Engine::MatureAndExecuteObservedDeferredAssertions",
                std::format(
                    "payload size mismatch: record={} site={}",
                    rec.payload_size, expected));
          }

          // Build execution context with instance_id dispatch invariant.
          DeferredAssertionExecContext ctx{};
          if (rec.instance_id != kNoInstanceId) {
            auto* inst = FindInstanceMut(InstanceId{rec.instance_id});
            if (inst == nullptr) {
              throw common::InternalError(
                  "Engine::MatureAndExecuteObservedDeferredAssertions",
                  std::format("instance_id {} not found", rec.instance_id));
            }
            ctx.instance = inst;
          }

          thunk(
              design_state_base_, this, &ctx, rec.payload.Data(),
              rec.ref_bindings.data(),
              static_cast<uint32_t>(rec.ref_bindings.size()));
          break;
        }
        default:
          throw common::InternalError(
              "Engine::MatureAndExecuteObservedDeferredAssertions",
              std::format("unknown disposition {}", rec.disposition));
      }
    }

    state.pending.clear();
    deferred_pending_flags_[pid.Index()] = 0;
  }
  pending_deferred_processes_.clear();
}

}  // namespace lyra::runtime
