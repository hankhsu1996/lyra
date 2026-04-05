#include "lyra/runtime/assertions.hpp"

#include <format>
#include <optional>

#include "lyra/common/deferred_assertion_abi.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/reporting.hpp"

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
    void* engine, uint32_t process_id, uint32_t site_id, uint8_t disposition) {
  static_cast<lyra::runtime::Engine*>(engine)->EnqueueDeferredAssertion(
      process_id, site_id, disposition);
}

namespace lyra::runtime {

void Engine::InitDeferredAssertionSites(
    const LyraDeferredAssertionSiteMeta* sites, uint32_t count) {
  deferred_assertion_site_meta_ = sites;
  num_deferred_assertion_sites_ = count;
}

void Engine::EnqueueDeferredAssertion(
    uint32_t process_id, uint32_t site_id, uint8_t disposition) {
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
  state.pending.push_back(
      DeferredAssertionRecord{
          .enqueue_generation = state.flush_generation,
          .site_id = site_id,
          .disposition = disposition,
      });

  if (deferred_pending_flags_[process_id] == 0) {
    deferred_pending_flags_[process_id] = 1;
    pending_deferred_processes_.push_back(ProcessId::FromIndex(process_id));
  }
}

void Engine::FlushDeferredAssertionsForProcess(ProcessId pid) {
  if (pid.Index() >= deferred_assertion_states_.size()) return;
  deferred_assertion_states_[pid.Index()].flush_generation++;
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
          throw common::InternalError(
              "Engine::MatureAndExecuteObservedDeferredAssertions",
              "kFailAction reached runtime before thunk support");
        case Disp::kPassAction:
          throw common::InternalError(
              "Engine::MatureAndExecuteObservedDeferredAssertions",
              "kPassAction reached runtime before thunk support");
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
