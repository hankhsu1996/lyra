#include "render_analysis.hpp"

#include <cstdint>
#include <iterator>
#include <string_view>

#include <fmt/core.h>
#include <fmt/format.h>

#include "lyra/llvm_backend/lowering_reports.hpp"
#include "text_sink.hpp"

namespace lyra::driver {

void RenderForwardingAnalysisReport(
    TextSink& sink,
    const lowering::mir_to_llvm::ForwardingAnalysisReport& report) {
  if (report.Empty()) return;

  fmt::memory_buffer buf;
  fmt::format_to(
      std::back_inserter(buf),
      "[lyra][forwarding_analysis] total={} provable_pass={}"
      " (trace_ref {} on all; not transform-safe yet)\n",
      report.Candidates().size(), report.ProvablePassCount(),
      report.AllTraceRefsUnresolved() ? "unresolved" : "mixed");
  sink.Write(std::string_view(buf.data(), buf.size()));

  for (const auto& c : report.Candidates()) {
    int provable_pass = c.IsProvablyPassingCurrentChecks() ? 1 : 0;
    fmt::memory_buffer row;
    fmt::format_to(
        std::back_inserter(row),
        "[lyra][forwarding_analysis] slot={}"
        " up={} down={}"
        " provable_pass={}"
        " writer={} down_ok={} port={}"
        " no_proc={} no_comb={}"
        " up_copy={} down_read={}"
        " trace={}\n",
        c.intermediate_slot_id.value, c.upstream_connection_index.value,
        c.downstream_connection_index.value, provable_pass,
        static_cast<int>(c.single_writer),
        static_cast<int>(c.single_downstream),
        static_cast<int>(c.both_port_binding),
        static_cast<int>(c.no_process_trigger),
        static_cast<int>(c.no_comb_trigger),
        static_cast<int>(c.upstream_full_copy_shape),
        static_cast<int>(c.downstream_matching_read_shape),
        c.trace_ref_unresolved ? "unresolved" : "resolved");
    sink.Write(std::string_view(row.data(), row.size()));
  }
}

}  // namespace lyra::driver
