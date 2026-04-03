#pragma once

#include <cstdint>
#include <cstdio>
#include <string>

#include "lyra/runtime/trace_signal_meta.hpp"
#include "lyra/trace/instance_trace_resolver.hpp"
#include "lyra/trace/trace_sink.hpp"

namespace lyra::trace {

// Text signal trace sink: emits compact one-line value-change output.
//
// Format:
//   t=<time> d=<delta> <hierarchical_name> = <rendered_value>
//
// Value rendering:
//   1-bit packed: 1'b0 / 1'b1
//   wider packed: N'h<lowercase hex, MSB-first>
//   string: "<content>"
//
// Constructor dependencies:
//   - TraceSignalMetaRegistry for global signal names/widths (non-owning)
//   - Output destination: stdout (borrowed) or file (owned)
//
// R5: Handles both GlobalValueChange (via TraceSignalMetaRegistry, keyed
// by GlobalSignalId) and LocalValueChange (via InstanceTraceResolver +
// ComposeHierarchicalTraceName + BodyTraceMeta).
// Ignores MemoryDirty events.
//
// Contract: once trace dispatch is enabled, global events require populated
// global trace metadata, and local events require a non-null instance
// resolver. Malformed event identity or missing metadata is an InternalError,
// not a silently dropped event.
class TextTraceSink : public TraceSink {
 public:
  // Borrow stdout for output.
  explicit TextTraceSink(const runtime::TraceSignalMetaRegistry* meta);

  // Own a file at the given path for output.
  TextTraceSink(
      const runtime::TraceSignalMetaRegistry* meta, const std::string& path);

  ~TextTraceSink() override;

  TextTraceSink(const TextTraceSink&) = delete;
  auto operator=(const TextTraceSink&) -> TextTraceSink& = delete;
  TextTraceSink(TextTraceSink&&) = delete;
  auto operator=(TextTraceSink&&) -> TextTraceSink& = delete;

  // Set non-owning pointer to the instance resolver for local signal
  // name resolution. Must be called before trace events are dispatched.
  void SetInstanceResolver(const InstanceTraceResolver* resolver) {
    resolver_ = resolver;
  }

  void OnEvent(const TraceEvent& event) override;

 private:
  void HandleTimeAdvance(const struct TimeAdvance& ta);
  void HandleGlobalValueChange(const struct GlobalValueChange& vc);
  void HandleLocalValueChange(const struct LocalValueChange& vc);

  const runtime::TraceSignalMetaRegistry* meta_;
  const InstanceTraceResolver* resolver_ = nullptr;
  FILE* output_;
  bool owns_file_;
  uint64_t current_time_ = 0;
  uint32_t current_delta_ = 0;
};

}  // namespace lyra::trace
