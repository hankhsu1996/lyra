#pragma once

#include <cstdint>
#include <cstdio>
#include <memory>
#include <string>

#include "lyra/runtime/trace_signal_meta.hpp"
#include "lyra/trace/trace_sink.hpp"

namespace lyra::runtime {
class OutputDispatcher;
}  // namespace lyra::runtime

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
// Handles both GlobalValueChange (via TraceSignalMetaRegistry, keyed
// by GlobalSignalId) and LocalValueChange (via RuntimeInstance* from the
// event + ComposeHierarchicalTraceName + BodyTraceMeta).
// Ignores MemoryDirty events.
//
// Contract: once trace dispatch is enabled, global events require populated
// global trace metadata, and local events carry a non-null RuntimeInstance*.
// Malformed event identity or missing metadata is an InternalError,
// not a silently dropped event.
class TextTraceSink : public TraceSink {
 public:
  // Borrow stdout for output via explicit dispatcher.
  TextTraceSink(
      const runtime::TraceSignalMetaRegistry* meta,
      runtime::OutputDispatcher* output_dispatcher);

  // Own a file at the given path for output.
  TextTraceSink(
      const runtime::TraceSignalMetaRegistry* meta, const std::string& path);

  ~TextTraceSink() override;

  TextTraceSink(const TextTraceSink&) = delete;
  auto operator=(const TextTraceSink&) -> TextTraceSink& = delete;
  TextTraceSink(TextTraceSink&&) = delete;
  auto operator=(TextTraceSink&&) -> TextTraceSink& = delete;

  void OnEvent(const TraceEvent& event) override;

 private:
  void HandleTimeAdvance(const struct TimeAdvance& ta);
  void HandleGlobalValueChange(const struct GlobalValueChange& vc);
  void HandleLocalValueChange(const struct LocalValueChange& vc);

  // NOLINTBEGIN(cppcoreguidelines-owning-memory)
  // fclose is the only correct way to release a FILE* opened by fopen.
  // Ownership is managed by the enclosing unique_ptr.
  struct FileCloser {
    void operator()(FILE* f) const {
      std::fclose(f);
    }
  };
  // NOLINTEND(cppcoreguidelines-owning-memory)

  const runtime::TraceSignalMetaRegistry* meta_;
  runtime::OutputDispatcher* output_dispatcher_ = nullptr;
  std::unique_ptr<FILE, FileCloser> output_;
  uint64_t current_time_ = 0;
  uint32_t current_delta_ = 0;
};

}  // namespace lyra::trace
