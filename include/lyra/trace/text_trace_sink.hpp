#pragma once

#include <cstdint>
#include <cstdio>
#include <string>

#include "lyra/runtime/trace_signal_meta.hpp"
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
//   - TraceSignalMetaRegistry for signal names/widths (non-owning)
//   - Output destination: stdout (borrowed) or file (owned)
//
// Ignores MemoryDirty events.
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

  void OnEvent(const TraceEvent& event) override;

 private:
  void HandleTimeAdvance(const struct TimeAdvance& ta);
  void HandleValueChange(const struct ValueChange& vc);

  const runtime::TraceSignalMetaRegistry* meta_;
  FILE* output_;
  bool owns_file_;
  uint64_t current_time_ = 0;
  uint32_t current_delta_ = 0;
};

}  // namespace lyra::trace
