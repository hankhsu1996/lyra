#pragma once

#include <cstdint>

#include "lyra/base/source_location.hpp"

namespace lyra::value {

// What a queue says while the design writes it, where the standard asks the
// tool to say something and the write goes on: a write that left a bounded
// queue longer than its declared bound, so the elements past it were dropped
// (LRM 7.10.5), and a write through an index naming neither an element nor
// the position one past the last, which was ignored (LRM 7.10.1). Neither is
// an error, but each loses what the design wrote. Every way of writing a queue
// says either through the functions below, so each is said alike.
enum class QueueReport : std::uint8_t {
  kGrewPastItsBound,
  kWrittenAtAnInvalidIndex,
};

void ReportFromAQueue(QueueReport report);

// Each of them, said where the write was written (LRM 20.10).
void ReportQueueGrewPastItsBound(base::SourceLocation at);
void ReportQueueWrittenAtAnInvalidIndex(base::SourceLocation at);

// Who hears those reports. A value knows nothing of when it is written, so the
// run listens for as long as it lasts and says each as one of its own reports;
// with nobody listening, nothing is said.
using QueueReportListener = void (*)(QueueReport report);

// Installs `listener` and answers the one it replaces.
auto ListenForQueueReports(QueueReportListener listener) -> QueueReportListener;

}  // namespace lyra::value
