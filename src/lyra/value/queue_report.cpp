#include "lyra/value/queue_report.hpp"

#include <utility>

namespace lyra::value {

namespace {

auto CurrentListener() -> QueueReportListener& {
  static QueueReportListener listener = nullptr;
  return listener;
}

}  // namespace

void ReportFromAQueue(QueueReport report) {
  if (const QueueReportListener listener = CurrentListener();
      listener != nullptr) {
    listener(report);
  }
}

void ReportQueueGrewPastItsBound(base::SourceLocation at) {
  const base::SourceLocationScope bound(at);
  ReportFromAQueue(QueueReport::kGrewPastItsBound);
}

void ReportQueueWrittenAtAnInvalidIndex(base::SourceLocation at) {
  const base::SourceLocationScope bound(at);
  ReportFromAQueue(QueueReport::kWrittenAtAnInvalidIndex);
}

auto ListenForQueueReports(QueueReportListener listener)
    -> QueueReportListener {
  return std::exchange(CurrentListener(), listener);
}

}  // namespace lyra::value
