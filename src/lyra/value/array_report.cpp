#include "lyra/value/array_report.hpp"

#include <utility>

namespace lyra::value {

namespace {

auto CurrentListener() -> ArrayReportListener& {
  static ArrayReportListener listener = nullptr;
  return listener;
}

void ReportAt(base::SourceLocation at, ArrayReport report) {
  const base::SourceLocationScope bound(at);
  if (const ArrayReportListener listener = CurrentListener();
      listener != nullptr) {
    listener(report);
  }
}

}  // namespace

void ReportQueueGrewPastItsBound(base::SourceLocation at) {
  ReportAt(at, ArrayReport::kQueueGrewPastItsBound);
}

void ReportQueueWrittenAtAnInvalidIndex(base::SourceLocation at) {
  ReportAt(at, ArrayReport::kQueueWrittenAtAnInvalidIndex);
}

void ReportAssociativeArrayReadAtAnInvalidIndex(base::SourceLocation at) {
  ReportAt(at, ArrayReport::kAssociativeArrayReadAtAnInvalidIndex);
}

void ReportAssociativeArrayReadAtAMissingEntry(base::SourceLocation at) {
  ReportAt(at, ArrayReport::kAssociativeArrayReadAtAMissingEntry);
}

void ReportAssociativeArrayWrittenAtAnInvalidIndex(base::SourceLocation at) {
  ReportAt(at, ArrayReport::kAssociativeArrayWrittenAtAnInvalidIndex);
}

auto ListenForArrayReports(ArrayReportListener listener)
    -> ArrayReportListener {
  return std::exchange(CurrentListener(), listener);
}

}  // namespace lyra::value
