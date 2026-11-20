#pragma once

#include <cstdint>

#include "lyra/base/source_location.hpp"

namespace lyra::value {

// What an array says while the design reads or writes it, where the standard
// asks the tool to say something and the run goes on: a write that left a
// bounded queue longer than its declared bound, so the elements past it were
// dropped (LRM 7.10.5); a write to a queue through an index naming neither an
// element nor the position one past the last, which was ignored (LRM 7.10.1);
// and a read of an associative array through an index holding x or z or
// naming no entry, which read the array's element default, and a write
// through one holding x or z, which was ignored (LRM 7.8.6). None is an
// error, but each loses or makes up what the design meant. Every way of
// reading or writing an array says each through the functions below, so each
// is said alike.
enum class ArrayReport : std::uint8_t {
  kQueueGrewPastItsBound,
  kQueueWrittenAtAnInvalidIndex,
  kAssociativeArrayReadAtAnInvalidIndex,
  kAssociativeArrayReadAtAMissingEntry,
  kAssociativeArrayWrittenAtAnInvalidIndex,
};

// Each of them, said where the read or the write was written (LRM 20.10).
void ReportQueueGrewPastItsBound(base::SourceLocation at);
void ReportQueueWrittenAtAnInvalidIndex(base::SourceLocation at);
void ReportAssociativeArrayReadAtAnInvalidIndex(base::SourceLocation at);
void ReportAssociativeArrayReadAtAMissingEntry(base::SourceLocation at);
void ReportAssociativeArrayWrittenAtAnInvalidIndex(base::SourceLocation at);

// Who hears those reports. A value knows nothing of when it is read or
// written, so the run listens for as long as it lasts and says each as one of
// its own reports; with nobody listening, nothing is said.
using ArrayReportListener = void (*)(ArrayReport report);

// Installs `listener` and answers the one it replaces.
auto ListenForArrayReports(ArrayReportListener listener) -> ArrayReportListener;

}  // namespace lyra::value
