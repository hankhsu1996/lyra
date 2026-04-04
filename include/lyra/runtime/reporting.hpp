#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

#include "lyra/common/severity.hpp"
#include "lyra/runtime/string.hpp"

namespace lyra::runtime {

class Engine;

// Runtime report event classification.
// Each producer creates the appropriate kind; the renderer uses it only
// for prefix selection (kFatalTermination -> "fatal: ").
enum class ReportKind : uint8_t {
  kUserSeverity,
  kAssertionFailure,
  kDecisionViolation,
  kFatalTermination,
};

// Post-report action: continue execution or finish simulation.
enum class ReportAction : uint8_t {
  kContinue,
  kFinish,
};

// Resolved source location for a report.
struct ResolvedOrigin {
  const char* file;
  uint32_t line;
  uint32_t col;
};

// Owned runtime report request. All semantic decoration is baked into
// `message` by the producer; the renderer only handles prefix and origin.
struct ReportRequest {
  ReportKind kind;
  Severity severity;
  std::optional<ResolvedOrigin> origin;
  std::string message;
  ReportAction action = ReportAction::kContinue;
};

// Returns the user-visible prefix for a report.
// Fatal prefix is determined by kind, not severity.
auto ReportPrefix(ReportKind kind, Severity severity) -> std::string_view;

// Canonical runtime report entrypoint. All semantic reports converge here.
// Formats origin + prefix + message + newline, writes to output sink,
// then optionally finishes simulation.
void EmitReport(Engine* engine, const ReportRequest& req);

// Compact ABI payload for generated-code -> runtime boundary.
// Codegen populates this struct on the stack; runtime decodes it.
// Separate from ReportRequest: this is transport, not the internal model.
struct AbiReportPayload {
  uint8_t kind;
  uint8_t severity;
  uint8_t action;
  uint8_t reserved;
  const char* file;
  uint32_t line;
  uint32_t col;
  LyraStringHandle message;
};

// Decode ABI payload into owned ReportRequest.
auto DecodeAbiReportPayload(const AbiReportPayload& abi) -> ReportRequest;

extern "C" {

// Runtime entrypoint called from generated code.
// Decodes payload and delegates to EmitReport().
void LyraEmitReport(void* engine, const AbiReportPayload* payload);
}

}  // namespace lyra::runtime
