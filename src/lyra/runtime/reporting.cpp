#include "lyra/runtime/reporting.hpp"

#include <format>
#include <string>
#include <string_view>

#include "lyra/common/severity.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/output_sink.hpp"

namespace lyra::runtime {

auto ReportPrefix(ReportKind kind, Severity severity) -> std::string_view {
  if (kind == ReportKind::kFatalTermination) return "fatal: ";
  return SeverityPrefixCStr(severity);
}

void EmitReport(Engine* engine, const ReportRequest& req) {
  std::string text;
  if (req.origin.has_value()) {
    text += std::format(
        "{}:{}:{}: ", req.origin->file, req.origin->line, req.origin->col);
  }
  text += ReportPrefix(req.kind, req.severity);
  text += req.message;
  text += "\n";
  WriteOutput(text);
  if (req.action == ReportAction::kFinish) {
    engine->Finish();
  }
}

auto DecodeAbiReportPayload(const AbiReportPayload& abi) -> ReportRequest {
  ReportRequest req{
      .kind = static_cast<ReportKind>(abi.kind),
      .severity = static_cast<Severity>(abi.severity),
      .origin = std::nullopt,
      .message = {},
      .action = static_cast<ReportAction>(abi.action),
  };
  if (abi.file != nullptr) {
    req.origin = ResolvedOrigin{
        .file = abi.file,
        .line = abi.line,
        .col = abi.col,
    };
  }
  if (abi.message != nullptr) {
    req.message = LyraStringGetCStr(abi.message);
  }
  return req;
}

}  // namespace lyra::runtime

extern "C" void LyraEmitReport(
    void* engine_ptr, const lyra::runtime::AbiReportPayload* payload) {
  auto* engine = static_cast<lyra::runtime::Engine*>(engine_ptr);
  auto req = lyra::runtime::DecodeAbiReportPayload(*payload);
  lyra::runtime::EmitReport(engine, req);
}
