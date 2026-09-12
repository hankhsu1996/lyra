#include "lyra/support/builtin_fn.hpp"

#include "lyra/base/internal_error.hpp"

namespace lyra::support {

auto RuntimeEntryOf(BuiltinFn id) -> RuntimeEntry {
  switch (id) {
    case BuiltinFn::kElement:
      return {
          .name = "element",
          .declaration = Method{"Element"},
          .index_operand = 1};
    case BuiltinFn::kSlice:
      return {.name = "slice", .declaration = Method{"Slice"}};
    case BuiltinFn::kElementRef:
      return {
          .name = "element_ref",
          .declaration = Method{"ElementRef"},
          .answers_with_the_part = true};
    case BuiltinFn::kSliceRef:
      return {
          .name = "slice_ref",
          .declaration = Method{"SliceRef"},
          .answers_with_the_part = true};
    case BuiltinFn::kPart:
      return {.name = "extract", .declaration = Method{"Get"}};
    case BuiltinFn::kPartRef:
      return {
          .name = "part_ref",
          .declaration = Method{"GetRef"},
          .answers_with_the_part = true};
    case BuiltinFn::kTagMatches:
      return {.name = "tag_matches", .declaration = Method{"IsTagged"}};
    case BuiltinFn::kMakeActiveMember:
      return {.name = "make", .declaration = StaticFactory{"Make"}};
    case BuiltinFn::kRequire:
      return {
          .name = "require",
          .declaration = FreeFunction{"lyra::value::Require"}};
    case BuiltinFn::kSize:
      return {.name = "size", .declaration = Method{"Size"}};
    case BuiltinFn::kLen:
      return {.name = "len", .declaration = Method{"Len"}};
    case BuiltinFn::kBitstreamWidth:
      return {
          .name = "bitstream_width", .declaration = Method{"BitstreamWidth"}};
    case BuiltinFn::kToBitstream:
      return {.name = "to_bitstream", .declaration = Method{"ToBitstream"}};
    case BuiltinFn::kFromBitstream:
      return {
          .name = "from_bitstream",
          .declaration = StaticFactory{"FromBitstream"},
          .result_prototype_operand = 1};
    case BuiltinFn::kReverseBlocks:
      return {.name = "reverse_blocks", .declaration = Method{"ReverseBlocks"}};
    case BuiltinFn::kToOwned:
      return {.name = "to_owned", .declaration = Method{"ToOwned"}};
    case BuiltinFn::kDelete:
      return {
          .name = "delete",
          .declaration = Method{"Delete"},
          .mutates_receiver = true};
    case BuiltinFn::kDeleteIndex:
      return {
          .name = "delete_index",
          .declaration = Method{"DeleteIndex"},
          .mutates_receiver = true,
          .index_operand = 1};
    case BuiltinFn::kReverse:
      return {
          .name = "reverse",
          .declaration = Method{"Reverse"},
          .mutates_receiver = true};
    case BuiltinFn::kSort:
      return {
          .name = "sort",
          .declaration = Method{"Sort"},
          .mutates_receiver = true,
          .takes_closure = true};
    case BuiltinFn::kRsort:
      return {
          .name = "rsort",
          .declaration = Method{"Rsort"},
          .mutates_receiver = true,
          .takes_closure = true};
    case BuiltinFn::kSum:
      return {
          .name = "sum",
          .declaration = Method{"Sum"},
          .takes_closure = true,
          .result_prototype_operand = 2};
    case BuiltinFn::kProduct:
      return {
          .name = "product",
          .declaration = Method{"Product"},
          .takes_closure = true,
          .result_prototype_operand = 2};
    case BuiltinFn::kAnd:
      return {
          .name = "and",
          .declaration = Method{"And"},
          .takes_closure = true,
          .result_prototype_operand = 2};
    case BuiltinFn::kOr:
      return {
          .name = "or",
          .declaration = Method{"Or"},
          .takes_closure = true,
          .result_prototype_operand = 2};
    case BuiltinFn::kXor:
      return {
          .name = "xor",
          .declaration = Method{"Xor"},
          .takes_closure = true,
          .result_prototype_operand = 2};
    case BuiltinFn::kFind:
      return {
          .name = "find",
          .declaration = Method{"Find"},
          .takes_closure = true,
          .result_prototype_operand = 2};
    case BuiltinFn::kFindIndex:
      return {
          .name = "find_index",
          .declaration = Method{"FindIndex"},
          .takes_closure = true,
          .result_prototype_operand = 2};
    case BuiltinFn::kFindFirst:
      return {
          .name = "find_first",
          .declaration = Method{"FindFirst"},
          .takes_closure = true,
          .result_prototype_operand = 2};
    case BuiltinFn::kFindFirstIndex:
      return {
          .name = "find_first_index",
          .declaration = Method{"FindFirstIndex"},
          .takes_closure = true,
          .result_prototype_operand = 2};
    case BuiltinFn::kFindLast:
      return {
          .name = "find_last",
          .declaration = Method{"FindLast"},
          .takes_closure = true,
          .result_prototype_operand = 2};
    case BuiltinFn::kFindLastIndex:
      return {
          .name = "find_last_index",
          .declaration = Method{"FindLastIndex"},
          .takes_closure = true,
          .result_prototype_operand = 2};
    case BuiltinFn::kMin:
      return {
          .name = "min",
          .declaration = Method{"Min"},
          .takes_closure = true,
          .result_prototype_operand = 2};
    case BuiltinFn::kMax:
      return {
          .name = "max",
          .declaration = Method{"Max"},
          .takes_closure = true,
          .result_prototype_operand = 2};
    case BuiltinFn::kUnique:
      return {
          .name = "unique",
          .declaration = Method{"Unique"},
          .takes_closure = true,
          .result_prototype_operand = 2};
    case BuiltinFn::kUniqueIndex:
      return {
          .name = "unique_index",
          .declaration = Method{"UniqueIndex"},
          .takes_closure = true,
          .result_prototype_operand = 2};
    case BuiltinFn::kMap:
      return {
          .name = "map",
          .declaration = Method{"Map"},
          .takes_closure = true,
          .result_prototype_operand = 2};
    case BuiltinFn::kInsert:
      return {
          .name = "insert",
          .declaration = Method{"Insert"},
          .mutates_receiver = true};
    case BuiltinFn::kPopFront:
      return {
          .name = "pop_front",
          .declaration = Method{"PopFront"},
          .mutates_receiver = true};
    case BuiltinFn::kPopBack:
      return {
          .name = "pop_back",
          .declaration = Method{"PopBack"},
          .mutates_receiver = true};
    case BuiltinFn::kPushFront:
      return {
          .name = "push_front",
          .declaration = Method{"PushFront"},
          .mutates_receiver = true};
    case BuiltinFn::kPushBack:
      return {
          .name = "push_back",
          .declaration = Method{"PushBack"},
          .mutates_receiver = true};
    case BuiltinFn::kExists:
      return {
          .name = "exists",
          .declaration = Method{"Exists"},
          .index_operand = 1};
    case BuiltinFn::kAssocFirst:
      return {
          .name = "assoc_first",
          .declaration = Method{"First"},
          .writes_the_index_back = true,
          .index_operand = 1};
    case BuiltinFn::kAssocLast:
      return {
          .name = "assoc_last",
          .declaration = Method{"Last"},
          .writes_the_index_back = true,
          .index_operand = 1};
    case BuiltinFn::kAssocNext:
      return {
          .name = "assoc_next",
          .declaration = Method{"Next"},
          .writes_the_index_back = true,
          .index_operand = 1};
    case BuiltinFn::kAssocPrev:
      return {
          .name = "assoc_prev",
          .declaration = Method{"Prev"},
          .writes_the_index_back = true,
          .index_operand = 1};
    case BuiltinFn::kAssocMinIndex:
      return {
          .name = "assoc_min_index",
          .declaration = Method{"MinIndex"},
          .result_prototype_operand = 1};
    case BuiltinFn::kAssocMaxIndex:
      return {
          .name = "assoc_max_index",
          .declaration = Method{"MaxIndex"},
          .result_prototype_operand = 1};
    case BuiltinFn::kGetc:
      return {.name = "getc", .declaration = Method{"Getc"}};
    case BuiltinFn::kPutc:
      return {
          .name = "putc",
          .declaration = Method{"Putc"},
          .mutates_receiver = true};
    case BuiltinFn::kToupper:
      return {.name = "toupper", .declaration = Method{"Toupper"}};
    case BuiltinFn::kTolower:
      return {.name = "tolower", .declaration = Method{"Tolower"}};
    case BuiltinFn::kCompare:
      return {.name = "compare", .declaration = Method{"Compare"}};
    case BuiltinFn::kIcompare:
      return {.name = "icompare", .declaration = Method{"Icompare"}};
    case BuiltinFn::kSubstr:
      return {.name = "substr", .declaration = Method{"Substr"}};
    case BuiltinFn::kAtoi:
      return {.name = "atoi", .declaration = Method{"Atoi"}};
    case BuiltinFn::kAtohex:
      return {.name = "atohex", .declaration = Method{"Atohex"}};
    case BuiltinFn::kAtooct:
      return {.name = "atooct", .declaration = Method{"Atooct"}};
    case BuiltinFn::kAtobin:
      return {.name = "atobin", .declaration = Method{"Atobin"}};
    case BuiltinFn::kAtoreal:
      return {.name = "atoreal", .declaration = Method{"Atoreal"}};
    case BuiltinFn::kItoa:
      return {
          .name = "itoa",
          .declaration = Method{"Itoa"},
          .mutates_receiver = true};
    case BuiltinFn::kHextoa:
      return {
          .name = "hextoa",
          .declaration = Method{"Hextoa"},
          .mutates_receiver = true};
    case BuiltinFn::kOcttoa:
      return {
          .name = "octtoa",
          .declaration = Method{"Octtoa"},
          .mutates_receiver = true};
    case BuiltinFn::kBintoa:
      return {
          .name = "bintoa",
          .declaration = Method{"Bintoa"},
          .mutates_receiver = true};
    case BuiltinFn::kRealtoa:
      return {
          .name = "realtoa",
          .declaration = Method{"Realtoa"},
          .mutates_receiver = true};
    case BuiltinFn::kTrigger:
      return {
          .name = "trigger",
          .declaration = Method{"Trigger"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kTriggered:
      return {
          .name = "triggered",
          .declaration = Method{"Triggered"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kSampledHistoryInstall:
      return {
          .name = "sampled_history_install", .declaration = Method{"Install"}};
    case BuiltinFn::kSampledHistoryPush:
      return {.name = "sampled_history_push", .declaration = Method{"Push"}};
    case BuiltinFn::kSampledHistoryAt:
      return {.name = "sampled_history_at", .declaration = Method{"At"}};
    case BuiltinFn::kEvaluationAttemptsInstall:
      return {
          .name = "evaluation_attempts_install",
          .declaration = Method{"Install"}};
    case BuiltinFn::kEvaluationAttemptsSeedWord:
      return {
          .name = "evaluation_attempts_seed_word",
          .declaration = Method{"SeedWord"}};
    case BuiltinFn::kEvaluationAttemptsBeginTick:
      return {
          .name = "evaluation_attempts_begin_tick",
          .declaration = Method{"BeginTick"}};
    case BuiltinFn::kEvaluationAttemptsDisableTick:
      return {
          .name = "evaluation_attempts_disable_tick",
          .declaration = Method{"DisableTick"}};
    case BuiltinFn::kEvaluationAttemptsLiveWord:
      return {
          .name = "evaluation_attempts_live_word",
          .declaration = Method{"LiveWord"}};
    case BuiltinFn::kEvaluationAttemptsNextUnstepped:
      return {
          .name = "evaluation_attempts_next_unstepped",
          .declaration = Method{"NextUnstepped"}};
    case BuiltinFn::kEvaluationAttemptsBitsAt:
      return {
          .name = "evaluation_attempts_bits_at",
          .declaration = Method{"BitsAt"}};
    case BuiltinFn::kEvaluationAttemptsSetWord:
      return {
          .name = "evaluation_attempts_set_word",
          .declaration = Method{"SetWord"}};
    case BuiltinFn::kEvaluationAttemptsStep:
      return {
          .name = "evaluation_attempts_step", .declaration = Method{"Step"}};
    case BuiltinFn::kEvaluationAttemptsSeed:
      return {
          .name = "evaluation_attempts_seed", .declaration = Method{"Seed"}};
    case BuiltinFn::kEvaluationAttemptsSettle:
      return {
          .name = "evaluation_attempts_settle",
          .declaration = Method{"Settle"}};
    case BuiltinFn::kIsUnknown:
      return {.name = "is_unknown", .declaration = Method{"IsUnknown"}};
    case BuiltinFn::kCountBits:
      return {.name = "count_bits", .declaration = Method{"CountBits"}};
    case BuiltinFn::kClog2:
      return {.name = "clog2", .declaration = Method{"Clog2"}};
    case BuiltinFn::kLn:
      return {.name = "ln", .declaration = Method{"Ln"}};
    case BuiltinFn::kLog10:
      return {.name = "log10", .declaration = Method{"Log10"}};
    case BuiltinFn::kExp:
      return {.name = "exp", .declaration = Method{"Exp"}};
    case BuiltinFn::kSqrt:
      return {.name = "sqrt", .declaration = Method{"Sqrt"}};
    case BuiltinFn::kFloor:
      return {.name = "floor", .declaration = Method{"Floor"}};
    case BuiltinFn::kCeil:
      return {.name = "ceil", .declaration = Method{"Ceil"}};
    case BuiltinFn::kSin:
      return {.name = "sin", .declaration = Method{"Sin"}};
    case BuiltinFn::kCos:
      return {.name = "cos", .declaration = Method{"Cos"}};
    case BuiltinFn::kTan:
      return {.name = "tan", .declaration = Method{"Tan"}};
    case BuiltinFn::kAsin:
      return {.name = "asin", .declaration = Method{"Asin"}};
    case BuiltinFn::kAcos:
      return {.name = "acos", .declaration = Method{"Acos"}};
    case BuiltinFn::kAtan:
      return {.name = "atan", .declaration = Method{"Atan"}};
    case BuiltinFn::kAtan2:
      return {.name = "atan2", .declaration = Method{"Atan2"}};
    case BuiltinFn::kHypot:
      return {.name = "hypot", .declaration = Method{"Hypot"}};
    case BuiltinFn::kSinh:
      return {.name = "sinh", .declaration = Method{"Sinh"}};
    case BuiltinFn::kCosh:
      return {.name = "cosh", .declaration = Method{"Cosh"}};
    case BuiltinFn::kTanh:
      return {.name = "tanh", .declaration = Method{"Tanh"}};
    case BuiltinFn::kAsinh:
      return {.name = "asinh", .declaration = Method{"Asinh"}};
    case BuiltinFn::kAcosh:
      return {.name = "acosh", .declaration = Method{"Acosh"}};
    case BuiltinFn::kAtanh:
      return {.name = "atanh", .declaration = Method{"Atanh"}};
    case BuiltinFn::kInitialize:
      return {.name = "initialize", .declaration = Method{"Initialize"}};
    case BuiltinFn::kNetInitializeTriState:
      return {
          .name = "net_initialize_tri_state",
          .declaration = Method{"InitializeTriState"}};
    case BuiltinFn::kNetInitializeWiredAnd:
      return {
          .name = "net_initialize_wired_and",
          .declaration = Method{"InitializeWiredAnd"}};
    case BuiltinFn::kNetInitializeWiredOr:
      return {
          .name = "net_initialize_wired_or",
          .declaration = Method{"InitializeWiredOr"}};
    case BuiltinFn::kNetInitializeRetaining:
      return {
          .name = "net_initialize_retaining",
          .declaration = Method{"InitializeRetaining"}};
    case BuiltinFn::kLoad:
      return {.name = "get", .declaration = Method{"Get"}};
    case BuiltinFn::kStore:
      return {.name = "set", .declaration = Method{"Set"}};
    case BuiltinFn::kSampledLoad:
      return {.name = "sampled_load", .declaration = Method{"SampledGet"}};
    case BuiltinFn::kArmSampling:
      return {.name = "arm_sampling", .declaration = Method{"ArmSampling"}};
    case BuiltinFn::kOpenForWrite:
      return {.name = "open_for_write", .declaration = Method{"Mutate"}};
    case BuiltinFn::kAttachDriver:
      return {.name = "attach_driver", .declaration = Method{"AttachDriver"}};
    case BuiltinFn::kNetJoin:
      return {.name = "net_join", .declaration = Method{"Join"}};
    case BuiltinFn::kBeginTakeover:
      return {.name = "begin_takeover", .declaration = Method{"BeginTakeover"}};
    case BuiltinFn::kDriveTakeover:
      return {.name = "drive_takeover", .declaration = Method{"DriveTakeover"}};
    case BuiltinFn::kEndTakeover:
      return {.name = "end_takeover", .declaration = Method{"EndTakeover"}};
    case BuiltinFn::kCurrentRuntime:
      return {
          .name = "current_runtime",
          .declaration = FreeFunction{"lyra::runtime::current_runtime"}};
    case BuiltinFn::kSubmitNba:
      return {
          .name = "submit_nba",
          .declaration = Method{"SubmitNba"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kSubmitNbaAfter:
      return {
          .name = "submit_nba_after",
          .declaration = Method{"SubmitNbaAfter"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kSubmitNbaAfterReal:
      return {
          .name = "submit_nba_after_real",
          .declaration = Method{"SubmitNbaAfterReal"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kRunDetached:
      return {
          .name = "run_detached",
          .declaration = Method{"RunDetached"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kResumeInNbaRegion:
      return {
          .name = "resume_in_nba_region",
          .declaration = FreeFunction{"lyra::runtime::ResumeInNbaRegion"},
          .takes_the_runtime_handle = true,
          .parks_the_caller = true};
    case BuiltinFn::kSubmitPostponed:
      return {
          .name = "submit_postponed",
          .declaration = Method{"SubmitPostponed"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kSubmitObserved:
      return {
          .name = "submit_observed",
          .declaration = Method{"SubmitObserved"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kSubmitViolationReport:
      return {
          .name = "submit_violation_report",
          .declaration = Method{"SubmitViolationReport"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kSubmitDeferredObserved:
      return {
          .name = "submit_deferred_observed",
          .declaration = Method{"SubmitDeferredObserved"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kSubmitDeferredFinal:
      return {
          .name = "submit_deferred_final",
          .declaration = Method{"SubmitDeferredFinal"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kFiles:
      return {
          .name = "files",
          .declaration = Method{"Files"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kCancellationFor:
      return {
          .name = "cancellation_for", .declaration = Method{"CancellationFor"}};
    case BuiltinFn::kIsCancelled:
      return {.name = "is_cancelled", .declaration = Method{"IsCancelled"}};
    case BuiltinFn::kFormat:
      return {
          .name = "format", .declaration = FreeFunction{"lyra::value::Format"}};
    case BuiltinFn::kFormatRuntime:
      return {
          .name = "format_runtime",
          .declaration = FreeFunction{"lyra::value::FormatRuntime"}};
    case BuiltinFn::kMakeRenderedFormatArg:
      return {
          .name = "make_rendered_format_arg",
          .declaration = StaticFactory{"Rendered"}};
    case BuiltinFn::kWrite:
      return {.name = "write", .declaration = Method{"Write"}};
    case BuiltinFn::kWriteln:
      return {.name = "writeln", .declaration = Method{"Writeln"}};
    case BuiltinFn::kDiagnostic:
      return {
          .name = "diagnostic",
          .declaration = Method{"Diagnostic"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kEmitInfo:
      return {.name = "emit_info", .declaration = Method{"EmitInfo"}};
    case BuiltinFn::kEmitWarning:
      return {.name = "emit_warning", .declaration = Method{"EmitWarning"}};
    case BuiltinFn::kEmitError:
      return {.name = "emit_error", .declaration = Method{"EmitError"}};
    case BuiltinFn::kEmitFatal:
      return {.name = "emit_fatal", .declaration = Method{"EmitFatal"}};
    case BuiltinFn::kRecordCoverage:
      return {
          .name = "record_coverage",
          .declaration = Method{"RecordCoverage"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kTimeFormat:
      return {
          .name = "time_format",
          .declaration = Method{"TimeFormat"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kSetTimeFormat:
      return {
          .name = "set_time_format",
          .declaration = Method{"SetTimeFormat"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kResetTimeFormat:
      return {
          .name = "reset_time_format",
          .declaration = Method{"ResetTimeFormat"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kScanString:
      return {
          .name = "scan_string",
          .declaration = FreeFunction{"lyra::value::ScanString"}};
    case BuiltinFn::kScanFile:
      return {
          .name = "scan_file",
          .declaration = FreeFunction{"lyra::value::ScanFile"}};
    case BuiltinFn::kPeekBuffered:
      return {.name = "peek_buffered", .declaration = Method{"PeekBuffered"}};
    case BuiltinFn::kAdvanceFd:
      return {.name = "advance_fd", .declaration = Method{"AdvanceFd"}};
    case BuiltinFn::kFileOpen:
      return {.name = "file_open", .declaration = Method{"Open"}};
    case BuiltinFn::kFileOpenMode:
      return {.name = "file_open_mode", .declaration = Method{"OpenWithMode"}};
    case BuiltinFn::kFileClose:
      return {.name = "file_close", .declaration = Method{"Close"}};
    case BuiltinFn::kFileGetc:
      return {.name = "file_getc", .declaration = Method{"Getc"}};
    case BuiltinFn::kFileUngetc:
      return {.name = "file_ungetc", .declaration = Method{"Ungetc"}};
    case BuiltinFn::kFileGets:
      return {.name = "file_gets", .declaration = Method{"Gets"}};
    case BuiltinFn::kFileRead:
      return {.name = "file_read", .declaration = Method{"Read"}};
    case BuiltinFn::kFileReadMemory:
      return {.name = "file_read_memory", .declaration = Method{"ReadMemory"}};
    case BuiltinFn::kFileSeek:
      return {.name = "file_seek", .declaration = Method{"Seek"}};
    case BuiltinFn::kFileRewind:
      return {.name = "file_rewind", .declaration = Method{"Rewind"}};
    case BuiltinFn::kFileTell:
      return {.name = "file_tell", .declaration = Method{"Tell"}};
    case BuiltinFn::kFileEof:
      return {.name = "file_eof", .declaration = Method{"Eof"}};
    case BuiltinFn::kFileError:
      return {.name = "file_error", .declaration = Method{"Error"}};
    case BuiltinFn::kFileFlush:
      return {.name = "file_flush", .declaration = Method{"Flush"}};
    case BuiltinFn::kFileFlushAll:
      return {.name = "file_flush_all", .declaration = Method{"FlushAll"}};
    case BuiltinFn::kTestPlusargs:
      return {
          .name = "test_plusargs",
          .declaration = FreeFunction{"lyra::runtime::TestPlusargs"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kValuePlusargs:
      return {
          .name = "value_plusargs",
          .declaration = FreeFunction{"lyra::runtime::ValuePlusargs"}};
    case BuiltinFn::kRunHostCommand:
      return {
          .name = "run_host_command",
          .declaration = FreeFunction{"lyra::runtime::RunHostCommand"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kRunNullHostCommand:
      return {
          .name = "run_null_host_command",
          .declaration = FreeFunction{"lyra::runtime::RunNullHostCommand"}};
    case BuiltinFn::kReadMem:
      return {
          .name = "read_mem",
          .declaration = FreeFunction{"lyra::runtime::ReadMem"}};
    case BuiltinFn::kReadMemWithin:
      return {
          .name = "read_mem_within",
          .declaration = FreeFunction{"lyra::runtime::ReadMemWithin"}};
    case BuiltinFn::kWriteMem:
      return {
          .name = "write_mem",
          .declaration = FreeFunction{"lyra::runtime::WriteMem"}};
    case BuiltinFn::kWriteMemWithin:
      return {
          .name = "write_mem_within",
          .declaration = FreeFunction{"lyra::runtime::WriteMemWithin"}};
    case BuiltinFn::kDelay:
      return {
          .name = "delay",
          .declaration = FreeFunction{"lyra::runtime::Delay"},
          .takes_the_runtime_handle = true,
          .parks_the_caller = true};
    case BuiltinFn::kDelayReal:
      return {
          .name = "delay_real",
          .declaration = FreeFunction{"lyra::runtime::DelayReal"},
          .takes_the_runtime_handle = true,
          .parks_the_caller = true};
    case BuiltinFn::kObservationOnReaching:
      return {
          .name = "observation_on_reaching",
          .declaration = StaticFactory{"OnReaching"}};
    case BuiltinFn::kObservationOfValue:
      return {
          .name = "observation_of_value",
          .declaration = StaticFactory{"OfValue"}};
    case BuiltinFn::kObservationOfValueQualified:
      return {
          .name = "observation_of_value_qualified",
          .declaration = StaticFactory{"OfValueQualified"}};
    case BuiltinFn::kObservationQualified:
      return {
          .name = "observation_qualified",
          .declaration = StaticFactory{"Qualified"}};
    case BuiltinFn::kWaitAny:
      return {
          .name = "wait_any",
          .declaration = FreeFunction{"lyra::runtime::WaitAny"},
          .takes_the_runtime_handle = true,
          .parks_the_caller = true};
    case BuiltinFn::kSimTime:
      return {
          .name = "sim_time",
          .declaration = FreeFunction{"lyra::runtime::SimTimeInUnit"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kSTime:
      return {
          .name = "stime",
          .declaration = FreeFunction{"lyra::runtime::STimeInUnit"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kRealTime:
      return {
          .name = "realtime",
          .declaration = FreeFunction{"lyra::runtime::RealTimeInUnit"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kUrandom:
      return {
          .name = "urandom",
          .declaration = FreeFunction{"lyra::runtime::Urandom"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kUrandomSeeded:
      return {
          .name = "urandom_seeded",
          .declaration = FreeFunction{"lyra::runtime::UrandomSeeded"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kUrandomRange:
      return {
          .name = "urandom_range",
          .declaration = FreeFunction{"lyra::runtime::UrandomRange"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kRandom:
      return {
          .name = "random",
          .declaration = FreeFunction{"lyra::runtime::Random"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kDistUniform:
      return {
          .name = "dist_uniform",
          .declaration = FreeFunction{"lyra::runtime::DistUniform"}};
    case BuiltinFn::kDistNormal:
      return {
          .name = "dist_normal",
          .declaration = FreeFunction{"lyra::runtime::DistNormal"}};
    case BuiltinFn::kDistExponential:
      return {
          .name = "dist_exponential",
          .declaration = FreeFunction{"lyra::runtime::DistExponential"}};
    case BuiltinFn::kDistPoisson:
      return {
          .name = "dist_poisson",
          .declaration = FreeFunction{"lyra::runtime::DistPoisson"}};
    case BuiltinFn::kDistChiSquare:
      return {
          .name = "dist_chi_square",
          .declaration = FreeFunction{"lyra::runtime::DistChiSquare"}};
    case BuiltinFn::kDistT:
      return {
          .name = "dist_t",
          .declaration = FreeFunction{"lyra::runtime::DistT"}};
    case BuiltinFn::kDistErlang:
      return {
          .name = "dist_erlang",
          .declaration = FreeFunction{"lyra::runtime::DistErlang"}};
    case BuiltinFn::kFinish:
      return {
          .name = "finish",
          .declaration = FreeFunction{"lyra::runtime::Finish"},
          .takes_the_runtime_handle = true,
          .parks_the_caller = true};
    case BuiltinFn::kStop:
      return {
          .name = "stop",
          .declaration = FreeFunction{"lyra::runtime::Stop"},
          .takes_the_runtime_handle = true,
          .parks_the_caller = true};
    case BuiltinFn::kResolveRoot:
      return {.name = "resolve_root", .declaration = Method{"ResolveRoot"}};
    case BuiltinFn::kResolveVisibleChild:
      return {
          .name = "resolve_visible_child",
          .declaration = Method{"ResolveVisibleChild"}};
    case BuiltinFn::kRegisterSignal:
      return {
          .name = "register_signal", .declaration = Method{"RegisterSignal"}};
    case BuiltinFn::kAddOwnedChild:
      return {
          .name = "add_owned_child", .declaration = Method{"AddOwnedChild"}};
    case BuiltinFn::kRegisterDisableTarget:
      return {
          .name = "register_disable_target",
          .declaration = Method{"RegisterDisableTarget"}};
    case BuiltinFn::kFindDisableTarget:
      return {
          .name = "find_disable_target",
          .declaration = Method{"FindDisableTarget"}};
    case BuiltinFn::kFindSignal:
      return {.name = "find_signal", .declaration = Method{"FindSignal"}};
    case BuiltinFn::kFindChild:
      return {.name = "find_child", .declaration = Method{"FindChild"}};
    case BuiltinFn::kFindSubroutine:
      return {
          .name = "find_subroutine", .declaration = Method{"FindSubroutine"}};
    case BuiltinFn::kFindClass:
      return {.name = "find_class", .declaration = Method{"FindClass"}};
    case BuiltinFn::kClassFindProperty:
      return {
          .name = "class_find_property",
          .declaration = FreeFunction{"lyra::runtime::FindProperty"}};
    case BuiltinFn::kClassFindBehavior:
      return {
          .name = "class_find_behavior",
          .declaration = FreeFunction{"lyra::runtime::FindBehavior"}};
    case BuiltinFn::kForkWaitAll:
      return {
          .name = "fork_wait_all",
          .declaration = FreeFunction{"lyra::runtime::ForkWaitAll"},
          .takes_the_runtime_handle = true,
          .parks_the_caller = true};
    case BuiltinFn::kForkWaitFirst:
      return {
          .name = "fork_wait_first",
          .declaration = FreeFunction{"lyra::runtime::ForkWaitFirst"},
          .takes_the_runtime_handle = true,
          .parks_the_caller = true};
    case BuiltinFn::kSpawnAll:
      return {
          .name = "spawn_all",
          .declaration = FreeFunction{"lyra::runtime::SpawnAll"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kWaitFork:
      return {
          .name = "wait_fork",
          .declaration = FreeFunction{"lyra::runtime::WaitFork"},
          .takes_the_runtime_handle = true,
          .parks_the_caller = true};
    case BuiltinFn::kDisableFork:
      return {
          .name = "disable_fork",
          .declaration = FreeFunction{"lyra::runtime::DisableFork"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kDisable:
      return {
          .name = "disable",
          .declaration = FreeFunction{"lyra::runtime::Disable"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kEnterTarget:
      return {
          .name = "enter_target",
          .declaration = FreeFunction{"lyra::runtime::EnterCancellationTarget"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kLeaveTarget:
      return {
          .name = "leave_target",
          .declaration = FreeFunction{"lyra::runtime::LeaveCancellationTarget"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kEffectNamesTarget:
      return {
          .name = "effect_names_target",
          .declaration = FreeFunction{"lyra::runtime::EffectNamesTarget"}};
    case BuiltinFn::kProcessSelf:
      return {
          .name = "process_self",
          .declaration = FreeFunction{"lyra::runtime::ProcessSelf"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kProcessStatus:
      return {
          .name = "process_status",
          .declaration = FreeFunction{"lyra::runtime::ProcessStatus"}};
    case BuiltinFn::kProcessKill:
      return {
          .name = "process_kill",
          .declaration = FreeFunction{"lyra::runtime::ProcessKill"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kProcessAwait:
      return {
          .name = "process_await",
          .declaration = FreeFunction{"lyra::runtime::ProcessAwait"},
          .takes_the_runtime_handle = true,
          .parks_the_caller = true};
    case BuiltinFn::kProcessSuspend:
      return {
          .name = "process_suspend",
          .declaration = FreeFunction{"lyra::runtime::ProcessSuspend"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kProcessResume:
      return {
          .name = "process_resume",
          .declaration = FreeFunction{"lyra::runtime::ProcessResume"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kRegisterInitial:
      return {
          .name = "register_initial",
          .declaration = FreeFunction{"lyra::runtime::RegisterInitialProcess"}};
    case BuiltinFn::kRegisterFinal:
      return {
          .name = "register_final",
          .declaration = FreeFunction{"lyra::runtime::RegisterFinalProcess"}};
    case BuiltinFn::kEnterScopeStaticInit:
      return {
          .name = "enter_scope_static_init",
          .declaration = FreeFunction{"lyra::runtime::EnterScopeStaticInit"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kEnterNamespaceStaticInit:
      return {
          .name = "enter_namespace_static_init",
          .declaration =
              FreeFunction{"lyra::runtime::EnterNamespaceStaticInit"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kLeaveStaticInit:
      return {
          .name = "leave_static_init",
          .declaration = FreeFunction{"lyra::runtime::LeaveStaticInit"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kToInt64:
      return {.name = "to_int64", .declaration = Method{"ToInt64"}};
    case BuiltinFn::kRound:
      return {.name = "round", .declaration = Method{"Round"}};
    case BuiltinFn::kTruncate:
      return {.name = "truncate", .declaration = Method{"Truncate"}};
    case BuiltinFn::kToBits:
      return {.name = "to_bits", .declaration = Method{"ToBits"}};
    case BuiltinFn::kFromBits:
      return {.name = "from_bits", .declaration = StaticFactory{"FromBits"}};
    case BuiltinFn::kRealValue:
      return {.name = "real_value", .declaration = Method{"Value"}};
    case BuiltinFn::kStringCStr:
      return {.name = "string_cstr", .declaration = Method{"CStr"}};
    case BuiltinFn::kChandlePtr:
      return {.name = "chandle_ptr", .declaration = Method{"Ptr"}};
    case BuiltinFn::kToSvLogic:
      return {
          .name = "to_sv_logic",
          .declaration = FreeFunction{"lyra::value::ToSvLogic"}};
    case BuiltinFn::kFromSvLogic:
      return {
          .name = "from_sv_logic",
          .declaration = FreeFunction{"lyra::value::FromSvLogic"}};
    case BuiltinFn::kReadCanonicalBitVec:
      return {
          .name = "read_canonical_bit_vec",
          .declaration = FreeFunction{"lyra::value::ReadCanonicalBitVec"}};
    case BuiltinFn::kReadCanonicalLogicVec:
      return {
          .name = "read_canonical_logic_vec",
          .declaration = FreeFunction{"lyra::value::ReadCanonicalLogicVec"}};
    case BuiltinFn::kWriteCanonicalBitVec:
      return {
          .name = "write_canonical_bit_vec",
          .declaration = FreeFunction{"lyra::value::WriteCanonicalBitVec"}};
    case BuiltinFn::kWriteCanonicalLogicVec:
      return {
          .name = "write_canonical_logic_vec",
          .declaration = FreeFunction{"lyra::value::WriteCanonicalLogicVec"}};
    case BuiltinFn::kDpiBitBufferData:
      return {.name = "dpi_bit_buffer_data", .declaration = Method{"Data"}};
    case BuiltinFn::kDpiLogicBufferData:
      return {.name = "dpi_logic_buffer_data", .declaration = Method{"Data"}};
    case BuiltinFn::kDpiOpenArrayHandle:
      return {.name = "dpi_open_array_handle", .declaration = Method{"Handle"}};
    case BuiltinFn::kDpiOpenArrayValue:
      return {
          .name = "dpi_open_array_value",
          .declaration = Method{"ToValue"},
          .result_prototype_operand = 1};
    case BuiltinFn::kRunForeignTaskOnFiber:
      return {
          .name = "run_foreign_task_on_fiber",
          .declaration = FreeFunction{"lyra::runtime::RunForeignTaskOnFiber"}};
    case BuiltinFn::kRunExportedTaskToCompletion:
      return {
          .name = "run_exported_task_to_completion",
          .declaration =
              FreeFunction{"lyra::runtime::RunExportedTaskToCompletion"}};
    case BuiltinFn::kCurrentExportScope:
      return {
          .name = "current_export_scope",
          .declaration = FreeFunction{"lyra::runtime::CurrentExportScope"}};
    case BuiltinFn::kFindExportEntry:
      return {
          .name = "find_export_entry",
          .declaration = FreeFunction{"lyra::runtime::FindExportEntry"}};
    case BuiltinFn::kFromInt:
      return {.name = "from_int", .declaration = StaticFactory{"FromInt"}};
    case BuiltinFn::kFromWords:
      return {.name = "from_words", .declaration = StaticFactory{"FromWords"}};
    case BuiltinFn::kConvertFrom:
      return {
          .name = "convert_from", .declaration = StaticFactory{"ConvertFrom"}};
    case BuiltinFn::kFromPackedArray:
      return {
          .name = "from_packed_array",
          .declaration = StaticFactory{"FromPackedArray"}};
    case BuiltinFn::kFromByteArray:
      return {
          .name = "from_byte_array",
          .declaration = StaticFactory{"FromByteArray"}};
    case BuiltinFn::kFromString:
      return {
          .name = "from_string", .declaration = StaticFactory{"FromString"}};
    case BuiltinFn::kFromArray:
      return {
          .name = "from_array",
          .declaration = StaticFactory{"FromArray"},
          .result_prototype_operand = 1};
    case BuiltinFn::kConformBound:
      return {.name = "conform_bound", .declaration = Method{"ConformBound"}};
    case BuiltinFn::kArrayConcatElement:
      return {.name = "concat_element", .declaration = Method{"ConcatElement"}};
    case BuiltinFn::kArrayConcatSpread:
      return {
          .name = "concat_spread",
          .declaration = Method{"ConcatSpread"},
          .spread_operand = 1};
    case BuiltinFn::kArrayConformSize:
      return {
          .name = "conform_size", .declaration = StaticFactory{"ConformSize"}};
    case BuiltinFn::kMakeDynamicArrayDefault:
      return {
          .name = "make_dynamic_array_default",
          .declaration = StaticFactory{"Default"},
          .result_prototype_operand = 0};
    case BuiltinFn::kMakeDynamicArrayNew:
      return {
          .name = "make_dynamic_array_new",
          .declaration = StaticFactory{"New"},
          .result_prototype_operand = 1};
    case BuiltinFn::kMakeDynamicArrayNewCopy:
      return {
          .name = "make_dynamic_array_new_copy",
          .declaration = StaticFactory{"NewCopy"},
          .result_prototype_operand = 1};
    case BuiltinFn::kConcat:
      return {.name = "concat", .declaration = Method{"Concat"}};
    case BuiltinFn::kReplicate:
      return {.name = "replicate", .declaration = Method{"Replicate"}};
    case BuiltinFn::kPow:
      return {.name = "pow", .declaration = Method{"Pow"}};
    case BuiltinFn::kShiftLeft:
      return {.name = "shift_left", .declaration = Method{"ShiftLeft"}};
    case BuiltinFn::kLogicalShiftRight:
      return {
          .name = "logical_shift_right",
          .declaration = Method{"LogicalShiftRight"}};
    case BuiltinFn::kArithmeticShiftRight:
      return {
          .name = "arithmetic_shift_right",
          .declaration = Method{"ArithmeticShiftRight"}};
    case BuiltinFn::kShiftLeftAssign:
      return {
          .name = "shift_left_assign",
          .declaration = Method{"ShiftLeftAssign"},
          .mutates_receiver = true};
    case BuiltinFn::kLogicalShiftRightAssign:
      return {
          .name = "logical_shift_right_assign",
          .declaration = Method{"LogicalShiftRightAssign"},
          .mutates_receiver = true};
    case BuiltinFn::kArithmeticShiftRightAssign:
      return {
          .name = "arithmetic_shift_right_assign",
          .declaration = Method{"ArithmeticShiftRightAssign"},
          .mutates_receiver = true};
    case BuiltinFn::kBitwiseXnor:
      return {.name = "bitwise_xnor", .declaration = Method{"BitwiseXnor"}};
    case BuiltinFn::kLogicalImplication:
      return {
          .name = "logical_implication",
          .declaration = Method{"LogicalImplication"}};
    case BuiltinFn::kLogicalEquivalence:
      return {
          .name = "logical_equivalence",
          .declaration = Method{"LogicalEquivalence"}};
    case BuiltinFn::kWildcardEquals:
      return {
          .name = "wildcard_equals", .declaration = Method{"WildcardEquals"}};
    case BuiltinFn::kCaseEqual:
      return {.name = "case_equal", .declaration = Method{"CaseEqual"}};
    case BuiltinFn::kCasezEquals:
      return {.name = "casez_equals", .declaration = Method{"CasezEquals"}};
    case BuiltinFn::kCasexEquals:
      return {.name = "casex_equals", .declaration = Method{"CasexEquals"}};
    case BuiltinFn::kMergeConditional:
      return {
          .name = "merge_conditional",
          .declaration = Method{"MergeConditional"}};
    case BuiltinFn::kReductionAnd:
      return {.name = "reduction_and", .declaration = Method{"ReductionAnd"}};
    case BuiltinFn::kReductionOr:
      return {.name = "reduction_or", .declaration = Method{"ReductionOr"}};
    case BuiltinFn::kReductionXor:
      return {.name = "reduction_xor", .declaration = Method{"ReductionXor"}};
    case BuiltinFn::kReductionNand:
      return {.name = "reduction_nand", .declaration = Method{"ReductionNand"}};
    case BuiltinFn::kReductionNor:
      return {.name = "reduction_nor", .declaration = Method{"ReductionNor"}};
    case BuiltinFn::kReductionXnor:
      return {.name = "reduction_xnor", .declaration = Method{"ReductionXnor"}};
    case BuiltinFn::kFromBool:
      return {.name = "from_bool", .declaration = StaticFactory{"FromBool"}};
    case BuiltinFn::kParent:
      return {.name = "parent", .declaration = Method{"Parent"}};
    case BuiltinFn::kSelfHandle:
      return {
          .name = "self_handle",
          .declaration = FreeFunction{"lyra::runtime::SelfHandle"}};
    case BuiltinFn::kHierarchicalPath:
      return {
          .name = "hierarchical_path",
          .declaration = Method{"HierarchicalPath"}};
  }
  throw InternalError("RuntimeEntryOf: unknown BuiltinFn");
}

}  // namespace lyra::support
