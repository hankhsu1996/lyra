#include "lyra/support/builtin_fn.hpp"

#include <cstddef>
#include <optional>

#include "lyra/base/internal_error.hpp"

namespace lyra::support {

auto IsMutatingBuiltinFn(BuiltinFn id) -> bool {
  switch (id) {
    case BuiltinFn::kPutc:
    case BuiltinFn::kItoa:
    case BuiltinFn::kHextoa:
    case BuiltinFn::kOcttoa:
    case BuiltinFn::kBintoa:
    case BuiltinFn::kRealtoa:
    case BuiltinFn::kDelete:
    case BuiltinFn::kDeleteIndex:
    case BuiltinFn::kReverse:
    case BuiltinFn::kSort:
    case BuiltinFn::kRsort:
    case BuiltinFn::kInsert:
    case BuiltinFn::kPopFront:
    case BuiltinFn::kPopBack:
    case BuiltinFn::kPushFront:
    case BuiltinFn::kPushBack:
      return true;
    default:
      return false;
  }
}

auto IsPassThroughBuiltinFn(BuiltinFn id) -> bool {
  return id == BuiltinFn::kRequire;
}

auto ArrayMethodTakesClosure(BuiltinFn id) -> bool {
  switch (id) {
    case BuiltinFn::kSort:
    case BuiltinFn::kRsort:
    case BuiltinFn::kSum:
    case BuiltinFn::kProduct:
    case BuiltinFn::kAnd:
    case BuiltinFn::kOr:
    case BuiltinFn::kXor:
    case BuiltinFn::kFind:
    case BuiltinFn::kFindIndex:
    case BuiltinFn::kFindFirst:
    case BuiltinFn::kFindFirstIndex:
    case BuiltinFn::kFindLast:
    case BuiltinFn::kFindLastIndex:
    case BuiltinFn::kMin:
    case BuiltinFn::kMax:
    case BuiltinFn::kUnique:
    case BuiltinFn::kUniqueIndex:
    case BuiltinFn::kMap:
      return true;
    default:
      return false;
  }
}

auto BuiltinFnTakesResultPrototype(BuiltinFn id) -> bool {
  switch (id) {
    case BuiltinFn::kAssocMinIndex:
    case BuiltinFn::kAssocMaxIndex:
    case BuiltinFn::kSum:
    case BuiltinFn::kProduct:
    case BuiltinFn::kAnd:
    case BuiltinFn::kOr:
    case BuiltinFn::kXor:
    case BuiltinFn::kFind:
    case BuiltinFn::kFindIndex:
    case BuiltinFn::kFindFirst:
    case BuiltinFn::kFindFirstIndex:
    case BuiltinFn::kFindLast:
    case BuiltinFn::kFindLastIndex:
    case BuiltinFn::kMin:
    case BuiltinFn::kMax:
    case BuiltinFn::kUnique:
    case BuiltinFn::kUniqueIndex:
    case BuiltinFn::kMap:
      return true;
    default:
      return false;
  }
}

auto IsAssociativeTraversalFn(BuiltinFn id) -> bool {
  switch (id) {
    case BuiltinFn::kAssocFirst:
    case BuiltinFn::kAssocLast:
    case BuiltinFn::kAssocNext:
    case BuiltinFn::kAssocPrev:
      return true;
    default:
      return false;
  }
}

auto ContainerIndexOperand(BuiltinFn id) -> std::optional<std::size_t> {
  switch (id) {
    // The receiver leads, and the index it selects by follows.
    case BuiltinFn::kElement:
    case BuiltinFn::kExists:
    // LRM 7.9.3 / 7.10.2.3: the index naming the one entry that goes.
    case BuiltinFn::kDeleteIndex:
    // LRM 7.9.4 -- 7.9.7: the index the traversal starts from, which it also
    // answers with when there is no neighbour to move to.
    case BuiltinFn::kAssocFirst:
    case BuiltinFn::kAssocLast:
    case BuiltinFn::kAssocNext:
    case BuiltinFn::kAssocPrev:
      return 1;
    default:
      return std::nullopt;
  }
}

auto SpreadPartOperand(BuiltinFn id) -> std::optional<std::size_t> {
  switch (id) {
    // LRM 10.10: the accumulating array leads, and the spread part whose
    // elements it appends follows. The part is a container of any domain, which
    // the appending entry cannot name, so it crosses erased -- boxed into a
    // runtime value in its own domain and read back element by element.
    case BuiltinFn::kArrayConcatSpread:
      return 1;
    default:
      return std::nullopt;
  }
}

auto BuiltinFnName(BuiltinFn id) -> std::string_view {
  switch (id) {
    case BuiltinFn::kElement:
      return "element";
    case BuiltinFn::kSlice:
      return "slice";
    case BuiltinFn::kRequire:
      return "require";
    case BuiltinFn::kSize:
      return "size";
    case BuiltinFn::kLen:
      return "len";
    case BuiltinFn::kBitstreamWidth:
      return "bitstream_width";
    case BuiltinFn::kToOwned:
      return "to_owned";
    case BuiltinFn::kDelete:
      return "delete";
    case BuiltinFn::kDeleteIndex:
      return "delete_index";
    case BuiltinFn::kReverse:
      return "reverse";
    case BuiltinFn::kSort:
      return "sort";
    case BuiltinFn::kRsort:
      return "rsort";
    case BuiltinFn::kSum:
      return "sum";
    case BuiltinFn::kProduct:
      return "product";
    case BuiltinFn::kAnd:
      return "and";
    case BuiltinFn::kOr:
      return "or";
    case BuiltinFn::kXor:
      return "xor";
    case BuiltinFn::kFind:
      return "find";
    case BuiltinFn::kFindIndex:
      return "find_index";
    case BuiltinFn::kFindFirst:
      return "find_first";
    case BuiltinFn::kFindFirstIndex:
      return "find_first_index";
    case BuiltinFn::kFindLast:
      return "find_last";
    case BuiltinFn::kFindLastIndex:
      return "find_last_index";
    case BuiltinFn::kMin:
      return "min";
    case BuiltinFn::kMax:
      return "max";
    case BuiltinFn::kUnique:
      return "unique";
    case BuiltinFn::kUniqueIndex:
      return "unique_index";
    case BuiltinFn::kMap:
      return "map";
    case BuiltinFn::kInsert:
      return "insert";
    case BuiltinFn::kPopFront:
      return "pop_front";
    case BuiltinFn::kPopBack:
      return "pop_back";
    case BuiltinFn::kPushFront:
      return "push_front";
    case BuiltinFn::kPushBack:
      return "push_back";
    case BuiltinFn::kExists:
      return "exists";
    case BuiltinFn::kAssocFirst:
      return "assoc_first";
    case BuiltinFn::kAssocLast:
      return "assoc_last";
    case BuiltinFn::kAssocNext:
      return "assoc_next";
    case BuiltinFn::kAssocPrev:
      return "assoc_prev";
    case BuiltinFn::kAssocMinIndex:
      return "assoc_min_index";
    case BuiltinFn::kAssocMaxIndex:
      return "assoc_max_index";
    case BuiltinFn::kGetc:
      return "getc";
    case BuiltinFn::kPutc:
      return "putc";
    case BuiltinFn::kToupper:
      return "toupper";
    case BuiltinFn::kTolower:
      return "tolower";
    case BuiltinFn::kCompare:
      return "compare";
    case BuiltinFn::kIcompare:
      return "icompare";
    case BuiltinFn::kSubstr:
      return "substr";
    case BuiltinFn::kAtoi:
      return "atoi";
    case BuiltinFn::kAtohex:
      return "atohex";
    case BuiltinFn::kAtooct:
      return "atooct";
    case BuiltinFn::kAtobin:
      return "atobin";
    case BuiltinFn::kAtoreal:
      return "atoreal";
    case BuiltinFn::kItoa:
      return "itoa";
    case BuiltinFn::kHextoa:
      return "hextoa";
    case BuiltinFn::kOcttoa:
      return "octtoa";
    case BuiltinFn::kBintoa:
      return "bintoa";
    case BuiltinFn::kRealtoa:
      return "realtoa";
    case BuiltinFn::kTrigger:
      return "trigger";
    case BuiltinFn::kAwait:
      return "await";
    case BuiltinFn::kTriggered:
      return "triggered";
    case BuiltinFn::kEnumFirst:
      return "enum_first";
    case BuiltinFn::kEnumLast:
      return "enum_last";
    case BuiltinFn::kEnumNum:
      return "enum_num";
    case BuiltinFn::kEnumName:
      return "enum_name";
    case BuiltinFn::kEnumNext:
      return "enum_next";
    case BuiltinFn::kEnumPrev:
      return "enum_prev";
    case BuiltinFn::kIsUnknown:
      return "is_unknown";
    case BuiltinFn::kCountBits:
      return "count_bits";
    case BuiltinFn::kClog2:
      return "clog2";
    case BuiltinFn::kLn:
      return "ln";
    case BuiltinFn::kLog10:
      return "log10";
    case BuiltinFn::kExp:
      return "exp";
    case BuiltinFn::kSqrt:
      return "sqrt";
    case BuiltinFn::kFloor:
      return "floor";
    case BuiltinFn::kCeil:
      return "ceil";
    case BuiltinFn::kSin:
      return "sin";
    case BuiltinFn::kCos:
      return "cos";
    case BuiltinFn::kTan:
      return "tan";
    case BuiltinFn::kAsin:
      return "asin";
    case BuiltinFn::kAcos:
      return "acos";
    case BuiltinFn::kAtan:
      return "atan";
    case BuiltinFn::kAtan2:
      return "atan2";
    case BuiltinFn::kHypot:
      return "hypot";
    case BuiltinFn::kSinh:
      return "sinh";
    case BuiltinFn::kCosh:
      return "cosh";
    case BuiltinFn::kTanh:
      return "tanh";
    case BuiltinFn::kAsinh:
      return "asinh";
    case BuiltinFn::kAcosh:
      return "acosh";
    case BuiltinFn::kAtanh:
      return "atanh";
    case BuiltinFn::kInitialize:
      return "initialize";
    case BuiltinFn::kAttachDriver:
      return "attach_driver";
    case BuiltinFn::kCurrentRuntime:
      return "current_runtime";
    case BuiltinFn::kSubmitNba:
      return "submit_nba";
    case BuiltinFn::kSubmitPostponed:
      return "submit_postponed";
    case BuiltinFn::kSubmitObserved:
      return "submit_observed";
    case BuiltinFn::kFiles:
      return "files";
    case BuiltinFn::kCancellationFor:
      return "cancellation_for";
    case BuiltinFn::kIsCancelled:
      return "is_cancelled";
    case BuiltinFn::kFormat:
      return "format";
    case BuiltinFn::kFormatRuntime:
      return "format_runtime";
    case BuiltinFn::kWrite:
      return "write";
    case BuiltinFn::kWriteln:
      return "writeln";
    case BuiltinFn::kScanString:
      return "scan_string";
    case BuiltinFn::kScanFile:
      return "scan_file";
    case BuiltinFn::kPeekBuffered:
      return "peek_buffered";
    case BuiltinFn::kAdvanceFd:
      return "advance_fd";
    case BuiltinFn::kTestPlusargs:
      return "test_plusargs";
    case BuiltinFn::kValuePlusargs:
      return "value_plusargs";
    case BuiltinFn::kRunHostCommand:
      return "run_host_command";
    case BuiltinFn::kRunNullHostCommand:
      return "run_null_host_command";
    case BuiltinFn::kReadMemWithin:
      return "read_mem_within";
    case BuiltinFn::kWriteMemWithin:
      return "write_mem_within";
    case BuiltinFn::kReadMem:
      return "read_mem";
    case BuiltinFn::kWriteMem:
      return "write_mem";
    case BuiltinFn::kDelay:
      return "delay";
    case BuiltinFn::kWaitAny:
      return "wait_any";
    case BuiltinFn::kSimTime:
      return "sim_time";
    case BuiltinFn::kSTime:
      return "stime";
    case BuiltinFn::kRealTime:
      return "realtime";
    case BuiltinFn::kUrandom:
      return "urandom";
    case BuiltinFn::kUrandomSeeded:
      return "urandom_seeded";
    case BuiltinFn::kUrandomRange:
      return "urandom_range";
    case BuiltinFn::kRandom:
      return "random";
    case BuiltinFn::kDistUniform:
      return "dist_uniform";
    case BuiltinFn::kDistNormal:
      return "dist_normal";
    case BuiltinFn::kDistExponential:
      return "dist_exponential";
    case BuiltinFn::kDistPoisson:
      return "dist_poisson";
    case BuiltinFn::kDistChiSquare:
      return "dist_chi_square";
    case BuiltinFn::kDistT:
      return "dist_t";
    case BuiltinFn::kDistErlang:
      return "dist_erlang";
    case BuiltinFn::kFinish:
      return "finish";
    case BuiltinFn::kFatalFinish:
      return "fatal_finish";
    case BuiltinFn::kResolveRoot:
      return "resolve_root";
    case BuiltinFn::kResolveVisibleChild:
      return "resolve_visible_child";
    case BuiltinFn::kRegisterSignal:
      return "register_signal";
    case BuiltinFn::kAddOwnedChild:
      return "add_owned_child";
    case BuiltinFn::kGetSignal:
      return "get_signal";
    case BuiltinFn::kGetChild:
      return "get_child";
    case BuiltinFn::kForkWaitAll:
      return "fork_wait_all";
    case BuiltinFn::kForkWaitFirst:
      return "fork_wait_first";
    case BuiltinFn::kSpawnAll:
      return "spawn_all";
    case BuiltinFn::kWaitFork:
      return "wait_fork";
    case BuiltinFn::kDisableFork:
      return "disable_fork";
    case BuiltinFn::kDisable:
      return "disable";
    case BuiltinFn::kEnterTarget:
      return "enter_target";
    case BuiltinFn::kLeaveTarget:
      return "leave_target";
    case BuiltinFn::kEffectNamesTarget:
      return "effect_names_target";
    case BuiltinFn::kRegisterInitial:
      return "register_initial";
    case BuiltinFn::kRegisterFinal:
      return "register_final";
    case BuiltinFn::kParent:
      return "parent";
    case BuiltinFn::kFileOpen:
      return "file_open";
    case BuiltinFn::kFileOpenMode:
      return "file_open_mode";
    case BuiltinFn::kFileClose:
      return "file_close";
    case BuiltinFn::kFileGetc:
      return "file_getc";
    case BuiltinFn::kFileUngetc:
      return "file_ungetc";
    case BuiltinFn::kFileGets:
      return "file_gets";
    case BuiltinFn::kFileRead:
      return "file_read";
    case BuiltinFn::kFileReadMemory:
      return "file_read_memory";
    case BuiltinFn::kFileSeek:
      return "file_seek";
    case BuiltinFn::kFileRewind:
      return "file_rewind";
    case BuiltinFn::kFileTell:
      return "file_tell";
    case BuiltinFn::kFileEof:
      return "file_eof";
    case BuiltinFn::kFileError:
      return "file_error";
    case BuiltinFn::kFileFlush:
      return "file_flush";
    case BuiltinFn::kFileFlushAll:
      return "file_flush_all";
    case BuiltinFn::kDiagnostic:
      return "diagnostic";
    case BuiltinFn::kEmitInfo:
      return "emit_info";
    case BuiltinFn::kEmitWarning:
      return "emit_warning";
    case BuiltinFn::kEmitError:
      return "emit_error";
    case BuiltinFn::kEmitFatal:
      return "emit_fatal";
    case BuiltinFn::kRecordCoverage:
      return "record_coverage";
    case BuiltinFn::kTimeFormat:
      return "time_format";
    case BuiltinFn::kSetTimeFormat:
      return "set_time_format";
    case BuiltinFn::kResetTimeFormat:
      return "reset_time_format";
    case BuiltinFn::kToInt64:
      return "to_int64";
    case BuiltinFn::kRound:
      return "round";
    case BuiltinFn::kTruncate:
      return "truncate";
    case BuiltinFn::kToBits:
      return "to_bits";
    case BuiltinFn::kFromBits:
      return "from_bits";
    case BuiltinFn::kRealValue:
      return "real_value";
    case BuiltinFn::kStringCStr:
      return "string_cstr";
    case BuiltinFn::kChandlePtr:
      return "chandle_ptr";
    case BuiltinFn::kToSvLogic:
      return "to_sv_logic";
    case BuiltinFn::kReadCanonicalBitVec:
      return "read_canonical_bit_vec";
    case BuiltinFn::kReadCanonicalLogicVec:
      return "read_canonical_logic_vec";
    case BuiltinFn::kWriteCanonicalBitVec:
      return "write_canonical_bit_vec";
    case BuiltinFn::kWriteCanonicalLogicVec:
      return "write_canonical_logic_vec";
    case BuiltinFn::kDpiBufferData:
      return "dpi_buffer_data";
    case BuiltinFn::kDpiOpenArrayHandle:
      return "dpi_open_array_handle";
    case BuiltinFn::kDpiOpenArrayValue:
      return "dpi_open_array_value";
    case BuiltinFn::kRunForeignTaskOnFiber:
      return "run_foreign_task_on_fiber";
    case BuiltinFn::kRunExportedTaskToCompletion:
      return "run_exported_task_to_completion";
    case BuiltinFn::kCurrentExportScope:
      return "current_export_scope";
    case BuiltinFn::kFindExportEntry:
      return "find_export_entry";
    case BuiltinFn::kFromSvLogic:
      return "from_sv_logic";
    case BuiltinFn::kFromInt:
      return "from_int";
    case BuiltinFn::kFromWords:
      return "from_words";
    case BuiltinFn::kConvertFrom:
      return "convert_from";
    case BuiltinFn::kFromPackedArray:
      return "from_packed_array";
    case BuiltinFn::kFromByteArray:
      return "from_byte_array";
    case BuiltinFn::kFromString:
      return "from_string";
    case BuiltinFn::kConformBound:
      return "conform_bound";
    case BuiltinFn::kArrayConcatElement:
      return "concat_element";
    case BuiltinFn::kArrayConcatSpread:
      return "concat_spread";
    case BuiltinFn::kArrayConformSize:
      return "conform_size";
    case BuiltinFn::kMakeDynamicArrayDefault:
      return "make_dynamic_array_default";
    case BuiltinFn::kMakeDynamicArrayNew:
      return "make_dynamic_array_new";
    case BuiltinFn::kMakeDynamicArrayNewCopy:
      return "make_dynamic_array_new_copy";
    case BuiltinFn::kConcat:
      return "concat";
    case BuiltinFn::kReplicate:
      return "replicate";
    case BuiltinFn::kPow:
      return "pow";
    case BuiltinFn::kShiftLeft:
      return "shift_left";
    case BuiltinFn::kLogicalShiftRight:
      return "logical_shift_right";
    case BuiltinFn::kArithmeticShiftRight:
      return "arithmetic_shift_right";
    case BuiltinFn::kBitwiseXnor:
      return "bitwise_xnor";
    case BuiltinFn::kLogicalImplication:
      return "logical_implication";
    case BuiltinFn::kLogicalEquivalence:
      return "logical_equivalence";
    case BuiltinFn::kWildcardEquals:
      return "wildcard_equals";
    case BuiltinFn::kCaseEqual:
      return "case_equal";
    case BuiltinFn::kCasezEquals:
      return "casez_equals";
    case BuiltinFn::kCasexEquals:
      return "casex_equals";
    case BuiltinFn::kMergeConditional:
      return "merge_conditional";
    case BuiltinFn::kReductionAnd:
      return "reduction_and";
    case BuiltinFn::kReductionOr:
      return "reduction_or";
    case BuiltinFn::kReductionXor:
      return "reduction_xor";
    case BuiltinFn::kReductionNand:
      return "reduction_nand";
    case BuiltinFn::kReductionNor:
      return "reduction_nor";
    case BuiltinFn::kReductionXnor:
      return "reduction_xnor";
    case BuiltinFn::kFromBool:
      return "from_bool";
    case BuiltinFn::kHierarchicalPath:
      return "hierarchical_path";
  }
  throw InternalError("BuiltinFnName: unknown BuiltinFn");
}

}  // namespace lyra::support
