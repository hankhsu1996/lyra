#include "lyra/backend/cpp/render_call.hpp"

#include <cstdint>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/backend/cpp/formatting.hpp"
#include "lyra/backend/cpp/render_expr.hpp"
#include "lyra/backend/cpp/render_type.hpp"
#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::backend::cpp {

namespace {

// The bare C++ identifier this backend declares the builtin fn as, without the
// scope it is reached through: what that scope is depends on how the library
// declares the entry and on what the call site qualifies it with, and neither
// changes the identifier.
auto BuiltinFnCppName(support::BuiltinFn id) -> std::string_view {
  switch (id) {
    case support::BuiltinFn::kParent:
      return "Parent";
    case support::BuiltinFn::kSelfHandle:
      return "SelfHandle";
    case support::BuiltinFn::kCurrentRuntime:
      return "current_runtime";
    case support::BuiltinFn::kInitialize:
      return "Initialize";
    case support::BuiltinFn::kLoad:
      return "Get";
    case support::BuiltinFn::kStore:
      return "Set";
    case support::BuiltinFn::kSampledLoad:
      return "SampledGet";
    case support::BuiltinFn::kArmSampling:
      return "ArmSampling";
    case support::BuiltinFn::kOpenForWrite:
      return "Mutate";
    case support::BuiltinFn::kAttachDriver:
      return "AttachDriver";
    case support::BuiltinFn::kSubmitNba:
      return "SubmitNba";
    case support::BuiltinFn::kSubmitNbaAfter:
      return "SubmitNbaAfter";
    case support::BuiltinFn::kSubmitNbaAfterReal:
      return "SubmitNbaAfterReal";
    case support::BuiltinFn::kRunDetached:
      return "RunDetached";
    case support::BuiltinFn::kResumeInNbaRegion:
      return "ResumeInNbaRegion";
    case support::BuiltinFn::kSubmitPostponed:
      return "SubmitPostponed";
    case support::BuiltinFn::kSubmitObserved:
      return "SubmitObserved";
    case support::BuiltinFn::kSubmitDeferredObserved:
      return "SubmitDeferredObserved";
    case support::BuiltinFn::kSubmitDeferredFinal:
      return "SubmitDeferredFinal";
    case support::BuiltinFn::kFiles:
      return "Files";
    case support::BuiltinFn::kCancellationFor:
      return "CancellationFor";
    case support::BuiltinFn::kIsCancelled:
      return "IsCancelled";
    case support::BuiltinFn::kFormat:
      return "Format";
    case support::BuiltinFn::kFormatRuntime:
      return "FormatRuntime";
    case support::BuiltinFn::kWrite:
      return "Write";
    case support::BuiltinFn::kWriteln:
      return "Writeln";
    case support::BuiltinFn::kDiagnostic:
      return "Diagnostic";
    case support::BuiltinFn::kEmitInfo:
      return "EmitInfo";
    case support::BuiltinFn::kEmitWarning:
      return "EmitWarning";
    case support::BuiltinFn::kEmitError:
      return "EmitError";
    case support::BuiltinFn::kEmitFatal:
      return "EmitFatal";
    case support::BuiltinFn::kRecordCoverage:
      return "RecordCoverage";
    case support::BuiltinFn::kTimeFormat:
      return "TimeFormat";
    case support::BuiltinFn::kSetTimeFormat:
      return "SetTimeFormat";
    case support::BuiltinFn::kResetTimeFormat:
      return "ResetTimeFormat";
    case support::BuiltinFn::kScanString:
      return "ScanString";
    case support::BuiltinFn::kScanFile:
      return "ScanFile";
    case support::BuiltinFn::kPeekBuffered:
      return "PeekBuffered";
    case support::BuiltinFn::kAdvanceFd:
      return "AdvanceFd";
    case support::BuiltinFn::kTestPlusargs:
      return "TestPlusargs";
    case support::BuiltinFn::kValuePlusargs:
      return "ValuePlusargs";
    case support::BuiltinFn::kRunHostCommand:
    case support::BuiltinFn::kRunNullHostCommand:
      return "RunHostCommand";
    case support::BuiltinFn::kReadMem:
      return "ReadMem";
    case support::BuiltinFn::kReadMemWithin:
      return "ReadMemWithin";
    case support::BuiltinFn::kWriteMemWithin:
      return "WriteMemWithin";
    case support::BuiltinFn::kWriteMem:
      return "WriteMem";
    case support::BuiltinFn::kTrigger:
      return "Trigger";
    case support::BuiltinFn::kTriggered:
      return "Triggered";
    case support::BuiltinFn::kIsUnknown:
      return "IsUnknown";
    case support::BuiltinFn::kCountBits:
      return "CountBits";
    case support::BuiltinFn::kClog2:
      return "Clog2";
    case support::BuiltinFn::kLn:
      return "Ln";
    case support::BuiltinFn::kLog10:
      return "Log10";
    case support::BuiltinFn::kExp:
      return "Exp";
    case support::BuiltinFn::kSqrt:
      return "Sqrt";
    case support::BuiltinFn::kFloor:
      return "Floor";
    case support::BuiltinFn::kCeil:
      return "Ceil";
    case support::BuiltinFn::kSin:
      return "Sin";
    case support::BuiltinFn::kCos:
      return "Cos";
    case support::BuiltinFn::kTan:
      return "Tan";
    case support::BuiltinFn::kAsin:
      return "Asin";
    case support::BuiltinFn::kAcos:
      return "Acos";
    case support::BuiltinFn::kAtan:
      return "Atan";
    case support::BuiltinFn::kAtan2:
      return "Atan2";
    case support::BuiltinFn::kHypot:
      return "Hypot";
    case support::BuiltinFn::kSinh:
      return "Sinh";
    case support::BuiltinFn::kCosh:
      return "Cosh";
    case support::BuiltinFn::kTanh:
      return "Tanh";
    case support::BuiltinFn::kAsinh:
      return "Asinh";
    case support::BuiltinFn::kAcosh:
      return "Acosh";
    case support::BuiltinFn::kAtanh:
      return "Atanh";
    case support::BuiltinFn::kLen:
      return "Len";
    case support::BuiltinFn::kGetc:
      return "Getc";
    case support::BuiltinFn::kPutc:
      return "Putc";
    case support::BuiltinFn::kToupper:
      return "Toupper";
    case support::BuiltinFn::kTolower:
      return "Tolower";
    case support::BuiltinFn::kCompare:
      return "Compare";
    case support::BuiltinFn::kIcompare:
      return "Icompare";
    case support::BuiltinFn::kSubstr:
      return "Substr";
    case support::BuiltinFn::kAtoi:
      return "Atoi";
    case support::BuiltinFn::kAtohex:
      return "Atohex";
    case support::BuiltinFn::kAtooct:
      return "Atooct";
    case support::BuiltinFn::kAtobin:
      return "Atobin";
    case support::BuiltinFn::kAtoreal:
      return "Atoreal";
    case support::BuiltinFn::kItoa:
      return "Itoa";
    case support::BuiltinFn::kHextoa:
      return "Hextoa";
    case support::BuiltinFn::kOcttoa:
      return "Octtoa";
    case support::BuiltinFn::kBintoa:
      return "Bintoa";
    case support::BuiltinFn::kRealtoa:
      return "Realtoa";
    case support::BuiltinFn::kElement:
      return "Element";
    case support::BuiltinFn::kElementRef:
      return "ElementRef";
    case support::BuiltinFn::kSliceRef:
      return "SliceRef";
    case support::BuiltinFn::kEnumFirst:
    case support::BuiltinFn::kEnumLast:
    case support::BuiltinFn::kEnumNum:
    case support::BuiltinFn::kEnumName:
    case support::BuiltinFn::kEnumNext:
    case support::BuiltinFn::kEnumPrev:
      // LRM 6.19.5 enum methods are resolved at HIR-to-MIR -- to constants
      // (first / last / num) or synthesized callables (name / next / prev) --
      // so no enum method ever reaches the backend as a builtin.
      throw InternalError(
          "BuiltinFnCppName: enum methods are lowered at HIR-to-MIR and never "
          "reach the backend");
    case support::BuiltinFn::kSlice:
      return "Slice";
    case support::BuiltinFn::kRequire:
      return "Require";
    case support::BuiltinFn::kSize:
      return "Size";
    case support::BuiltinFn::kBitstreamWidth:
      return "BitstreamWidth";
    case support::BuiltinFn::kToOwned:
      return "ToOwned";
    // Both forms name the target language's one overload set; which member it
    // resolves to follows from the arguments the call already carries.
    case support::BuiltinFn::kDelete:
    case support::BuiltinFn::kDeleteIndex:
      return "Delete";
    case support::BuiltinFn::kReverse:
      return "Reverse";
    case support::BuiltinFn::kSort:
      return "Sort";
    case support::BuiltinFn::kRsort:
      return "Rsort";
    case support::BuiltinFn::kSum:
      return "Sum";
    case support::BuiltinFn::kProduct:
      return "Product";
    case support::BuiltinFn::kAnd:
      return "And";
    case support::BuiltinFn::kOr:
      return "Or";
    case support::BuiltinFn::kXor:
      return "Xor";
    case support::BuiltinFn::kFind:
      return "Find";
    case support::BuiltinFn::kFindIndex:
      return "FindIndex";
    case support::BuiltinFn::kFindFirst:
      return "FindFirst";
    case support::BuiltinFn::kFindFirstIndex:
      return "FindFirstIndex";
    case support::BuiltinFn::kFindLast:
      return "FindLast";
    case support::BuiltinFn::kFindLastIndex:
      return "FindLastIndex";
    case support::BuiltinFn::kMin:
      return "Min";
    case support::BuiltinFn::kMax:
      return "Max";
    case support::BuiltinFn::kUnique:
      return "Unique";
    case support::BuiltinFn::kUniqueIndex:
      return "UniqueIndex";
    case support::BuiltinFn::kMap:
      return "Map";
    case support::BuiltinFn::kInsert:
      return "Insert";
    case support::BuiltinFn::kPopFront:
      return "PopFront";
    case support::BuiltinFn::kPopBack:
      return "PopBack";
    case support::BuiltinFn::kPushFront:
      return "PushFront";
    case support::BuiltinFn::kPushBack:
      return "PushBack";
    case support::BuiltinFn::kExists:
      return "Exists";
    case support::BuiltinFn::kAssocFirst:
      return "First";
    case support::BuiltinFn::kAssocLast:
      return "Last";
    case support::BuiltinFn::kAssocNext:
      return "Next";
    case support::BuiltinFn::kAssocPrev:
      return "Prev";
    case support::BuiltinFn::kAssocMinIndex:
      return "MinIndex";
    case support::BuiltinFn::kAssocMaxIndex:
      return "MaxIndex";
    case support::BuiltinFn::kDelay:
      return "Delay";
    case support::BuiltinFn::kDelayReal:
      return "DelayReal";
    case support::BuiltinFn::kWaitAny:
      return "WaitAny";
    case support::BuiltinFn::kSimTime:
      return "SimTimeInUnit";
    case support::BuiltinFn::kSTime:
      return "STimeInUnit";
    case support::BuiltinFn::kRealTime:
      return "RealTimeInUnit";
    case support::BuiltinFn::kUrandom:
      return "Urandom";
    case support::BuiltinFn::kUrandomSeeded:
      return "UrandomSeeded";
    case support::BuiltinFn::kUrandomRange:
      return "UrandomRange";
    case support::BuiltinFn::kRandom:
      return "Random";
    case support::BuiltinFn::kDistUniform:
      return "DistUniform";
    case support::BuiltinFn::kDistNormal:
      return "DistNormal";
    case support::BuiltinFn::kDistExponential:
      return "DistExponential";
    case support::BuiltinFn::kDistPoisson:
      return "DistPoisson";
    case support::BuiltinFn::kDistChiSquare:
      return "DistChiSquare";
    case support::BuiltinFn::kDistT:
      return "DistT";
    case support::BuiltinFn::kDistErlang:
      return "DistErlang";
    case support::BuiltinFn::kFinish:
      return "Finish";
    case support::BuiltinFn::kStop:
      return "Stop";
    case support::BuiltinFn::kResolveRoot:
      return "ResolveRoot";
    case support::BuiltinFn::kResolveVisibleChild:
      return "ResolveVisibleChild";
    case support::BuiltinFn::kRegisterSignal:
      return "RegisterSignal";
    case support::BuiltinFn::kAddOwnedChild:
      return "AddOwnedChild";
    case support::BuiltinFn::kGetSignal:
      return "GetSignal";
    case support::BuiltinFn::kGetChild:
      return "GetChild";
    case support::BuiltinFn::kRegisterInitial:
      return "RegisterInitialProcess";
    case support::BuiltinFn::kRegisterFinal:
      return "RegisterFinalProcess";
    case support::BuiltinFn::kForkWaitAll:
      return "ForkWaitAll";
    case support::BuiltinFn::kForkWaitFirst:
      return "ForkWaitFirst";
    case support::BuiltinFn::kSpawnAll:
      return "SpawnAll";
    case support::BuiltinFn::kWaitFork:
      return "WaitFork";
    case support::BuiltinFn::kDisableFork:
      return "DisableFork";
    case support::BuiltinFn::kDisable:
      return "Disable";
    case support::BuiltinFn::kEnterTarget:
      return "EnterCancellationTarget";
    case support::BuiltinFn::kLeaveTarget:
      return "LeaveCancellationTarget";
    case support::BuiltinFn::kEffectNamesTarget:
      return "EffectNamesTarget";
    case support::BuiltinFn::kToInt64:
      return "ToInt64";
    case support::BuiltinFn::kRound:
      return "Round";
    case support::BuiltinFn::kTruncate:
      return "Truncate";
    case support::BuiltinFn::kToBits:
      return "ToBits";
    case support::BuiltinFn::kFromBits:
      return "FromBits";
    case support::BuiltinFn::kRealValue:
      return "Value";
    case support::BuiltinFn::kStringCStr:
      return "CStr";
    case support::BuiltinFn::kChandlePtr:
      return "Ptr";
    case support::BuiltinFn::kToSvLogic:
      return "ToSvLogic";
    case support::BuiltinFn::kReadCanonicalBitVec:
      return "ReadCanonicalBitVec";
    case support::BuiltinFn::kReadCanonicalLogicVec:
      return "ReadCanonicalLogicVec";
    case support::BuiltinFn::kWriteCanonicalBitVec:
      return "WriteCanonicalBitVec";
    case support::BuiltinFn::kWriteCanonicalLogicVec:
      return "WriteCanonicalLogicVec";
    case support::BuiltinFn::kDpiBufferData:
      return "Data";
    case support::BuiltinFn::kDpiOpenArrayHandle:
      return "Handle";
    case support::BuiltinFn::kDpiOpenArrayValue:
      return "ToValue";
    case support::BuiltinFn::kRunForeignTaskOnFiber:
      return "RunForeignTaskOnFiber";
    case support::BuiltinFn::kRunExportedTaskToCompletion:
      return "RunExportedTaskToCompletion";
    case support::BuiltinFn::kCurrentExportScope:
      return "CurrentExportScope";
    case support::BuiltinFn::kFindExportEntry:
      return "FindExportEntry";
    case support::BuiltinFn::kFromSvLogic:
      return "FromSvLogic";
    case support::BuiltinFn::kFromInt:
      return "FromInt";
    case support::BuiltinFn::kFromWords:
      return "FromWords";
    case support::BuiltinFn::kConvertFrom:
      return "ConvertFrom";
    case support::BuiltinFn::kFromPackedArray:
      return "FromPackedArray";
    case support::BuiltinFn::kFromByteArray:
      return "FromByteArray";
    case support::BuiltinFn::kFromString:
      return "FromString";
    case support::BuiltinFn::kFromArray:
      return "FromArray";
    case support::BuiltinFn::kConformBound:
      return "ConformBound";
    case support::BuiltinFn::kArrayConcatElement:
      return "ConcatElement";
    case support::BuiltinFn::kArrayConcatSpread:
      return "ConcatSpread";
    case support::BuiltinFn::kArrayConformSize:
      return "ConformSize";
    case support::BuiltinFn::kMakeDynamicArrayDefault:
      return "Default";
    case support::BuiltinFn::kMakeDynamicArrayNew:
      return "New";
    case support::BuiltinFn::kMakeDynamicArrayNewCopy:
      return "NewCopy";
    case support::BuiltinFn::kConcat:
      return "Concat";
    case support::BuiltinFn::kReplicate:
      return "Replicate";
    case support::BuiltinFn::kPow:
      return "Pow";
    case support::BuiltinFn::kShiftLeft:
      return "ShiftLeft";
    case support::BuiltinFn::kLogicalShiftRight:
      return "LogicalShiftRight";
    case support::BuiltinFn::kArithmeticShiftRight:
      return "ArithmeticShiftRight";
    case support::BuiltinFn::kBitwiseXnor:
      return "BitwiseXnor";
    case support::BuiltinFn::kLogicalImplication:
      return "LogicalImplication";
    case support::BuiltinFn::kLogicalEquivalence:
      return "LogicalEquivalence";
    case support::BuiltinFn::kWildcardEquals:
      return "WildcardEquals";
    case support::BuiltinFn::kCaseEqual:
      return "CaseEqual";
    case support::BuiltinFn::kCasezEquals:
      return "CasezEquals";
    case support::BuiltinFn::kCasexEquals:
      return "CasexEquals";
    case support::BuiltinFn::kMergeConditional:
      return "MergeConditional";
    case support::BuiltinFn::kReductionAnd:
      return "ReductionAnd";
    case support::BuiltinFn::kReductionOr:
      return "ReductionOr";
    case support::BuiltinFn::kReductionXor:
      return "ReductionXor";
    case support::BuiltinFn::kReductionNand:
      return "ReductionNand";
    case support::BuiltinFn::kReductionNor:
      return "ReductionNor";
    case support::BuiltinFn::kReductionXnor:
      return "ReductionXnor";
    case support::BuiltinFn::kFromBool:
      return "FromBool";
    case support::BuiltinFn::kFileOpen:
    case support::BuiltinFn::kFileOpenMode:
      return "Open";
    case support::BuiltinFn::kFileClose:
      return "Close";
    case support::BuiltinFn::kFileGetc:
      return "Getc";
    case support::BuiltinFn::kFileUngetc:
      return "Ungetc";
    case support::BuiltinFn::kFileGets:
      return "Gets";
    case support::BuiltinFn::kFileRead:
    case support::BuiltinFn::kFileReadMemory:
      return "Read";
    case support::BuiltinFn::kFileSeek:
      return "Seek";
    case support::BuiltinFn::kFileRewind:
      return "Rewind";
    case support::BuiltinFn::kFileTell:
      return "Tell";
    case support::BuiltinFn::kFileEof:
      return "Eof";
    case support::BuiltinFn::kFileError:
      return "Error";
    case support::BuiltinFn::kFileFlush:
    case support::BuiltinFn::kFileFlushAll:
      return "Flush";
    case support::BuiltinFn::kHierarchicalPath:
      return "HierarchicalPath";
  }
  throw InternalError("BuiltinFnCppName: unknown BuiltinFn");
}

// How the C++ runtime library declares a built-in entry. A free function is
// reached by its namespace and takes the object the entry acts on as an
// ordinary leading argument, because a free function binds no receiver; a
// member is reached through that object and binds it.
struct FreeFunction {
  std::string_view namespace_name;
};
struct InstanceMethod {};
using BuiltinCppForm = std::variant<FreeFunction, InstanceMethod>;

// Which of those two this backend declares the entry as. Most entries are
// declared on the type they act on, so that is what an entry this table does
// not place in a namespace is.
auto BuiltinFnCppForm(support::BuiltinFn id) -> BuiltinCppForm {
  switch (id) {
    case support::BuiltinFn::kScanString:
    case support::BuiltinFn::kScanFile:
    case support::BuiltinFn::kFormat:
    case support::BuiltinFn::kFormatRuntime:
    case support::BuiltinFn::kToSvLogic:
    case support::BuiltinFn::kReadCanonicalBitVec:
    case support::BuiltinFn::kReadCanonicalLogicVec:
    case support::BuiltinFn::kWriteCanonicalBitVec:
    case support::BuiltinFn::kWriteCanonicalLogicVec:
    case support::BuiltinFn::kFromSvLogic:
    case support::BuiltinFn::kRequire:
      return FreeFunction{"lyra::value"};
    case support::BuiltinFn::kCurrentRuntime:
    case support::BuiltinFn::kRegisterInitial:
    case support::BuiltinFn::kRegisterFinal:
    case support::BuiltinFn::kDelay:
    case support::BuiltinFn::kDelayReal:
    case support::BuiltinFn::kWaitAny:
    case support::BuiltinFn::kResumeInNbaRegion:
    case support::BuiltinFn::kSimTime:
    case support::BuiltinFn::kSTime:
    case support::BuiltinFn::kRealTime:
    case support::BuiltinFn::kUrandom:
    case support::BuiltinFn::kUrandomSeeded:
    case support::BuiltinFn::kUrandomRange:
    case support::BuiltinFn::kRandom:
    case support::BuiltinFn::kDistUniform:
    case support::BuiltinFn::kDistNormal:
    case support::BuiltinFn::kDistExponential:
    case support::BuiltinFn::kDistPoisson:
    case support::BuiltinFn::kDistChiSquare:
    case support::BuiltinFn::kDistT:
    case support::BuiltinFn::kDistErlang:
    case support::BuiltinFn::kFinish:
    case support::BuiltinFn::kStop:
    case support::BuiltinFn::kForkWaitAll:
    case support::BuiltinFn::kForkWaitFirst:
    case support::BuiltinFn::kSpawnAll:
    case support::BuiltinFn::kWaitFork:
    case support::BuiltinFn::kDisableFork:
    case support::BuiltinFn::kDisable:
    case support::BuiltinFn::kEnterTarget:
    case support::BuiltinFn::kLeaveTarget:
    case support::BuiltinFn::kEffectNamesTarget:
    case support::BuiltinFn::kTestPlusargs:
    case support::BuiltinFn::kValuePlusargs:
    case support::BuiltinFn::kRunHostCommand:
    case support::BuiltinFn::kRunNullHostCommand:
    case support::BuiltinFn::kReadMem:
    case support::BuiltinFn::kReadMemWithin:
    case support::BuiltinFn::kWriteMem:
    case support::BuiltinFn::kWriteMemWithin:
    case support::BuiltinFn::kRunForeignTaskOnFiber:
    case support::BuiltinFn::kRunExportedTaskToCompletion:
    case support::BuiltinFn::kCurrentExportScope:
    case support::BuiltinFn::kFindExportEntry:
    case support::BuiltinFn::kSelfHandle:
      return FreeFunction{"lyra::runtime"};
    default:
      return InstanceMethod{};
  }
}

// Where in the call text the object a call dispatches on goes. C++ offers two
// positions and no third: a member call reaches the object through the callee
// expression, and a free function takes it as an ordinary leading argument,
// because a free function binds nothing. This says where such an object would
// go, not that there is one -- a callee that dispatches on nothing has nothing
// to place and answers here vacuously.
enum class ReceiverPlacement : std::uint8_t {
  kIntoCalleeName,
  kIntoArgumentList
};

// What C++ names a callee, and where the object it dispatches on goes. Every
// callee form -- an instance method, a type-qualified static, a free function,
// an indirect closure, a type constructor -- answers with these two and nothing
// else, so one site composes the call text out of them.
struct CalleeSpelling {
  std::string name;
  ReceiverPlacement placement;
};

// The object a call dispatches on, ready to compose into a callee: the rendered
// expression, and the token C++ reaches a member through it with. A method that
// writes through it, a guard that hands it back, and an access that answers
// with part of it all name storage rather than a value, so those render it as a
// place.
struct RenderedReceiver {
  std::string expr;
  std::string_view member_access;
};

auto RenderReceiver(const ScopeView& view, const mir::Callee& callee)
    -> std::optional<RenderedReceiver> {
  const std::optional<mir::ExprId> receiver = mir::CalleeReceiver(callee);
  if (!receiver.has_value()) {
    return std::nullopt;
  }
  const mir::Expr& expr = view.Expr(*receiver);
  const bool names_storage =
      mir::IsMutatingCallee(callee) || mir::ReachesThroughReceiver(callee);
  return RenderedReceiver{
      .expr =
          names_storage ? RenderLhsExpr(view, expr) : RenderExpr(view, expr),
      .member_access =
          view.Unit().types.Get(expr.type).Is<mir::PointerType>() ? "->" : "."};
}

// A built-in runtime entry. Where MIR states a qualification the entry is
// reached on the type it names -- a conversion's destination, the type a
// library value is built as -- and otherwise the library's own declaration
// says how the entry is reached.
auto ResolveBuiltinSpelling(
    const ScopeView& view, support::BuiltinFn id,
    const std::optional<mir::ScopeQualifier>& qualification,
    const std::optional<RenderedReceiver>& receiver) -> CalleeSpelling {
  const std::string_view name = BuiltinFnCppName(id);
  if (qualification.has_value()) {
    const auto& tq = std::get<mir::TypeQualifier>(*qualification);
    return {
        .name =
            std::format("{}::{}", RenderTypeAsCpp(view.Unit(), tq.type), name),
        .placement = ReceiverPlacement::kIntoCalleeName};
  }
  return std::visit(
      Overloaded{
          [&](const FreeFunction& f) -> CalleeSpelling {
            return {
                .name = std::format("{}::{}", f.namespace_name, name),
                .placement = ReceiverPlacement::kIntoArgumentList};
          },
          // A member names nothing on its own, so a call reaching this spelling
          // without an object to reach it through has no C++ text at all.
          [&](const InstanceMethod&) -> CalleeSpelling {
            if (!receiver.has_value()) {
              throw InternalError(
                  "Direct builtin call: the instance form of a runtime entry "
                  "is reached through the object it acts on, and this call "
                  "names none -- please report this as a bug");
            }
            return {
                .name = std::string{name},
                .placement = ReceiverPlacement::kIntoCalleeName};
          }},
      BuiltinFnCppForm(id));
}

// What C++ names a `Direct` callee. Each alternative is a lookup in the table
// that resolves its own identity space, and how a receiver rides follows from
// what that lookup found.
auto ResolveDirectSpelling(
    const ScopeView& view, const mir::Direct& direct,
    const std::optional<RenderedReceiver>& receiver) -> CalleeSpelling {
  return std::visit(
      Overloaded{
          // The owner prefix is a fixed function of the target's owner: it is
          // redundant for a non-virtual method and, for a virtual one a direct
          // call reaches (LRM 8.15 super), is what makes C++ bypass the vtable.
          // No qualification is allowed today -- cross-class explicit
          // qualification is gated on SV class support.
          [&](const mir::CallableTarget& t) -> CalleeSpelling {
            if (direct.qualification.has_value()) {
              throw InternalError(
                  "Direct callable call: qualification is not yet implemented");
            }
            const auto& cls = view.Unit().GetClass(t.owner);
            return {
                .name = std::format(
                    "{}::{}", ToCppName(cls.name),
                    cls.callables.Get(t.slot).name),
                .placement = ReceiverPlacement::kIntoCalleeName};
          },
          [&](const support::BuiltinFn& id) -> CalleeSpelling {
            return ResolveBuiltinSpelling(
                view, id, direct.qualification, receiver);
          },
          // The runtime library provides an imported class's methods (LRM 9.7)
          // as symbols named by the method identity.
          [](const mir::ImportedRuntimeCallTarget& t) -> CalleeSpelling {
            return {
                .name = std::format(
                    "lyra::runtime::{}",
                    support::ImportedRuntimeMethodSymbol(t.method)),
                .placement = ReceiverPlacement::kIntoArgumentList};
          },
          // Another compilation unit's C++ peer is a namespace, so a callable
          // of it (LRM 26.3) is named through that namespace.
          [](const mir::ExternalUnitCallableTarget& t) -> CalleeSpelling {
            return {
                .name = std::format(
                    "{}::{}", ToCppName(t.unit_name), t.callable_name),
                .placement = ReceiverPlacement::kIntoCalleeName};
          },
          // A method on one of that namespace's classes is named through the
          // class as well. Target-language name lookup resolves it once the
          // declaring unit's header is included, and the class qualification
          // makes C++ bypass the vtable, exactly as a direct call to a virtual
          // method demands (LRM 8.15 super).
          [](const mir::ExternalUnitClassMethodTarget& t) -> CalleeSpelling {
            return {
                .name = std::format(
                    "{}::{}::{}", ToCppName(t.unit_name),
                    ToCppName(t.class_name), t.method_name),
                .placement = ReceiverPlacement::kIntoCalleeName};
          },
          // A DPI-C symbol is program-global, so it is spelled unqualified
          // (LRM 35.4); the prototype it resolves against is declared once in
          // this artifact.
          [](const mir::ForeignSymbolTarget& t) -> CalleeSpelling {
            return {
                .name = t.linkage_name,
                .placement = ReceiverPlacement::kIntoCalleeName};
          }},
      direct.target);
}

auto ResolveCalleeSpelling(
    const ScopeView& view, const mir::CallExpr& call,
    const std::optional<RenderedReceiver>& receiver, mir::TypeId result_type)
    -> CalleeSpelling {
  return std::visit(
      Overloaded{
          [&](const mir::Direct& d) -> CalleeSpelling {
            return ResolveDirectSpelling(view, d, receiver);
          },
          [&](const mir::Indirect& i) -> CalleeSpelling {
            return {
                .name =
                    std::format("({})", RenderExpr(view, view.Expr(i.code))),
                .placement = ReceiverPlacement::kIntoCalleeName};
          },
          [&](const mir::Virtual& v) -> CalleeSpelling {
            return {
                .name = std::visit(
                    Overloaded{
                        [&](const mir::LocalVirtualSlot& l) -> std::string {
                          return view.Unit()
                              .GetClass(l.owner_class)
                              .callables.Get(l.slot)
                              .name;
                        },
                        [](const mir::ExternalVirtualSlot& e) -> std::string {
                          return e.method_name;
                        }},
                    v.slot),
                .placement = ReceiverPlacement::kIntoCalleeName};
          },
          // A type has one way to come into existence, and what names it is the
          // type's own answer -- read through type mapping, the way every other
          // target-language spelling of a type is.
          [&](const mir::Construct&) -> CalleeSpelling {
            return {
                .name = RenderTypeConstructionAsCpp(view.Unit(), result_type),
                .placement = ReceiverPlacement::kIntoCalleeName};
          }},
      call.callee);
}

}  // namespace

auto RenderCallExpr(
    const ScopeView& view, const mir::CallExpr& call, mir::TypeId result_type)
    -> std::string {
  const std::optional<RenderedReceiver> receiver =
      RenderReceiver(view, call.callee);
  const CalleeSpelling callee =
      ResolveCalleeSpelling(view, call, receiver, result_type);

  // The object the call dispatches on goes where the spelling puts it, and a
  // call that dispatches on none puts nothing anywhere. Everything after this
  // is punctuation.
  std::string callee_text = callee.name;
  std::vector<std::string> args;
  args.reserve(call.arguments.size() + 1);
  if (receiver.has_value()) {
    switch (callee.placement) {
      case ReceiverPlacement::kIntoCalleeName:
        callee_text = std::format(
            "({}){}{}", receiver->expr, receiver->member_access, callee.name);
        break;
      case ReceiverPlacement::kIntoArgumentList:
        args.push_back(receiver->expr);
        break;
    }
  }
  for (const mir::ExprId id : call.arguments) {
    args.push_back(RenderExpr(view, view.Expr(id)));
  }
  return CallOf(callee_text, args);
}

}  // namespace lyra::backend::cpp
