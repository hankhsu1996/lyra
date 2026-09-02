#include "lyra/backend/cpp/render_call.hpp"

#include <cstddef>
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

// The bare C++ identifier this backend declares the builtin fn as. One of
// three orthogonal facts a render path composes: name, namespace, and the
// receiver expression / type qualifier supplied by the callee variant.
auto BuiltinFnCppName(support::BuiltinFn id) -> std::string_view {
  switch (id) {
    case support::BuiltinFn::kParent:
      return "Parent";
    case support::BuiltinFn::kCurrentRuntime:
      return "current_runtime";
    case support::BuiltinFn::kInitialize:
      return "Initialize";
    case support::BuiltinFn::kAttachDriver:
      return "AttachDriver";
    case support::BuiltinFn::kSubmitNba:
      return "SubmitNba";
    case support::BuiltinFn::kSubmitPostponed:
      return "SubmitPostponed";
    case support::BuiltinFn::kSubmitObserved:
      return "SubmitObserved";
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
    case support::BuiltinFn::kAwait:
      return "Await";
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
    case support::BuiltinFn::kFatalFinish:
      return "FatalFinish";
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
    case support::BuiltinFn::kConformBound:
      return "ConformBound";
    case support::BuiltinFn::kMakeQueueConcat:
      return "MakeQueueConcat";
    case support::BuiltinFn::kMakeDynamicArrayDefault:
      return "Default";
    case support::BuiltinFn::kMakeDynamicArrayNew:
      return "New";
    case support::BuiltinFn::kMakeDynamicArrayNewCopy:
      return "NewCopy";
    case support::BuiltinFn::kSpread:
      return "QSpread";
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

// The C++ namespace the runtime helper is declared in (e.g. `lyra::value`
// for `lyra::value::Scan`). Used only for the free-function render form --
// a built-in whose id has no receiver and is not qualified by the call
// site needs the namespace to spell its symbol. A built-in id with a
// receiver consumes its receiver expression instead; a static-qualified
// call (per `IsStaticBuiltinFn`) consumes the call site's qualification.
// An empty return means "no namespace declared"; ids whose render does
// not need one (instance and static forms) leave this unset.
auto BuiltinFnCppNamespace(support::BuiltinFn id) -> std::string_view {
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
    case support::BuiltinFn::kMakeQueueConcat:
    case support::BuiltinFn::kSpread:
      return "lyra::value";
    case support::BuiltinFn::kCurrentRuntime:
    case support::BuiltinFn::kRegisterInitial:
    case support::BuiltinFn::kRegisterFinal:
    case support::BuiltinFn::kDelay:
    case support::BuiltinFn::kWaitAny:
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
    case support::BuiltinFn::kFatalFinish:
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
      return "lyra::runtime";
    default:
      return "";
  }
}

// Each callee shape resolves to a `(callee_expr, leading_arg_count)` pair:
// the C++ text written before the `(args)` list, and how many leading MIR
// arguments are absorbed into the callee text (the receiver of an instance
// method) and skipped when rendering the user-visible argument list. The
// outer `RenderCallExpr` then does one concatenation -- `{callee}({args})` --
// so every form (instance, type-qualified static, free function, indirect
// closure, type constructor, heap-construct `make_unique<T>`) renders
// through the same final formatter.
struct CalleeRender {
  std::string expr;
  std::size_t leading_arg_count;
};

// Renders a `Direct` callee naming an owner-qualified callable this class
// owns -- an instance method (LRM 8.6) or a static method (LRM 8.10), one
// arena. An instance callable renders as `(receiver)->Owner::name`, an
// owner-qualified C++ member call: the owner prefix is a fixed function of
// the target's owner, redundant for a non-virtual method and, for a virtual
// method reached through Direct (LRM 8.15 super), forcing C++ to bypass the
// vtable. The receiver is the first argument, absorbed into the callee
// text, so one leading argument is consumed. A static callable renders as
// the free type-qualified form `Owner::name`, with no receiver argument
// absorbed. No qualification is allowed today -- cross-class explicit
// qualification is gated on SV class support.
auto RenderDirectCallableCall(
    const ScopeView& view, const mir::CallExpr& call,
    const mir::CallableTarget& target,
    const std::optional<mir::ScopeQualifier>& qualification) -> CalleeRender {
  if (qualification.has_value()) {
    throw InternalError(
        "Direct callable call: qualification is not yet implemented");
  }
  const auto& cls = view.Unit().GetClass(target.owner);
  const auto& callable = cls.callables.Get(target.slot);
  // Instance vs static (LRM 8.10) reads off the target's signature: an
  // instance call binds the receiver as `(recv)->Owner::name(rest)`, a
  // static call is the free type-qualified form `Owner::name(args)` with no
  // receiver absorbed.
  const mir::CallableCode& code = callable.code;
  if (!code.HasReceiver(cls.self_pointer_type)) {
    return {
        .expr = std::format("{}::{}", ToCppName(cls.name), callable.name),
        .leading_arg_count = 0};
  }
  if (call.arguments.empty()) {
    throw InternalError("Direct method call expects a receiver argument");
  }
  const mir::Expr& receiver = view.Expr(call.arguments[0]);
  return {
      .expr = std::format(
          "({})->{}::{}", RenderExpr(view, receiver), ToCppName(cls.name),
          callable.name),
      .leading_arg_count = 1};
}

// Renders a `Direct` callee whose target is a `BuiltinFn`. Picks one of
// three C++ forms from the id's metadata: qualification present -> type-
// qualified static `Qual::Name(args)`; no qualification + declared
// namespace -> free function `ns::Name(args)`; otherwise -> instance form
// `(args[0]).Name(rest)`. The form is a fixed function of the id and the
// presence of a qualification on the call.
auto RenderDirectBuiltinCall(
    const ScopeView& view, const mir::CallExpr& call, support::BuiltinFn id,
    const std::optional<mir::ScopeQualifier>& qualification) -> CalleeRender {
  if (qualification.has_value()) {
    const auto& tq = std::get<mir::TypeQualifier>(*qualification);
    return {
        .expr = std::format(
            "{}::{}", RenderTypeAsCpp(view.Unit(), tq.type),
            BuiltinFnCppName(id)),
        .leading_arg_count = 0};
  }
  const std::string_view ns = BuiltinFnCppNamespace(id);
  if (!ns.empty()) {
    return {
        .expr = std::format("{}::{}", ns, BuiltinFnCppName(id)),
        .leading_arg_count = 0};
  }
  if (call.arguments.empty()) {
    throw InternalError(
        "Direct builtin call: instance form expects a receiver argument");
  }
  const mir::Expr& receiver = view.Expr(call.arguments[0]);
  const std::string_view sep =
      view.Unit().types.Get(receiver.type).Is<mir::PointerType>() ? "->" : ".";
  // A mutating built-in writes through its receiver, so the receiver names a
  // place -- the same distinction an assignment target draws, and the reason a
  // receiver reaching through a capability wrapper reaches its write protocol
  // rather than its read.
  const std::string receiver_text = mir::IsMutatingCallee(call.callee)
                                        ? RenderLhsExpr(view, receiver)
                                        : RenderExpr(view, receiver);
  return {
      .expr = std::format("({}){}{}", receiver_text, sep, BuiltinFnCppName(id)),
      .leading_arg_count = 1};
}

// Renders a `Direct` callee whose target is a receiver-less callable of another
// compilation unit -- a package function or task (LRM 26.3). The unit's C++
// peer is a namespace, so the callee is the free qualified form
// `unit_name::callable_name`, with no receiver and no leading argument
// absorbed.
auto RenderDirectExternalUnitCall(const mir::ExternalUnitCallableTarget& target)
    -> CalleeRender {
  return {
      .expr = std::format(
          "{}::{}", ToCppName(target.unit_name), target.callable_name),
      .leading_arg_count = 0};
}

// Renders a `Direct` callee whose target is an instance method of another
// compilation unit (LRM 8.6, and LRM 25.7 for a subroutine on the object a
// unit's instances are). The receiver arrives as `arguments[0]`, a pointer to
// the object, and the callee is `(receiver)->method`, so target-language
// name-lookup resolves the method through the receiver's static type after the
// declaring unit's header is included. A `Direct` call is non-virtual by
// construction (LRM 8.15 super, or a non-virtual callee), so the form is
// owner-qualified `(receiver)->unit::Class::method` -- the same shape the
// intra-unit owner-qualified render uses -- which bypasses the target
// language's vtable exactly as super demands and is equivalent to an
// unqualified call for a non-virtual callee.
auto RenderDirectExternalUnitClassMethodCall(
    const ScopeView& view, const mir::CallExpr& call,
    const mir::ExternalUnitClassMethodTarget& target) -> CalleeRender {
  return {
      .expr = std::format(
          "({})->{}::{}::{}", RenderExpr(view, view.Expr(call.arguments[0])),
          ToCppName(target.unit_name), ToCppName(target.class_name),
          target.method_name),
      .leading_arg_count = 1};
}

// Renders a `Direct` callee whose target is a type-associated method of another
// compilation unit (LRM 8.10). It takes no receiver, so nothing leads the
// arguments and the callee is the free qualified form `unit::Class::method`.
auto RenderDirectExternalUnitStaticMethodCall(
    const mir::ExternalUnitStaticMethodTarget& target) -> CalleeRender {
  return {
      .expr = std::format(
          "{}::{}::{}", ToCppName(target.unit_name),
          ToCppName(target.class_name), target.method_name),
      .leading_arg_count = 0};
}

// Renders a call to a method the runtime library provides for an imported class
// (LRM 9.7 `process`). The callee is the runtime symbol named by the method
// identity; every argument -- a receiver handle or the runtime handle -- is
// passed positionally, so no leading argument is absorbed into the callee text.
auto RenderDirectImportedRuntimeCall(
    const mir::ImportedRuntimeCallTarget& target) -> CalleeRender {
  return {
      .expr = std::format(
          "lyra::runtime::{}",
          support::ImportedRuntimeMethodSymbol(target.method)),
      .leading_arg_count = 0};
}

// Renders a call to a name in the DPI-C name space (LRM 35.4). The symbol is
// program-global, so it is spelled unqualified and carries no receiver; the
// prototype it resolves against is declared once in this artifact.
auto RenderDirectForeignSymbolCall(const mir::ForeignSymbolTarget& target)
    -> CalleeRender {
  return {.expr = target.linkage_name, .leading_arg_count = 0};
}

auto RenderCalleePart(
    const ScopeView& view, const mir::CallExpr& call, mir::TypeId result_type)
    -> CalleeRender {
  return std::visit(
      Overloaded{
          [&](const mir::Direct& d) -> CalleeRender {
            return std::visit(
                Overloaded{
                    [&](const mir::CallableTarget& t) {
                      return RenderDirectCallableCall(
                          view, call, t, d.qualification);
                    },
                    [&](const support::BuiltinFn& id) {
                      return RenderDirectBuiltinCall(
                          view, call, id, d.qualification);
                    },
                    [&](const mir::ExternalUnitCallableTarget& e) {
                      return RenderDirectExternalUnitCall(e);
                    },
                    [&](const mir::ExternalUnitClassMethodTarget& e) {
                      return RenderDirectExternalUnitClassMethodCall(
                          view, call, e);
                    },
                    [](const mir::ExternalUnitStaticMethodTarget& e) {
                      return RenderDirectExternalUnitStaticMethodCall(e);
                    },
                    [&](const mir::ImportedRuntimeCallTarget& i) {
                      return RenderDirectImportedRuntimeCall(i);
                    },
                    [](const mir::ForeignSymbolTarget& f) {
                      return RenderDirectForeignSymbolCall(f);
                    },
                },
                d.target);
          },
          [&](const mir::Indirect& i) -> CalleeRender {
            return {
                .expr =
                    std::format("({})", RenderExpr(view, view.Expr(i.code))),
                .leading_arg_count = 0};
          },
          [&](const mir::Virtual& v) -> CalleeRender {
            const std::string method_name = std::visit(
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
                v.slot);
            return {
                .expr = std::format(
                    "({})->{}", RenderExpr(view, view.Expr(v.receiver)),
                    method_name),
                .leading_arg_count = 0};
          },
          // A type has one way to come into existence, and what names it is the
          // type's own answer -- read through type mapping, the way every other
          // target-language spelling of a type is.
          [&](const mir::Construct&) -> CalleeRender {
            return {
                .expr = RenderTypeConstructionAsCpp(view.Unit(), result_type),
                .leading_arg_count = 0};
          },
      },
      call.callee);
}

auto RenderCall(
    const ScopeView& view, const mir::CallExpr& call, mir::TypeId result_type,
    std::optional<std::size_t> place_argument) -> std::string {
  const CalleeRender callee = RenderCalleePart(view, call, result_type);
  std::vector<std::string> args;
  args.reserve(call.arguments.size() - callee.leading_arg_count);
  for (std::size_t i = callee.leading_arg_count; i < call.arguments.size();
       ++i) {
    const mir::Expr& arg = view.Expr(call.arguments[i]);
    args.push_back(
        place_argument == i ? RenderLhsExpr(view, arg) : RenderExpr(view, arg));
  }
  return CallOf(callee.expr, args);
}

}  // namespace

auto RenderCallExpr(
    const ScopeView& view, const mir::CallExpr& call, mir::TypeId result_type)
    -> std::string {
  return RenderCall(view, call, result_type, std::nullopt);
}

auto RenderLhsCallExpr(
    const ScopeView& view, const mir::CallExpr& call, mir::TypeId result_type)
    -> std::string {
  const std::optional<std::size_t> place_argument =
      mir::IsPassThroughCallee(call.callee) ? std::optional<std::size_t>{0}
                                            : std::nullopt;
  return RenderCall(view, call, result_type, place_argument);
}

}  // namespace lyra::backend::cpp
