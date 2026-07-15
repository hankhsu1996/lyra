#include "lyra/backend/cpp/render_call.hpp"

#include <cstddef>
#include <format>
#include <string>
#include <string_view>
#include <variant>

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
    case support::BuiltinFn::kServices:
      return "Services";
    case support::BuiltinFn::kGet:
      return "Get";
    case support::BuiltinFn::kInitialize:
      return "Initialize";
    case support::BuiltinFn::kSet:
      return "Set";
    case support::BuiltinFn::kMutate:
      return "Mutate";
    case support::BuiltinFn::kAttachDriver:
      return "AttachDriver";
    case support::BuiltinFn::kUpdateDriver:
      return "Update";
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
    case support::BuiltinFn::kScan:
      return "Scan";
    case support::BuiltinFn::kPeekBuffered:
      return "PeekBuffered";
    case support::BuiltinFn::kAdvanceFd:
      return "AdvanceFd";
    case support::BuiltinFn::kTestPlusargs:
      return "TestPlusargs";
    case support::BuiltinFn::kValuePlusargs:
      return "ValuePlusargs";
    case support::BuiltinFn::kTrigger:
      return "Trigger";
    case support::BuiltinFn::kAwait:
      return "Await";
    case support::BuiltinFn::kTriggered:
      return "Triggered";
    case support::BuiltinFn::kIsUnknown:
      return "IsUnknown";
    case support::BuiltinFn::kClog2:
      return "Clog2";
    case support::BuiltinFn::kEnumFirst:
      return "First";
    case support::BuiltinFn::kEnumLast:
      return "Last";
    case support::BuiltinFn::kEnumNum:
      return "Num";
    case support::BuiltinFn::kEnumName:
      return "Name";
    case support::BuiltinFn::kEnumNext:
      return "Next";
    case support::BuiltinFn::kEnumPrev:
      return "Prev";
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
    case support::BuiltinFn::kSlice:
      return "Slice";
    case support::BuiltinFn::kSliceRef:
      return "SliceRef";
    case support::BuiltinFn::kSize:
      return "Size";
    case support::BuiltinFn::kToOwned:
      return "ToOwned";
    case support::BuiltinFn::kDelete:
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
      return "RegisterInitial";
    case support::BuiltinFn::kRegisterFinal:
      return "RegisterFinal";
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
    case support::BuiltinFn::kToInt64:
      return "ToInt64";
    case support::BuiltinFn::kRound:
      return "Round";
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
    case support::BuiltinFn::kDpiBufferData:
      return "Data";
    case support::BuiltinFn::kFromSvLogic:
      return "FromSvLogic";
    case support::BuiltinFn::kFromInt:
      return "FromInt";
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
    case support::BuiltinFn::kScan:
    case support::BuiltinFn::kFormat:
    case support::BuiltinFn::kFormatRuntime:
    case support::BuiltinFn::kToSvLogic:
    case support::BuiltinFn::kReadCanonicalBitVec:
    case support::BuiltinFn::kReadCanonicalLogicVec:
    case support::BuiltinFn::kFromSvLogic:
      return "lyra::value";
    case support::BuiltinFn::kDelay:
    case support::BuiltinFn::kWaitAny:
    case support::BuiltinFn::kSimTime:
    case support::BuiltinFn::kSTime:
    case support::BuiltinFn::kRealTime:
    case support::BuiltinFn::kFinish:
    case support::BuiltinFn::kFatalFinish:
    case support::BuiltinFn::kForkWaitAll:
    case support::BuiltinFn::kForkWaitFirst:
    case support::BuiltinFn::kSpawnAll:
    case support::BuiltinFn::kWaitFork:
    case support::BuiltinFn::kDisableFork:
    case support::BuiltinFn::kTestPlusargs:
    case support::BuiltinFn::kValuePlusargs:
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

// Renders a `Direct` callee whose target is a user-declared method. The target
// states its owning class explicitly, so the emitted qualification names the
// declaring class arena without deriving it from the receiver's runtime type
// (which may differ from the declaring class when the method is inherited).
// The `self` receiver stays an ordinary leading argument. No qualification is
// allowed today -- cross-class explicit qualification is gated on SV class
// support.
auto RenderDirectMethodCall(
    const ScopeView& view, const mir::CallExpr& call,
    const mir::MethodTarget& method,
    const std::optional<mir::ScopeQualifier>& qualification) -> CalleeRender {
  if (qualification.has_value()) {
    throw InternalError(
        "Direct method call: qualification is not yet implemented");
  }
  if (call.arguments.empty()) {
    throw InternalError("Direct method call expects a receiver argument");
  }
  const auto& cls = view.Unit().GetClass(method.owner);
  return {
      .expr = std::format(
          "{}::{}", ToCppName(cls.name), cls.methods.Get(method.slot).name),
      .leading_arg_count = 0};
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
            "{}::{}", RenderTypeAsCpp(view.Unit(), view.Class(), tq.type),
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
      view.Unit().types.Get(receiver.type).Kind() == mir::TypeKind::kPointer
          ? "->"
          : ".";
  return {
      .expr = std::format(
          "({}){}{}", RenderExpr(view, receiver), sep, BuiltinFnCppName(id)),
      .leading_arg_count = 1};
}

// Renders a `Direct` callee whose target is a class-level static callable.
// The target names the class whose associated namespace declares it, which need
// not be the class being rendered -- an import declared in the module is called
// from a generate block's class. Today that callable is a DPI-C import: a
// global `extern "C"` symbol reached by its foreign linkage name, with no
// receiver and no qualifier, so the callee text is the bare foreign name and no
// leading argument is absorbed into it.
auto RenderDirectStaticCallableCall(
    const ScopeView& view, const mir::StaticCallableTarget& target)
    -> CalleeRender {
  const auto& callable =
      view.Unit().GetClass(target.owner).static_callables.Get(target.slot);
  return {.expr = callable.external.foreign_name, .leading_arg_count = 0};
}

auto RenderCalleePart(
    const ScopeView& view, const mir::CallExpr& call, mir::TypeId result_type)
    -> CalleeRender {
  return std::visit(
      Overloaded{
          [&](const mir::Direct& d) -> CalleeRender {
            return std::visit(
                Overloaded{
                    [&](const mir::MethodTarget& m) {
                      return RenderDirectMethodCall(
                          view, call, m, d.qualification);
                    },
                    [&](const support::BuiltinFn& id) {
                      return RenderDirectBuiltinCall(
                          view, call, id, d.qualification);
                    },
                    [&](const mir::StaticCallableTarget& s) {
                      return RenderDirectStaticCallableCall(view, s);
                    },
                },
                d.target);
          },
          [&](const mir::Indirect& i) -> CalleeRender {
            return {
                .expr =
                    std::format("({})", RenderExpr(view, view.Expr(i.closure))),
                .leading_arg_count = 0};
          },
          [&](const mir::Construct&) -> CalleeRender {
            // The result type names what is being constructed. A
            // `unique_ptr<T>` constructor is `std::make_unique<T>` -- it
            // forwards the arguments to T's constructor and heap-allocates --
            // so which C++ syntax to spell is read off the result type.
            const auto& result_ty = view.Unit().types.Get(result_type);
            if (const auto* ptr =
                    std::get_if<mir::PointerType>(&result_ty.data);
                ptr != nullptr &&
                ptr->ownership == mir::PointerOwnership::kUnique) {
              return {
                  .expr = std::format(
                      "std::make_unique<{}>",
                      RenderTypeAsCpp(view.Unit(), view.Class(), ptr->pointee)),
                  .leading_arg_count = 0};
            }
            if (const auto* ptr =
                    std::get_if<mir::PointerType>(&result_ty.data);
                ptr != nullptr &&
                ptr->ownership == mir::PointerOwnership::kShared) {
              return {
                  .expr = std::format(
                      "std::make_shared<{}>",
                      RenderTypeAsCpp(view.Unit(), view.Class(), ptr->pointee)),
                  .leading_arg_count = 0};
            }
            // A managed reference result is a class `new`: allocate on the
            // managed heap and run the object's constructor.
            if (const auto* managed =
                    std::get_if<mir::ManagedRefType>(&result_ty.data);
                managed != nullptr) {
              return {
                  .expr = std::format(
                      "lyra::runtime::GcNew<{}>",
                      RenderTypeAsCpp(
                          view.Unit(), view.Class(), managed->pointee)),
                  .leading_arg_count = 0};
            }
            return {
                .expr = RenderTypeAsCpp(view.Unit(), view.Class(), result_type),
                .leading_arg_count = 0};
          },
      },
      call.callee);
}

}  // namespace

// LRM 10.10 unpacked-queue concatenation. The first two arguments seed the
// builder with a default element of the queue's element type and its LRM 7.10.5
// bound (a negative bound means unbounded); the remaining arguments are the
// parts, each spliced (an unpacked container of the element type) or appended
// as one element by its own type. The element type names the `<T>` template
// argument.
auto RenderMakeQueueConcatCall(
    const ScopeView& view, const mir::CallExpr& call, mir::TypeId result_type)
    -> std::string {
  const auto& queue_ty =
      std::get<mir::QueueType>(view.Unit().types.Get(result_type).data);
  std::string out = std::format(
      "lyra::value::MakeQueueConcat<{}>({}, {}",
      RenderTypeAsCpp(view.Unit(), view.Class(), queue_ty.element_type),
      RenderExpr(view, view.Expr(call.arguments[0])),
      RenderExpr(view, view.Expr(call.arguments[1])));
  for (std::size_t i = 2; i < call.arguments.size(); ++i) {
    const auto& part = view.Expr(call.arguments[i]);
    const auto& part_ty = view.Unit().types.Get(part.type);
    const bool spread =
        std::holds_alternative<mir::QueueType>(part_ty.data) ||
        std::holds_alternative<mir::DynamicArrayType>(part_ty.data) ||
        std::holds_alternative<mir::UnpackedArrayType>(part_ty.data);
    const std::string rendered = RenderExpr(view, part);
    out += spread ? std::format(", lyra::value::QSpread({})", rendered)
                  : std::format(", lyra::value::QElem({})", rendered);
  }
  out += ")";
  return out;
}

auto RenderCallExpr(
    const ScopeView& view, const mir::CallExpr& call, mir::TypeId result_type)
    -> std::string {
  if (const auto* direct = std::get_if<mir::Direct>(&call.callee)) {
    if (const auto* id = std::get_if<support::BuiltinFn>(&direct->target);
        id != nullptr && *id == support::BuiltinFn::kMakeQueueConcat) {
      return RenderMakeQueueConcatCall(view, call, result_type);
    }
  }
  const CalleeRender callee = RenderCalleePart(view, call, result_type);
  std::string args;
  for (std::size_t i = callee.leading_arg_count; i < call.arguments.size();
       ++i) {
    if (i != callee.leading_arg_count) args += ", ";
    args += RenderExpr(view, view.Expr(call.arguments[i]));
  }
  return std::format("{}({})", callee.expr, args);
}

}  // namespace lyra::backend::cpp
