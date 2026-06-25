#include "lyra/backend/cpp/render_call.hpp"

#include <cstddef>
#include <format>
#include <string>
#include <string_view>
#include <variant>

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
    case support::BuiltinFn::kSet:
      return "Set";
    case support::BuiltinFn::kMutate:
      return "Mutate";
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
    case support::BuiltinFn::kTrigger:
      return "Trigger";
    case support::BuiltinFn::kAwait:
      return "Await";
    case support::BuiltinFn::kTriggered:
      return "Triggered";
    case support::BuiltinFn::kIsUnknown:
      return "IsUnknown";
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
    case support::BuiltinFn::kDelay:
      return "Delay";
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
    case support::BuiltinFn::kAsObservable:
      return "AsObservable";
    case support::BuiltinFn::kRegisterSignal:
      return "RegisterSignal";
    case support::BuiltinFn::kRegisterChild:
      return "RegisterChild";
    case support::BuiltinFn::kGetSignal:
      return "GetSignal";
    case support::BuiltinFn::kGetChild:
      return "GetChild";
    case support::BuiltinFn::kVectorEmplace:
      return "push_back";
    case support::BuiltinFn::kVectorBack:
      return "back";
    case support::BuiltinFn::kForkWaitAll:
      return "ForkWaitAll";
    case support::BuiltinFn::kForkWaitFirst:
      return "ForkWaitFirst";
    case support::BuiltinFn::kSpawnAll:
      return "SpawnAll";
    case support::BuiltinFn::kToInt64:
      return "ToInt64";
    case support::BuiltinFn::kRound:
      return "Round";
    case support::BuiltinFn::kFromInt:
      return "FromInt";
    case support::BuiltinFn::kConvertFrom:
      return "ConvertFrom";
    case support::BuiltinFn::kFromPackedArray:
      return "FromPackedArray";
    case support::BuiltinFn::kFromByteArray:
      return "FromByteArray";
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
  }
  throw InternalError("BuiltinFnCppName: unknown BuiltinFn");
}

// The C++ namespace path the builtin fn is declared in (e.g. `lyra::value`
// for `lyra::value::Scan`). A separate axis from the bare name and from the
// calling shape in the MIR `Callee` variant. Every id may declare one; render
// paths choose whether to consume it -- a `FreeFnCallee` must (no receiver
// supplies the scope), a `BuiltinFnCallee` need not (the receiver
// expression's type already names the enclosing class), a `BuiltinStaticCallee`
// need not (the `type_qual` already names the SV type). Returns empty for ids
// whose render path never needs an explicit qualifier; extend this table when
// adding a free function or any caller that wants the fully-qualified path.
auto BuiltinFnCppNamespace(support::BuiltinFn id) -> std::string_view {
  switch (id) {
    case support::BuiltinFn::kScan:
    case support::BuiltinFn::kFormat:
      return "lyra::value";
    case support::BuiltinFn::kDelay:
    case support::BuiltinFn::kSimTime:
    case support::BuiltinFn::kSTime:
    case support::BuiltinFn::kRealTime:
    case support::BuiltinFn::kFinish:
    case support::BuiltinFn::kFatalFinish:
    case support::BuiltinFn::kForkWaitAll:
    case support::BuiltinFn::kForkWaitFirst:
    case support::BuiltinFn::kSpawnAll:
      return "lyra::runtime";
    default:
      return "";
  }
}

// Each callee variant resolves to a `(callee_expr, leading_arg_count)` pair:
// the C++ text written before the `(args)` list, and how many leading MIR
// arguments are absorbed into the callee text (the receiver of an instance
// method) and skipped when rendering the user-visible argument list. The
// outer `RenderCallExpr` then does one concatenation -- `{callee}({args})` --
// so every shape (instance, free function, static method, closure, scope
// method, type constructor, heap-construct `make_unique<T>`) renders through
// the same final formatter.
struct CalleeRender {
  std::string expr;
  std::size_t leading_arg_count;
};

auto RenderCalleePart(
    const ScopeView& view, const mir::CallExpr& call, mir::TypeId result_type)
    -> CalleeRender {
  return std::visit(
      Overloaded{
          [&](const mir::MethodRef& ref) -> CalleeRender {
            if (ref.hops.value != 0) {
              throw InternalError(
                  "method call across classes is not yet "
                  "implemented in cpp emit");
            }
            const auto& cls = view.EnclosingClassAtHops(
                mir::EnclosingHops{.value = ref.hops.value});
            // The first argument is the callee's `self` handle, threaded as
            // an ordinary argument; no leading skip.
            return {
                .expr = std::string(cls.methods.Get(ref.method).name),
                .leading_arg_count = 0};
          },
          [&](const mir::BuiltinFnCallee& b) -> CalleeRender {
            // Instance method: render the receiver, pick `.` or `->` from
            // its MIR type, append the method name. The receiver consumes
            // the first MIR argument.
            if (call.arguments.empty()) {
              throw InternalError(
                  "BuiltinFnCallee: instance call expects a receiver "
                  "argument");
            }
            const mir::Expr& receiver = view.Expr(call.arguments[0]);
            const std::string_view sep =
                view.Unit().GetType(receiver.type).Kind() ==
                        mir::TypeKind::kPointer
                    ? "->"
                    : ".";
            return {
                .expr = std::format(
                    "({}){}{}", RenderExpr(view, receiver), sep,
                    BuiltinFnCppName(b.id)),
                .leading_arg_count = 1};
          },
          [&](const mir::BuiltinStaticCallee& b) -> CalleeRender {
            // Type-qualified static call: `Qualifier::name`. The qualifier
            // is just the result of rendering the SV type as C++ -- enum
            // types fold through `RenderEnumClassName` inside there, so the
            // call site does not need to discriminate on the type kind.
            return {
                .expr = std::format(
                    "{}::{}",
                    RenderTypeAsCpp(view.Unit(), view.Class(), b.type_qual),
                    BuiltinFnCppName(b.id)),
                .leading_arg_count = 0};
          },
          [&](const mir::FreeFnCallee& f) -> CalleeRender {
            const std::string_view ns = BuiltinFnCppNamespace(f.id);
            if (ns.empty()) {
              throw InternalError("FreeFnCallee: id has no declared namespace");
            }
            return {
                .expr = std::format("{}::{}", ns, BuiltinFnCppName(f.id)),
                .leading_arg_count = 0};
          },
          [&](const mir::ClosureRef& cr) -> CalleeRender {
            return {
                .expr = std::format(
                    "({})", RenderExpr(view, view.Expr(cr.closure))),
                .leading_arg_count = 0};
          },
          [&](const mir::ConstructorCallee&) -> CalleeRender {
            // The result type names what is being constructed. A
            // `unique_ptr<T>` constructor is `std::make_unique<T>` -- it
            // forwards the arguments to T's constructor and heap-allocates
            // -- so the call site stays an ordinary `ConstructorCallee`,
            // and which C++ syntax to spell is read off the result type.
            const auto& result_ty = view.Unit().GetType(result_type);
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
            return {
                .expr = RenderTypeAsCpp(view.Unit(), view.Class(), result_type),
                .leading_arg_count = 0};
          },
      },
      call.callee);
}

}  // namespace

auto RenderCallExpr(
    const ScopeView& view, const mir::CallExpr& call, mir::TypeId result_type)
    -> std::string {
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
