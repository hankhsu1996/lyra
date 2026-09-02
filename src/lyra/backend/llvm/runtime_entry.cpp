#include "lyra/backend/llvm/runtime_entry.hpp"

#include <format>
#include <optional>
#include <string>
#include <string_view>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/operator.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/value_domain.hpp"

namespace lyra::backend::llvm_backend {

namespace {

auto Symbol(std::string_view operation) -> std::string {
  return std::format("lyra_rt_{}", operation);
}

auto Symbol(support::ValueDomain domain, std::string_view operation)
    -> std::string {
  return std::format(
      "lyra_rt_{}_{}", support::ValueDomainName(domain), operation);
}

// The operation's stable spelling. This is an interface contract, not a display
// string: it is the operation half of the runtime-library symbol a generated
// module calls, so changing it renames a linked symbol.
auto RuntimeOpName(RuntimeOp op) -> std::string_view {
  switch (op) {
    case RuntimeOp::kCellAlloc:
      return "cell_alloc";
    case RuntimeOp::kCellInitialize:
      return "cell_initialize";
    case RuntimeOp::kCellGet:
      return "cell_get";
    case RuntimeOp::kCellSet:
      return "cell_set";
    case RuntimeOp::kNetInitialize:
      return "net_initialize";
    case RuntimeOp::kNetGet:
      return "net_get";
    case RuntimeOp::kDriverGet:
      return "driver_get";
    case RuntimeOp::kDriverSet:
      return "driver_set";
    case RuntimeOp::kMemberAddress:
      return "member_addr";
    case RuntimeOp::kSequenceMake:
      return "sequence_make";
    case RuntimeOp::kSequenceElement:
      return "sequence_element";
    case RuntimeOp::kClosureMake:
      return "closure_make";
    case RuntimeOp::kObjectMake:
      return "object_make";
    case RuntimeOp::kObjectDeref:
      return "object_deref";
    case RuntimeOp::kObjectMemberAddress:
      return "object_member_addr";
    case RuntimeOp::kClosureCapture:
      return "closure_capture";
    case RuntimeOp::kConst:
      return "const";
    case RuntimeOp::kToBool:
      return "to_bool";
    case RuntimeOp::kValueBox:
      return "value_box";
    case RuntimeOp::kMake:
      return "make";
    case RuntimeOp::kExtract:
      return "extract";
    case RuntimeOp::kUpdate:
      return "update";
    case RuntimeOp::kTagMatches:
      return "tag_matches";
    case RuntimeOp::kWithElement:
      return "with_element";
    case RuntimeOp::kWithSlice:
      return "with_slice";
    case RuntimeOp::kDefault:
      return "default";
    case RuntimeOp::kDefaultBounded:
      return "default_bounded";
    case RuntimeOp::kFromLiteral:
      return "from_literal";
    case RuntimeOp::kFromLiteralBounded:
      return "from_literal_bounded";
    case RuntimeOp::kFromEntries:
      return "from_entries";
    case RuntimeOp::kFromEntriesDefault:
      return "from_entries_default";
    case RuntimeOp::kMakeScope:
      return "make_scope";
    case RuntimeOp::kMakeSegment:
      return "make_segment";
    case RuntimeOp::kMakeTrigger:
      return "make_trigger";
    case RuntimeOp::kMakePackedRange:
      return "make_packed_range";
    case RuntimeOp::kMakePackedType:
      return "make_packed_type";
    case RuntimeOp::kMakePrintLiteralItem:
      return "make_print_literal_item";
    case RuntimeOp::kMakePrintValueItem:
      return "make_print_value_item";
    case RuntimeOp::kMakeFormatSpec:
      return "make_format_spec";
    case RuntimeOp::kMakeFormatSpecOfKind:
      return "make_format_spec_of_kind";
  }
  throw InternalError("llvm codegen: unknown runtime operation");
}

}  // namespace

auto DeclaredIndexType(const lir::CompilationUnit& unit, lir::TypeId container)
    -> std::optional<lir::TypeId> {
  const auto* associative =
      unit.types.Get(container).As<lir::AssociativeArrayType>();
  if (associative == nullptr) {
    return std::nullopt;
  }
  return associative->key_type;
}

auto NetResolutionOf(lir::NetResolution resolution) -> support::NetResolution {
  switch (resolution) {
    case lir::NetResolution::kTriState:
      return support::NetResolution::kTriState;
  }
  throw InternalError("llvm codegen: unknown net resolution");
}

auto ValueDomainOf(const lir::CompilationUnit& unit, lir::TypeId type)
    -> std::optional<support::ValueDomain> {
  using Domain = std::optional<support::ValueDomain>;
  return unit.types.Get(type).Visit(
      Overloaded{
          [](const lir::PackedArrayType&) -> Domain {
            return support::ValueDomain::kPacked;
          },
          // An enumeration is a packed value at runtime; only its own entries,
          // which read its declared members, need more than that.
          [](const lir::EnumType&) -> Domain {
            return support::ValueDomain::kPacked;
          },
          [](const lir::StringType&) -> Domain {
            return support::ValueDomain::kString;
          },
          // `real` and `realtime` are one host-precision value (LRM 6.12.1);
          // `shortreal` is the single-precision one.
          [](const lir::RealType&) -> Domain {
            return support::ValueDomain::kReal;
          },
          [](const lir::RealTimeType&) -> Domain {
            return support::ValueDomain::kReal;
          },
          [](const lir::ShortRealType&) -> Domain {
            return support::ValueDomain::kShortReal;
          },
          // A chandle (LRM 6.14) is a pointer-sized value carried inline: the
          // domain's handle is the chandle value itself, not a reference to a
          // runtime-owned value object.
          [](const lir::ChandleType&) -> Domain {
            return support::ValueDomain::kChandle;
          },
          [](const lir::TupleType&) -> Domain {
            return support::ValueDomain::kTuple;
          },
          // An untagged union erases its tag and gives a cross-member read the
          // component default; a tagged union keeps the tag observable and
          // faults a mismatched access (LRM 7.3 / 7.3.2), so the two realize as
          // different runtime value types and name different domains.
          [](const lir::UnionType&) -> Domain {
            return support::ValueDomain::kUnion;
          },
          [](const lir::TaggedUnionType&) -> Domain {
            return support::ValueDomain::kTaggedUnion;
          },
          // A tagged union's `void` member (LRM 7.3.2) is a value carrying no
          // bits; it crosses the boundary as its own domain so a build's
          // payload is uniform whatever the member type.
          [](const lir::EmptyType&) -> Domain {
            return support::ValueDomain::kEmpty;
          },
          [](const lir::DynamicArrayType&) -> Domain {
            return support::ValueDomain::kDynArray;
          },
          // A container's domain names how its elements are held and nothing
          // its declaration says: an unpacked array's range (LRM 7.4.2), a
          // queue's bound (LRM 7.10), and an associative array's index type
          // (LRM 7.8) each reach an operation as an operand of their own, so
          // one realization per domain serves every declared shape.
          [](const lir::UnpackedArrayType&) -> Domain {
            return support::ValueDomain::kUnpackedArray;
          },
          [](const lir::QueueType&) -> Domain {
            return support::ValueDomain::kQueue;
          },
          [](const lir::AssociativeArrayType&) -> Domain {
            return support::ValueDomain::kAssocArray;
          },
          // A class handle (LRM 8.3) refers to an object the simulator owns.
          // Which object it refers to is the whole value, so the domain's
          // operations are the ones over a reference -- defaulting to null,
          // copying, and comparing identity -- and never operations on the
          // object it names.
          [](const lir::ManagedRefType&) -> Domain {
            return support::ValueDomain::kManagedRef;
          },
          [](const auto&) -> Domain { return std::nullopt; }});
}

auto RuntimeSymbol(RuntimeOp op) -> std::string {
  return Symbol(RuntimeOpName(op));
}

auto RuntimeSymbol(support::ValueDomain domain, RuntimeOp op) -> std::string {
  return Symbol(domain, RuntimeOpName(op));
}

auto RuntimeSymbol(support::ValueDomain domain, lir::BinaryOp op)
    -> std::string {
  return Symbol(domain, lir::BinaryOpName(op));
}

auto RuntimeSymbol(support::ValueDomain domain, lir::UnaryOp op)
    -> std::string {
  return Symbol(domain, lir::UnaryOpName(op));
}

auto RuntimeSymbol(lir::ControlEffectTarget::Op op) -> std::string {
  return Symbol(lir::ControlEffectOpName(op));
}

auto RuntimeSymbol(lir::CoroutineTarget::Op op) -> std::string {
  return Symbol(lir::CoroutineOpName(op));
}

auto RuntimeSymbol(support::ValueDomain domain, lir::ValueCellTarget::Op op)
    -> std::string {
  return Symbol(domain, lir::ValueCellOpName(op));
}

auto RuntimeSymbol(support::BuiltinFn fn) -> std::string {
  return Symbol(support::BuiltinFnName(fn));
}

auto RuntimeSymbol(support::ValueDomain domain, support::BuiltinFn fn)
    -> std::string {
  return Symbol(domain, support::BuiltinFnName(fn));
}

auto RuntimeSymbol(
    support::ValueDomain destination, support::BuiltinFn fn,
    support::ValueDomain source) -> std::string {
  return Symbol(
      destination, std::format(
                       "{}_{}", support::BuiltinFnName(fn),
                       support::ValueDomainName(source)));
}

auto LoadOpOf(WrapperKind kind) -> RuntimeOp {
  switch (kind) {
    case WrapperKind::kCell:
      return RuntimeOp::kCellGet;
    case WrapperKind::kNet:
      return RuntimeOp::kNetGet;
    case WrapperKind::kDriver:
      return RuntimeOp::kDriverGet;
  }
  throw InternalError("llvm codegen: unknown capability wrapper");
}

auto StoreOpOf(WrapperKind kind) -> RuntimeOp {
  switch (kind) {
    case WrapperKind::kCell:
      return RuntimeOp::kCellSet;
    case WrapperKind::kNet:
      throw InternalError(
          "llvm codegen: a net's resolved value takes no store; a value "
          "reaches a net through one of its drivers");
    case WrapperKind::kDriver:
      return RuntimeOp::kDriverSet;
  }
  throw InternalError("llvm codegen: unknown capability wrapper");
}

auto InstallOpOf(WrapperKind kind) -> RuntimeOp {
  switch (kind) {
    case WrapperKind::kCell:
      return RuntimeOp::kCellInitialize;
    case WrapperKind::kNet:
      return RuntimeOp::kNetInitialize;
    case WrapperKind::kDriver:
      throw InternalError(
          "llvm codegen: a driver installs no representation of its own; what "
          "it contributes before it drives is the identity the net gave it "
          "when it attached");
  }
  throw InternalError("llvm codegen: unknown capability wrapper");
}

auto EntryNamingOf(support::BuiltinFn fn) -> EntryNaming {
  // An enumeration's own entries read its declared members, which no library
  // over the packed representation can answer. They belong to the enumeration's
  // generated artifact, not to the value domain its representation shares.
  constexpr std::string_view kReadsDeclaredMembers =
      "reads an enumeration's declared members";
  // A foreign call that can suspend runs the SV side on a stack the runtime did
  // not create (LRM 35.5.6, 35.8), which the value library reaches only through
  // types the host compiler laid out for it. Nothing crosses a C ABI that
  // stands for one.
  constexpr std::string_view kCrossesAForeignStack =
      "carries an execution across a stack the runtime does not own";

  switch (fn) {
    case support::BuiltinFn::kElement:
    case support::BuiltinFn::kSlice:
    case support::BuiltinFn::kRequire:
    case support::BuiltinFn::kSize:
    case support::BuiltinFn::kLen:
    case support::BuiltinFn::kBitstreamWidth:
    case support::BuiltinFn::kToOwned:
    case support::BuiltinFn::kDelete:
    case support::BuiltinFn::kDeleteIndex:
    case support::BuiltinFn::kAssocFirst:
    case support::BuiltinFn::kAssocLast:
    case support::BuiltinFn::kAssocNext:
    case support::BuiltinFn::kAssocPrev:
    case support::BuiltinFn::kScanString:
    case support::BuiltinFn::kScanFile:
    case support::BuiltinFn::kInsert:
    case support::BuiltinFn::kPopFront:
    case support::BuiltinFn::kPopBack:
    case support::BuiltinFn::kPushFront:
    case support::BuiltinFn::kPushBack:
    case support::BuiltinFn::kExists:
    case support::BuiltinFn::kAssocMinIndex:
    case support::BuiltinFn::kAssocMaxIndex:
    case support::BuiltinFn::kGetc:
    case support::BuiltinFn::kPutc:
    case support::BuiltinFn::kToupper:
    case support::BuiltinFn::kTolower:
    case support::BuiltinFn::kCompare:
    case support::BuiltinFn::kIcompare:
    case support::BuiltinFn::kSubstr:
    case support::BuiltinFn::kAtoi:
    case support::BuiltinFn::kAtohex:
    case support::BuiltinFn::kAtooct:
    case support::BuiltinFn::kAtobin:
    case support::BuiltinFn::kAtoreal:
    case support::BuiltinFn::kItoa:
    case support::BuiltinFn::kHextoa:
    case support::BuiltinFn::kOcttoa:
    case support::BuiltinFn::kBintoa:
    case support::BuiltinFn::kRealtoa:
    case support::BuiltinFn::kEnumNext:
    case support::BuiltinFn::kEnumPrev:
    case support::BuiltinFn::kIsUnknown:
    case support::BuiltinFn::kCountBits:
    case support::BuiltinFn::kClog2:
    case support::BuiltinFn::kLn:
    case support::BuiltinFn::kLog10:
    case support::BuiltinFn::kExp:
    case support::BuiltinFn::kSqrt:
    case support::BuiltinFn::kFloor:
    case support::BuiltinFn::kCeil:
    case support::BuiltinFn::kSin:
    case support::BuiltinFn::kCos:
    case support::BuiltinFn::kTan:
    case support::BuiltinFn::kAsin:
    case support::BuiltinFn::kAcos:
    case support::BuiltinFn::kAtan:
    case support::BuiltinFn::kAtan2:
    case support::BuiltinFn::kHypot:
    case support::BuiltinFn::kSinh:
    case support::BuiltinFn::kCosh:
    case support::BuiltinFn::kTanh:
    case support::BuiltinFn::kAsinh:
    case support::BuiltinFn::kAcosh:
    case support::BuiltinFn::kAtanh:
    case support::BuiltinFn::kToInt64:
    case support::BuiltinFn::kRound:
    case support::BuiltinFn::kTruncate:
    case support::BuiltinFn::kToBits:
    case support::BuiltinFn::kFromBits:
    case support::BuiltinFn::kRealValue:
    case support::BuiltinFn::kStringCStr:
    case support::BuiltinFn::kChandlePtr:
    case support::BuiltinFn::kToSvLogic:
    case support::BuiltinFn::kFromSvLogic:
    case support::BuiltinFn::kReadCanonicalBitVec:
    case support::BuiltinFn::kReadCanonicalLogicVec:
    case support::BuiltinFn::kWriteCanonicalBitVec:
    case support::BuiltinFn::kWriteCanonicalLogicVec:
    case support::BuiltinFn::kDpiBufferData:
    case support::BuiltinFn::kDpiOpenArrayHandle:
    case support::BuiltinFn::kDpiOpenArrayValue:
    case support::BuiltinFn::kFromInt:
    case support::BuiltinFn::kFromPackedArray:
    case support::BuiltinFn::kFromByteArray:
    case support::BuiltinFn::kFromString:
    case support::BuiltinFn::kConformBound:
    case support::BuiltinFn::kArrayConcatElement:
    case support::BuiltinFn::kArrayConcatSpread:
    case support::BuiltinFn::kArrayConformSize:
    case support::BuiltinFn::kConcat:
    case support::BuiltinFn::kReplicate:
    case support::BuiltinFn::kPow:
    case support::BuiltinFn::kShiftLeft:
    case support::BuiltinFn::kLogicalShiftRight:
    case support::BuiltinFn::kArithmeticShiftRight:
    case support::BuiltinFn::kBitwiseXnor:
    case support::BuiltinFn::kLogicalImplication:
    case support::BuiltinFn::kLogicalEquivalence:
    case support::BuiltinFn::kWildcardEquals:
    case support::BuiltinFn::kCaseEqual:
    case support::BuiltinFn::kCasezEquals:
    case support::BuiltinFn::kCasexEquals:
    case support::BuiltinFn::kMergeConditional:
    case support::BuiltinFn::kReductionAnd:
    case support::BuiltinFn::kReductionOr:
    case support::BuiltinFn::kReductionXor:
    case support::BuiltinFn::kReductionNand:
    case support::BuiltinFn::kReductionNor:
    case support::BuiltinFn::kReductionXnor:
    case support::BuiltinFn::kFromBool:
    case support::BuiltinFn::kFromWords:
    case support::BuiltinFn::kReverse:
    case support::BuiltinFn::kSort:
    case support::BuiltinFn::kRsort:
    case support::BuiltinFn::kSum:
    case support::BuiltinFn::kProduct:
    case support::BuiltinFn::kAnd:
    case support::BuiltinFn::kOr:
    case support::BuiltinFn::kXor:
    case support::BuiltinFn::kFind:
    case support::BuiltinFn::kFindIndex:
    case support::BuiltinFn::kFindFirst:
    case support::BuiltinFn::kFindFirstIndex:
    case support::BuiltinFn::kFindLast:
    case support::BuiltinFn::kFindLastIndex:
    case support::BuiltinFn::kMin:
    case support::BuiltinFn::kMax:
    case support::BuiltinFn::kUnique:
    case support::BuiltinFn::kUniqueIndex:
    case support::BuiltinFn::kMap:
      return NamedByValue{};

    case support::BuiltinFn::kConvertFrom:
      return NamedByConversion{};

    case support::BuiltinFn::kInitialize:
      return NamedByWrapperInstall{};

    // A driver is attached by the net that issues it, so what names the entry
    // is the representation that net resolves in.
    case support::BuiltinFn::kAttachDriver:
      return NamedByWrapperDomain{};

    case support::BuiltinFn::kEnumFirst:
    case support::BuiltinFn::kEnumLast:
    case support::BuiltinFn::kEnumNum:
    case support::BuiltinFn::kEnumName:
      return NotRealized{.shape = kReadsDeclaredMembers};

    // The runtime, then the user string, then the destination whose
    // representation names the entry.
    case support::BuiltinFn::kValuePlusargs:
      return NamedByValue{.operand = 2};

    // The runtime leads, and the memory whose addressing names the entry
    // follows.
    case support::BuiltinFn::kReadMem:
    case support::BuiltinFn::kReadMemWithin:
    case support::BuiltinFn::kWriteMem:
    case support::BuiltinFn::kWriteMemWithin:
      return NamedByValue{.operand = 1};

    case support::BuiltinFn::kRunForeignTaskOnFiber:
    case support::BuiltinFn::kRunExportedTaskToCompletion:
    case support::BuiltinFn::kCurrentExportScope:
    case support::BuiltinFn::kFindExportEntry:
      return NotRealized{.shape = kCrossesAForeignStack};

    case support::BuiltinFn::kTrigger:
    case support::BuiltinFn::kAwait:
    case support::BuiltinFn::kTriggered:
    case support::BuiltinFn::kCurrentRuntime:
    case support::BuiltinFn::kSubmitNba:
    case support::BuiltinFn::kSubmitPostponed:
    case support::BuiltinFn::kSubmitObserved:
    case support::BuiltinFn::kFiles:
    case support::BuiltinFn::kCancellationFor:
    case support::BuiltinFn::kIsCancelled:
    case support::BuiltinFn::kFormat:
    case support::BuiltinFn::kFormatRuntime:
    case support::BuiltinFn::kWrite:
    case support::BuiltinFn::kWriteln:
    case support::BuiltinFn::kDiagnostic:
    case support::BuiltinFn::kEmitInfo:
    case support::BuiltinFn::kEmitWarning:
    case support::BuiltinFn::kEmitError:
    case support::BuiltinFn::kEmitFatal:
    case support::BuiltinFn::kRecordCoverage:
    case support::BuiltinFn::kTimeFormat:
    case support::BuiltinFn::kSetTimeFormat:
    case support::BuiltinFn::kResetTimeFormat:
    case support::BuiltinFn::kPeekBuffered:
    case support::BuiltinFn::kAdvanceFd:
    case support::BuiltinFn::kFileOpen:
    case support::BuiltinFn::kFileOpenMode:
    case support::BuiltinFn::kFileClose:
    case support::BuiltinFn::kFileGetc:
    case support::BuiltinFn::kFileGets:
    case support::BuiltinFn::kFileRead:
    case support::BuiltinFn::kFileReadMemory:
    case support::BuiltinFn::kFileError:
    case support::BuiltinFn::kFileUngetc:
    case support::BuiltinFn::kFileSeek:
    case support::BuiltinFn::kFileRewind:
    case support::BuiltinFn::kFileTell:
    case support::BuiltinFn::kFileEof:
    case support::BuiltinFn::kFileFlush:
    case support::BuiltinFn::kFileFlushAll:
    case support::BuiltinFn::kTestPlusargs:
    case support::BuiltinFn::kRunHostCommand:
    case support::BuiltinFn::kRunNullHostCommand:
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
    case support::BuiltinFn::kResolveRoot:
    case support::BuiltinFn::kResolveVisibleChild:
    case support::BuiltinFn::kRegisterSignal:
    case support::BuiltinFn::kAddOwnedChild:
    case support::BuiltinFn::kGetSignal:
    case support::BuiltinFn::kGetChild:
    case support::BuiltinFn::kForkWaitAll:
    case support::BuiltinFn::kForkWaitFirst:
    case support::BuiltinFn::kSpawnAll:
    case support::BuiltinFn::kWaitFork:
    case support::BuiltinFn::kDisableFork:
    case support::BuiltinFn::kDisable:
    case support::BuiltinFn::kRegisterInitial:
    case support::BuiltinFn::kRegisterFinal:
    case support::BuiltinFn::kMakeDynamicArrayDefault:
    case support::BuiltinFn::kMakeDynamicArrayNew:
    case support::BuiltinFn::kMakeDynamicArrayNewCopy:
    case support::BuiltinFn::kEnterTarget:
    case support::BuiltinFn::kLeaveTarget:
    case support::BuiltinFn::kEffectNamesTarget:
    case support::BuiltinFn::kParent:
    case support::BuiltinFn::kHierarchicalPath:
      return NamedAlone{};
  }
  throw InternalError("llvm codegen: unknown builtin");
}

}  // namespace lyra::backend::llvm_backend
