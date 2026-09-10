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
  return std::format("{}{}", kRuntimeSymbolPrefix, operation);
}

auto Symbol(support::ValueDomain domain, std::string_view operation)
    -> std::string {
  return std::format(
      "{}{}_{}", kRuntimeSymbolPrefix, support::ValueDomainName(domain),
      operation);
}

// The operation's stable spelling. This is an interface contract, not a display
// string: it is the operation half of the runtime-library symbol a generated
// module calls, so changing it renames a linked symbol.
auto RuntimeOpName(RuntimeOp op) -> std::string_view {
  switch (op) {
    case RuntimeOp::kCellAlloc:
      return "cell_alloc";
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
    case RuntimeOp::kObjectMethod:
      return "object_method";
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
    case RuntimeOp::kFromLiteral:
      return "from_literal";
    case RuntimeOp::kFromLiteralBounded:
      return "from_literal_bounded";
    case RuntimeOp::kFromEntriesDefault:
      return "from_entries_default";
    case RuntimeOp::kMakeScope:
      return "make_scope";
    case RuntimeOp::kMakeSegment:
      return "make_segment";
    case RuntimeOp::kMakeTrigger:
      return "make_trigger";
    case RuntimeOp::kMakeObservedTrigger:
      return "make_observed_trigger";
    case RuntimeOp::kMakeObservation:
      return "make_observation";
    case RuntimeOp::kMakeQualifiedObservation:
      return "make_qualified_observation";
    case RuntimeOp::kMakeConditionObservation:
      return "make_condition_observation";
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
    case lir::NetResolution::kWiredAnd:
      return support::NetResolution::kWiredAnd;
    case lir::NetResolution::kWiredOr:
      return support::NetResolution::kWiredOr;
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
          // An enumeration is a packed value at runtime. What its declared
          // members answer (LRM 6.19.5) is settled where the source is read,
          // so nothing reaching this layer needs more than the packed value.
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

auto RuntimeSymbol(support::ImportedRuntimeMethod method) -> std::string {
  return Symbol(support::ImportedRuntimeMethodEntryName(method));
}

auto MemberStorageKindOf(
    const lir::CompilationUnit& unit, lir::TypeId type, MemberSlotRole role)
    -> std::optional<MemberStorageKind> {
  // A value the owner holds, in the slot its role asks for. It is a kind of
  // storage only where the runtime realizes values of that type at all.
  const auto held_value =
      [&](lir::TypeId value) -> std::optional<MemberStorageKind> {
    if (!ValueDomainOf(unit, value)) {
      return std::nullopt;
    }
    return role == MemberSlotRole::kVariable ? MemberStorageKind::kValueCell
                                             : MemberStorageKind::kInlineValue;
  };
  const auto value_of = [&](const auto&) { return held_value(type); };
  const auto borrowed = [](const auto&) -> std::optional<MemberStorageKind> {
    return MemberStorageKind::kBorrowedHandle;
  };
  const auto none = [](const auto&) -> std::optional<MemberStorageKind> {
    return std::nullopt;
  };
  return unit.types.Get(type).Visit(
      Overloaded{
          [&](const lir::ObservableType& observable)
              -> std::optional<MemberStorageKind> {
            if (!ValueDomainOf(unit, observable.value)) {
              return std::nullopt;
            }
            return MemberStorageKind::kObservableCell;
          },
          [&](const lir::ResolvedType& net)
              -> std::optional<MemberStorageKind> {
            if (!ValueDomainOf(unit, net.value)) {
              return std::nullopt;
            }
            return MemberStorageKind::kResolvedNet;
          },
          // A driver is a handle on a contribution the net owns and issues (LRM
          // 6.5); a reference and a pointer name storage living elsewhere; and
          // a declaration standing for several objects keeps a handle on the
          // sequence of them, built once where the owner is built. None owns
          // what it names.
          [&](const lir::DriverType& t) { return borrowed(t); },
          [&](const lir::RefType& t) { return borrowed(t); },
          [&](const lir::PointerType& t) { return borrowed(t); },
          [&](const lir::VectorType& t) { return borrowed(t); },
          [&](const lir::RuntimeLibraryType& library)
              -> std::optional<MemberStorageKind> {
            switch (library.kind) {
              case lir::RuntimeLibraryKind::kCancellationTarget:
                return MemberStorageKind::kCancellationTarget;
              case lir::RuntimeLibraryKind::kChannelCancellation:
                return MemberStorageKind::kChannelCancellation;
              // An integral type's descriptor, held once per type for the whole
              // run, so a member that names one points at storage outliving
              // every closure that reads it rather than owning a copy.
              case lir::RuntimeLibraryKind::kPackedType:
                return MemberStorageKind::kBorrowedHandle;
              default:
                return std::nullopt;
            }
          },
          [](const lir::EventType&) -> std::optional<MemberStorageKind> {
            return MemberStorageKind::kNamedEvent;
          },
          // A class handle is a value the member holds rather than a pointer it
          // merely points with: the object stays alive because the member
          // refers to it (LRM 8.3), so a write copies the handle's share of
          // ownership and not just its address. That is what separates it from
          // every borrowed form above, and from a chandle, whose value is the
          // bare pointer it carries and which owns nothing (LRM 6.14).
          [&](const lir::ManagedRefType& t) { return value_of(t); },
          [&](const lir::ChandleType& t) { return value_of(t); },
          [&](const lir::PackedArrayType& t) { return value_of(t); },
          [&](const lir::EnumType& t) { return value_of(t); },
          [&](const lir::UnpackedArrayType& t) { return value_of(t); },
          [&](const lir::DynamicArrayType& t) { return value_of(t); },
          [&](const lir::QueueType& t) { return value_of(t); },
          [&](const lir::AssociativeArrayType& t) { return value_of(t); },
          [&](const lir::StringType& t) { return value_of(t); },
          [&](const lir::RealType& t) { return value_of(t); },
          [&](const lir::ShortRealType& t) { return value_of(t); },
          [&](const lir::RealTimeType& t) { return value_of(t); },
          [&](const lir::TupleType& t) { return value_of(t); },
          [&](const lir::UnionType& t) { return value_of(t); },
          [&](const lir::TaggedUnionType& t) { return value_of(t); },
          [&](const lir::EmptyType& t) { return value_of(t); },
          // The rest name no storage a member can be. A machine primitive is a
          // computed value rather than a declaration's storage; an object-tree
          // node, a generated storage record, a closure, a coroutine and a
          // runtime facade are reached through a handle, so a member holding
          // one holds that handle and arrives here as its own type; and a
          // wildcard index and `void` have no runtime realization at all.
          [&](const lir::WildcardIndexType& t) { return none(t); },
          [&](const lir::MachineCStringType& t) { return none(t); },
          [&](const lir::MachineBoolType& t) { return none(t); },
          [&](const lir::MachineIntType& t) { return none(t); },
          [&](const lir::MachineFloatType& t) { return none(t); },
          [&](const lir::MachineArrayType& t) { return none(t); },
          [&](const lir::VoidType& t) { return none(t); },
          [&](const lir::ObjectType& t) { return none(t); },
          [&](const lir::ExternalUnitObjectType& t) { return none(t); },
          [&](const lir::StructType& t) { return none(t); },
          [&](const lir::CrossUnitClassType& t) { return none(t); },
          [&](const lir::RuntimeClassType& t) { return none(t); },
          [&](const lir::ClosureType& t) { return none(t); },
          [&](const lir::RuntimeEffectsType& t) { return none(t); },
          [&](const lir::FilesType& t) { return none(t); },
          [&](const lir::DiagnosticType& t) { return none(t); },
          [&](const lir::CoroutineType& t) { return none(t); }});
}

auto RuntimeSymbol(support::ValueDomain domain, lir::ValueCellTarget::Op op)
    -> std::string {
  return Symbol(domain, lir::ValueCellOpName(op));
}

auto RuntimeSymbol(support::BuiltinFn fn) -> std::string {
  return Symbol(support::RuntimeEntryOf(fn).name);
}

auto RuntimeSymbol(support::ValueDomain domain, support::BuiltinFn fn)
    -> std::string {
  return Symbol(domain, support::RuntimeEntryOf(fn).name);
}

auto RuntimeSymbol(
    support::ValueDomain destination, support::BuiltinFn fn,
    support::ValueDomain source) -> std::string {
  return Symbol(
      destination, std::format(
                       "{}_{}", support::RuntimeEntryOf(fn).name,
                       support::ValueDomainName(source)));
}

auto RuntimeSymbol(
    support::ValueDomain domain, WrapperKind wrapper, support::BuiltinFn fn)
    -> std::string {
  const auto spelled = [&](std::string_view family) -> std::string {
    return Symbol(
        domain, std::format("{}_{}", family, support::RuntimeEntryOf(fn).name));
  };
  switch (wrapper) {
    case WrapperKind::kCell:
      return spelled("cell");
    case WrapperKind::kNet:
      if (fn == support::BuiltinFn::kStore) {
        throw InternalError(
            "llvm codegen: a net's resolved value takes no store; a value "
            "reaches a net through one of its drivers");
      }
      return spelled("net");
    case WrapperKind::kDriver:
      if (fn == support::BuiltinFn::kInitialize) {
        throw InternalError(
            "llvm codegen: a driver installs no representation of its own; "
            "what it contributes before it drives is the identity the net "
            "gave it when it attached");
      }
      return spelled("driver");
  }
  throw InternalError("llvm codegen: unknown capability wrapper");
}

auto EntryNamingOf(support::BuiltinFn fn) -> EntryNaming {
  // A foreign call that can suspend runs the SV side on a stack the runtime did
  // not create (LRM 35.5.6, 35.8), which the value library reaches only through
  // types the host compiler laid out for it. Nothing crosses a C ABI that
  // stands for one.
  constexpr std::string_view kCrossesAForeignStack =
      "carries an execution across a stack the runtime does not own";
  // A value crosses this boundary as a handle a copy may alias, so nothing here
  // may answer with the part of one: a write through such an answer would be
  // visible through every copy. What this backend needs instead is the
  // functional update the part's owner performs, which the lowering builds from
  // the parts rather than reaching for an entry here.
  constexpr std::string_view kAnswersWithPartOfAValue =
      "answers with part of a value rather than its contents";
  // Recovering a handle from the object it refers to is what a shared-owner
  // realization needs and a traced one does not, since there the handle is the
  // pointer a body already holds. So this target owes no entry: it owes the
  // tracing that makes the recovery unnecessary.
  constexpr std::string_view kRecoversAHandleFromItsObject =
      "answers with the handle referring to the object a body runs on";
  // A sampled value is state a cell keeps beside its contents, and producing
  // one is the cell's own decision about which of the two to answer with (LRM
  // 16.5.1). It reaches a cell the way an ordinary read does and differs only
  // in which state answers, so what it needs is an entry per value domain of
  // its own, which the library does not carry.
  constexpr std::string_view kAnswersFromStateBesideTheContents =
      "answers from state a cell keeps beside its contents";
  // A history is member storage holding one value per tick of a clocking event
  // (LRM 16.9.3), so filling it, appending to it, and reading the tick a read
  // names are each an operation on that storage rather than on a value. Each
  // needs an entry per value domain, which the library does not carry.
  constexpr std::string_view kKeepsAValuePerTick =
      "keeps one value per tick of a clocking event";
  // A concurrent assertion's attempts are member storage this backend has no
  // realization of, so every operation on them is refused at the storage rather
  // than one entry at a time. The entries themselves carry only machine words,
  // which is why nothing here is about a value domain.
  constexpr std::string_view kHoldsEvaluationAttempts =
      "holds the evaluation attempts of a concurrent assertion";

  switch (fn) {
    case support::BuiltinFn::kElement:
    case support::BuiltinFn::kSlice:
    case support::BuiltinFn::kElementRef:
    case support::BuiltinFn::kSliceRef:
    case support::BuiltinFn::kPart:
    case support::BuiltinFn::kPartRef:
    case support::BuiltinFn::kTagMatches:
    case support::BuiltinFn::kMakeActiveMember:
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

    // LRM 7.6 assignment between unpacked array kinds crosses two container
    // representations and reads the source through the one it actually has, so
    // neither side alone names the entry.
    case support::BuiltinFn::kConvertFrom:
    case support::BuiltinFn::kFromArray:
      return NamedByConversion{};

    // The three accesses a capability wrapper defines. Which wrapper the call
    // acts on decides both which family answers and which representation it
    // answers in, so neither half is the call's own.
    case support::BuiltinFn::kInitialize:
    case support::BuiltinFn::kLoad:
    case support::BuiltinFn::kStore:
      return NamedByWrapper{};

    // A driver is attached by the net that issues it, so what names the entry
    // is the representation that net resolves in.
    case support::BuiltinFn::kAttachDriver:
      return NamedByWrapperDomain{};

    case support::BuiltinFn::kSampledLoad:
    case support::BuiltinFn::kArmSampling:
      return NotRealized{.shape = kAnswersFromStateBesideTheContents};

    case support::BuiltinFn::kSampledHistoryInstall:
    case support::BuiltinFn::kSampledHistoryPush:
    case support::BuiltinFn::kSampledHistoryAt:
      return NotRealized{.shape = kKeepsAValuePerTick};

    case support::BuiltinFn::kEvaluationAttemptsInstall:
    case support::BuiltinFn::kEvaluationAttemptsSeedWord:
    case support::BuiltinFn::kEvaluationAttemptsBeginTick:
    case support::BuiltinFn::kEvaluationAttemptsDisableTick:
    case support::BuiltinFn::kEvaluationAttemptsLiveWord:
    case support::BuiltinFn::kEvaluationAttemptsNextUnstepped:
    case support::BuiltinFn::kEvaluationAttemptsBitsAt:
    case support::BuiltinFn::kEvaluationAttemptsSetWord:
    case support::BuiltinFn::kEvaluationAttemptsStep:
    case support::BuiltinFn::kEvaluationAttemptsSeed:
    case support::BuiltinFn::kEvaluationAttemptsSettle:
      return NotRealized{.shape = kHoldsEvaluationAttempts};

    case support::BuiltinFn::kOpenForWrite:
      return NotRealized{.shape = kAnswersWithPartOfAValue};

    case support::BuiltinFn::kSelfHandle:
      return NotRealized{.shape = kRecoversAHandleFromItsObject};

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
    case support::BuiltinFn::kTriggered:
    case support::BuiltinFn::kCurrentRuntime:
    case support::BuiltinFn::kSubmitNba:
    case support::BuiltinFn::kSubmitNbaAfter:
    case support::BuiltinFn::kSubmitNbaAfterReal:
    case support::BuiltinFn::kRunDetached:
    case support::BuiltinFn::kResumeInNbaRegion:
    case support::BuiltinFn::kSubmitPostponed:
    case support::BuiltinFn::kSubmitObserved:
    case support::BuiltinFn::kSubmitViolationReport:
    case support::BuiltinFn::kSubmitDeferredObserved:
    case support::BuiltinFn::kSubmitDeferredFinal:
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
    case support::BuiltinFn::kDelayReal:
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
    case support::BuiltinFn::kStop:
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
