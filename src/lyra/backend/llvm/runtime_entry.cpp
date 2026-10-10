#include "lyra/backend/llvm/runtime_entry.hpp"

#include <array>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/operator.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/support/value_domain.hpp"
#include "lyra/value/integral_fwd.hpp"
#include "lyra/value/integral_value_type.hpp"

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
    case RuntimeOp::kCellRefer:
      return "cell_refer";
    case RuntimeOp::kReferStorage:
      return "refer_storage";
    case RuntimeOp::kRunProgram:
      return "run_program";
    case RuntimeOp::kSequenceMake:
      return "sequence_make";
    case RuntimeOp::kSequenceElement:
      return "sequence_element";
    case RuntimeOp::kClosureMake:
      return "closure_make";
    case RuntimeOp::kObjectAdopt:
      return "object_adopt";
    case RuntimeOp::kSharedCellMake:
      return "shared_cell_make";
    case RuntimeOp::kSharedPointerDeref:
      return "shared_pointer_deref";
    case RuntimeOp::kHandleView:
      return "handle_view";
    case RuntimeOp::kHandleWithView:
      return "handle_with_view";
    case RuntimeOp::kConst:
      return "const";
    case RuntimeOp::kToBool:
      return "to_bool";
    case RuntimeOp::kMake:
      return "make";
    case RuntimeOp::kMakeWildcardIndex:
      return "wildcard_index_make";
    case RuntimeOp::kWithComponent:
      return "with_component";
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
    case RuntimeOp::kFromEntriesDefaultWildcard:
      return "from_entries_default_wildcard";
    case RuntimeOp::kMakeSegment:
      return "make_segment";
    case RuntimeOp::kMakeTrigger:
      return "make_trigger";
    case RuntimeOp::kMakePrintLiteralItem:
      return "make_print_literal_item";
    case RuntimeOp::kMakePrintValueItem:
      return "make_print_value_item";
    case RuntimeOp::kMakeIntegralPrintValueItem:
      return "integral_make_print_value_item";
    case RuntimeOp::kMakeFormatSpec:
      return "make_format_spec";
    case RuntimeOp::kMakeFormatArg:
      return "make_format_arg";
    case RuntimeOp::kMakeIntegralFormatArg:
      return "integral_make_format_arg";
    case RuntimeOp::kMakeDpiBitBuffer:
      return "make_dpi_bit_buffer";
    case RuntimeOp::kMakeDpiLogicBuffer:
      return "make_dpi_logic_buffer";
    case RuntimeOp::kMakeDpiOpenArray:
      return "make_dpi_open_array";
    case RuntimeOp::kSettleDeparture:
      return "settle_departure";
    case RuntimeOp::kStreamWrite:
      return "stream_write";
    case RuntimeOp::kStreamRead:
      return "stream_read";
    case RuntimeOp::kConstruct:
      return "construct";
    case RuntimeOp::kDestroy:
      return "destroy";
    case RuntimeOp::kCopy:
      return "copy";
    case RuntimeOp::kMove:
      return "move";
    case RuntimeOp::kAssign:
      return "assign";
  }
  throw InternalError("llvm codegen: unknown runtime operation");
}

// The family of entries a capability wrapper defines its accesses in, which is
// part of each one's symbol.
auto WrapperName(WrapperKind wrapper) -> std::string_view {
  switch (wrapper) {
    case WrapperKind::kCell:
      return "cell";
    case WrapperKind::kNet:
      return "net";
    case WrapperKind::kDriver:
      return "driver";
    case WrapperKind::kRef:
      return "ref";
  }
  throw InternalError("llvm codegen: unknown capability wrapper");
}

using Domain = support::ValueDomain;

constexpr DomainSet kOneWordIntegral{
    Domain::kBit8,   Domain::kBit16,   Domain::kBit32,   Domain::kBit64,
    Domain::kLogic8, Domain::kLogic16, Domain::kLogic32, Domain::kLogic64};
constexpr DomainSet kWideIntegral{Domain::kBitWide, Domain::kLogicWide};
constexpr DomainSet kIntegral = kOneWordIntegral | kWideIntegral;
constexpr DomainSet kOneWordFourState{
    Domain::kLogic8, Domain::kLogic16, Domain::kLogic32, Domain::kLogic64};
constexpr DomainSet kFourStateIntegral =
    kOneWordFourState | DomainSet{Domain::kLogicWide};
constexpr DomainSet kSequences{
    Domain::kDynArray, Domain::kUnpackedArray, Domain::kQueue};
constexpr DomainSet kContainers = kSequences | DomainSet{Domain::kAssocArray};
constexpr DomainSet kUnions{Domain::kUnion, Domain::kTaggedUnion};
constexpr DomainSet kReals{Domain::kReal, Domain::kShortReal};
// What a stream of bits is read out of (LRM 6.24.3) besides an integral value.
constexpr DomainSet kBitstreams = kContainers | kUnions |
                                  DomainSet{Domain::kString} |
                                  DomainSet{Domain::kManagedRef};
// The values the library holds as an object of its own, apart from a tuple,
// whose type is compiled with the design.
constexpr DomainSet kLibraryObjects =
    kContainers | kUnions | kReals |
    DomainSet{
        Domain::kWildcardIndex, Domain::kString, Domain::kChandle,
        Domain::kEmpty, Domain::kManagedRef};
// What an aggregate net resolves in (LRM 6.7.1), and what any net does.
constexpr DomainSet kAggregateNets{
    Domain::kTuple, Domain::kUnion, Domain::kUnpackedArray};
constexpr DomainSet kNetValues = kFourStateIntegral | kAggregateNets;
// What a variable holds besides an integral value.
constexpr DomainSet kVariableObjects =
    kContainers | kUnions | kReals |
    DomainSet{
        Domain::kString, Domain::kChandle, Domain::kTuple, Domain::kManagedRef};
constexpr DomainSet kVariableValues = kIntegral | kVariableObjects;

constexpr std::array kForAssociativeArrays{
    RealizedFor{.domains = {Domain::kAssocArray}}};
constexpr std::array kForDynamicArraysAndQueues{
    RealizedFor{.domains = {Domain::kDynArray, Domain::kQueue}}};
constexpr std::array kForWhatDeletesWhole{RealizedFor{
    .domains = {Domain::kDynArray, Domain::kQueue, Domain::kAssocArray}}};
constexpr std::array kForArraysOfFixedExtent{
    RealizedFor{.domains = {Domain::kDynArray, Domain::kUnpackedArray}}};
constexpr std::array kForSequences{RealizedFor{.domains = kSequences}};
constexpr std::array kForContainers{RealizedFor{.domains = kContainers}};
constexpr std::array kForQueues{RealizedFor{.domains = {Domain::kQueue}}};
constexpr std::array kForReal{RealizedFor{.domains = {Domain::kReal}}};
constexpr std::array kForReals{RealizedFor{.domains = kReals}};
constexpr std::array kForStrings{RealizedFor{.domains = {Domain::kString}}};
constexpr std::array kForWhatCaseEqualityCompares{RealizedFor{
    .domains =
        kContainers | kUnions |
        DomainSet{Domain::kString, Domain::kChandle, Domain::kManagedRef}}};
constexpr std::array kForWhatNumbersItsElements{
    RealizedFor{.domains = kSequences | DomainSet{Domain::kString}}};
constexpr std::array kForWhatIsComparedBitForBit{RealizedFor{
    .domains =
        kContainers | kUnions | kReals |
        DomainSet{Domain::kString, Domain::kChandle, Domain::kManagedRef}}};
constexpr std::array kForBitstreams{RealizedFor{.domains = kBitstreams}};
constexpr std::array kForTaggedUnions{
    RealizedFor{.domains = {Domain::kTaggedUnion}}};
constexpr std::array kForUnions{RealizedFor{.domains = kUnions}};
constexpr std::array kForWhatMayHoldUnknowns{
    RealizedFor{.domains = kUnions | DomainSet{Domain::kUnpackedArray}}};
constexpr std::array kForWhatAnAggregateNetResolves{
    RealizedFor{.domains = {Domain::kUnion, Domain::kUnpackedArray}}};
constexpr std::array kForUnpackedArrays{
    RealizedFor{.domains = {Domain::kUnpackedArray}}};
constexpr std::array kForUnpackedArraysAndQueues{
    RealizedFor{.domains = {Domain::kUnpackedArray, Domain::kQueue}}};
constexpr std::array kForTuples{RealizedFor{.domains = {Domain::kTuple}}};
constexpr std::array kForJoinedNets{RealizedFor{.domains = kFourStateIntegral}};
constexpr std::array kForNets{RealizedFor{.domains = kNetValues}};
constexpr std::array kForVariables{RealizedFor{.domains = kVariableValues}};
constexpr std::array kForLibraryObjects{
    RealizedFor{.domains = kLibraryObjects}};

// A memory a file fills (LRM 21.4): one reached by a key is told the type its
// keys are built at, after the memory.
constexpr std::array kForLoadedMemories{
    RealizedFor{.domains = kSequences},
    RealizedFor{
        .domains = {Domain::kAssocArray}, .told = ToldKeyType{.after = 1}}};
// A union is built holding the member its callee names (LRM 7.3).
constexpr std::array kForUnionsBuilt{
    RealizedFor{.domains = kUnions, .told = ToldMemberType{.after = 0}}};
// A history is filled with a default, which fixes how wide its values are.
constexpr std::array kForHistoriesInstalled{
    RealizedFor{
        .domains =
            kOneWordIntegral | kContainers | kUnions | kReals |
            DomainSet{Domain::kString, Domain::kTuple, Domain::kManagedRef}},
    RealizedFor{.domains = kWideIntegral, .told = ToldHeldWidth{.after = 1}}};
constexpr std::array kForHistories{RealizedFor{
    .domains =
        kIntegral | kContainers | kUnions | kReals |
        DomainSet{Domain::kString, Domain::kTuple, Domain::kManagedRef}}};
constexpr std::array kForIntegralNets{
    RealizedFor{.domains = kFourStateIntegral}};
constexpr std::array kForAggregateNets{RealizedFor{.domains = kAggregateNets}};

constexpr std::array kTakeovers{
    ThroughWrapper{.wrapper = WrapperKind::kCell, .domains = kIntegral},
    ThroughWrapper{.wrapper = WrapperKind::kNet, .domains = kNetValues}};
// A variable's cell is installed with its declaration's value, which fixes how
// wide a cell of words is.
constexpr std::array kInstalls{
    ThroughWrapper{
        .wrapper = WrapperKind::kCell,
        .domains = kOneWordIntegral | kVariableObjects},
    ThroughWrapper{
        .wrapper = WrapperKind::kCell,
        .domains = kWideIntegral,
        .told = ToldHeldWidth{.after = 1}}};
// A cell and a net hold the bytes of a value no wider than a word where
// generated code reads them, so neither has an entry reading one.
constexpr std::array kLoads{
    ThroughWrapper{
        .wrapper = WrapperKind::kCell,
        .domains = kWideIntegral | kVariableObjects},
    ThroughWrapper{
        .wrapper = WrapperKind::kNet,
        .domains = kAggregateNets | DomainSet{Domain::kLogicWide}},
    ThroughWrapper{.wrapper = WrapperKind::kDriver, .domains = kNetValues},
    ThroughWrapper{.wrapper = WrapperKind::kRef, .domains = kVariableValues}};
constexpr std::array kArmings{
    ThroughWrapper{.wrapper = WrapperKind::kCell, .domains = kVariableValues},
    ThroughWrapper{.wrapper = WrapperKind::kRef, .domains = kVariableValues}};
// A reference to a value wider than a word names words lying in something
// else, so an access through one that takes or replaces them is told how wide
// they are.
constexpr ThroughWrapper kThroughAReferenceToBytesOrAnObject{
    .wrapper = WrapperKind::kRef,
    .domains = kOneWordIntegral | kVariableObjects};
constexpr ThroughWrapper kThroughAReferenceToWords{
    .wrapper = WrapperKind::kRef,
    .domains = kWideIntegral,
    .told = ToldHeldWidth{.after = 0}};
constexpr std::array kSampledLoads{
    ThroughWrapper{.wrapper = WrapperKind::kCell, .domains = kVariableValues},
    kThroughAReferenceToBytesOrAnObject, kThroughAReferenceToWords};
constexpr std::array kWholeWrites{
    ThroughWrapper{.wrapper = WrapperKind::kCell, .domains = kVariableValues},
    ThroughWrapper{.wrapper = WrapperKind::kDriver, .domains = kNetValues},
    kThroughAReferenceToBytesOrAnObject, kThroughAReferenceToWords};

constexpr std::array kRealConversions{
    Conversion{.destination = Domain::kReal, .source = Domain::kReal},
    Conversion{.destination = Domain::kReal, .source = Domain::kShortReal},
    Conversion{.destination = Domain::kShortReal, .source = Domain::kReal}};

// A value cell holding a value's bytes is read and written where they lie, so
// only allocating one is an entry for every value a variable holds; a cell of
// words is allocated at the width of the values it is to hold.
constexpr std::array kForCellsAllocated{
    RealizedFor{.domains = kOneWordIntegral | kVariableObjects},
    RealizedFor{.domains = kWideIntegral, .told = ToldHeldWidthAhead{}}};
constexpr std::array kForCellsReached{
    RealizedFor{.domains = kWideIntegral | kVariableObjects}};

// A designation of words is told how wide they are where the write lands, and
// one of any integral value where a write of some of its bits is reported
// (LRM 11.5.1), which holds the bits written to the value's width.
constexpr std::array kForLandings{
    RealizedFor{
        .domains =
            kOneWordIntegral | kVariableObjects | DomainSet{Domain::kEmpty}},
    RealizedFor{.domains = kWideIntegral, .told = ToldHeldWidth{.after = 0}}};
constexpr std::array kForBitsReported{
    RealizedFor{.domains = kIntegral, .told = ToldHeldWidth{.after = 0}}};

constexpr std::array kForWhatIsNull{RealizedFor{
    .domains = {Domain::kChandle, Domain::kEmpty, Domain::kManagedRef}}};
constexpr std::array kForWhatIsTrueOrFalse{RealizedFor{
    .domains = kReals | DomainSet{Domain::kChandle, Domain::kManagedRef}}};
constexpr std::array kForWhatFormatsItself{RealizedFor{
    .domains =
        kReals |
        DomainSet{Domain::kString, Domain::kChandle, Domain::kManagedRef}}};
constexpr std::array kForWhatAHostValueBuilds{
    RealizedFor{.domains = {Domain::kString, Domain::kChandle}}};
constexpr std::array kForWhatEndsWithWork{RealizedFor{
    .domains =
        kContainers | kUnions |
        DomainSet{
            Domain::kWildcardIndex, Domain::kString, Domain::kManagedRef}}};
// A union with one member replaced is told the type of that member, after the
// value that takes its place (LRM 7.3).
constexpr std::array kForUnionsUpdated{
    RealizedFor{.domains = kUnions, .told = ToldMemberType{.after = 2}}};

// What refusing a realization the library does not hold says.
auto Refusal(std::string_view entry, std::string_view realization)
    -> std::string {
  return std::format(
      "llvm codegen: the library realizes no {} for {} -- please report this "
      "as a bug",
      entry, realization);
}

// The realizations of an entry named by one value domain, none for an entry
// named any other way.
auto PerDomain(const EntryNaming& naming) -> std::span<const RealizedFor> {
  using Realized = std::span<const RealizedFor>;
  return std::visit(
      Overloaded{
          [](const NamedByValue& named) -> Realized { return named.realized; },
          [](const NamedByResult& named) -> Realized { return named.realized; },
          [](const NamedByStorageDomain& named) -> Realized {
            return named.realized;
          },
          [](const NamedAlone&) -> Realized { return {}; },
          [](const NamedByWrapper&) -> Realized { return {}; },
          [](const NamedByConversion&) -> Realized { return {}; },
          [](const OverIntegralValues&) -> Realized { return {}; },
          [](const NotRealized&) -> Realized { return {}; }},
      naming);
}

// The same for an entry named by a capability wrapper, and for a conversion.
auto ThroughWrappers(const EntryNaming& naming)
    -> std::span<const ThroughWrapper> {
  using Realized = std::span<const ThroughWrapper>;
  return std::visit(
      Overloaded{
          [](const NamedByWrapper& named) -> Realized {
            return named.realized;
          },
          [](const NamedAlone&) -> Realized { return {}; },
          [](const NamedByValue&) -> Realized { return {}; },
          [](const NamedByResult&) -> Realized { return {}; },
          [](const NamedByStorageDomain&) -> Realized { return {}; },
          [](const NamedByConversion&) -> Realized { return {}; },
          [](const OverIntegralValues&) -> Realized { return {}; },
          [](const NotRealized&) -> Realized { return {}; }},
      naming);
}

auto Conversions(const EntryNaming& naming) -> std::span<const Conversion> {
  using Realized = std::span<const Conversion>;
  return std::visit(
      Overloaded{
          [](const NamedByConversion& named) -> Realized {
            return named.realized;
          },
          [](const NamedAlone&) -> Realized { return {}; },
          [](const NamedByValue&) -> Realized { return {}; },
          [](const NamedByResult&) -> Realized { return {}; },
          [](const NamedByWrapper&) -> Realized { return {}; },
          [](const NamedByStorageDomain&) -> Realized { return {}; },
          [](const OverIntegralValues&) -> Realized { return {}; },
          [](const NotRealized&) -> Realized { return {}; }},
      naming);
}

auto ToldIn(
    std::span<const RealizedFor> realized, support::ValueDomain domain,
    std::string_view entry) -> Told {
  for (const RealizedFor& one : realized) {
    if (one.domains.Holds(domain)) {
      return one.told;
    }
  }
  throw InternalError(Refusal(entry, support::ValueDomainName(domain)));
}

}  // namespace

auto ToldOf(support::ValueDomain domain, RuntimeOp op) -> Told {
  return ToldIn(RealizationsOf(op), domain, RuntimeOpName(op));
}

auto ToldOf(support::ValueDomain domain, lir::ValueCellTarget::Op op) -> Told {
  return ToldIn(RealizationsOf(op), domain, lir::ValueCellOpName(op));
}

auto ToldOf(support::ValueDomain domain, lir::OpenWriteTarget::Op op) -> Told {
  return ToldIn(RealizationsOf(op), domain, lir::OpenWriteOpName(op));
}

auto ToldOf(support::ValueDomain domain, lir::DesignatedBitsTarget::Op op)
    -> Told {
  return ToldIn(RealizationsOf(op), domain, lir::DesignatedBitsOpName(op));
}

auto ToldOf(support::ValueDomain domain, support::BuiltinFn fn) -> Told {
  return ToldIn(
      PerDomain(EntryNamingOf(fn)), domain, support::RuntimeEntryOf(fn).name);
}

auto ToldOf(
    support::ValueDomain domain, WrapperKind wrapper, support::BuiltinFn fn)
    -> Told {
  const EntryNaming naming = EntryNamingOf(fn);
  for (const ThroughWrapper& one : ThroughWrappers(naming)) {
    if (one.wrapper == wrapper && one.domains.Holds(domain)) {
      return one.told;
    }
  }
  throw InternalError(Refusal(
      support::RuntimeEntryOf(fn).name,
      std::format(
          "{} through a {}", support::ValueDomainName(domain),
          WrapperName(wrapper))));
}

auto IntegralOpOf(lir::BinaryOp op) -> support::IntegralOp {
  switch (op) {
    case lir::BinaryOp::kAdd:
      return support::IntegralOp::kAdd;
    case lir::BinaryOp::kSub:
      return support::IntegralOp::kSubtract;
    case lir::BinaryOp::kMul:
      return support::IntegralOp::kMultiply;
    case lir::BinaryOp::kDiv:
      return support::IntegralOp::kDivide;
    case lir::BinaryOp::kMod:
      return support::IntegralOp::kModulo;
    case lir::BinaryOp::kBitwiseAnd:
      return support::IntegralOp::kBitwiseAnd;
    case lir::BinaryOp::kBitwiseOr:
      return support::IntegralOp::kBitwiseOr;
    case lir::BinaryOp::kBitwiseXor:
      return support::IntegralOp::kBitwiseXor;
    case lir::BinaryOp::kEquality:
      return support::IntegralOp::kEqual;
    case lir::BinaryOp::kInequality:
      return support::IntegralOp::kNotEqual;
    case lir::BinaryOp::kLessThan:
      return support::IntegralOp::kLess;
    case lir::BinaryOp::kLessEqual:
      return support::IntegralOp::kLessEqual;
    case lir::BinaryOp::kGreaterThan:
      return support::IntegralOp::kGreater;
    case lir::BinaryOp::kGreaterEqual:
      return support::IntegralOp::kGreaterEqual;
    case lir::BinaryOp::kLogicalAnd:
      return support::IntegralOp::kLogicalAnd;
    case lir::BinaryOp::kLogicalOr:
      return support::IntegralOp::kLogicalOr;
  }
  throw InternalError("llvm codegen: unknown binary operator");
}

auto IntegralOpOf(lir::UnaryOp op) -> support::IntegralOp {
  switch (op) {
    case lir::UnaryOp::kMinus:
      return support::IntegralOp::kNegate;
    case lir::UnaryOp::kBitwiseNot:
      return support::IntegralOp::kBitwiseNot;
    case lir::UnaryOp::kLogicalNot:
      return support::IntegralOp::kLogicalNot;
  }
  throw InternalError("llvm codegen: unknown unary operator");
}

auto ReachedByKey(support::ValueDomain domain) -> bool {
  switch (domain) {
    case support::ValueDomain::kAssocArray:
      return true;
    case support::ValueDomain::kBit8:
    case support::ValueDomain::kBit16:
    case support::ValueDomain::kBit32:
    case support::ValueDomain::kBit64:
    case support::ValueDomain::kLogic8:
    case support::ValueDomain::kLogic16:
    case support::ValueDomain::kLogic32:
    case support::ValueDomain::kLogic64:
    case support::ValueDomain::kBitWide:
    case support::ValueDomain::kLogicWide:
    case support::ValueDomain::kWildcardIndex:
    case support::ValueDomain::kString:
    case support::ValueDomain::kReal:
    case support::ValueDomain::kShortReal:
    case support::ValueDomain::kChandle:
    case support::ValueDomain::kEmpty:
    case support::ValueDomain::kTuple:
    case support::ValueDomain::kUnion:
    case support::ValueDomain::kTaggedUnion:
    case support::ValueDomain::kDynArray:
    case support::ValueDomain::kUnpackedArray:
    case support::ValueDomain::kQueue:
    case support::ValueDomain::kManagedRef:
      return false;
  }
  throw InternalError("llvm codegen: unknown value domain");
}

auto ElementEntriesOf(support::ValueDomain domain) -> ElementEntries {
  if (ReachedByKey(domain)) {
    return {
        .read =
            {.entry = support::BuiltinFn::kAssocElement,
             .symbol = RuntimeSymbol(support::BuiltinFn::kAssocElement)},
        .write = {
            .entry = support::BuiltinFn::kAssocElementRef,
            .symbol = RuntimeSymbol(support::BuiltinFn::kAssocElementRef)}};
  }
  return {
      .read =
          {.entry = support::BuiltinFn::kElement,
           .symbol = RuntimeSymbol(domain, support::BuiltinFn::kElement)},
      .write = {
          .entry = support::BuiltinFn::kElementRef,
          .symbol = RuntimeSymbol(domain, support::BuiltinFn::kElementRef)}};
}

auto HeldObjectOf(const lir::Type& type)
    -> std::optional<support::RuntimeObject> {
  using Object = std::optional<support::RuntimeObject>;
  const std::optional<lir::Holding> held = type.HeldAs();
  if (!held.has_value()) {
    return std::nullopt;
  }
  return std::visit(
      Overloaded{
          // The layout an integral value's bits are held in follows from how
          // many there are and whether an unknown plane goes with them.
          [&](const lir::IntegralBits&) -> Object {
            const auto& integral = type.Get<lir::IntegralType>();
            switch (integral.state_kind) {
              case lir::IntegralStateKind::kTwoState:
                return value::IntegralDomainFor(
                    integral.bit_width, value::StateDomain::kTwoState);
              case lir::IntegralStateKind::kFourState:
                return value::IntegralDomainFor(
                    integral.bit_width, value::StateDomain::kFourState);
            }
            throw InternalError("llvm codegen: unknown integral state kind");
          },
          [](support::ValueDomain domain) -> Object { return domain; },
          [](support::LibraryObject object) -> Object { return object; }},
      *held);
}

auto ValueDomainOf(const lir::CompilationUnit& unit, lir::TypeId type)
    -> std::optional<support::ValueDomain> {
  const std::optional<support::RuntimeObject> held =
      HeldObjectOf(unit.types.Get(type));
  if (!held.has_value()) {
    return std::nullopt;
  }
  return std::visit(
      Overloaded{
          [](support::ValueDomain domain)
              -> std::optional<support::ValueDomain> { return domain; },
          [](support::LibraryObject) -> std::optional<support::ValueDomain> {
            return std::nullopt;
          }},
      *held);
}

auto RuntimeSymbol(RuntimeOp op) -> std::string {
  return Symbol(RuntimeOpName(op));
}

auto RuntimeSymbol(support::ValueDomain domain, RuntimeOp op) -> std::string {
  ToldOf(domain, op);
  return Symbol(domain, RuntimeOpName(op));
}

auto ValueTypeSymbol(support::ValueDomain domain) -> std::string {
  constexpr std::string_view kValueType = "value_type";
  ToldIn(kForLibraryObjects, domain, kValueType);
  return Symbol(domain, kValueType);
}

auto RuntimeSymbol(support::RuntimeObject object, RuntimeOp op) -> std::string {
  return std::format(
      "{}{}_{}", kRuntimeSymbolPrefix, support::RuntimeObjectName(object),
      RuntimeOpName(op));
}

auto RuntimeSymbol(support::DeclaredMemberStorage storage, RuntimeOp op)
    -> std::string {
  const std::string_view kind = support::MemberStorageKindName(storage.kind);
  switch (storage.kind) {
    case support::MemberStorageKind::kObservableCell:
    case support::MemberStorageKind::kResolvedNet:
    case support::MemberStorageKind::kSampledHistory:
    case support::MemberStorageKind::kValueCell:
      return Symbol(
          storage.domain, std::format("{}_{}", kind, RuntimeOpName(op)));
    // Storage holding a value inline is that value, so what builds and ends it
    // is what builds and ends a value of its domain.
    case support::MemberStorageKind::kInlineValue:
      return RuntimeSymbol(storage.domain, op);
    case support::MemberStorageKind::kBorrowedHandle:
    case support::MemberStorageKind::kReference:
    case support::MemberStorageKind::kSharedPointer:
    case support::MemberStorageKind::kNamedEvent:
    case support::MemberStorageKind::kCancellationTarget:
    case support::MemberStorageKind::kChannelCancellation:
    case support::MemberStorageKind::kEvaluationAttempts:
      return Symbol(std::format("{}_{}", kind, RuntimeOpName(op)));
  }
  throw InternalError("llvm codegen: unknown member storage kind");
}

auto BuiltAtTheWidthHeld(support::DeclaredMemberStorage storage) -> bool {
  switch (storage.kind) {
    case support::MemberStorageKind::kValueCell:
      return kWideIntegral.Holds(storage.domain);
    // A variable's cell, a net and a history are each installed by an entry of
    // their own, and every other storage holds no integral value's words.
    case support::MemberStorageKind::kObservableCell:
    case support::MemberStorageKind::kResolvedNet:
    case support::MemberStorageKind::kSampledHistory:
    case support::MemberStorageKind::kInlineValue:
    case support::MemberStorageKind::kBorrowedHandle:
    case support::MemberStorageKind::kReference:
    case support::MemberStorageKind::kSharedPointer:
    case support::MemberStorageKind::kNamedEvent:
    case support::MemberStorageKind::kCancellationTarget:
    case support::MemberStorageKind::kChannelCancellation:
    case support::MemberStorageKind::kEvaluationAttempts:
      return false;
  }
  throw InternalError("llvm codegen: unknown member storage kind");
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

auto MemberSlotRoleOf(const lir::TypeDeclaration& declaration)
    -> MemberSlotRole {
  return std::visit(
      Overloaded{
          [](const lir::ObjectType&) { return MemberSlotRole::kVariable; },
          [](const lir::CrossUnitClassType&) {
            return MemberSlotRole::kVariable;
          },
          [](const lir::ClosureType&) { return MemberSlotRole::kSnapshot; }},
      declaration);
}

auto MemberStorageKindOf(
    const lir::CompilationUnit& unit, lir::TypeId type, MemberSlotRole role)
    -> std::optional<support::MemberStorageKind> {
  // A value the owner holds, in the slot its role asks for. It is a kind of
  // storage only where the runtime realizes values of that type at all.
  const auto held_value =
      [&](lir::TypeId value) -> std::optional<support::MemberStorageKind> {
    if (!ValueDomainOf(unit, value)) {
      return std::nullopt;
    }
    return role == MemberSlotRole::kVariable
               ? support::MemberStorageKind::kValueCell
               : support::MemberStorageKind::kInlineValue;
  };
  const auto value_of = [&](const auto&) { return held_value(type); };
  // Storage the owner holds over values of one domain -- a cell, a net's
  // resolution node, a history. Each is that storage only where the runtime
  // realizes values of the domain it holds, for the same reason a value member
  // is: the storage is built from the domain and there is nothing to build it
  // from otherwise.
  const auto over_values = [&](lir::TypeId value,
                               support::MemberStorageKind kind)
      -> std::optional<support::MemberStorageKind> {
    if (!ValueDomainOf(unit, value)) {
      return std::nullopt;
    }
    return kind;
  };
  const auto borrowed =
      [](const auto&) -> std::optional<support::MemberStorageKind> {
    return support::MemberStorageKind::kBorrowedHandle;
  };
  const auto none =
      [](const auto&) -> std::optional<support::MemberStorageKind> {
    return std::nullopt;
  };
  return unit.types.Get(type).Visit(
      Overloaded{
          [&](const lir::ObservableType& observable) {
            return over_values(
                observable.value, support::MemberStorageKind::kObservableCell);
          },
          [&](const lir::SampledHistoryType& history) {
            return over_values(
                history.value, support::MemberStorageKind::kSampledHistory);
          },
          [&](const lir::ResolvedType& net) {
            return over_values(
                net.value, support::MemberStorageKind::kResolvedNet);
          },
          // A driver is a handle on a contribution the net owns and issues (LRM
          // 6.5); a declaration standing for several objects keeps a handle on
          // the sequence of them, built once where the owner is built; and a
          // code address names a body that outlives every owner there is. None
          // of these owns what it names.
          [&](const lir::DriverType& t) { return borrowed(t); },
          [&](const lir::VectorType& t) { return borrowed(t); },
          // A reference names storage living elsewhere too, and it is more than
          // an address: the variable that storage belongs to travels with it.
          [](const lir::RefType&) -> std::optional<support::MemberStorageKind> {
            return support::MemberStorageKind::kReference;
          },
          [&](const lir::MachineFunctionType& t) { return borrowed(t); },
          // A pointer is the one whose ownership decides the answer. A unique
          // or borrowed one likewise names storage somebody else ends, while a
          // shared one is a hold: it keeps what it names in existence and the
          // storage ends once no holder is left, which is how a scope outlives
          // the control flow that left it (LRM 6.21).
          [&](const lir::PointerType& pointer)
              -> std::optional<support::MemberStorageKind> {
            switch (pointer.ownership) {
              case lir::PointerOwnership::kUnique:
              case lir::PointerOwnership::kBorrowed:
                return support::MemberStorageKind::kBorrowedHandle;
              case lir::PointerOwnership::kShared:
                return support::MemberStorageKind::kSharedPointer;
            }
            throw InternalError("llvm codegen: unknown pointer ownership");
          },
          [&](const lir::RuntimeLibraryType& library)
              -> std::optional<support::MemberStorageKind> {
            switch (library.kind) {
              case lir::RuntimeLibraryKind::kCancellationTarget:
                return support::MemberStorageKind::kCancellationTarget;
              case lir::RuntimeLibraryKind::kChannelCancellation:
                return support::MemberStorageKind::kChannelCancellation;
              // An enumeration's member table, held once for the whole run, so
              // a member that names one points at storage outliving every
              // closure that reads it rather than owning a copy.
              case lir::RuntimeLibraryKind::kEnumeration:
              // A class's definition is one per class for the whole run and
              // every object of it shares it, so a member naming one points at
              // storage outliving it for the same reason.
              case lir::RuntimeLibraryKind::kObjectDefinition:
                return support::MemberStorageKind::kBorrowedHandle;
              // The rest are transients of one call -- what a print or a
              // format is assembled from, what a boundary object images an
              // argument in, what a wait registers, a write in progress, and
              // what another entry answers with. An owner holds none of them
              // past the call that made one.
              case lir::RuntimeLibraryKind::kPrintItem:
              case lir::RuntimeLibraryKind::kPrintLiteralItem:
              case lir::RuntimeLibraryKind::kPrintValueItem:
              case lir::RuntimeLibraryKind::kFormatSpec:
              case lir::RuntimeLibraryKind::kFormatArg:
              case lir::RuntimeLibraryKind::kTimeFormat:
              case lir::RuntimeLibraryKind::kHierarchySegment:
              case lir::RuntimeLibraryKind::kDpiBitBuffer:
              case lir::RuntimeLibraryKind::kDpiLogicBuffer:
              case lir::RuntimeLibraryKind::kDpiBitChunk:
              case lir::RuntimeLibraryKind::kDpiLogicChunk:
              case lir::RuntimeLibraryKind::kDpiOpenArray:
              case lir::RuntimeLibraryKind::kDpiOpenArrayHandle:
              case lir::RuntimeLibraryKind::kTrigger:
              case lir::RuntimeLibraryKind::kObservation:
              case lir::RuntimeLibraryKind::kReadReport:
              case lir::RuntimeLibraryKind::kWait:
              case lir::RuntimeLibraryKind::kObjectWrite:
              case lir::RuntimeLibraryKind::kControlEffect:
              // What a constant is made of, which no member holds.
              case lir::RuntimeLibraryKind::kScopeInfo:
              case lir::RuntimeLibraryKind::kScopeCallable:
                return std::nullopt;
            }
            throw InternalError("llvm codegen: unknown runtime library kind");
          },
          [](const lir::EventType&)
              -> std::optional<support::MemberStorageKind> {
            return support::MemberStorageKind::kNamedEvent;
          },
          [](const lir::EvaluationAttemptsType&)
              -> std::optional<support::MemberStorageKind> {
            return support::MemberStorageKind::kEvaluationAttempts;
          },
          // A class handle is a value the member holds rather than a pointer it
          // merely points with: the object stays alive because the member
          // refers to it (LRM 8.3), so a write copies the handle's share of
          // ownership and not just its address. That is what separates it from
          // every borrowed form above, and from a chandle, whose value is the
          // bare pointer it carries and which owns nothing (LRM 6.14).
          [&](const lir::ManagedRefType& t) { return value_of(t); },
          [&](const lir::ChandleType& t) { return value_of(t); },
          [&](const lir::IntegralType& t) { return value_of(t); },
          [&](const lir::UnpackedArrayType& t) { return value_of(t); },
          [&](const lir::DynamicArrayType& t) { return value_of(t); },
          [&](const lir::QueueType& t) { return value_of(t); },
          [&](const lir::AssociativeArrayType& t) { return value_of(t); },
          [&](const lir::StringType& t) { return value_of(t); },
          [&](const lir::RealType& t) { return value_of(t); },
          [&](const lir::ShortRealType& t) { return value_of(t); },
          [&](const lir::TupleType& t) { return value_of(t); },
          [&](const lir::StructType& t) { return value_of(t); },
          [&](const lir::UnionType& t) { return value_of(t); },
          [&](const lir::TaggedUnionType& t) { return value_of(t); },
          [&](const lir::EmptyType& t) { return value_of(t); },
          // A machine integer is a computed value rather than a declaration's
          // storage, so no variable is one; a closure built with one holds it
          // as that integer, which is what a read of the capture hands back.
          [&](const lir::MachineIntType&)
              -> std::optional<support::MemberStorageKind> {
            switch (role) {
              case MemberSlotRole::kVariable:
                return std::nullopt;
              case MemberSlotRole::kSnapshot:
                return support::MemberStorageKind::kInlineValue;
            }
            throw InternalError("llvm codegen: unknown member slot role");
          },
          // The rest name no storage a member can be. Any other machine
          // primitive is a computed value no owner holds; an object-tree
          // node, a closure, a coroutine and a runtime facade are reached
          // through a handle, so a member holding one holds that handle and
          // arrives here as its own type; a wildcard index names where an index
          // goes (LRM 7.8.1) rather than a type a declaration is of; and `void`
          // has no runtime realization at all.
          [&](const lir::WildcardIndexType& t) { return none(t); },
          [&](const lir::MachineCStringType& t) { return none(t); },
          [&](const lir::MachineBoolType& t) { return none(t); },
          [&](const lir::MachineFloatType& t) { return none(t); },
          [&](const lir::MachineArrayType& t) { return none(t); },
          [&](const lir::VoidType& t) { return none(t); },
          [&](const lir::ObjectType& t) { return none(t); },
          [&](const lir::CrossUnitClassType& t) { return none(t); },
          [&](const lir::RuntimeClassType& t) { return none(t); },
          [&](const lir::ClosureType& t) { return none(t); },
          [&](const lir::RuntimeEffectsType& t) { return none(t); },
          [&](const lir::FilesType& t) { return none(t); },
          [&](const lir::DiagnosticType& t) { return none(t); },
          [&](const lir::CoroutineType& t) { return none(t); },
          // A write is open for the length of the full-expression doing it,
          // which no owner outlasts, and so is a part designated within it.
          [&](const lir::OpenWriteType& t) { return none(t); },
          [&](const lir::DesignationType& t) { return none(t); }});
}

auto DeclaredStorageOf(
    const lir::CompilationUnit& unit, lir::TypeId type, MemberSlotRole role)
    -> std::optional<support::DeclaredMemberStorage> {
  const std::optional<support::MemberStorageKind> kind =
      MemberStorageKindOf(unit, type, role);
  if (!kind) {
    return std::nullopt;
  }
  // Which type the domain is read from follows from the kind: the storage
  // forms that wrap a value name the type they wrap, and the ones that are the
  // value name the member's own type. A kind that holds no value reads none.
  const lir::Type& data = unit.types.Get(type);
  const auto domain_of = [&](lir::TypeId value) -> support::ValueDomain {
    const std::optional<support::ValueDomain> domain =
        ValueDomainOf(unit, value);
    if (!domain) {
      throw InternalError(
          "runtime abi: a storage kind naming a value domain was read from a "
          "type that has none");
    }
    return *domain;
  };
  const auto declared = [&](support::ValueDomain domain) {
    return support::DeclaredMemberStorage{.kind = *kind, .domain = domain};
  };
  switch (*kind) {
    case support::MemberStorageKind::kObservableCell:
      return declared(domain_of(data.Get<lir::ObservableType>().value));
    case support::MemberStorageKind::kResolvedNet:
      return declared(domain_of(data.Get<lir::ResolvedType>().value));
    case support::MemberStorageKind::kSampledHistory:
      return declared(domain_of(data.Get<lir::SampledHistoryType>().value));
    case support::MemberStorageKind::kValueCell:
      return declared(domain_of(type));
    // A machine integer held inline is no value of the library's, so the slot
    // holding one has no domain to state and states the empty one.
    case support::MemberStorageKind::kInlineValue:
      return declared(
          ValueDomainOf(unit, type).value_or(support::ValueDomain::kEmpty));
    case support::MemberStorageKind::kBorrowedHandle:
    case support::MemberStorageKind::kReference:
    case support::MemberStorageKind::kSharedPointer:
    case support::MemberStorageKind::kNamedEvent:
    case support::MemberStorageKind::kCancellationTarget:
    case support::MemberStorageKind::kChannelCancellation:
    case support::MemberStorageKind::kEvaluationAttempts:
      return declared(support::ValueDomain::kEmpty);
  }
  throw InternalError("runtime abi: unknown member storage kind");
}

auto RuntimeSymbol(support::ValueDomain domain, lir::ValueCellTarget::Op op)
    -> std::string {
  ToldOf(domain, op);
  return Symbol(domain, lir::ValueCellOpName(op));
}

auto RuntimeSymbol(support::ValueDomain domain, lir::OpenWriteTarget::Op op)
    -> std::string {
  ToldOf(domain, op);
  return Symbol(domain, lir::OpenWriteOpName(op));
}

auto RuntimeSymbol(
    support::ValueDomain domain, lir::DesignatedBitsTarget::Op op)
    -> std::string {
  ToldOf(domain, op);
  return Symbol(domain, lir::DesignatedBitsOpName(op));
}

auto RuntimeSymbol(support::BuiltinFn fn) -> std::string {
  return Symbol(support::RuntimeEntryOf(fn).name);
}

auto RuntimeSymbol(support::ValueDomain domain, support::BuiltinFn fn)
    -> std::string {
  ToldOf(domain, fn);
  return Symbol(domain, support::RuntimeEntryOf(fn).name);
}

auto RuntimeSymbol(
    support::ValueDomain destination, support::BuiltinFn fn,
    support::ValueDomain source) -> std::string {
  const std::string_view name = support::RuntimeEntryOf(fn).name;
  const EntryNaming naming = EntryNamingOf(fn);
  for (const Conversion& one : Conversions(naming)) {
    if (one.destination == destination && one.source == source) {
      return Symbol(
          destination,
          std::format("{}_{}", name, support::ValueDomainName(source)));
    }
  }
  throw InternalError(Refusal(
      name, std::format(
                "{} out of {}", support::ValueDomainName(destination),
                support::ValueDomainName(source))));
}

auto RuntimeSymbol(
    support::ValueDomain domain, WrapperKind wrapper, support::BuiltinFn fn)
    -> std::string {
  ToldOf(domain, wrapper, fn);
  return Symbol(
      domain,
      std::format(
          "{}_{}", WrapperName(wrapper), support::RuntimeEntryOf(fn).name));
}

auto OperandReadingsOf(RuntimeOp op) -> support::OperandReadings {
  using enum support::OperandReading;
  switch (op) {
    // The aggregate, which member, and the value that member is to hold (LRM
    // 7.3).
    case RuntimeOp::kWithComponent:
      return {kMachine, kMachine, kHeld};
    // A character is the one element that is a view of its whole (LRM 6.16.2):
    // the string, where the character goes, and the number it is to hold.
    case RuntimeOp::kWithElement:
      return {kMachine, kPosition, kNumber};
    // The array, where the elements start, how many there are, and the
    // elements that take their place (LRM 7.4.6).
    case RuntimeOp::kWithSlice:
      return {kMachine, kPosition, kMachine, kHeld};
    // A container is built over an element list laid down a stated number of
    // times, and holds elements of a type nothing else states, so it takes a
    // value of that type first (LRM 7.5.1, 7.10).
    case RuntimeOp::kFromLiteral:
    case RuntimeOp::kFromLiteralBounded:
    case RuntimeOp::kMakeDpiOpenArray:
    case RuntimeOp::kMakeWildcardIndex:
      return {kTyped};
    // That value, the entries, and what a read of an index holding no entry
    // yields (LRM 7.8.6).
    case RuntimeOp::kFromEntriesDefault:
    case RuntimeOp::kFromEntriesDefaultWildcard:
      return {kTyped, kMachine, kHeld};
    case RuntimeOp::kMakeIntegralPrintValueItem:
    case RuntimeOp::kMakeIntegralFormatArg:
      return {kNumber};
    // The image of a vector in the form a foreign function reads (LRM
    // 35.5.6.1).
    case RuntimeOp::kMakeDpiBitBuffer:
    case RuntimeOp::kMakeDpiLogicBuffer:
    // The integral value whose bits go into a stream, ahead of the stream.
    case RuntimeOp::kStreamWrite:
      return {kBits};
    case RuntimeOp::kCellRefer:
    case RuntimeOp::kReferStorage:
    case RuntimeOp::kRunProgram:
    case RuntimeOp::kSequenceMake:
    case RuntimeOp::kSequenceElement:
    case RuntimeOp::kClosureMake:
    case RuntimeOp::kObjectAdopt:
    case RuntimeOp::kSharedCellMake:
    case RuntimeOp::kSharedPointerDeref:
    case RuntimeOp::kHandleView:
    case RuntimeOp::kHandleWithView:
    case RuntimeOp::kConst:
    case RuntimeOp::kToBool:
    case RuntimeOp::kMake:
    case RuntimeOp::kDefault:
    case RuntimeOp::kMakeSegment:
    case RuntimeOp::kMakeTrigger:
    case RuntimeOp::kMakePrintLiteralItem:
    case RuntimeOp::kMakePrintValueItem:
    case RuntimeOp::kMakeFormatSpec:
    case RuntimeOp::kMakeFormatArg:
    case RuntimeOp::kSettleDeparture:
    case RuntimeOp::kStreamRead:
    case RuntimeOp::kConstruct:
    case RuntimeOp::kDestroy:
    case RuntimeOp::kCopy:
    case RuntimeOp::kMove:
    case RuntimeOp::kAssign:
      return {};
  }
  throw InternalError("llvm codegen: unknown runtime operation");
}

auto AnswerToldOf(RuntimeOp op) -> support::AnswerTold {
  switch (op) {
    // The integral value planes hold is laid out at a width and with the
    // states no operand fixes (LRM 6.24.3).
    case RuntimeOp::kStreamRead:
      return support::AnswerTold::kIntegralExtent;
    case RuntimeOp::kCellRefer:
    case RuntimeOp::kReferStorage:
    case RuntimeOp::kRunProgram:
    case RuntimeOp::kSequenceMake:
    case RuntimeOp::kSequenceElement:
    case RuntimeOp::kClosureMake:
    case RuntimeOp::kObjectAdopt:
    case RuntimeOp::kSharedCellMake:
    case RuntimeOp::kSharedPointerDeref:
    case RuntimeOp::kHandleView:
    case RuntimeOp::kHandleWithView:
    case RuntimeOp::kConst:
    case RuntimeOp::kToBool:
    case RuntimeOp::kMake:
    case RuntimeOp::kMakeWildcardIndex:
    case RuntimeOp::kWithComponent:
    case RuntimeOp::kWithElement:
    case RuntimeOp::kWithSlice:
    case RuntimeOp::kDefault:
    case RuntimeOp::kFromLiteral:
    case RuntimeOp::kFromLiteralBounded:
    case RuntimeOp::kFromEntriesDefault:
    case RuntimeOp::kFromEntriesDefaultWildcard:
    case RuntimeOp::kMakeSegment:
    case RuntimeOp::kMakeTrigger:
    case RuntimeOp::kMakePrintLiteralItem:
    case RuntimeOp::kMakePrintValueItem:
    case RuntimeOp::kMakeIntegralPrintValueItem:
    case RuntimeOp::kMakeFormatSpec:
    case RuntimeOp::kMakeFormatArg:
    case RuntimeOp::kMakeIntegralFormatArg:
    case RuntimeOp::kMakeDpiBitBuffer:
    case RuntimeOp::kMakeDpiLogicBuffer:
    case RuntimeOp::kMakeDpiOpenArray:
    case RuntimeOp::kSettleDeparture:
    case RuntimeOp::kStreamWrite:
    case RuntimeOp::kConstruct:
    case RuntimeOp::kDestroy:
    case RuntimeOp::kCopy:
    case RuntimeOp::kMove:
    case RuntimeOp::kAssign:
      return support::AnswerTold::kNothing;
  }
  throw InternalError("llvm codegen: unknown runtime operation");
}

auto OperandReadingsOf(lir::ValueCellTarget::Op op)
    -> support::OperandReadings {
  using enum support::OperandReading;
  switch (op) {
    case lir::ValueCellTarget::Op::kAllocate:
    case lir::ValueCellTarget::Op::kLoad:
      return {};
    // The cell, then the value it is to hold.
    case lir::ValueCellTarget::Op::kStore:
      return {kMachine, kHeld};
  }
  throw InternalError("llvm codegen: unknown value cell operation");
}

auto OperandReadingsOf(lir::OpenWriteTarget::Op op)
    -> support::OperandReadings {
  using enum support::OperandReading;
  // Each acts on what a designation names. A write of a run of elements takes
  // where they start, how many there are, and the elements, which the
  // container holds the type of.
  switch (op) {
    case lir::OpenWriteTarget::Op::kLand:
      return {};
    case lir::OpenWriteTarget::Op::kAssignSlice:
      return {kMachine, kPosition, kMachine, kHeld};
    case lir::OpenWriteTarget::Op::kReadSlice:
      return {kMachine, kPosition};
  }
  throw InternalError("llvm codegen: unknown open-write operation");
}

auto RealizationsOf(RuntimeOp op) -> std::span<const RealizedFor> {
  switch (op) {
    case RuntimeOp::kCellRefer:
    case RuntimeOp::kSharedCellMake:
      return kForVariables;
    case RuntimeOp::kFromEntriesDefault:
    case RuntimeOp::kFromEntriesDefaultWildcard:
      return kForAssociativeArrays;
    case RuntimeOp::kDefault:
      return kForWhatIsNull;
    case RuntimeOp::kFromLiteral:
      return kForSequences;
    case RuntimeOp::kFromLiteralBounded:
      return kForQueues;
    case RuntimeOp::kConst:
      return kForReals;
    case RuntimeOp::kToBool:
      return kForWhatIsTrueOrFalse;
    case RuntimeOp::kWithElement:
      return kForStrings;
    case RuntimeOp::kMakePrintValueItem:
    case RuntimeOp::kMakeFormatArg:
      return kForWhatFormatsItself;
    case RuntimeOp::kWithComponent:
      return kForUnionsUpdated;
    case RuntimeOp::kMake:
      return kForWhatAHostValueBuilds;
    case RuntimeOp::kCopy:
    case RuntimeOp::kMove:
    case RuntimeOp::kAssign:
      return kForLibraryObjects;
    case RuntimeOp::kDestroy:
      return kForWhatEndsWithWork;
    // Realized once, or per object or member storage and never per value
    // domain alone. No value library realizes a run of elements replaced in a
    // value: a write of one goes through the write's own slice write.
    case RuntimeOp::kReferStorage:
    case RuntimeOp::kRunProgram:
    case RuntimeOp::kSequenceMake:
    case RuntimeOp::kSequenceElement:
    case RuntimeOp::kClosureMake:
    case RuntimeOp::kObjectAdopt:
    case RuntimeOp::kSharedPointerDeref:
    case RuntimeOp::kHandleView:
    case RuntimeOp::kHandleWithView:
    case RuntimeOp::kMakeWildcardIndex:
    case RuntimeOp::kWithSlice:
    case RuntimeOp::kMakeSegment:
    case RuntimeOp::kMakeTrigger:
    case RuntimeOp::kMakePrintLiteralItem:
    case RuntimeOp::kMakeIntegralPrintValueItem:
    case RuntimeOp::kMakeFormatSpec:
    case RuntimeOp::kMakeIntegralFormatArg:
    case RuntimeOp::kMakeDpiBitBuffer:
    case RuntimeOp::kMakeDpiLogicBuffer:
    case RuntimeOp::kMakeDpiOpenArray:
    case RuntimeOp::kSettleDeparture:
    case RuntimeOp::kStreamWrite:
    case RuntimeOp::kStreamRead:
    case RuntimeOp::kConstruct:
      return {};
  }
  throw InternalError("llvm codegen: unknown runtime operation");
}

auto RealizationsOf(lir::ValueCellTarget::Op op)
    -> std::span<const RealizedFor> {
  switch (op) {
    case lir::ValueCellTarget::Op::kAllocate:
      return kForCellsAllocated;
    case lir::ValueCellTarget::Op::kLoad:
    case lir::ValueCellTarget::Op::kStore:
      return kForCellsReached;
  }
  throw InternalError("llvm codegen: unknown value cell operation");
}

auto RealizationsOf(lir::OpenWriteTarget::Op op)
    -> std::span<const RealizedFor> {
  switch (op) {
    case lir::OpenWriteTarget::Op::kLand:
      return kForLandings;
    case lir::OpenWriteTarget::Op::kAssignSlice:
      return kForArraysOfFixedExtent;
    // No container lends a run of its elements within a write.
    case lir::OpenWriteTarget::Op::kReadSlice:
      return {};
  }
  throw InternalError("llvm codegen: unknown open-write operation");
}

auto OperandReadingsOf(lir::DesignatedBitsTarget::Op op)
    -> support::OperandReadings {
  using enum support::OperandReading;
  // Each takes the designation and where the bits start. Placing takes the
  // bits, which the operation over integral values reads, and reporting the
  // whole value as it stands with them in it.
  switch (op) {
    case lir::DesignatedBitsTarget::Op::kRead:
      return {kMachine, kPosition};
    case lir::DesignatedBitsTarget::Op::kPlace:
      return {kMachine, kPosition, kBits};
    case lir::DesignatedBitsTarget::Op::kReport:
      return {kMachine, kPosition, kHeld};
  }
  throw InternalError("llvm codegen: unknown designated-bits operation");
}

auto AnswerToldOf(lir::DesignatedBitsTarget::Op op) -> support::AnswerTold {
  switch (op) {
    case lir::DesignatedBitsTarget::Op::kRead:
    case lir::DesignatedBitsTarget::Op::kPlace:
      return support::AnswerTold::kNothing;
    // How many bits were written, which is how wide the type they were
    // written at is.
    case lir::DesignatedBitsTarget::Op::kReport:
      return support::AnswerTold::kIntegralWidth;
  }
  throw InternalError("llvm codegen: unknown designated-bits operation");
}

auto RealizationsOf(lir::DesignatedBitsTarget::Op op)
    -> std::span<const RealizedFor> {
  switch (op) {
    // Bits are read and placed by the operation over integral values, which
    // is no entry named here.
    case lir::DesignatedBitsTarget::Op::kRead:
    case lir::DesignatedBitsTarget::Op::kPlace:
      return {};
    case lir::DesignatedBitsTarget::Op::kReport:
      return kForBitsReported;
  }
  throw InternalError("llvm codegen: unknown designated-bits operation");
}

auto EntryNamingOf(support::BuiltinFn fn) -> EntryNaming {
  constexpr std::string_view kLeavesByUnwinding =
      "leaves a body by unwinding it, where this target's bodies leave by "
      "branching";
  constexpr std::string_view kWrittenByTheSliceWrite =
      "designates a slice within a write, which this target writes by the "
      "write's own slice write";
  switch (fn) {
    // The operations over integral values alone (LRM 11.4, 11.5.1, 20.9).
    case support::BuiltinFn::kReverseBlocks:
    case support::BuiltinFn::kClog2:
    case support::BuiltinFn::kBitwiseXnor:
    case support::BuiltinFn::kWildcardEquals:
    case support::BuiltinFn::kCasezEquals:
    case support::BuiltinFn::kCasexEquals:
    case support::BuiltinFn::kReductionAnd:
    case support::BuiltinFn::kReductionOr:
    case support::BuiltinFn::kReductionXor:
    case support::BuiltinFn::kReductionNand:
    case support::BuiltinFn::kReductionNor:
    case support::BuiltinFn::kReductionXnor:
    case support::BuiltinFn::kToInt64:
    case support::BuiltinFn::kLogicalEquivalence:
    case support::BuiltinFn::kShiftLeft:
    case support::BuiltinFn::kLogicalShiftRight:
    case support::BuiltinFn::kArithmeticShiftRight:
    case support::BuiltinFn::kReplicateBits:
    case support::BuiltinFn::kSlice:
    case support::BuiltinFn::kFromBool:
    case support::BuiltinFn::kToPosition:
    case support::BuiltinFn::kIntegralFromString:
    case support::BuiltinFn::kFromSvLogic:
    case support::BuiltinFn::kReadCanonicalBitVec:
    case support::BuiltinFn::kReadCanonicalLogicVec:
    case support::BuiltinFn::kConcatBits:
    case support::BuiltinFn::kIntegralPow:
    case support::BuiltinFn::kIntegralCaseEqual:
    case support::BuiltinFn::kIntegralBitIdentical:
    case support::BuiltinFn::kIntegralHasUnknown:
    case support::BuiltinFn::kIntegralIsUnknown:
    case support::BuiltinFn::kIntegralCountBits:
    case support::BuiltinFn::kIntegralResolveTriState:
    case support::BuiltinFn::kIntegralResolveWiredAnd:
    case support::BuiltinFn::kIntegralResolveWiredOr:
    case support::BuiltinFn::kIntegralDominating:
    case support::BuiltinFn::kIntegralMergeConditional:
    case support::BuiltinFn::kIntegralFromInt:
    case support::BuiltinFn::kIntegralConvert:
      return OverIntegralValues{};

    // A part of a value that is storage of its own is reached where it lies by
    // a place, so no library realizes the access that answers with one.
    case support::BuiltinFn::kSliceRef:
    case support::BuiltinFn::kComponentRef:
      return NamedByValue{};

    case support::BuiltinFn::kExists:
    case support::BuiltinFn::kAssocFirst:
    case support::BuiltinFn::kAssocLast:
    case support::BuiltinFn::kAssocNext:
    case support::BuiltinFn::kAssocPrev:
    case support::BuiltinFn::kAssocMinIndex:
    case support::BuiltinFn::kAssocMaxIndex:
      return NamedByValue{.realized = kForAssociativeArrays};

    // LRM 7.6 assignment between unpacked array kinds, one entry per
    // destination kind, reads its source through the representation the source
    // has.
    case support::BuiltinFn::kUnpackedArrayFromArray:
    case support::BuiltinFn::kArrayConcatElement:
    case support::BuiltinFn::kArrayConcatSpread:
      return NamedByValue{.realized = kForDynamicArraysAndQueues};
    case support::BuiltinFn::kQueueFromArray:
    case support::BuiltinFn::kElementSlice:
    case support::BuiltinFn::kElementSliceRef:
      return NamedByValue{.realized = kForArraysOfFixedExtent};
    case support::BuiltinFn::kDynamicArrayFromArray:
      return NamedByValue{.realized = kForUnpackedArraysAndQueues};

    case support::BuiltinFn::kDelete:
      return NamedByValue{.realized = kForWhatDeletesWhole};

    case support::BuiltinFn::kReverse:
    case support::BuiltinFn::kSort:
    case support::BuiltinFn::kRsort:
    case support::BuiltinFn::kElementRef:
      return NamedByValue{.realized = kForSequences};

    case support::BuiltinFn::kSize:
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
      return NamedByValue{.realized = kForContainers};

    case support::BuiltinFn::kInsert:
    case support::BuiltinFn::kPopFront:
    case support::BuiltinFn::kPopBack:
    case support::BuiltinFn::kPushFront:
    case support::BuiltinFn::kPushBack:
    case support::BuiltinFn::kConformBound:
    case support::BuiltinFn::kDeleteIndex:
      return NamedByValue{.realized = kForQueues};

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
    case support::BuiltinFn::kTruncate:
      return NamedByValue{.realized = kForReal};
    case support::BuiltinFn::kRound:
    case support::BuiltinFn::kToBits:
    case support::BuiltinFn::kRealValue:
    case support::BuiltinFn::kPow:
      return NamedByValue{.realized = kForReals};

    case support::BuiltinFn::kLen:
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
    case support::BuiltinFn::kScanString:
    case support::BuiltinFn::kScanFile:
    case support::BuiltinFn::kConcat:
      return NamedByValue{.realized = kForStrings};

    case support::BuiltinFn::kElement:
      return NamedByValue{.realized = kForWhatNumbersItsElements};
    case support::BuiltinFn::kCaseEqual:
      return NamedByValue{.realized = kForWhatCaseEqualityCompares};
    case support::BuiltinFn::kBitIdentical:
    case support::BuiltinFn::kHasUnknown:
      return NamedByValue{.realized = kForWhatIsComparedBitForBit};
    case support::BuiltinFn::kBitstreamWidth:
    case support::BuiltinFn::kToBitstream:
    case support::BuiltinFn::kCountBits:
      return NamedByValue{.realized = kForBitstreams};
    case support::BuiltinFn::kTagMatches:
      return NamedByValue{.realized = kForTaggedUnions};
    case support::BuiltinFn::kComponent:
      return NamedByValue{.realized = kForUnions};
    case support::BuiltinFn::kIsUnknown:
      return NamedByValue{.realized = kForWhatMayHoldUnknowns};
    case support::BuiltinFn::kResolveTriState:
    case support::BuiltinFn::kResolveWiredAnd:
    case support::BuiltinFn::kResolveWiredOr:
    case support::BuiltinFn::kDominating:
    case support::BuiltinFn::kFilledLike:
      return NamedByValue{.realized = kForWhatAnAggregateNetResolves};
    case support::BuiltinFn::kMergeConditional:
      return NamedByValue{.realized = kForUnpackedArrays};

    // The factories. Each builds a value out of operands that are the material
    // of one -- a machine integer, a byte array, the member a union is to hold
    // -- so none of them carries the representation the entry is realized for,
    // and only what the call answers with does.
    case support::BuiltinFn::kMakeActiveMember:
      return NamedByResult{.realized = kForUnionsBuilt};
    case support::BuiltinFn::kFromByteArray:
      return NamedByResult{.realized = kForStrings};
    case support::BuiltinFn::kArrayConformSize:
      return NamedByResult{.realized = kForUnpackedArrays};
    case support::BuiltinFn::kFromBits:
    case support::BuiltinFn::kFromInt:
      return NamedByResult{.realized = kForReals};

    // LRM 6.12.1: a real of one precision as a real of the other, whose
    // realization depends on both.
    case support::BuiltinFn::kConvertFrom:
      return NamedByConversion{.realized = kRealConversions};

    // The accesses a capability wrapper defines. Which wrapper the call acts
    // on decides both which family answers and which representation it answers
    // in, so neither half is the call's own. Arming a cell and reading what it
    // retained (LRM 16.5.1) are two more of them: what a sampled read yields is
    // the wrapper's own decision about which of the two values it holds to
    // answer with, which is the same operation on the same storage as an
    // ordinary read.
    case support::BuiltinFn::kInitialize:
      return NamedByWrapper{.realized = kInstalls};
    case support::BuiltinFn::kLoad:
      return NamedByWrapper{.realized = kLoads};
    case support::BuiltinFn::kSampledLoad:
      return NamedByWrapper{.realized = kSampledLoads};
    case support::BuiltinFn::kArmSampling:
      return NamedByWrapper{.realized = kArmings};
    // A takeover acts on the cell it covers and carries that cell's value, so
    // the wrapper it is reached through is what names the entry (LRM 10.6).
    case support::BuiltinFn::kBeginTakeover:
    case support::BuiltinFn::kDriveTakeover:
    case support::BuiltinFn::kEndTakeover:
      return NamedByWrapper{.realized = kTakeovers};
    // Opening a write reaches the contents of the wrapper it is opened on, and
    // what the write reports when it ends is that wrapper's own business.
    case support::BuiltinFn::kStore:
    case support::BuiltinFn::kOpenForWrite:
      return NamedByWrapper{.realized = kWholeWrites};

    // A driver is attached by the net that issues it, a fold is installed on
    // the net that applies it, and a join takes two nets that resolve in the
    // same representation, so in each case what names the entry is the
    // representation those nets resolve in. A history's three operations
    // likewise take the storage they act on and are named by the one domain
    // every value in it is realized in (LRM 16.9.3).
    case support::BuiltinFn::kAttachDriver:
      return NamedByStorageDomain{.realized = kForNets};
    case support::BuiltinFn::kNetJoin:
      return NamedByStorageDomain{.realized = kForJoinedNets};
    case support::BuiltinFn::kNetInitializeTriState:
    case support::BuiltinFn::kNetInitializeWiredAnd:
    case support::BuiltinFn::kNetInitializeWiredOr:
    case support::BuiltinFn::kNetInitializeRetaining:
      return NamedByStorageDomain{.realized = kForIntegralNets};
    case support::BuiltinFn::kAggregateNetInitializeTriState:
    case support::BuiltinFn::kAggregateNetInitializeWiredAnd:
    case support::BuiltinFn::kAggregateNetInitializeWiredOr:
    case support::BuiltinFn::kAggregateNetInitializeRetaining:
      return NamedByStorageDomain{.realized = kForAggregateNets};
    case support::BuiltinFn::kSampledHistoryInstall:
      return NamedByStorageDomain{.realized = kForHistoriesInstalled};
    case support::BuiltinFn::kSampledHistoryPush:
    case support::BuiltinFn::kSampledHistoryAt:
      return NamedByStorageDomain{.realized = kForHistories};
    // A step within a write is taken on a designation, and one lending a part
    // on a reference; each is named by the representation of the value
    // designated or referred to there.
    case support::BuiltinFn::kDesignateElement:
    case support::BuiltinFn::kReferElement:
      return NamedByStorageDomain{.realized = kForSequences};
    case support::BuiltinFn::kDesignateComponent:
    case support::BuiltinFn::kReferComponent:
      return NamedByStorageDomain{.realized = kForTuples};

    // A body that leaves by branching reaches the gate as the pair of questions
    // the lowering already asks wherever this execution regains control -- and
    // it asks them here in place of this call rather than through an entry of
    // its own.
    case support::BuiltinFn::kTakeDepartureIfDue:
      return NotRealized{.shape = kLeavesByUnwinding};

    // Several bits or elements are no one place, so a slice designated within a
    // write is written by the write's own slice write, which the lowering
    // reaches in place of this call.
    case support::BuiltinFn::kDesignateSlice:
    case support::BuiltinFn::kDesignateElementSlice:
      return NotRealized{.shape = kWrittenByTheSliceWrite};

    // The runtime leads, and the memory whose addressing names the entry
    // follows.
    case support::BuiltinFn::kReadMem:
    case support::BuiltinFn::kReadMemWithin:
      return NamedByValue{.operand = 1, .realized = kForLoadedMemories};
    case support::BuiltinFn::kWriteMem:
    case support::BuiltinFn::kWriteMemWithin:
      return NamedByValue{.operand = 1, .realized = kForContainers};

    // Designating the whole of what a write was opened on reads nothing of the
    // value there, so the library realizes it once.
    case support::BuiltinFn::kDesignateWhole:
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
    case support::BuiltinFn::kMakeRenderedFormatArg:
    case support::BuiltinFn::kMakePatternedFormatArg:
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
    case support::BuiltinFn::kValuePlusargs:
    case support::BuiltinFn::kValuePlusargsString:
    case support::BuiltinFn::kRunHostCommand:
    case support::BuiltinFn::kRunNullHostCommand:
    case support::BuiltinFn::kDelay:
    case support::BuiltinFn::kDelayReal:
    case support::BuiltinFn::kObservationOnReaching:
    case support::BuiltinFn::kObservationOfValue:
    case support::BuiltinFn::kObservationOfValueQualified:
    case support::BuiltinFn::kObservationQualified:
    case support::BuiltinFn::kObservationFires:
    case support::BuiltinFn::kWaitOn:
    case support::BuiltinFn::kWaitOnImplicitList:
    case support::BuiltinFn::kParkAt:
    case support::BuiltinFn::kWaitRecollecting:
    case support::BuiltinFn::kWaitUntil:
    case support::BuiltinFn::kReadReportEmpty:
    case support::BuiltinFn::kReadReportForImplicitList:
    case support::BuiltinFn::kReadReportAdd:
    case support::BuiltinFn::kReadReportAddThroughHandle:
    case support::BuiltinFn::kReadReportEnterCallOnHandle:
    case support::BuiltinFn::kReadReportLeaveCallOnHandle:
    case support::BuiltinFn::kReadReportAddEveryObject:
    case support::BuiltinFn::kReadReportAddWrite:
    case support::BuiltinFn::kReadReportSettleAsImplicitList:
    case support::BuiltinFn::kReadReportEnter:
    case support::BuiltinFn::kReadReportLeave:
    case support::BuiltinFn::kReadReportRunsTheBody:
    case support::BuiltinFn::kRefuseReport:
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
    case support::BuiltinFn::kEnclosingScope:
    case support::BuiltinFn::kIsOfClass:
    case support::BuiltinFn::kAddOwnedChild:
    case support::BuiltinFn::kExtendSequence:
    // Recovering the object a handle names, or a handle naming the object a
    // body runs on. Each is one library function serving every class, so the
    // operation's own name is the whole of what a symbol needs.
    case support::BuiltinFn::kViewOf:
    case support::BuiltinFn::kSelfHandle:
    // What reports a change to an object's properties, one library function
    // each for every class.
    case support::BuiltinFn::kObjectEventSource:
    case support::BuiltinFn::kOpenObjectWrite:
    case support::BuiltinFn::kWrittenObject:
    // A reference to a property holds the property erased, so one function
    // serves every property's type; and what any reference reports to is a
    // fact of the reference, whatever it names.
    case support::BuiltinFn::kReferProperty:
    case support::BuiltinFn::kReferenceReportsTo:
    // Binding a member and forcing one act on the reference alone.
    case support::BuiltinFn::kBindMember:
    case support::BuiltinFn::kBeginForce:
    case support::BuiltinFn::kRetargetMember:
    case support::BuiltinFn::kStillForcing:
    case support::BuiltinFn::kForceEnded:
    case support::BuiltinFn::kDriverOfMember:
    case support::BuiltinFn::kReleaseMember:
    case support::BuiltinFn::kReestablishedOf:
    // What an enumeration's member list answers about a value. One routine
    // serves every enumeration, because the list is the receiver and every
    // member is an integral value.
    case support::BuiltinFn::kEnumerationHas:
    case support::BuiltinFn::kEnumerationName:
    case support::BuiltinFn::kEnumerationNext:
    case support::BuiltinFn::kEnumerationPrev:
    case support::BuiltinFn::kForkWaitAll:
    case support::BuiltinFn::kForkWaitFirst:
    case support::BuiltinFn::kSpawnAll:
    case support::BuiltinFn::kWaitFork:
    case support::BuiltinFn::kDisableFork:
    case support::BuiltinFn::kDisable:
    case support::BuiltinFn::kRegisterInitial:
    case support::BuiltinFn::kRegisterFinal:
    case support::BuiltinFn::kEnterScopeStaticInit:
    case support::BuiltinFn::kEnterNamespaceStaticInit:
    case support::BuiltinFn::kLeaveStaticInit:
    case support::BuiltinFn::kEnterDpiScope:
    case support::BuiltinFn::kLeaveDpiScope:
    case support::BuiltinFn::kDisableIsActive:
    case support::BuiltinFn::kCheckImportTaskAcknowledged:
    case support::BuiltinFn::kCheckImportFunctionAcknowledged:
    case support::BuiltinFn::kCheckExportReachable:
    case support::BuiltinFn::kClaimNamespaceInitialize:
    case support::BuiltinFn::kCurrentExportScope:
    case support::BuiltinFn::kFindExportEntry:
    case support::BuiltinFn::kRunForeignTaskOnFiber:
    case support::BuiltinFn::kRunExportedTaskToCompletion:
    case support::BuiltinFn::kMakeDynamicArrayDefault:
    case support::BuiltinFn::kMakeDynamicArrayNew:
    case support::BuiltinFn::kMakeDynamicArrayNewCopy:
    case support::BuiltinFn::kEnterTarget:
    case support::BuiltinFn::kLeaveTarget:
    case support::BuiltinFn::kEffectNamesTarget:
    case support::BuiltinFn::kReceiveDeparture:
    case support::BuiltinFn::kProcessSelf:
    case support::BuiltinFn::kProcessStatus:
    case support::BuiltinFn::kProcessKill:
    case support::BuiltinFn::kProcessAwait:
    case support::BuiltinFn::kProcessSuspend:
    case support::BuiltinFn::kProcessResume:
    case support::BuiltinFn::kParent:
    case support::BuiltinFn::kHierarchicalPath:
    // An assertion's attempts hold machine words and no value of the design,
    // so there is no representation for these to be named by (LRM 16.14.1).
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
    // Reading the host representation a value carries. Only one domain carries
    // each of these -- a C string is a string's, a host pointer a chandle's --
    // so the operation's own name is the whole of what a symbol needs, where
    // reading a host float out of a value serves two and takes the domain to
    // tell them apart.
    case support::BuiltinFn::kStringCStr:
    case support::BuiltinFn::kChandlePtr:
    // A guard the language requires to run as part of evaluating an access
    // (LRM 11.3.5). It reads the condition and yields what it was handed, so
    // what it guards decides nothing about the code: one realization serves
    // every value, and serves a cell reached through the same access too.
    case support::BuiltinFn::kRequire:
    // A value built from a stream of bits under a prototype (LRM 6.24.3), asked
    // of the type the prototype crosses with, so one function serves every
    // type.
    case support::BuiltinFn::kFromBitstream:
    // The DPI-C boundary marshaling (LRM 35.5.6, Annex H.7.7, H.10). Each of
    // these is one library function and not a family: what a canonical buffer,
    // an `svLogic` scalar and an open-array image hold is fixed by the C ABI,
    // so the SV value on the far side of one is always an integral value and
    // the buffer itself belongs to no value domain at all.
    case support::BuiltinFn::kToSvLogic:
    case support::BuiltinFn::kWriteCanonicalBitVec:
    case support::BuiltinFn::kWriteCanonicalLogicVec:
    case support::BuiltinFn::kDpiBitBufferData:
    case support::BuiltinFn::kDpiLogicBufferData:
    case support::BuiltinFn::kDpiOpenArrayHandle:
    case support::BuiltinFn::kDpiOpenArrayValue:
    // An operation whose own name says the one kind of value it is over or
    // builds: a queue's slice (LRM 7.10.1), a string repeated or built out of
    // bits (LRM 11.4.12.1, 6.16), an unpacked array of byte built out of text
    // or of bits (LRM 5.9); an access into an associative array by its key
    // (LRM 7.8).
    case support::BuiltinFn::kAssocElement:
    case support::BuiltinFn::kAssocElementRef:
    case support::BuiltinFn::kAssocDesignateElement:
    case support::BuiltinFn::kAssocReferElement:
    case support::BuiltinFn::kAssocDeleteIndex:
    case support::BuiltinFn::kQueueSlice:
    case support::BuiltinFn::kReplicateString:
    case support::BuiltinFn::kStringFromBits:
    case support::BuiltinFn::kByteArrayFromString:
    case support::BuiltinFn::kByteArrayFromBits:
      return NamedAlone{};
  }
  throw InternalError("llvm codegen: unknown builtin");
}

}  // namespace lyra::backend::llvm_backend
