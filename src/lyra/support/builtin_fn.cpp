#include "lyra/support/builtin_fn.hpp"

#include <string_view>

#include "lyra/base/internal_error.hpp"
#include "lyra/support/integral_operation.hpp"

namespace lyra::support {

namespace {

// A function of the value library, and one of the runtime's.
constexpr auto ValueFunction(std::string_view identifier) -> FreeFunction {
  return FreeFunction{.scope = "lyra::value", .identifier = identifier};
}

constexpr auto RuntimeFunction(std::string_view identifier) -> FreeFunction {
  return FreeFunction{.scope = "lyra::runtime", .identifier = identifier};
}

// Whether an operation's operands leave the type it answers at unsettled, so
// an entry that is that operation is generic over it.
auto AnswersAtTheTypeOfTheCall(IntegralAnswer answer) -> bool {
  switch (answer) {
    case IntegralAnswer::kOfTheCall:
      return true;
    case IntegralAnswer::kOfFirstOperand:
    case IntegralAnswer::kOneBit:
    case IntegralAnswer::kTwoStateBit:
    case IntegralAnswer::kJoined:
    case IntegralAnswer::kInt:
    case IntegralAnswer::kInteger:
    case IntegralAnswer::kPosition:
    case IntegralAnswer::kMachineBool:
    case IntegralAnswer::kMachineInt:
      return false;
  }
  throw InternalError("RuntimeEntryOf: unknown integral answer");
}

// An entry that is the operation `op` over integral values and nothing else,
// reached as `declaration` says and selecting a part as `selects` says. It is
// spelled as the operation is, and what it takes is the operation's to say.
auto IntegralEntry(
    IntegralOp op, EntryDeclaration declaration,
    std::optional<PartSelection> selects = std::nullopt) -> RuntimeEntry {
  const IntegralOperation& operation = IntegralOperationOf(op);
  return {
      .name = operation.name,
      .declaration = declaration,
      .takes_a_type_argument = AnswersAtTheTypeOfTheCall(operation.answer),
      .selects = selects,
      .integral = op};
}

}  // namespace

auto RuntimeEntryOf(BuiltinFn id) -> RuntimeEntry {
  using enum OperandReading;
  switch (id) {
    case BuiltinFn::kElement:
      return {
          .name = "element",
          .declaration = Method{"Element"},
          .selects = PartSelection::kElement,
          .operands = {kMachine, kPosition}};
    case BuiltinFn::kAssocElement:
      return {
          .name = "assocarray_element",
          .declaration = Method{"Element"},
          .selects = PartSelection::kElement,
          .operands = {kMachine, kKey}};
    case BuiltinFn::kSlice:
      return IntegralEntry(
          IntegralOp::kSlice, ValueFunction("Slice"), PartSelection::kSlice);
    case BuiltinFn::kElementSlice:
      return {
          .name = "element_slice",
          .declaration = Method{"Slice"},
          .selects = PartSelection::kSlice,
          .operands = {kMachine, kPosition}};
    case BuiltinFn::kQueueSlice:
      return {
          .name = "queue_slice",
          .declaration = Method{"Slice"},
          .selects = PartSelection::kSlice,
          .operands = {kMachine, kPosition, kPosition}};
    case BuiltinFn::kElementRef:
      return {
          .name = "element_ref",
          .declaration = Method{"ElementRef"},
          .answer = EntryAnswer::kPartOfTheReceiver,
          .selects = PartSelection::kElement,
          .operands = {kMachine, kPosition}};
    case BuiltinFn::kAssocElementRef:
      return {
          .name = "assocarray_element_ref",
          .declaration = Method{"ElementRef"},
          .answer = EntryAnswer::kPartOfTheReceiver,
          .selects = PartSelection::kElement,
          .operands = {kMachine, kKey}};
    case BuiltinFn::kSliceRef:
      return {
          .name = "slice_ref",
          .declaration = Method{"SliceRef"},
          .takes_a_type_argument = true,
          .answer = EntryAnswer::kPartOfTheReceiver,
          .selects = PartSelection::kSlice,
          .operands = {kMachine, kPosition}};
    case BuiltinFn::kElementSliceRef:
      return {
          .name = "element_slice_ref",
          .declaration = Method{"SliceRef"},
          .answer = EntryAnswer::kPartOfTheReceiver,
          .selects = PartSelection::kSlice,
          .operands = {kMachine, kPosition, kMachine, kHeld}};
    case BuiltinFn::kComponent:
      return {
          .name = "component",
          .declaration = Method{"Component"},
          .selects = PartSelection::kComponent};
    case BuiltinFn::kComponentRef:
      return {
          .name = "component_ref",
          .declaration = Method{"ComponentRef"},
          .answer = EntryAnswer::kPartOfTheReceiver,
          .selects = PartSelection::kComponent};
    case BuiltinFn::kTagMatches:
      return {.name = "tag_matches", .declaration = Method{"IsTagged"}};
    case BuiltinFn::kMakeActiveMember:
      return {
          .name = "make",
          .declaration = StaticFactory{"Make"},
          .takes_a_type_argument = true,
          .operands = {kHeld}};
    case BuiltinFn::kRequire:
      return {
          .name = "require",
          .declaration = ValueFunction("Require"),
          .answer = EntryAnswer::kTheReceiver,
          .operands = {kHeld, kBits}};
    case BuiltinFn::kSize:
      return {.name = "size", .declaration = Method{"Size"}};
    case BuiltinFn::kLen:
      return {.name = "len", .declaration = Method{"Len"}};
    case BuiltinFn::kBitstreamWidth:
      return {
          .name = "bitstream_width", .declaration = Method{"BitstreamWidth"}};
    case BuiltinFn::kToBitstream:
      return {
          .name = "to_bitstream",
          .declaration = Method{"ToBitstream"},
          .takes_a_type_argument = true,
          .answer_told = AnswerTold::kIntegralExtent};
    case BuiltinFn::kFromBitstream:
      return {
          .name = "from_bitstream",
          .declaration = StaticFactory{"FromBitstream"},
          .operands = {kBits, kTyped}};
    case BuiltinFn::kReverseBlocks:
      return IntegralEntry(IntegralOp::kReverseBlocks, Method{"ReverseBlocks"});
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
          .operands = {kMachine, kPosition}};
    case BuiltinFn::kAssocDeleteIndex:
      return {
          .name = "assocarray_delete_index",
          .declaration = Method{"DeleteIndex"},
          .mutates_receiver = true,
          .operands = {kMachine, kKey}};
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
          .operands = {kMachine, kMachine, kTyped},
          .answer_starts_from = 2};
    case BuiltinFn::kProduct:
      return {
          .name = "product",
          .declaration = Method{"Product"},
          .takes_closure = true,
          .operands = {kMachine, kMachine, kTyped},
          .answer_starts_from = 2};
    case BuiltinFn::kAnd:
      return {
          .name = "and",
          .declaration = Method{"And"},
          .takes_closure = true,
          .operands = {kMachine, kMachine, kTyped},
          .answer_starts_from = 2};
    case BuiltinFn::kOr:
      return {
          .name = "or",
          .declaration = Method{"Or"},
          .takes_closure = true,
          .operands = {kMachine, kMachine, kTyped},
          .answer_starts_from = 2};
    case BuiltinFn::kXor:
      return {
          .name = "xor",
          .declaration = Method{"Xor"},
          .takes_closure = true,
          .operands = {kMachine, kMachine, kTyped},
          .answer_starts_from = 2};
    case BuiltinFn::kFind:
      return {
          .name = "find",
          .declaration = Method{"Find"},
          .takes_closure = true,
          .operands = {kMachine, kMachine, kTyped},
          .answer_starts_from = 2};
    case BuiltinFn::kFindIndex:
      return {
          .name = "find_index",
          .declaration = Method{"FindIndex"},
          .takes_closure = true,
          .operands = {kMachine, kMachine, kTyped},
          .answer_starts_from = 2};
    case BuiltinFn::kFindFirst:
      return {
          .name = "find_first",
          .declaration = Method{"FindFirst"},
          .takes_closure = true,
          .operands = {kMachine, kMachine, kTyped},
          .answer_starts_from = 2};
    case BuiltinFn::kFindFirstIndex:
      return {
          .name = "find_first_index",
          .declaration = Method{"FindFirstIndex"},
          .takes_closure = true,
          .operands = {kMachine, kMachine, kTyped},
          .answer_starts_from = 2};
    case BuiltinFn::kFindLast:
      return {
          .name = "find_last",
          .declaration = Method{"FindLast"},
          .takes_closure = true,
          .operands = {kMachine, kMachine, kTyped},
          .answer_starts_from = 2};
    case BuiltinFn::kFindLastIndex:
      return {
          .name = "find_last_index",
          .declaration = Method{"FindLastIndex"},
          .takes_closure = true,
          .operands = {kMachine, kMachine, kTyped},
          .answer_starts_from = 2};
    case BuiltinFn::kMin:
      return {
          .name = "min",
          .declaration = Method{"Min"},
          .takes_closure = true,
          .operands = {kMachine, kMachine, kTyped},
          .answer_starts_from = 2};
    case BuiltinFn::kMax:
      return {
          .name = "max",
          .declaration = Method{"Max"},
          .takes_closure = true,
          .operands = {kMachine, kMachine, kTyped},
          .answer_starts_from = 2};
    case BuiltinFn::kUnique:
      return {
          .name = "unique",
          .declaration = Method{"Unique"},
          .takes_closure = true,
          .operands = {kMachine, kMachine, kTyped},
          .answer_starts_from = 2};
    case BuiltinFn::kUniqueIndex:
      return {
          .name = "unique_index",
          .declaration = Method{"UniqueIndex"},
          .takes_closure = true,
          .operands = {kMachine, kMachine, kTyped},
          .answer_starts_from = 2};
    case BuiltinFn::kMap:
      return {
          .name = "map",
          .declaration = Method{"Map"},
          .takes_closure = true,
          .operands = {kMachine, kMachine, kTyped},
          .answer_starts_from = 2};
    case BuiltinFn::kInsert:
      return {
          .name = "insert",
          .declaration = Method{"Insert"},
          .mutates_receiver = true,
          .operands = {kMachine, kPosition, kHeld}};
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
          .mutates_receiver = true,
          .operands = {kMachine, kHeld}};
    case BuiltinFn::kPushBack:
      return {
          .name = "push_back",
          .declaration = Method{"PushBack"},
          .mutates_receiver = true,
          .operands = {kMachine, kHeld}};
    case BuiltinFn::kExists:
      return {
          .name = "exists",
          .declaration = Method{"Exists"},
          .operands = {kMachine, kKey}};
    case BuiltinFn::kAssocFirst:
      return {
          .name = "assoc_first",
          .declaration = Method{"First"},
          .writes_the_index_back = true,
          .operands = {kMachine, kKey}};
    case BuiltinFn::kAssocLast:
      return {
          .name = "assoc_last",
          .declaration = Method{"Last"},
          .writes_the_index_back = true,
          .operands = {kMachine, kKey}};
    case BuiltinFn::kAssocNext:
      return {
          .name = "assoc_next",
          .declaration = Method{"Next"},
          .writes_the_index_back = true,
          .operands = {kMachine, kKey}};
    case BuiltinFn::kAssocPrev:
      return {
          .name = "assoc_prev",
          .declaration = Method{"Prev"},
          .writes_the_index_back = true,
          .operands = {kMachine, kKey}};
    case BuiltinFn::kAssocMinIndex:
      return {
          .name = "assoc_min_index",
          .declaration = Method{"MinIndex"},
          .operands = {kMachine, kTyped},
          .answer_starts_from = 1};
    case BuiltinFn::kAssocMaxIndex:
      return {
          .name = "assoc_max_index",
          .declaration = Method{"MaxIndex"},
          .operands = {kMachine, kTyped},
          .answer_starts_from = 1};
    case BuiltinFn::kGetc:
      return {
          .name = "getc",
          .declaration = Method{"Getc"},
          .operands = {kMachine, kPosition}};
    case BuiltinFn::kPutc:
      return {
          .name = "putc",
          .declaration = Method{"Putc"},
          .mutates_receiver = true,
          .operands = {kMachine, kPosition, kNumber}};
    case BuiltinFn::kToupper:
      return {.name = "toupper", .declaration = Method{"Toupper"}};
    case BuiltinFn::kTolower:
      return {.name = "tolower", .declaration = Method{"Tolower"}};
    case BuiltinFn::kCompare:
      return {.name = "compare", .declaration = Method{"Compare"}};
    case BuiltinFn::kIcompare:
      return {.name = "icompare", .declaration = Method{"Icompare"}};
    case BuiltinFn::kSubstr:
      return {
          .name = "substr",
          .declaration = Method{"Substr"},
          .operands = {kMachine, kPosition, kPosition}};
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
          .mutates_receiver = true,
          .operands = {kMachine, kNumber}};
    case BuiltinFn::kHextoa:
      return {
          .name = "hextoa",
          .declaration = Method{"Hextoa"},
          .mutates_receiver = true,
          .operands = {kMachine, kNumber}};
    case BuiltinFn::kOcttoa:
      return {
          .name = "octtoa",
          .declaration = Method{"Octtoa"},
          .mutates_receiver = true,
          .operands = {kMachine, kNumber}};
    case BuiltinFn::kBintoa:
      return {
          .name = "bintoa",
          .declaration = Method{"Bintoa"},
          .mutates_receiver = true,
          .operands = {kMachine, kNumber}};
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
          .name = "sampled_history_install",
          .declaration = Method{"Install"},
          .operands = {kMachine, kHeld}};
    case BuiltinFn::kSampledHistoryPush:
      return {
          .name = "sampled_history_push",
          .declaration = Method{"Push"},
          .operands = {kMachine, kHeld}};
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
      return {
          .name = "is_unknown",
          .declaration = Method{"IsUnknown"},
          .over_integral_values = BuiltinFn::kIntegralIsUnknown};
    case BuiltinFn::kIntegralIsUnknown:
      return IntegralEntry(IntegralOp::kIsUnknown, Method{"IsUnknown"});
    case BuiltinFn::kCountBits:
      return {
          .name = "count_bits",
          .declaration = Method{"CountBits"},
          .operands = {kMachine, kBits},
          .over_integral_values = BuiltinFn::kIntegralCountBits};
    case BuiltinFn::kIntegralCountBits:
      return IntegralEntry(IntegralOp::kCountBits, Method{"CountBits"});
    case BuiltinFn::kBitIdentical:
      return {
          .name = "bit_identical",
          .declaration = Method{"IsBitIdentical"},
          .over_integral_values = BuiltinFn::kIntegralBitIdentical};
    case BuiltinFn::kIntegralBitIdentical:
      return IntegralEntry(IntegralOp::kBitIdentical, Method{"IsBitIdentical"});
    case BuiltinFn::kHasUnknown:
      return {
          .name = "has_unknown",
          .declaration = Method{"HasUnknown"},
          .over_integral_values = BuiltinFn::kIntegralHasUnknown};
    case BuiltinFn::kIntegralHasUnknown:
      return IntegralEntry(IntegralOp::kHasUnknown, Method{"HasUnknown"});
    case BuiltinFn::kResolveTriState:
      return {
          .name = "resolve_tri_state",
          .declaration = Method{"ResolveTriState"},
          .over_integral_values = BuiltinFn::kIntegralResolveTriState};
    case BuiltinFn::kIntegralResolveTriState:
      return IntegralEntry(
          IntegralOp::kResolveTriState, Method{"ResolveTriState"});
    case BuiltinFn::kResolveWiredAnd:
      return {
          .name = "resolve_wired_and",
          .declaration = Method{"ResolveWiredAnd"},
          .over_integral_values = BuiltinFn::kIntegralResolveWiredAnd};
    case BuiltinFn::kIntegralResolveWiredAnd:
      return IntegralEntry(
          IntegralOp::kResolveWiredAnd, Method{"ResolveWiredAnd"});
    case BuiltinFn::kResolveWiredOr:
      return {
          .name = "resolve_wired_or",
          .declaration = Method{"ResolveWiredOr"},
          .over_integral_values = BuiltinFn::kIntegralResolveWiredOr};
    case BuiltinFn::kIntegralResolveWiredOr:
      return IntegralEntry(
          IntegralOp::kResolveWiredOr, Method{"ResolveWiredOr"});
    case BuiltinFn::kDominating:
      return {
          .name = "dominating",
          .declaration = Method{"Dominating"},
          .over_integral_values = BuiltinFn::kIntegralDominating};
    case BuiltinFn::kIntegralDominating:
      return IntegralEntry(IntegralOp::kDominate, Method{"Dominating"});
    case BuiltinFn::kFilledLike:
      return {
          .name = "filled_like",
          .declaration = StaticFactory{"FilledLike"},
          .operands = {kMachine, kBits}};
    case BuiltinFn::kClog2:
      return IntegralEntry(IntegralOp::kCeilLog2, Method{"Clog2"});
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
      return {
          .name = "initialize",
          .declaration = Method{"Initialize"},
          .ending = CallEnding::kReturns,
          .operands = {kMachine, kHeld}};
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
    case BuiltinFn::kAggregateNetInitializeTriState:
      return {
          .name = "aggregate_net_initialize_tri_state",
          .declaration = Method{"InitializeTriState"},
          .operands = {kMachine, kHeld}};
    case BuiltinFn::kAggregateNetInitializeWiredAnd:
      return {
          .name = "aggregate_net_initialize_wired_and",
          .declaration = Method{"InitializeWiredAnd"},
          .operands = {kMachine, kHeld}};
    case BuiltinFn::kAggregateNetInitializeWiredOr:
      return {
          .name = "aggregate_net_initialize_wired_or",
          .declaration = Method{"InitializeWiredOr"},
          .operands = {kMachine, kHeld}};
    case BuiltinFn::kAggregateNetInitializeRetaining:
      return {
          .name = "aggregate_net_initialize_retaining",
          .declaration = Method{"InitializeRetaining"},
          .operands = {kMachine, kHeld}};
    case BuiltinFn::kLoad:
      return {.name = "get", .declaration = Method{"Get"}};
    case BuiltinFn::kStore:
      return {
          .name = "set",
          .declaration = Method{"Set"},
          .operands = {kMachine, kHeld}};
    case BuiltinFn::kSampledLoad:
      return {.name = "sampled_load", .declaration = Method{"SampledGet"}};
    case BuiltinFn::kArmSampling:
      return {.name = "arm_sampling", .declaration = Method{"ArmSampling"}};
    case BuiltinFn::kOpenForWrite:
      return {.name = "open_for_write", .declaration = Method{"Mutate"}};
    case BuiltinFn::kDesignateWhole:
      return {.name = "designate_whole", .declaration = Method{"WholeRef"}};
    case BuiltinFn::kDesignateElement:
      return {
          .name = "designate_element",
          .declaration = Method{"ElementRef"},
          .selects = PartSelection::kElement,
          .operands = {kMachine, kPosition}};
    case BuiltinFn::kAssocDesignateElement:
      return {
          .name = "assocarray_designate_element",
          .declaration = Method{"ElementRef"},
          .selects = PartSelection::kElement,
          .operands = {kMachine, kKey}};
    case BuiltinFn::kDesignateComponent:
      return {
          .name = "designate_component",
          .declaration = Method{"ComponentRef"},
          .selects = PartSelection::kComponent};
    case BuiltinFn::kDesignateSlice:
      return {
          .name = "designate_slice",
          .declaration = Method{"SliceRef"},
          .takes_a_type_argument = true,
          .selects = PartSelection::kSlice,
          .operands = {kMachine, kPosition}};
    case BuiltinFn::kDesignateElementSlice:
      return {
          .name = "designate_element_slice",
          .declaration = Method{"SliceRef"},
          .selects = PartSelection::kSlice,
          .operands = {kMachine, kPosition}};
    case BuiltinFn::kReferElement:
      return {
          .name = "refer_element",
          .declaration = Method{"ReferElement"},
          .selects = PartSelection::kElement,
          .operands = {kMachine, kPosition}};
    case BuiltinFn::kAssocReferElement:
      return {
          .name = "assocarray_refer_element",
          .declaration = Method{"ReferElement"},
          .selects = PartSelection::kElement,
          .operands = {kMachine, kKey}};
    case BuiltinFn::kReferComponent:
      return {
          .name = "refer_component",
          .declaration = Method{"ReferComponent"},
          .selects = PartSelection::kComponent};
    case BuiltinFn::kReferProperty:
      return {
          .name = "refer_property",
          .declaration = RuntimeFunction("ReferProperty"),
          .reaches_an_object = true};
    case BuiltinFn::kReferenceReportsTo:
      return {
          .name = "reference_reports_to",
          .declaration = RuntimeFunction("ReportsTo")};
    case BuiltinFn::kBindMember:
      return {
          .name = "bind_member", .declaration = RuntimeFunction("BindMember")};
    case BuiltinFn::kDrivesContinuously:
      return {
          .name = "drives_continuously",
          .declaration = RuntimeFunction("DrivesContinuously")};
    case BuiltinFn::kAttachDriver:
      return {.name = "attach_driver", .declaration = Method{"AttachDriver"}};
    case BuiltinFn::kNetJoin:
      return {.name = "net_join", .declaration = Method{"Join"}};
    case BuiltinFn::kBeginTakeover:
      return {.name = "begin_takeover", .declaration = Method{"BeginTakeover"}};
    case BuiltinFn::kDriveTakeover:
      return {
          .name = "drive_takeover",
          .declaration = Method{"DriveTakeover"},
          .operands = {kMachine, kMachine, kMachine, kHeld}};
    case BuiltinFn::kEndTakeover:
      return {.name = "end_takeover", .declaration = Method{"EndTakeover"}};
    case BuiltinFn::kCurrentRuntime:
      return {
          .name = "current_runtime",
          .declaration = RuntimeFunction("current_runtime"),
          .ending = CallEnding::kReturns};
    case BuiltinFn::kSubmitNba:
      return {
          .name = "submit_nba",
          .declaration = Method{"SubmitNba"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kSubmitNbaAfter:
      return {
          .name = "submit_nba_after",
          .declaration = Method{"SubmitNbaAfter"},
          .takes_the_runtime_handle = true,
          .operands = {kMachine, kNumber}};
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
          .declaration = RuntimeFunction("ResumeInNbaRegion")};
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
      return {.name = "format", .declaration = ValueFunction("Format")};
    case BuiltinFn::kFormatRuntime:
      return {
          .name = "format_runtime",
          .declaration = ValueFunction("FormatRuntime")};
    case BuiltinFn::kMakeRenderedFormatArg:
      return {
          .name = "make_rendered_format_arg",
          .declaration = StaticFactory{"Rendered"}};
    case BuiltinFn::kMakePatternedFormatArg:
      return {
          .name = "make_patterned_format_arg",
          .declaration = StaticFactory{"Patterned"},
          .operands = {kNumber}};
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
          .name = "scan_string", .declaration = ValueFunction("ScanString")};
    case BuiltinFn::kScanFile:
      return {.name = "scan_file", .declaration = ValueFunction("ScanFile")};
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
      return {
          .name = "file_read",
          .declaration = Method{"Read"},
          .operands = {kMachine, kNumber}};
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
          .declaration = RuntimeFunction("TestPlusargs"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kValuePlusargs:
      return {
          .name = "integral_value_plusargs",
          .declaration = RuntimeFunction("ValuePlusargs"),
          .takes_the_runtime_handle = true,
          .operands = {kMachine, kMachine, kNumber}};
    case BuiltinFn::kValuePlusargsString:
      return {
          .name = "string_value_plusargs",
          .declaration = RuntimeFunction("ValuePlusargs"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kRunHostCommand:
      return {
          .name = "run_host_command",
          .declaration = RuntimeFunction("RunHostCommand"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kRunNullHostCommand:
      return {
          .name = "run_null_host_command",
          .declaration = RuntimeFunction("RunNullHostCommand")};
    case BuiltinFn::kReadMem:
      return {.name = "read_mem", .declaration = RuntimeFunction("ReadMem")};
    case BuiltinFn::kReadMemWithin:
      return {
          .name = "read_mem_within",
          .declaration = RuntimeFunction("ReadMemWithin")};
    case BuiltinFn::kWriteMem:
      return {.name = "write_mem", .declaration = RuntimeFunction("WriteMem")};
    case BuiltinFn::kWriteMemWithin:
      return {
          .name = "write_mem_within",
          .declaration = RuntimeFunction("WriteMemWithin")};
    case BuiltinFn::kDelay:
      return {
          .name = "delay",
          .declaration = RuntimeFunction("Delay"),
          .takes_the_runtime_handle = true,
          .operands = {kMachine, kNumber}};
    case BuiltinFn::kDelayReal:
      return {
          .name = "delay_real",
          .declaration = RuntimeFunction("DelayReal"),
          .takes_the_runtime_handle = true};
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
    case BuiltinFn::kObservationFires:
      return {.name = "observation_fires", .declaration = Method{"Fires"}};
    case BuiltinFn::kWaitRecollecting:
      return {
          .name = "wait_recollecting",
          .declaration = RuntimeFunction("WaitRecollecting")};
    case BuiltinFn::kWaitUntil:
      return {
          .name = "wait_until", .declaration = RuntimeFunction("WaitUntil")};
    case BuiltinFn::kWaitOn:
      return {.name = "wait_on", .declaration = RuntimeFunction("WaitOn")};
    case BuiltinFn::kWaitOnImplicitList:
      return {
          .name = "wait_on_implicit_list",
          .declaration = RuntimeFunction("WaitOnImplicitList")};
    case BuiltinFn::kParkAt:
      return {
          .name = "park_at",
          .declaration = RuntimeFunction("ParkAt"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kReadReportEmpty:
      return {
          .name = "read_report_empty", .declaration = StaticFactory{"Empty"}};
    case BuiltinFn::kReadReportForImplicitList:
      return {
          .name = "read_report_for_implicit_list",
          .declaration = StaticFactory{"ForImplicitList"}};
    case BuiltinFn::kReadReportAdd:
      return {.name = "read_report_add", .declaration = Method{"Add"}};
    case BuiltinFn::kReadReportAddThroughHandle:
      return {
          .name = "read_report_add_through_handle",
          .declaration = Method{"AddThroughHandle"}};
    case BuiltinFn::kReadReportEnterCallOnHandle:
      return {
          .name = "read_report_enter_call_on_handle",
          .declaration = Method{"EnterCallOnHandle"}};
    case BuiltinFn::kReadReportLeaveCallOnHandle:
      return {
          .name = "read_report_leave_call_on_handle",
          .declaration = Method{"LeaveCallOnHandle"}};
    case BuiltinFn::kReadReportAddEveryObject:
      return {
          .name = "read_report_add_every_object",
          .declaration = Method{"AddEveryObject"}};
    case BuiltinFn::kReadReportAddWrite:
      return {
          .name = "read_report_add_write", .declaration = Method{"AddWrite"}};
    case BuiltinFn::kReadReportSettleAsImplicitList:
      return {
          .name = "read_report_settle_as_implicit_list",
          .declaration = Method{"SettleAsImplicitList"}};
    case BuiltinFn::kReadReportEnter:
      return {.name = "read_report_enter", .declaration = Method{"Enter"}};
    case BuiltinFn::kReadReportLeave:
      return {.name = "read_report_leave", .declaration = Method{"Leave"}};
    case BuiltinFn::kReadReportRunsTheBody:
      return {
          .name = "read_report_runs_the_body",
          .declaration = Method{"RunsTheBody"}};
    case BuiltinFn::kRefuseReport:
      return {
          .name = "refuse_report",
          .declaration = RuntimeFunction("RefuseReport")};
    case BuiltinFn::kSimTime:
      return {
          .name = "sim_time",
          .declaration = RuntimeFunction("SimTimeInUnit"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kSTime:
      return {
          .name = "stime",
          .declaration = RuntimeFunction("STimeInUnit"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kRealTime:
      return {
          .name = "realtime",
          .declaration = RuntimeFunction("RealTimeInUnit"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kUrandom:
      return {
          .name = "urandom",
          .declaration = RuntimeFunction("Urandom"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kUrandomSeeded:
      return {
          .name = "urandom_seeded",
          .declaration = RuntimeFunction("UrandomSeeded"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kUrandomRange:
      return {
          .name = "urandom_range",
          .declaration = RuntimeFunction("UrandomRange"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kRandom:
      return {
          .name = "random",
          .declaration = RuntimeFunction("Random"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kDistUniform:
      return {
          .name = "dist_uniform",
          .declaration = RuntimeFunction("DistUniform")};
    case BuiltinFn::kDistNormal:
      return {
          .name = "dist_normal", .declaration = RuntimeFunction("DistNormal")};
    case BuiltinFn::kDistExponential:
      return {
          .name = "dist_exponential",
          .declaration = RuntimeFunction("DistExponential")};
    case BuiltinFn::kDistPoisson:
      return {
          .name = "dist_poisson",
          .declaration = RuntimeFunction("DistPoisson")};
    case BuiltinFn::kDistChiSquare:
      return {
          .name = "dist_chi_square",
          .declaration = RuntimeFunction("DistChiSquare")};
    case BuiltinFn::kDistT:
      return {.name = "dist_t", .declaration = RuntimeFunction("DistT")};
    case BuiltinFn::kDistErlang:
      return {
          .name = "dist_erlang", .declaration = RuntimeFunction("DistErlang")};
    case BuiltinFn::kFinish:
      return {
          .name = "finish",
          .declaration = RuntimeFunction("Finish"),
          .takes_the_runtime_handle = true,
          .ending = CallEnding::kDeparts};
    case BuiltinFn::kStop:
      return {
          .name = "stop",
          .declaration = RuntimeFunction("Stop"),
          .takes_the_runtime_handle = true,
          .ending = CallEnding::kDeparts};
    case BuiltinFn::kEnclosingScope:
      return {
          .name = "enclosing_scope", .declaration = Method{"EnclosingScope"}};
    case BuiltinFn::kIsOfClass:
      return {.name = "is_of_class", .declaration = Method{"IsOfClass"}};
    case BuiltinFn::kAddOwnedChild:
      return {
          .name = "add_owned_child", .declaration = Method{"AddOwnedChild"}};
    case BuiltinFn::kExtendSequence:
      return {
          .name = "sequence_extend",
          .declaration = RuntimeFunction("ExtendSequence")};
    case BuiltinFn::kViewOf:
      return {.name = "view_of", .declaration = RuntimeFunction("ViewOf")};
    case BuiltinFn::kObjectEventSource:
      return {
          .name = "object_event_source",
          .declaration = RuntimeFunction("EventSourceOf"),
          .reaches_an_object = true};
    case BuiltinFn::kOpenObjectWrite:
      return {
          .name = "open_object_write",
          .declaration = RuntimeFunction("ErasedObjectWrite")};
    case BuiltinFn::kWrittenObject:
      return {.name = "written_object", .declaration = Method{"Object"}};
    case BuiltinFn::kForkWaitAll:
      return {
          .name = "fork_wait_all",
          .declaration = RuntimeFunction("ForkWaitAll"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kForkWaitFirst:
      return {
          .name = "fork_wait_first",
          .declaration = RuntimeFunction("ForkWaitFirst"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kSpawnAll:
      return {
          .name = "spawn_all",
          .declaration = RuntimeFunction("SpawnAll"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kWaitFork:
      return {
          .name = "wait_fork",
          .declaration = RuntimeFunction("WaitFork"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kDisableFork:
      return {
          .name = "disable_fork",
          .declaration = RuntimeFunction("DisableFork"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kDisable:
      return {
          .name = "disable",
          .declaration = RuntimeFunction("Disable"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kEnterTarget:
      return {
          .name = "enter_target",
          .declaration = RuntimeFunction("EnterCancellationTarget"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kLeaveTarget:
      return {
          .name = "leave_target",
          .declaration = RuntimeFunction("LeaveCancellationTarget"),
          .takes_the_runtime_handle = true,
          .ending = CallEnding::kReturns};
    case BuiltinFn::kEffectNamesTarget:
      return {
          .name = "effect_names_target",
          .declaration = RuntimeFunction("EffectNamesTarget"),
          .ending = CallEnding::kReturns};
    case BuiltinFn::kReceiveDeparture:
      return {
          .name = "receive_departure",
          .declaration = RuntimeFunction("ReceiveDeparture")};
    case BuiltinFn::kProcessSelf:
      return {
          .name = "process_self",
          .declaration = RuntimeFunction("ProcessSelf"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kProcessStatus:
      return {
          .name = "process_status",
          .declaration = RuntimeFunction("ProcessStatus")};
    case BuiltinFn::kProcessKill:
      return {
          .name = "process_kill",
          .declaration = RuntimeFunction("ProcessKill"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kProcessAwait:
      return {
          .name = "process_await",
          .declaration = RuntimeFunction("ProcessAwait"),
          .takes_the_runtime_handle = true,
          .answers_a_wait = true};
    case BuiltinFn::kProcessSuspend:
      return {
          .name = "process_suspend",
          .declaration = RuntimeFunction("ProcessSuspend"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kProcessResume:
      return {
          .name = "process_resume",
          .declaration = RuntimeFunction("ProcessResume"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kRegisterInitial:
      return {
          .name = "register_initial",
          .declaration = RuntimeFunction("RegisterInitialProcess")};
    case BuiltinFn::kRegisterFinal:
      return {
          .name = "register_final",
          .declaration = RuntimeFunction("RegisterFinalProcess")};
    case BuiltinFn::kEnterScopeStaticInit:
      return {
          .name = "enter_scope_static_init",
          .declaration = RuntimeFunction("EnterScopeStaticInit"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kEnterNamespaceStaticInit:
      return {
          .name = "enter_namespace_static_init",
          .declaration = RuntimeFunction("EnterNamespaceStaticInit"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kLeaveStaticInit:
      return {
          .name = "leave_static_init",
          .declaration = RuntimeFunction("LeaveStaticInit"),
          .takes_the_runtime_handle = true,
          .ending = CallEnding::kReturns};
    case BuiltinFn::kEnterDpiScope:
      return {
          .name = "enter_dpi_scope",
          .declaration = RuntimeFunction("EnterDpiScope"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kLeaveDpiScope:
      return {
          .name = "leave_dpi_scope",
          .declaration = RuntimeFunction("LeaveDpiScope"),
          .takes_the_runtime_handle = true,
          .ending = CallEnding::kReturns};
    case BuiltinFn::kDisableIsActive:
      return {
          .name = "disable_is_active",
          .declaration = RuntimeFunction("DisableIsActive"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kCheckImportTaskAcknowledged:
      return {
          .name = "check_import_task_acknowledged",
          .declaration = RuntimeFunction("CheckImportTaskAcknowledged"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kCheckImportFunctionAcknowledged:
      return {
          .name = "check_import_function_acknowledged",
          .declaration = RuntimeFunction("CheckImportFunctionAcknowledged"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kCheckExportReachable:
      return {
          .name = "check_export_reachable",
          .declaration = RuntimeFunction("CheckExportReachable"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kTakeDepartureIfDue:
      return {
          .name = "take_departure_if_due",
          .declaration = RuntimeFunction("TakeDepartureIfDue"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kClaimNamespaceInitialize:
      return {
          .name = "claim_namespace_initialize",
          .declaration = RuntimeFunction("ClaimNamespaceInitialization"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kToInt64:
      return IntegralEntry(IntegralOp::kToInt64, Method{"ToInt64"});
    case BuiltinFn::kRound:
      return {.name = "round", .declaration = Method{"Round"}};
    case BuiltinFn::kTruncate:
      return {.name = "truncate", .declaration = Method{"Truncate"}};
    case BuiltinFn::kToBits:
      return {.name = "to_bits", .declaration = Method{"ToBits"}};
    case BuiltinFn::kFromBits:
      return {
          .name = "from_bits",
          .declaration = StaticFactory{"FromBits"},
          .takes_a_type_argument = true};
    case BuiltinFn::kRealValue:
      return {.name = "real_value", .declaration = Method{"Value"}};
    case BuiltinFn::kStringCStr:
      return {.name = "string_cstr", .declaration = Method{"CStr"}};
    case BuiltinFn::kChandlePtr:
      return {.name = "chandle_ptr", .declaration = Method{"Ptr"}};
    case BuiltinFn::kToSvLogic:
      return {
          .name = "to_sv_logic",
          .declaration = ValueFunction("ToSvLogic"),
          .operands = {kBits}};
    case BuiltinFn::kFromSvLogic:
      return IntegralEntry(
          IntegralOp::kFromSvLogic, ValueFunction("FromSvLogic"));
    case BuiltinFn::kReadCanonicalBitVec:
      return IntegralEntry(
          IntegralOp::kReadCanonicalBits, ValueFunction("ReadCanonicalBitVec"));
    case BuiltinFn::kReadCanonicalLogicVec:
      return IntegralEntry(
          IntegralOp::kReadCanonicalLogic,
          ValueFunction("ReadCanonicalLogicVec"));
    case BuiltinFn::kWriteCanonicalBitVec:
      return {
          .name = "write_canonical_bit_vec",
          .declaration = ValueFunction("WriteCanonicalBitVec"),
          .operands = {kMachine, kBits}};
    case BuiltinFn::kWriteCanonicalLogicVec:
      return {
          .name = "write_canonical_logic_vec",
          .declaration = ValueFunction("WriteCanonicalLogicVec"),
          .operands = {kMachine, kBits}};
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
          .operands = {kMachine, kTyped}};
    case BuiltinFn::kRunForeignTaskOnFiber:
      return {
          .name = "run_foreign_task_on_fiber",
          .declaration = RuntimeFunction("RunForeignTaskOnFiber"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kRunExportedTaskToCompletion:
      return {
          .name = "run_exported_task_to_completion",
          .declaration = RuntimeFunction("RunExportedTaskToCompletion")};
    case BuiltinFn::kCurrentExportScope:
      return {
          .name = "current_export_scope",
          .declaration = RuntimeFunction("CurrentExportScope")};
    case BuiltinFn::kFindExportEntry:
      return {
          .name = "find_export_entry",
          .declaration = RuntimeFunction("FindExportEntry")};
    case BuiltinFn::kFromInt:
      return {
          .name = "from_int",
          .declaration = StaticFactory{"FromInt"},
          .takes_a_type_argument = true};
    case BuiltinFn::kIntegralFromInt:
      return IntegralEntry(IntegralOp::kFromInt, StaticFactory{"FromInt"});
    case BuiltinFn::kConvertFrom:
      return {
          .name = "convert_from",
          .declaration = ValueFunction("Convert"),
          .takes_a_type_argument = true};
    case BuiltinFn::kIntegralConvert:
      return IntegralEntry(IntegralOp::kConvert, ValueFunction("Convert"));
    case BuiltinFn::kToPosition:
      return IntegralEntry(
          IntegralOp::kToPosition, ValueFunction("ToPosition"));
    case BuiltinFn::kStringFromBits:
      return {
          .name = "string_from_bits",
          .declaration = StaticFactory{"FromIntegral"},
          .operands = {kBits}};
    case BuiltinFn::kFromByteArray:
      return {
          .name = "from_byte_array",
          .declaration = StaticFactory{"FromByteArray"}};
    case BuiltinFn::kIntegralFromString:
      return IntegralEntry(IntegralOp::kFromText, ValueFunction("FromString"));
    case BuiltinFn::kByteArrayFromString:
      return {
          .name = "byte_array_from_string",
          .declaration = StaticFactory{"FromString"},
          .takes_a_type_argument = true,
          .answer_told = AnswerTold::kElementType};
    case BuiltinFn::kByteArrayFromBits:
      return {
          .name = "byte_array_from_bits",
          .declaration = StaticFactory{"FromIntegral"},
          .takes_a_type_argument = true,
          .operands = {kBits},
          .answer_told = AnswerTold::kElementType};
    case BuiltinFn::kDynamicArrayFromArray:
      return {
          .name = "dynamic_array_from_array",
          .declaration = StaticFactory{"FromArray"},
          .operands = {kMachine, kTyped}};
    case BuiltinFn::kUnpackedArrayFromArray:
      return {
          .name = "unpacked_array_from_array",
          .declaration = StaticFactory{"FromArray"},
          .operands = {kMachine, kTyped}};
    case BuiltinFn::kQueueFromArray:
      return {
          .name = "queue_from_array",
          .declaration = StaticFactory{"FromArray"},
          .operands = {kMachine, kTyped}};
    case BuiltinFn::kConformBound:
      return {.name = "conform_bound", .declaration = Method{"ConformBound"}};
    case BuiltinFn::kArrayConcatElement:
      return {
          .name = "concat_element",
          .declaration = Method{"ConcatElement"},
          .operands = {kMachine, kHeld}};
    case BuiltinFn::kArrayConcatSpread:
      return {
          .name = "concat_spread",
          .declaration = Method{"ConcatSpread"},
          .operands = {kMachine, kTyped}};
    case BuiltinFn::kArrayConformSize:
      return {
          .name = "conform_size", .declaration = StaticFactory{"ConformSize"}};
    case BuiltinFn::kMakeDynamicArrayDefault:
      return {
          .name = "make_dynamic_array_default",
          .declaration = StaticFactory{"Default"},
          .operands = {kTyped}};
    case BuiltinFn::kMakeDynamicArrayNew:
      return {
          .name = "make_dynamic_array_new",
          .declaration = StaticFactory{"New"},
          .operands = {kMachine, kTyped}};
    case BuiltinFn::kMakeDynamicArrayNewCopy:
      return {
          .name = "make_dynamic_array_new_copy",
          .declaration = StaticFactory{"NewCopy"},
          .operands = {kMachine, kTyped}};
    case BuiltinFn::kConcat:
      return {
          .name = "concat",
          .declaration = Method{"Concat"},
          .over_integral_values = BuiltinFn::kConcatBits};
    case BuiltinFn::kConcatBits:
      return IntegralEntry(IntegralOp::kConcat, Method{"Concat"});
    case BuiltinFn::kReplicateBits:
      return IntegralEntry(IntegralOp::kReplicate, ValueFunction("Replicate"));
    case BuiltinFn::kReplicateString:
      return {.name = "replicate_string", .declaration = Method{"Replicate"}};
    case BuiltinFn::kPow:
      return {
          .name = "pow",
          .declaration = Method{"Pow"},
          .over_integral_values = BuiltinFn::kIntegralPow};
    case BuiltinFn::kIntegralPow:
      return IntegralEntry(IntegralOp::kPower, Method{"Pow"});
    case BuiltinFn::kShiftLeft:
      return IntegralEntry(IntegralOp::kShiftLeft, Method{"ShiftLeft"});
    case BuiltinFn::kLogicalShiftRight:
      return IntegralEntry(
          IntegralOp::kLogicalShiftRight, Method{"LogicalShiftRight"});
    case BuiltinFn::kArithmeticShiftRight:
      return IntegralEntry(
          IntegralOp::kArithmeticShiftRight, Method{"ArithmeticShiftRight"});
    case BuiltinFn::kBitwiseXnor:
      return IntegralEntry(IntegralOp::kBitwiseXnor, Method{"BitwiseXnor"});
    case BuiltinFn::kLogicalEquivalence:
      return IntegralEntry(
          IntegralOp::kLogicalEquivalence, Method{"LogicalEquivalence"});
    case BuiltinFn::kWildcardEquals:
      return IntegralEntry(
          IntegralOp::kWildcardEqual, Method{"WildcardEquals"});
    case BuiltinFn::kCaseEqual:
      return {
          .name = "case_equal",
          .declaration = Method{"CaseEqual"},
          .over_integral_values = BuiltinFn::kIntegralCaseEqual};
    case BuiltinFn::kIntegralCaseEqual:
      return IntegralEntry(IntegralOp::kCaseEqual, Method{"CaseEqual"});
    case BuiltinFn::kCasezEquals:
      return IntegralEntry(IntegralOp::kCasezMatch, Method{"CasezEquals"});
    case BuiltinFn::kCasexEquals:
      return IntegralEntry(IntegralOp::kCasexMatch, Method{"CasexEquals"});
    case BuiltinFn::kMergeConditional:
      return {
          .name = "merge_conditional",
          .declaration = Method{"MergeConditional"},
          .over_integral_values = BuiltinFn::kIntegralMergeConditional};
    case BuiltinFn::kIntegralMergeConditional:
      return IntegralEntry(
          IntegralOp::kMergeConditional, Method{"MergeConditional"});
    case BuiltinFn::kReductionAnd:
      return IntegralEntry(IntegralOp::kReductionAnd, Method{"ReductionAnd"});
    case BuiltinFn::kReductionOr:
      return IntegralEntry(IntegralOp::kReductionOr, Method{"ReductionOr"});
    case BuiltinFn::kReductionXor:
      return IntegralEntry(IntegralOp::kReductionXor, Method{"ReductionXor"});
    case BuiltinFn::kReductionNand:
      return IntegralEntry(IntegralOp::kReductionNand, Method{"ReductionNand"});
    case BuiltinFn::kReductionNor:
      return IntegralEntry(IntegralOp::kReductionNor, Method{"ReductionNor"});
    case BuiltinFn::kReductionXnor:
      return IntegralEntry(IntegralOp::kReductionXnor, Method{"ReductionXnor"});
    case BuiltinFn::kFromBool:
      return IntegralEntry(IntegralOp::kFromBool, StaticFactory{"FromBool"});
    case BuiltinFn::kEnumerationHas:
      return {
          .name = "enumeration_has",
          .declaration = Method{"Has"},
          .operands = {kMachine, kBits}};
    case BuiltinFn::kEnumerationName:
      return {
          .name = "enumeration_name",
          .declaration = Method{"Name"},
          .operands = {kMachine, kBits}};
    case BuiltinFn::kEnumerationNext:
      return {
          .name = "enumeration_next",
          .declaration = Method{"Next"},
          .operands = {kMachine, kBits}};
    case BuiltinFn::kEnumerationPrev:
      return {
          .name = "enumeration_prev",
          .declaration = Method{"Prev"},
          .operands = {kMachine, kBits}};
    case BuiltinFn::kParent:
      return {.name = "parent", .declaration = Method{"Parent"}};
    case BuiltinFn::kSelfHandle:
      return {
          .name = "self_handle", .declaration = RuntimeFunction("SelfHandle")};
    case BuiltinFn::kHierarchicalPath:
      return {
          .name = "hierarchical_path",
          .declaration = Method{"HierarchicalPath"}};
  }
  throw InternalError("RuntimeEntryOf: unknown BuiltinFn");
}

}  // namespace lyra::support
