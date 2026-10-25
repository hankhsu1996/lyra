#include "lyra/support/builtin_fn.hpp"

#include <optional>
#include <string_view>

#include "lyra/base/internal_error.hpp"
#include "lyra/support/callable_bounds.hpp"
#include "lyra/support/integral_operation.hpp"
#include "lyra/support/machine_signature.hpp"

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

constexpr MachineSignature kEffect = MachineSignatureOf<EffectSignature>::Get();
constexpr MachineSignature kCondition =
    MachineSignatureOf<ConditionSignature>::Get();
constexpr MachineSignature kWatch = MachineSignatureOf<WatchSignature>::Get();
constexpr MachineSignature kOrder = MachineSignatureOf<OrderSignature>::Get();

// The bounds of a callable an entry asks of: it calls it at one of the
// signatures above, many times through a shared view (`Fn`), many times as its
// only holder (`FnMut`) or once (`FnOnce`), or it starts it; and it keeps it
// for the call, while what it answers is held, or past its caller.
constexpr CallableBounds kOrderFnForTheCall{
    .trait = CalledClosure{.asks = ClosureKind::kFn, .signature = kOrder},
    .kept = CallableHold::kForTheCall};
constexpr CallableBounds kEffectFnPastTheCaller{
    .trait = CalledClosure{.asks = ClosureKind::kFn, .signature = kEffect},
    .kept = CallableHold::kPastTheCaller};
constexpr CallableBounds kEffectFnWhileItsAnswerIsHeld{
    .trait = CalledClosure{.asks = ClosureKind::kFn, .signature = kEffect},
    .kept = CallableHold::kWhileItsAnswerIsHeld};
constexpr CallableBounds kEffectFnOnceWhileItsAnswerIsHeld{
    .trait = CalledClosure{.asks = ClosureKind::kFnOnce, .signature = kEffect},
    .kept = CallableHold::kWhileItsAnswerIsHeld};
constexpr CallableBounds kEffectFnOncePastTheCaller{
    .trait = CalledClosure{.asks = ClosureKind::kFnOnce, .signature = kEffect},
    .kept = CallableHold::kPastTheCaller};
constexpr CallableBounds kConditionFnWhileItsAnswerIsHeld{
    .trait = CalledClosure{.asks = ClosureKind::kFn, .signature = kCondition},
    .kept = CallableHold::kWhileItsAnswerIsHeld};
constexpr CallableBounds kWatchFnMutWhileItsAnswerIsHeld{
    .trait = CalledClosure{.asks = ClosureKind::kFnMut, .signature = kWatch},
    .kept = CallableHold::kWhileItsAnswerIsHeld};
constexpr CallableBounds kStartedForTheCall{
    .trait = StartedBody{}, .kept = CallableHold::kForTheCall};
constexpr CallableBounds kStartedWhileItsAnswerIsHeld{
    .trait = StartedBody{}, .kept = CallableHold::kWhileItsAnswerIsHeld};
constexpr CallableBounds kStartedPastTheCaller{
    .trait = StartedBody{}, .kept = CallableHold::kPastTheCaller};

}  // namespace

auto RuntimeEntryOf(BuiltinFn id) -> RuntimeEntry {
  using enum OperandReading;
  switch (id) {
    case BuiltinFn::kElement:
      return {
          .name = "element",
          .declaration = Method{"Element"},
          .answer = EntryAnswer::kReadOnlyReferenceIntoTheReceiver,
          .selects = PartSelection::kElement,
          .operands = {kMachine, kPosition}};
    case BuiltinFn::kElementMut:
      return {
          .name = "element_mut",
          .declaration = Method{"ElementMut"},
          .answer = EntryAnswer::kMutableReferenceIntoTheReceiver,
          .selects = PartSelection::kElement,
          .operands = {kMachine, kPosition}};
    case BuiltinFn::kAssocElement:
      return {
          .name = "assocarray_element",
          .declaration = Method{"Element"},
          .answer = EntryAnswer::kReadOnlyReferenceIntoTheReceiver,
          .selects = PartSelection::kElement,
          .operands = {kMachine, kKey}};
    case BuiltinFn::kAssocElementMut:
      return {
          .name = "assocarray_element_mut",
          .declaration = Method{"ElementMut"},
          .answer = EntryAnswer::kMutableReferenceIntoTheReceiver,
          .selects = PartSelection::kElement,
          .operands = {kMachine, kKey}};
    case BuiltinFn::kSlice:
      return IntegralEntry(
          IntegralOp::kSlice, ValueFunction("Slice"), PartSelection::kBits);
    case BuiltinFn::kWithSlice:
      return IntegralEntry(IntegralOp::kWithSlice, ValueFunction("WithSlice"));
    case BuiltinFn::kElementSlice:
      return {
          .name = "element_slice",
          .declaration = Method{"Slice"},
          .takes_a_type_argument = true,
          .selects = PartSelection::kSlice,
          .operands = {kMachine, kPosition},
          .answer_told = AnswerTold::kBuiltArray};
    case BuiltinFn::kQueueSlice:
      return {
          .name = "queue_slice",
          .declaration = Method{"Slice"},
          .selects = PartSelection::kSlice,
          .operands = {kMachine, kPosition, kPosition},
          .answer_told = AnswerTold::kBuiltArray};
    case BuiltinFn::kSliceMut:
      return {
          .name = "slice_mut",
          .declaration = Method{"SliceMut"},
          .takes_a_type_argument = true,
          .selects = PartSelection::kBits,
          .operands = {kMachine, kPosition}};
    case BuiltinFn::kAssignElementSlice:
      return {
          .name = "assign_element_slice",
          .declaration = Method{"AssignSlice"},
          .mutates_receiver = true,
          .operands = {kMachine, kPosition, kMachine, kTyped}};
    case BuiltinFn::kAssignQueueSlice:
      return {
          .name = "queue_assign_element_slice",
          .declaration = Method{"AssignSlice"},
          .mutates_receiver = true,
          .operands = {kMachine, kPosition, kPosition, kTyped}};
    case BuiltinFn::kAssert:
      return {.name = "assert", .declaration = ValueFunction("Assert")};
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
          .operands = {kBits},
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kReverseBlocks:
      return IntegralEntry(IntegralOp::kReverseBlocks, Method{"ReverseBlocks"});
    case BuiltinFn::kToBitVec:
      return {.name = "to_bitvec", .declaration = Method{"ToBitVec"}};
    case BuiltinFn::kFromBitVec:
      return {
          .name = "from_bitvec",
          .declaration = StaticFactory{"FromBitVec"},
          .operands = {kMachine},
          .answer_told = AnswerTold::kBuiltArray};
    case BuiltinFn::kStringFromBitVec:
      return {
          .name = "string_from_bitvec",
          .declaration = StaticFactory{"FromBitVec"},
          .operands = {kMachine}};
    case BuiltinFn::kBitVecOf:
      return {
          .name = "bitvec_of",
          .declaration = StaticFactory{"Of"},
          .operands = {kBits}};
    case BuiltinFn::kBitVecExtend:
      return {
          .name = "bitvec_extend",
          .declaration = Method{"Extend"},
          .mutates_receiver = true,
          .operands = {kMachine, kBits}};
    case BuiltinFn::kBitVecAppend:
      return {
          .name = "bitvec_append",
          .declaration = Method{"Append"},
          .mutates_receiver = true,
          .operands = {kMachine, kMachine}};
    case BuiltinFn::kBitVecInsertBits:
      return {
          .name = "bitvec_insert_bits",
          .declaration = Method{"InsertBits"},
          .mutates_receiver = true,
          .operands = {kMachine, kMachine, kMachine}};
    case BuiltinFn::kBitVecReverseBlocks:
      return {
          .name = "bitvec_reverse_blocks",
          .declaration = Method{"ReverseBlocks"},
          .operands = {kMachine, kMachine}};
    case BuiltinFn::kBitVecLen:
      return {.name = "bitvec_len", .declaration = Method{"Len"}};
    case BuiltinFn::kBitVecExtractBits:
      return {
          .name = "bitvec_extract_bits",
          .declaration = Method{"ExtractBits"},
          .operands = {kMachine, kMachine, kMachine}};
    case BuiltinFn::kBitVecLoad:
      return {
          .name = "bitvec_load",
          .declaration = Method{"Load"},
          .takes_a_type_argument = true,
          .operands = {kMachine, kMachine},
          .answer_told = AnswerTold::kIntegralExtent};
    case BuiltinFn::kBitVecPaddedTo:
      return {
          .name = "bitvec_padded_to",
          .declaration = Method{"PaddedTo"},
          .operands = {kMachine, kMachine}};
    case BuiltinFn::kWindowToBitVec:
      return {
          .name = "window_to_bitvec",
          .declaration = Method{"WindowToBitVec"},
          .operands = {kMachine, kPosition, kMachine}};
    case BuiltinFn::kWithWindow:
      return {
          .name = "with_window",
          .declaration = Method{"WithWindow"},
          .operands = {kMachine, kPosition, kMachine, kTyped}};
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
          .operands = {kMachine, kMachine},
          .callables = {kOrderFnForTheCall}};
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
          .operands = {kMachine, kKey},
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kAssocLast:
      return {
          .name = "assoc_last",
          .declaration = Method{"Last"},
          .writes_the_index_back = true,
          .operands = {kMachine, kKey},
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kAssocNext:
      return {
          .name = "assoc_next",
          .declaration = Method{"Next"},
          .writes_the_index_back = true,
          .operands = {kMachine, kKey},
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kAssocPrev:
      return {
          .name = "assoc_prev",
          .declaration = Method{"Prev"},
          .writes_the_index_back = true,
          .operands = {kMachine, kKey},
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kAssocMinIndex:
      return {
          .name = "assoc_min_index",
          .declaration = Method{"MinIndex"},
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kAssocMaxIndex:
      return {
          .name = "assoc_max_index",
          .declaration = Method{"MaxIndex"},
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kAssocWithDefault:
      return {
          .name = "assoc_with_default",
          .declaration = Method{"WithDefault"},
          .operands = {kMachine, kHeld}};
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
    case BuiltinFn::kEventNew:
      return {.name = "event_new", .declaration = RuntimeFunction("EventNew")};
    case BuiltinFn::kTrigger:
      return {
          .name = "trigger",
          .declaration = RuntimeFunction("EventTrigger"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kTriggered:
      return {
          .name = "triggered",
          .declaration = RuntimeFunction("EventTriggered"),
          .takes_the_runtime_handle = true,
          .takes_the_read_report = true};
    case BuiltinFn::kEventOccurrences:
      return {
          .name = "event_occurrences",
          .declaration = RuntimeFunction("EventOccurrences"),
          .takes_the_read_report = true};
    case BuiltinFn::kEventReportsTo:
      return {
          .name = "event_reports_to",
          .declaration = RuntimeFunction("EventReportsTo")};
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
          .declaration = Method{"Install"},
          .callables = {kEffectFnPastTheCaller, kEffectFnPastTheCaller}};
    case BuiltinFn::kEvaluationAttemptsSeedWord:
      return {
          .name = "evaluation_attempts_seed_word",
          .declaration = Method{"SeedWord"}};
    case BuiltinFn::kEvaluationAttemptsOpenAttempt:
      return {
          .name = "evaluation_attempts_open_attempt",
          .declaration = Method{"OpenAttempt"}};
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
    case BuiltinFn::kEvaluationAttemptsCount:
      return {
          .name = "evaluation_attempts_count", .declaration = Method{"Count"}};
    case BuiltinFn::kEvaluationAttemptsPartOf:
      return {
          .name = "evaluation_attempts_part_of",
          .declaration = Method{"PartOf"}};
    case BuiltinFn::kEvaluationAttemptsImplicationAnswer:
      return {
          .name = "evaluation_attempts_implication_answer",
          .declaration = Method{"ImplicationAnswer"}};
    case BuiltinFn::kEvaluationAttemptsResolve:
      return {
          .name = "evaluation_attempts_resolve",
          .declaration = Method{"Resolve"}};
    case BuiltinFn::kEvaluationAttemptsSettle:
      return {
          .name = "evaluation_attempts_settle",
          .declaration = Method{"Settle"}};
    case BuiltinFn::kEvaluationAttemptsAnswer:
      return {
          .name = "evaluation_attempts_answer",
          .declaration = Method{"Answer"}};
    case BuiltinFn::kWaitOnPropertyEvaluation:
      return {
          .name = "wait_on_property_evaluation",
          .declaration = RuntimeFunction("WaitOnPropertyEvaluation"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kAttemptsOf:
      return {
          .name = "attempts_of", .declaration = RuntimeFunction("AttemptsOf")};
    case BuiltinFn::kBindPropertyEvaluation:
      return {
          .name = "bind_property_evaluation",
          .declaration = RuntimeFunction("BindPropertyEvaluation"),
          .callables = {
              kEffectFnWhileItsAnswerIsHeld, kEffectFnWhileItsAnswerIsHeld}};
    case BuiltinFn::kDisablePropertyEvaluation:
      return {
          .name = "disable_property_evaluation",
          .declaration = RuntimeFunction("DisablePropertyEvaluation")};
    case BuiltinFn::kTimerBind:
      return {
          .name = "timer_bind",
          .declaration = Method{"Bind"},
          .callables = {kEffectFnPastTheCaller}};
    case BuiltinFn::kTimerArm:
      return {
          .name = "timer_arm",
          .declaration = Method{"Arm"},
          .takes_the_runtime_handle = true};
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
    case BuiltinFn::kDeclaredDefault:
      return {
          .name = "declared_default",
          .declaration = StaticFactory{"DeclaredDefault"}};
    case BuiltinFn::kFilledLike:
      return {
          .name = "filled_like",
          .declaration = StaticFactory{"FilledLike"},
          .operands = {kMachine, kBits}};
    case BuiltinFn::kPositionCount:
      return {
          .name = "position_count",
          .declaration = StaticFactory{"PositionCount"}};
    case BuiltinFn::kTakePositions:
      return {
          .name = "take_positions",
          .declaration = Method{"TakePositions"},
          .over_integral_values = BuiltinFn::kIntegralTakePositions};
    case BuiltinFn::kIntegralTakePositions:
      return IntegralEntry(
          IntegralOp::kTakePositions, ValueFunction("TakePositions"));
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
    case BuiltinFn::kGet:
      return {
          .name = "get",
          .declaration = Method{"Get"},
          .answer = EntryAnswer::kReferenceToContents};
    case BuiltinFn::kSet:
      return {
          .name = "set",
          .declaration = Method{"Set"},
          .operands = {kMachine, kHeld}};
    case BuiltinFn::kSampledGet:
      return {
          .name = "sampled_get",
          .declaration = Method{"SampledGet"},
          .answer = EntryAnswer::kReferenceToSampledContents};
    case BuiltinFn::kArmSampling:
      return {.name = "arm_sampling", .declaration = Method{"ArmSampling"}};
    case BuiltinFn::kOpenForWrite:
      return {.name = "open_for_write", .declaration = Method{"Mutate"}};
    case BuiltinFn::kDesignateWhole:
      return {.name = "designate_whole", .declaration = Method{"WholeRef"}};
    case BuiltinFn::kDesignateElement:
      return {
          .name = "designate_element",
          .declaration = Method{"DesignateElement"},
          .selects = PartSelection::kElement,
          .operands = {kMachine, kPosition}};
    case BuiltinFn::kAssocDesignateElement:
      return {
          .name = "assocarray_designate_element",
          .declaration = Method{"DesignateElement"},
          .selects = PartSelection::kElement,
          .operands = {kMachine, kKey}};
    case BuiltinFn::kDesignateComponent:
      return {
          .name = "designate_component",
          .declaration = Method{"DesignateComponent"},
          .selects = PartSelection::kComponent};
    case BuiltinFn::kDesignateMember:
      return {
          .name = "designate_member",
          .declaration = Method{"DesignateMember"},
          .selects = PartSelection::kComponent};
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
    case BuiltinFn::kLeaseObject:
      return {
          .name = "lease_object",
          .declaration = RuntimeFunction("LeaseObject")};
    case BuiltinFn::kReferElementLeased:
      return {
          .name = "refer_element_leased",
          .declaration = Method{"ReferElementLeased"},
          .selects = PartSelection::kElement,
          .operands = {kMachine, kPosition}};
    case BuiltinFn::kAssocReferElementLeased:
      return {
          .name = "assocarray_refer_element_leased",
          .declaration = Method{"ReferElementLeased"},
          .selects = PartSelection::kElement,
          .operands = {kMachine, kKey}};
    case BuiltinFn::kLeasedReference:
      return {
          .name = "leased_reference",
          .declaration = Method{"Reference"},
          .takes_a_type_argument = true};
    case BuiltinFn::kReferenceReportsTo:
      return {
          .name = "reference_reports_to",
          .declaration = RuntimeFunction("ReportsTo")};
    case BuiltinFn::kNetReportsTo:
      return {
          .name = "reports_to", .declaration = RuntimeFunction("ReportsTo")};
    case BuiltinFn::kBindMember:
      return {
          .name = "bind_member", .declaration = RuntimeFunction("BindMember")};
    case BuiltinFn::kBindAlias:
      return {
          .name = "bind_alias", .declaration = RuntimeFunction("BindAlias")};
    case BuiltinFn::kBindReference:
      return {
          .name = "bind_reference",
          .declaration = RuntimeFunction("BindReference")};
    case BuiltinFn::kDrivesContinuously:
      return {
          .name = "drives_continuously",
          .declaration = RuntimeFunction("DrivesContinuously")};
    case BuiltinFn::kAttachDriver:
      return {.name = "attach_driver", .declaration = Method{"AttachDriver"}};
    case BuiltinFn::kDriveFromVariable:
      return {
          .name = "drive_from_variable", .declaration = Method{"DriveFrom"}};
    case BuiltinFn::kDriveFromNet:
      return {.name = "drive_from_net", .declaration = Method{"DriveFrom"}};
    case BuiltinFn::kDriveFromReference:
      return {
          .name = "drive_from_reference", .declaration = Method{"DriveFrom"}};
    case BuiltinFn::kShowSource:
      return {.name = "show_source", .declaration = Method{"ShowSource"}};
    case BuiltinFn::kNetJoin:
      return {.name = "net_join", .declaration = Method{"Join"}};
    case BuiltinFn::kBeginTakeover:
      return {
          .name = "begin_takeover",
          .declaration = Method{"BeginTakeover"},
          .operands = {kMachine, kMachine, kMachine, kMachine}};
    case BuiltinFn::kDriveTakeover:
      return {
          .name = "drive_takeover",
          .declaration = Method{"DriveTakeover"},
          .operands = {kMachine, kMachine, kMachine, kHeld}};
    case BuiltinFn::kEndTakeover:
      return {
          .name = "end_takeover",
          .declaration = Method{"EndTakeover"},
          .operands = {kMachine, kMachine, kMachine, kMachine}};
    case BuiltinFn::kCurrentRuntime:
      return {
          .name = "current_runtime",
          .declaration = RuntimeFunction("current_runtime"),
          .ending = CallEnding::kReturns};
    case BuiltinFn::kNbaEffects:
      return {
          .name = "nba_effects",
          .declaration = Method{"NbaEffects"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kNbaEffectsAfter:
      return {
          .name = "nba_effects_after",
          .declaration = Method{"NbaEffectsAfter"},
          .takes_the_runtime_handle = true,
          .operands = {kMachine, kNumber}};
    case BuiltinFn::kNbaEffectsAfterReal:
      return {
          .name = "nba_effects_after_real",
          .declaration = Method{"NbaEffectsAfterReal"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kRunDetached:
      return {
          .name = "run_detached",
          .declaration = Method{"RunDetached"},
          .takes_the_runtime_handle = true,
          .callables = {kStartedPastTheCaller}};
    case BuiltinFn::kRunObserving:
      return {
          .name = "run_observing",
          .declaration = Method{"RunObserving"},
          .takes_the_runtime_handle = true,
          .callables = {kStartedPastTheCaller}};
    case BuiltinFn::kResumeInNbaRegion:
      return {
          .name = "resume_in_nba_region",
          .declaration = RuntimeFunction("ResumeInNbaRegion")};
    case BuiltinFn::kPostponedEffects:
      return {
          .name = "postponed_effects",
          .declaration = Method{"PostponedEffects"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kSubmitAtRunEnd:
      return {
          .name = "submit_at_run_end",
          .declaration = Method{"SubmitAtRunEnd"},
          .takes_the_runtime_handle = true,
          .callables = {kEffectFnOncePastTheCaller}};
    case BuiltinFn::kObservedEffects:
      return {
          .name = "observed_effects",
          .declaration = Method{"ObservedEffects"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kViolationReports:
      return {
          .name = "violation_reports",
          .declaration = Method{"ViolationReports"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kDeferredEffectsAllocate:
      return {
          .name = "deferred_effects_allocate",
          .declaration = Method{"Allocate"},
          .hands_out_storage = true};
    case BuiltinFn::kDeferredEffectsCommit:
      return {
          .name = "deferred_effects_commit",
          .declaration = Method{kKeeperCommitMethod}};
    case BuiltinFn::kViolationReportsAllocate:
      return {
          .name = "violation_reports_allocate",
          .declaration = Method{"Allocate"},
          .hands_out_storage = true};
    case BuiltinFn::kViolationReportsCommit:
      return {
          .name = "violation_reports_commit",
          .declaration = Method{kKeeperCommitMethod}};
    case BuiltinFn::kSubmitDeferredObserved:
      return {
          .name = "submit_deferred_observed",
          .declaration = Method{"SubmitDeferredObserved"},
          .takes_the_runtime_handle = true,
          .callables = {kStartedPastTheCaller}};
    case BuiltinFn::kSubmitDeferredFinal:
      return {
          .name = "submit_deferred_final",
          .declaration = Method{"SubmitDeferredFinal"},
          .takes_the_runtime_handle = true,
          .callables = {kStartedPastTheCaller}};
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
    // The input, the format, and the value each conversion starts from, a
    // tuple crossing with its type (LRM 21.3.4.3).
    case BuiltinFn::kScanString:
      return {
          .name = "scan_string",
          .declaration = ValueFunction("ScanString"),
          .operands = {kMachine, kMachine, kTyped},
          .answer_told = AnswerTold::kAnswerType};
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
      return {
          .name = "file_gets",
          .declaration = Method{"Gets"},
          .answer_told = AnswerTold::kAnswerType};
    // The file table, the descriptor, the format, and the value each
    // conversion starts from, a tuple crossing with its type (LRM 21.3.4.3).
    case BuiltinFn::kFileScan:
      return {
          .name = "file_scan",
          .declaration = Method{"Scan"},
          .operands = {kMachine, kMachine, kMachine, kTyped},
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kFileRead:
      return {
          .name = "file_read",
          .declaration = Method{"Read"},
          .operands = {kMachine, kNumber},
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kFileReadMemory:
      return {
          .name = "file_read_memory",
          .declaration = Method{"ReadMemory"},
          .operands = {kMachine, kTyped},
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kFileSeek:
      return {.name = "file_seek", .declaration = Method{"Seek"}};
    case BuiltinFn::kFileRewind:
      return {.name = "file_rewind", .declaration = Method{"Rewind"}};
    case BuiltinFn::kFileTell:
      return {.name = "file_tell", .declaration = Method{"Tell"}};
    case BuiltinFn::kFileEof:
      return {.name = "file_eof", .declaration = Method{"Eof"}};
    case BuiltinFn::kFileError:
      return {
          .name = "file_error",
          .declaration = Method{"Error"},
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kFileFlush:
      return {.name = "file_flush", .declaration = Method{"Flush"}};
    case BuiltinFn::kFileFlushAll:
      return {.name = "file_flush_all", .declaration = Method{"FlushAll"}};
    case BuiltinFn::kDumpFile:
      return {
          .name = "dump_file",
          .declaration = RuntimeFunction("DumpFile"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kDumpVars:
      return {
          .name = "dump_vars",
          .declaration = RuntimeFunction("DumpVars"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kDumpVarsLevels:
      return {
          .name = "dump_vars_levels",
          .declaration = RuntimeFunction("DumpVarsLevels"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kDumpVarsOf:
      return {
          .name = "dump_vars_of",
          .declaration = RuntimeFunction("DumpVarsOf"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kDumpVarsOfVariable:
      return {
          .name = "dump_vars_of_variable",
          .declaration = RuntimeFunction("DumpVarsOfVariable"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kDumpOff:
      return {
          .name = "dump_off",
          .declaration = RuntimeFunction("DumpOff"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kDumpOn:
      return {
          .name = "dump_on",
          .declaration = RuntimeFunction("DumpOn"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kDumpAll:
      return {
          .name = "dump_all",
          .declaration = RuntimeFunction("DumpAll"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kDumpLimit:
      return {
          .name = "dump_limit",
          .declaration = RuntimeFunction("DumpLimit"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kDumpFlush:
      return {
          .name = "dump_flush",
          .declaration = RuntimeFunction("DumpFlush"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kDumpPortsOf:
      return {
          .name = "dump_ports_of",
          .declaration = RuntimeFunction("DumpPortsOf"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kDumpPortsOff:
      return {
          .name = "dump_ports_off",
          .declaration = RuntimeFunction("DumpPortsOff"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kDumpPortsOn:
      return {
          .name = "dump_ports_on",
          .declaration = RuntimeFunction("DumpPortsOn"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kDumpPortsAll:
      return {
          .name = "dump_ports_all",
          .declaration = RuntimeFunction("DumpPortsAll"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kDumpPortsLimit:
      return {
          .name = "dump_ports_limit",
          .declaration = RuntimeFunction("DumpPortsLimit"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kDumpPortsFlush:
      return {
          .name = "dump_ports_flush",
          .declaration = RuntimeFunction("DumpPortsFlush"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kDescribe:
      return {
          .name = "describe",
          .declaration = RuntimeFunction("DescribeTo"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kDescribeEvent:
      return {
          .name = "describe_event",
          .declaration = RuntimeFunction("DescribeTo"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kBeginDescribedBlock:
      return {
          .name = "begin_described_block",
          .declaration = RuntimeFunction("BeginDescribedBlock"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kEndDescribedBlock:
      return {
          .name = "end_described_block",
          .declaration = RuntimeFunction("EndDescribedBlock"),
          .takes_the_runtime_handle = true};
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
          .operands = {kMachine, kMachine, kNumber},
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kValuePlusargsString:
      return {
          .name = "string_value_plusargs",
          .declaration = RuntimeFunction("ValuePlusargs"),
          .takes_the_runtime_handle = true,
          .answer_told = AnswerTold::kAnswerType};
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
      return {
          .name = "read_mem",
          .declaration = RuntimeFunction("ReadMem"),
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kReadMemWithin:
      return {
          .name = "read_mem_within",
          .declaration = RuntimeFunction("ReadMemWithin"),
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kReadEnumeratedMem:
      return {
          .name = "read_enumerated_mem",
          .declaration = RuntimeFunction("ReadMem"),
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kReadEnumeratedMemWithin:
      return {
          .name = "read_enumerated_mem_within",
          .declaration = RuntimeFunction("ReadMemWithin"),
          .answer_told = AnswerTold::kAnswerType};
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
    case BuiltinFn::kDelayTicks:
      return {
          .name = "delay_ticks",
          .declaration = RuntimeFunction("DelayTicks"),
          .operands = {kNumber}};
    case BuiltinFn::kDelayTicksReal:
      return {
          .name = "delay_ticks_real",
          .declaration = RuntimeFunction("DelayTicksReal")};
    case BuiltinFn::kScalarPropagationDelay:
      return {
          .name = "scalar_propagation_delay",
          .declaration = RuntimeFunction("ScalarPropagationDelay"),
          .operands = {kBits}};
    case BuiltinFn::kVectorPropagationDelay:
      return {
          .name = "vector_propagation_delay",
          .declaration = RuntimeFunction("VectorPropagationDelay"),
          .operands = {kBits}};
    case BuiltinFn::kObservationOnReaching:
      return {
          .name = "observation_on_reaching",
          .declaration = StaticFactory{"OnReaching"}};
    case BuiltinFn::kObservationOfWatch:
      return {
          .name = "observation_of_watch",
          .declaration = StaticFactory{"OfWatch"},
          .callables = {kWatchFnMutWhileItsAnswerIsHeld}};
    case BuiltinFn::kObservationOfWatchQualified:
      return {
          .name = "observation_of_watch_qualified",
          .declaration = StaticFactory{"OfWatchQualified"},
          .callables = {
              kWatchFnMutWhileItsAnswerIsHeld,
              kConditionFnWhileItsAnswerIsHeld}};
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
      return {
          .name = "read_report_add",
          .declaration = Method{"Add"},
          .ending = CallEnding::kReturns};
    case BuiltinFn::kReadReportAddObject:
      return {
          .name = "read_report_add_object",
          .declaration = Method{"AddObject"},
          .reaches_an_object = true};
    case BuiltinFn::kReadReportAddWrite:
      return {
          .name = "read_report_add_write",
          .declaration = Method{"AddWrite"},
          .ending = CallEnding::kReturns};
    case BuiltinFn::kReadReportSettleAsImplicitList:
      return {
          .name = "read_report_settle_as_implicit_list",
          .declaration = Method{"SettleAsImplicitList"}};
    case BuiltinFn::kReadReportEnter:
      return {
          .name = "read_report_enter",
          .declaration = Method{"Enter"},
          .ending = CallEnding::kReturns};
    case BuiltinFn::kReadReportLeave:
      return {
          .name = "read_report_leave",
          .declaration = Method{"Leave"},
          .ending = CallEnding::kReturns};
    case BuiltinFn::kReadReportRunsTheBody:
      return {
          .name = "read_report_runs_the_body",
          .declaration = Method{"RunsTheBody"},
          .ending = CallEnding::kReturns};
    case BuiltinFn::kReadReportBeginEvaluation:
      return {
          .name = "read_report_begin_evaluation",
          .declaration = Method{"BeginEvaluation"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kReadReportEndEvaluation:
      return {
          .name = "read_report_end_evaluation",
          .declaration = Method{"EndEvaluation"},
          .takes_the_runtime_handle = true,
          .ending = CallEnding::kReturns};
    case BuiltinFn::kEvaluationInProgress:
      return {
          .name = "evaluation_in_progress",
          .declaration = RuntimeFunction("EvaluationInProgress"),
          .takes_the_runtime_handle = true};
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
          .declaration = RuntimeFunction("DistUniform"),
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kDistNormal:
      return {
          .name = "dist_normal",
          .declaration = RuntimeFunction("DistNormal"),
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kDistExponential:
      return {
          .name = "dist_exponential",
          .declaration = RuntimeFunction("DistExponential"),
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kDistPoisson:
      return {
          .name = "dist_poisson",
          .declaration = RuntimeFunction("DistPoisson"),
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kDistChiSquare:
      return {
          .name = "dist_chi_square",
          .declaration = RuntimeFunction("DistChiSquare"),
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kDistT:
      return {
          .name = "dist_t",
          .declaration = RuntimeFunction("DistT"),
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kDistErlang:
      return {
          .name = "dist_erlang",
          .declaration = RuntimeFunction("DistErlang"),
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kFinish:
      return {
          .name = "finish",
          .declaration = RuntimeFunction("Finish"),
          .takes_the_runtime_handle = true,
          .ending = CallEnding::kUnwinds};
    case BuiltinFn::kStop:
      return {
          .name = "stop",
          .declaration = RuntimeFunction("Stop"),
          .takes_the_runtime_handle = true,
          .ending = CallEnding::kUnwinds};
    case BuiltinFn::kExit:
      return {
          .name = "exit",
          .declaration = RuntimeFunction("Exit"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kEnclosingScope:
      return {
          .name = "enclosing_scope", .declaration = Method{"EnclosingScope"}};
    case BuiltinFn::kLoopBlocksAdd:
      return {.name = "loop_blocks_add", .declaration = Method{"Add"}};
    case BuiltinFn::kLoopBlocksAt:
      return {.name = "loop_blocks_at", .declaration = Method{"At"}};
    case BuiltinFn::kSetScopePlace:
      return {.name = "set_scope_place", .declaration = Method{"SetPlace"}};
    case BuiltinFn::kAttachScope:
      return {
          .name = "attach_scope",
          .declaration = FreeFunction{
              .scope = "lyra::runtime::Scope", .identifier = "Attach"}};
    case BuiltinFn::kInstantiationEntry:
      return {
          .name = "instantiation_entry",
          .declaration = RuntimeFunction("InstantiationEntry")};
    case BuiltinFn::kInstantiationTable:
      return {
          .name = "instantiation_table",
          .declaration = RuntimeFunction("InstantiationTable")};
    case BuiltinFn::kExtendSequence:
      return {
          .name = "sequence_extend",
          .declaration = RuntimeFunction("ExtendSequence")};
    case BuiltinFn::kViewOf:
      return {.name = "view_of", .declaration = RuntimeFunction("ViewOf")};
    case BuiltinFn::kDerivedToBase:
      return {
          .name = "derived_to_base",
          .declaration = RuntimeFunction("ViewAs"),
          .takes_a_type_argument = true,
          .type_argument_named = TypeArgumentNamed::kTheClassesSeen};
    case BuiltinFn::kDynamicCast:
      return {
          .name = "dynamic_cast",
          .declaration = RuntimeFunction("ViewAs"),
          .takes_a_type_argument = true,
          .type_argument_named = TypeArgumentNamed::kTheClassesSeen};
    case BuiltinFn::kUnsize:
      return {
          .name = "unsize",
          .declaration = RuntimeFunction("Unsize"),
          .takes_a_type_argument = true,
          .type_argument_named = TypeArgumentNamed::kTheCallSignature};
    case BuiltinFn::kMakeAny:
      return {
          .name = "any_make",
          .declaration = RuntimeFunction("MakeAny"),
          .operands = {kTyped, kMachine}};
    case BuiltinFn::kSeeAsAny:
      return {
          .name = "see_as_any",
          .declaration = RuntimeFunction("SeeAsAny"),
          .takes_a_type_argument = true,
          .operands = {kMachine, kMachine, kMachine},
          .answer_told = AnswerTold::kTypeCalledAt,
          .callables = {kEffectFnWhileItsAnswerIsHeld}};
    case BuiltinFn::kReceiveAny:
      return {
          .name = "receive_any",
          .declaration = RuntimeFunction("ReceiveAny"),
          .operands = {kMachine, kMachine}};
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
          .takes_the_runtime_handle = true,
          .callables = {kStartedWhileItsAnswerIsHeld}};
    case BuiltinFn::kForkWaitFirst:
      return {
          .name = "fork_wait_first",
          .declaration = RuntimeFunction("ForkWaitFirst"),
          .takes_the_runtime_handle = true,
          .callables = {kStartedPastTheCaller}};
    case BuiltinFn::kSpawnAll:
      return {
          .name = "spawn_all",
          .declaration = RuntimeFunction("SpawnAll"),
          .takes_the_runtime_handle = true,
          .callables = {kStartedPastTheCaller}};
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
    case BuiltinFn::kRegionLandsEffect:
      return {
          .name = "region_lands_effect",
          .declaration = RuntimeFunction("RegionLandsEffect"),
          .takes_the_runtime_handle = true,
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
    case BuiltinFn::kProcessSrandom:
      return {
          .name = "process_srandom",
          .declaration = RuntimeFunction("ProcessSrandom"),
          .operands = {kMachine, kNumber}};
    case BuiltinFn::kProcessGetRandstate:
      return {
          .name = "process_get_randstate",
          .declaration = RuntimeFunction("ProcessGetRandstate")};
    case BuiltinFn::kProcessSetRandstate:
      return {
          .name = "process_set_randstate",
          .declaration = RuntimeFunction("ProcessSetRandstate")};
    case BuiltinFn::kWeakDowngrade:
      return {
          .name = "weak_downgrade",
          .declaration = RuntimeFunction("Downgrade")};
    case BuiltinFn::kWeakUpgrade:
      return {
          .name = "weak_upgrade",
          .declaration = RuntimeFunction("Upgrade"),
          .takes_the_read_report = true};
    case BuiltinFn::kWeakUpgradeProcess:
      return {
          .name = "weak_upgrade_process",
          .declaration = RuntimeFunction("UpgradeProcess"),
          .takes_the_read_report = true};
    case BuiltinFn::kWeakRefersTo:
      return {
          .name = "weak_refers_to",
          .declaration = RuntimeFunction("WeakRefersTo")};
    case BuiltinFn::kObjectIdentity:
      return {
          .name = "object_identity",
          .declaration = RuntimeFunction("ObjectIdentity")};
    case BuiltinFn::kObjectPreRandomize:
      return {
          .name = "object_pre_randomize",
          .declaration = RuntimeFunction("ObjectPreRandomize"),
          .reaches_an_object = true};
    case BuiltinFn::kObjectPostRandomize:
      return {
          .name = "object_post_randomize",
          .declaration = RuntimeFunction("ObjectPostRandomize"),
          .reaches_an_object = true};
    case BuiltinFn::kObjectSrandom:
      return {
          .name = "object_srandom",
          .declaration = RuntimeFunction("ObjectSrandom"),
          .reaches_an_object = true,
          .operands = {kMachine, kNumber}};
    case BuiltinFn::kObjectGetRandstate:
      return {
          .name = "object_get_randstate",
          .declaration = RuntimeFunction("ObjectGetRandstate"),
          .reaches_an_object = true};
    case BuiltinFn::kObjectSetRandstate:
      return {
          .name = "object_set_randstate",
          .declaration = RuntimeFunction("ObjectSetRandstate"),
          .reaches_an_object = true};
    case BuiltinFn::kRandomizeObject:
      return {
          .name = "randomize_object",
          .declaration = RuntimeFunction("RandomizeObject"),
          .reaches_an_object = true,
          .takes_the_runtime_handle = true};
    case BuiltinFn::kRandomizeObjectListed:
      return {
          .name = "randomize_object_listed",
          .declaration = RuntimeFunction("RandomizeObjectListed"),
          .reaches_an_object = true,
          .takes_the_runtime_handle = true};
    case BuiltinFn::kConstraintBlockEnabled:
      return {
          .name = "constraint_block_enabled",
          .declaration = RuntimeFunction("ConstraintBlockEnabled"),
          .reaches_an_object = true};
    case BuiltinFn::kObjectRandModeSet:
      return {
          .name = "object_rand_mode_set",
          .declaration = RuntimeFunction("ObjectRandModeSet"),
          .reaches_an_object = true};
    case BuiltinFn::kConstraintModeSet:
      return {
          .name = "constraint_mode_set",
          .declaration = RuntimeFunction("ConstraintModeSet"),
          .reaches_an_object = true};
    case BuiltinFn::kConstraintModeGet:
      return {
          .name = "constraint_mode_get",
          .declaration = RuntimeFunction("ConstraintModeGet"),
          .reaches_an_object = true};
    case BuiltinFn::kObjectConstraintModeSet:
      return {
          .name = "object_constraint_mode_set",
          .declaration = RuntimeFunction("ObjectConstraintModeSet"),
          .reaches_an_object = true};
    case BuiltinFn::kRandomizeScope:
      return {
          .name = "randomize_scope",
          .declaration = RuntimeFunction("RandomizeScope"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kRandomizeList:
      return {
          .name = "randomize_list",
          .declaration = RuntimeFunction("RandomizeList"),
          .operands = {kMachine, kMachine, kTyped}};
    case BuiltinFn::kConstraintMembers:
      return {
          .name = "constraint_members",
          .declaration = RuntimeFunction("ConstraintMembers")};
    case BuiltinFn::kConstraintRand:
      return {
          .name = "constraint_rand",
          .declaration = RuntimeFunction("ConstraintRand"),
          .operands = {kMachine, kMachine, kTyped}};
    case BuiltinFn::kConstraintRandc:
      return {
          .name = "constraint_randc",
          .declaration = RuntimeFunction("ConstraintRandc"),
          .operands = {kMachine, kMachine, kTyped}};
    case BuiltinFn::kConstraintVariable:
      return {
          .name = "constraint_variable",
          .declaration = RuntimeFunction("ConstraintVariable"),
          .operands = {kMachine, kMachine, kTyped}};
    case BuiltinFn::kConstraintConstant:
      return {
          .name = "constraint_constant",
          .declaration = RuntimeFunction("ConstraintConstant"),
          .operands = {kMachine, kTyped}};
    case BuiltinFn::kConstraintNegate:
      return {
          .name = "constraint_negate",
          .declaration = RuntimeFunction("ConstraintNegate")};
    case BuiltinFn::kConstraintBitwiseNot:
      return {
          .name = "constraint_bitwise_not",
          .declaration = RuntimeFunction("ConstraintBitwiseNot")};
    case BuiltinFn::kConstraintReduceAnd:
      return {
          .name = "constraint_reduce_and",
          .declaration = RuntimeFunction("ConstraintReduceAnd")};
    case BuiltinFn::kConstraintReduceOr:
      return {
          .name = "constraint_reduce_or",
          .declaration = RuntimeFunction("ConstraintReduceOr")};
    case BuiltinFn::kConstraintReduceXor:
      return {
          .name = "constraint_reduce_xor",
          .declaration = RuntimeFunction("ConstraintReduceXor")};
    case BuiltinFn::kConstraintLogicalNot:
      return {
          .name = "constraint_logical_not",
          .declaration = RuntimeFunction("ConstraintLogicalNot")};
    case BuiltinFn::kConstraintAdd:
      return {
          .name = "constraint_add",
          .declaration = RuntimeFunction("ConstraintAdd")};
    case BuiltinFn::kConstraintSubtract:
      return {
          .name = "constraint_subtract",
          .declaration = RuntimeFunction("ConstraintSubtract")};
    case BuiltinFn::kConstraintMultiply:
      return {
          .name = "constraint_multiply",
          .declaration = RuntimeFunction("ConstraintMultiply")};
    case BuiltinFn::kConstraintDivide:
      return {
          .name = "constraint_divide",
          .declaration = RuntimeFunction("ConstraintDivide")};
    case BuiltinFn::kConstraintRemainder:
      return {
          .name = "constraint_remainder",
          .declaration = RuntimeFunction("ConstraintRemainder")};
    case BuiltinFn::kConstraintBitwiseAnd:
      return {
          .name = "constraint_bitwise_and",
          .declaration = RuntimeFunction("ConstraintBitwiseAnd")};
    case BuiltinFn::kConstraintBitwiseOr:
      return {
          .name = "constraint_bitwise_or",
          .declaration = RuntimeFunction("ConstraintBitwiseOr")};
    case BuiltinFn::kConstraintBitwiseXor:
      return {
          .name = "constraint_bitwise_xor",
          .declaration = RuntimeFunction("ConstraintBitwiseXor")};
    case BuiltinFn::kConstraintShiftLeft:
      return {
          .name = "constraint_shift_left",
          .declaration = RuntimeFunction("ConstraintShiftLeft")};
    case BuiltinFn::kConstraintShiftRight:
      return {
          .name = "constraint_shift_right",
          .declaration = RuntimeFunction("ConstraintShiftRight")};
    case BuiltinFn::kConstraintEqual:
      return {
          .name = "constraint_equal",
          .declaration = RuntimeFunction("ConstraintEqual")};
    case BuiltinFn::kConstraintLess:
      return {
          .name = "constraint_less",
          .declaration = RuntimeFunction("ConstraintLess")};
    case BuiltinFn::kConstraintLogicalAnd:
      return {
          .name = "constraint_logical_and",
          .declaration = RuntimeFunction("ConstraintLogicalAnd")};
    case BuiltinFn::kConstraintLogicalOr:
      return {
          .name = "constraint_logical_or",
          .declaration = RuntimeFunction("ConstraintLogicalOr")};
    case BuiltinFn::kConstraintConcatenate:
      return {
          .name = "constraint_concatenate",
          .declaration = RuntimeFunction("ConstraintConcatenate")};
    case BuiltinFn::kConstraintSelect:
      return {
          .name = "constraint_select",
          .declaration = RuntimeFunction("ConstraintSelect")};
    case BuiltinFn::kConstraintResize:
      return {
          .name = "constraint_resize",
          .declaration = RuntimeFunction("ConstraintResize")};
    case BuiltinFn::kConstraintSlice:
      return {
          .name = "constraint_slice",
          .declaration = RuntimeFunction("ConstraintSlice")};
    case BuiltinFn::kConstraintRequire:
      return {
          .name = "constraint_require",
          .declaration = RuntimeFunction("ConstraintRequire")};
    case BuiltinFn::kConstraintPrefer:
      return {
          .name = "constraint_prefer",
          .declaration = RuntimeFunction("ConstraintPrefer")};
    case BuiltinFn::kConstraintDisableSoft:
      return {
          .name = "constraint_disable_soft",
          .declaration = RuntimeFunction("ConstraintDisableSoft")};
    case BuiltinFn::kConstraintSolveBefore:
      return {
          .name = "constraint_solve_before",
          .declaration = RuntimeFunction("ConstraintSolveBefore")};
    case BuiltinFn::kConstraintDistribution:
      return {
          .name = "constraint_distribution",
          .declaration = RuntimeFunction("ConstraintDistribution")};
    case BuiltinFn::kConstraintDistributionItem:
      return {
          .name = "constraint_distribution_item",
          .declaration = RuntimeFunction("ConstraintDistributionItem")};
    case BuiltinFn::kConstraintDistributionDefault:
      return {
          .name = "constraint_distribution_default",
          .declaration = RuntimeFunction("ConstraintDistributionDefault")};
    case BuiltinFn::kConstraintUniqueSet:
      return {
          .name = "constraint_unique_set",
          .declaration = RuntimeFunction("ConstraintUniqueSet")};
    case BuiltinFn::kConstraintUniqueMember:
      return {
          .name = "constraint_unique_member",
          .declaration = RuntimeFunction("ConstraintUniqueMember")};
    case BuiltinFn::kConstraintUniqueArray:
      return {
          .name = "constraint_unique_array",
          .declaration = RuntimeFunction("ConstraintUniqueArray"),
          .operands = {kMachine, kMachine, kMachine, kTyped}};
    case BuiltinFn::kConstraintUniqueRequire:
      return {
          .name = "constraint_unique_require",
          .declaration = RuntimeFunction("ConstraintUniqueRequire")};
    case BuiltinFn::kConstraintArraySize:
      return {
          .name = "constraint_array_size",
          .declaration = RuntimeFunction("ConstraintArraySize"),
          .operands = {kMachine, kMachine, kTyped}};
    case BuiltinFn::kRandomizeSettleFirst:
      return {
          .name = "randomize_settle_first",
          .declaration = RuntimeFunction("RandomizeSettleFirst")};
    case BuiltinFn::kConstraintWant:
      return {
          .name = "constraint_want",
          .declaration = RuntimeFunction("ConstraintWant")};
    case BuiltinFn::kRandomizeConstrain:
      return {
          .name = "randomize_constrain",
          .declaration = RuntimeFunction("RandomizeConstrain")};
    case BuiltinFn::kRandomizeFinish:
      return {
          .name = "randomize_finish",
          .declaration = RuntimeFunction("RandomizeFinish")};
    case BuiltinFn::kRandomizeNested:
      return {
          .name = "randomize_nested",
          .declaration = RuntimeFunction("RandomizeNested"),
          .operands = {kMachine, kMachine, kTyped}};
    case BuiltinFn::kStaticConstraintBlockEnabled:
      return {
          .name = "static_constraint_block_enabled",
          .declaration = RuntimeFunction("StaticConstraintBlockEnabled")};
    case BuiltinFn::kRandModeSet:
      return {
          .name = "rand_mode_set",
          .declaration = RuntimeFunction("RandModeSet"),
          .operands = {kMachine, kTyped}};
    case BuiltinFn::kRandModeGet:
      return {
          .name = "rand_mode_get",
          .declaration = RuntimeFunction("RandModeGet"),
          .operands = {kMachine, kTyped}};
    case BuiltinFn::kStaticConstraintModeSet:
      return {
          .name = "static_constraint_mode_set",
          .declaration = RuntimeFunction("StaticConstraintModeSet")};
    case BuiltinFn::kStaticConstraintModeGet:
      return {
          .name = "static_constraint_mode_get",
          .declaration = RuntimeFunction("StaticConstraintModeGet")};
    case BuiltinFn::kRegisterAlways:
      return {
          .name = "register_always",
          .declaration = RuntimeFunction("RegisterAlwaysProcess"),
          .callables = {kStartedPastTheCaller}};
    case BuiltinFn::kRegisterContinuousDriver:
      return {
          .name = "register_continuous_driver",
          .declaration = RuntimeFunction("RegisterContinuousDriver"),
          .callables = {kStartedPastTheCaller}};
    case BuiltinFn::kRegisterInitial:
      return {
          .name = "register_initial",
          .declaration = RuntimeFunction("RegisterInitialProcess"),
          .callables = {kStartedPastTheCaller}};
    case BuiltinFn::kRegisterTriggered:
      return {
          .name = "register_triggered",
          .declaration = RuntimeFunction("RegisterTriggeredProcess"),
          .callables = {kStartedPastTheCaller}};
    case BuiltinFn::kRegisterFinal:
      return {
          .name = "register_final",
          .declaration = RuntimeFunction("RegisterFinalProcedure"),
          .callables = {kEffectFnOncePastTheCaller}};
    case BuiltinFn::kRunAssertionAction:
      return {
          .name = "run_assertion_action",
          .declaration = RuntimeFunction("RunAssertionAction"),
          .callables = {kStartedPastTheCaller}};
    case BuiltinFn::kEnterReactionRunningFirst:
      return {
          .name = "enter_reaction_running_first",
          .declaration = RuntimeFunction("EnterReactionRunningFirst"),
          .ending = CallEnding::kReturns,
          .answer = EntryAnswer::kExecution};
    case BuiltinFn::kEnterReactionStoppingFirst:
      return {
          .name = "enter_reaction_stopping_first",
          .declaration = RuntimeFunction("EnterReactionStoppingFirst"),
          .ending = CallEnding::kReturns,
          .answer = EntryAnswer::kExecution};
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
    case BuiltinFn::kHandStringToForeignCaller:
      return {
          .name = "hand_string_to_foreign_caller",
          .declaration = RuntimeFunction("HandStringToForeignCaller"),
          .takes_the_runtime_handle = true};
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
          .takes_a_type_argument = true,
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kRunForeignTaskOnFiber:
      return {
          .name = "run_foreign_task_on_fiber",
          .declaration = RuntimeFunction("RunForeignTaskOnFiber"),
          .takes_the_runtime_handle = true,
          .callables = {kEffectFnOnceWhileItsAnswerIsHeld}};
    case BuiltinFn::kRunExportedTaskToCompletion:
      return {
          .name = "run_exported_task_to_completion",
          .declaration = RuntimeFunction("RunExportedTaskToCompletion"),
          .callables = {kStartedForTheCall}};
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
          .declaration = StaticFactory{"FromByteArray"},
          .operands = {kTyped}};
    case BuiltinFn::kIntegralFromString:
      return IntegralEntry(IntegralOp::kFromText, ValueFunction("FromString"));
    case BuiltinFn::kByteArrayFromString:
      return {
          .name = "byte_array_from_string",
          .declaration = StaticFactory{"FromString"},
          .answer_told = AnswerTold::kBuiltArray};
    case BuiltinFn::kByteArrayFromBits:
      return {
          .name = "byte_array_from_bits",
          .declaration = StaticFactory{"FromIntegral"},
          .operands = {kBits},
          .answer_told = AnswerTold::kBuiltArray};
    case BuiltinFn::kDynamicArrayFromArray:
      return {
          .name = "dynamic_array_from_array",
          .declaration = StaticFactory{"FromArray"},
          .answer_told = AnswerTold::kBuiltArray};
    case BuiltinFn::kUnpackedArrayFromArray:
      return {
          .name = "unpacked_array_from_array",
          .declaration = StaticFactory{"FromArray"},
          .answer_told = AnswerTold::kBuiltArray};
    case BuiltinFn::kQueueFromArray:
      return {
          .name = "queue_from_array",
          .declaration = StaticFactory{"FromArray"},
          .answer_told = AnswerTold::kBuiltArray};
    case BuiltinFn::kConformBound:
      return {
          .name = "conform_bound",
          .declaration = StaticFactory{"ConformBound"},
          .answer_told = AnswerTold::kBuiltArray};
    case BuiltinFn::kMakeDynamicArrayNew:
      return {
          .name = "make_dynamic_array_new",
          .declaration = StaticFactory{"New"},
          .answer_told = AnswerTold::kBuiltArray};
    case BuiltinFn::kMakeDynamicArrayNewCopy:
      return {
          .name = "make_dynamic_array_new_copy",
          .declaration = StaticFactory{"NewCopy"},
          .answer_told = AnswerTold::kBuiltArray};
    case BuiltinFn::kConcatStrings:
      return {.name = "concat", .declaration = StaticFactory{"Concat"}};
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
    case BuiltinFn::kSelfHandle:
      return {
          .name = "self_handle", .declaration = RuntimeFunction("SelfHandle")};
    case BuiltinFn::kHierarchicalPathOf:
      return {
          .name = "hierarchical_path_of",
          .declaration = Method{"HierarchicalPathOf"}};
  }
  throw InternalError("RuntimeEntryOf: unknown BuiltinFn");
}

}  // namespace lyra::support
