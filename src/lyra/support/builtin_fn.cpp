#include "lyra/support/builtin_fn.hpp"

#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/support/callable_bounds.hpp"
#include "lyra/support/integral_operation.hpp"
#include "lyra/support/library_decl.hpp"
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

// An entry that is the operation `op` over integral values and nothing else,
// reached as `declaration` says. It is spelled as the operation is, and what
// it takes is the operation's to say.
auto IntegralEntry(IntegralOp op, EntryDeclaration declaration)
    -> RuntimeEntry {
  const IntegralOperation& operation = IntegralOperationOf(op);
  // An integral operation answers a value of every operand it is handed,
  // whatever bits they hold, so a call of one cannot unwind.
  return {
      .name = operation.name,
      .declaration = declaration,
      .takes_a_type_argument = AnswersAtTheTypeOfTheCall(operation.answer),
      .takes_the_location = false,
      .can_unwind = false,
      .integral = op};
}

constexpr MachineSignature kEffect = MachineSignatureOf<EffectSignature>::Get();
constexpr MachineSignature kCondition =
    MachineSignatureOf<ConditionSignature>::Get();
constexpr MachineSignature kWatch = MachineSignatureOf<WatchSignature>::Get();
constexpr MachineSignature kAttemptClosure =
    MachineSignatureOf<AttemptClosureSignature>::Get();

// The bounds of a callable an entry asks of: it calls it at one of the
// signatures above, many times through a shared view (`Fn`), many times as its
// only holder (`FnMut`) or once (`FnOnce`), or it starts it; and it keeps it
// for the call, while what it answers is held, or past its caller.
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
constexpr CallableBounds kAttemptClosureFnMutWhileItsAnswerIsHeld{
    .trait =
        CalledClosure{
            .asks = ClosureKind::kFnMut, .signature = kAttemptClosure},
    .kept = CallableHold::kWhileItsAnswerIsHeld};
constexpr CallableBounds kAttemptClosureFnMutPastTheCaller{
    .trait =
        CalledClosure{
            .asks = ClosureKind::kFnMut, .signature = kAttemptClosure},
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

// An entry of the core keeping an associative array's entries, a function of
// the library named `identifier`, which runs no code of the design's.
constexpr auto AssociativeArrayCoreEntry(
    std::string_view name, std::string_view identifier) -> RuntimeEntry {
  return {
      .name = name,
      .declaration =
          FreeFunction{.scope = "lyra::value", .identifier = identifier},
      .takes_the_location = false,
      .can_unwind = false};
}

}  // namespace

auto RuntimeEntryOf(BuiltinFn id) -> RuntimeEntry {
  using enum OperandReading;
  switch (id) {
    case BuiltinFn::kElement:
      return {
          .name = "element",
          .declaration = Method{"Element"},
          .takes_the_location = false,
          .can_unwind = false,
          .answer = EntryAnswer::kReadOnlyReferenceIntoTheReceiver,
          .selects = PartSelection::kElement,
          .operands = {kMachine, kPosition}};
    case BuiltinFn::kElementMut:
      return {
          .name = "element_mut",
          .declaration = Method{"ElementMut"},
          .takes_the_location = false,
          .answer = EntryAnswer::kMutableReferenceIntoTheReceiver,
          .selects = PartSelection::kElement,
          .operands = {kMachine, kPosition},
          .over_queues = BuiltinFn::kQueueElementMut};
    case BuiltinFn::kQueueElementMut:
      return {
          .name = "queue_element_mut",
          .declaration = Method{"ElementMut"},
          .answer = EntryAnswer::kMutableReferenceIntoTheReceiver,
          .selects = PartSelection::kElement,
          .operands = {kMachine, kPosition}};
    case BuiltinFn::kAssocElement:
      return {
          .name = "assoc_element",
          .declaration = Method{"Element"},
          .answer = EntryAnswer::kReadOnlyReferenceIntoTheReceiver,
          .selects = PartSelection::kElement,
          .operands = {kMachine, kKey}};
    case BuiltinFn::kAssocElementMut:
      return {
          .name = "assoc_element_mut",
          .declaration = Method{"ElementMut"},
          .answer = EntryAnswer::kMutableReferenceIntoTheReceiver,
          .selects = PartSelection::kElement,
          .operands = {kMachine, kKey}};
    case BuiltinFn::kWithSlice:
      return IntegralEntry(IntegralOp::kWithSlice, ValueFunction("WithSlice"));
    case BuiltinFn::kElementSlice:
      return {
          .name = "element_slice",
          .declaration = Method{"Slice"},
          .takes_a_type_argument = true,
          .takes_the_location = false,
          .selects = PartSelection::kSlice,
          .operands = {kMachine, kPosition},
          .answer_told = AnswerTold::kBuiltArray};
    case BuiltinFn::kQueueAllocate:
      return {
          .name = "queue_allocate",
          .declaration = Method{"Allocate"},
          .takes_the_location = false};
    case BuiltinFn::kQueueSlice:
      return {
          .name = "queue_slice",
          .declaration = Method{"Slice"},
          .takes_the_location = false,
          .selects = PartSelection::kSlice,
          .operands = {kMachine, kPosition, kPosition},
          .answer_told = AnswerTold::kBuiltArray};
    // A write of a run is handed where it was written, whatever array it
    // writes, so the designation's own write of one hands it on to whichever
    // the designated array has: a queue's reports a value of another count.
    case BuiltinFn::kAssignElementSlice:
      return {
          .name = "assign_element_slice",
          .declaration = Method{"AssignSlice"},
          .mutates_receiver = true,
          .operands = {kMachine, kPosition, kTyped}};
    case BuiltinFn::kAssignQueueSlice:
      return {
          .name = "queue_assign_element_slice",
          .declaration = Method{"AssignSlice"},
          .mutates_receiver = true,
          .operands = {kMachine, kPosition, kPosition, kTyped}};
    case BuiltinFn::kAssignDesignatedRun:
      return {
          .name = "assign_designated_run",
          .declaration = Method{"AssignSlice"},
          .takes_a_type_argument = true,
          .mutates_receiver = true};
    case BuiltinFn::kAssert:
      return {.name = "assert", .declaration = ValueFunction("Assert")};
    case BuiltinFn::kSize:
      return {
          .name = "size",
          .declaration = Method{"Size"},
          .takes_the_location = false};
    case BuiltinFn::kLen:
      return {
          .name = "len",
          .declaration = Method{"Len"},
          .takes_the_location = false};
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
          .answer_told = AnswerTold::kAnswerTypeAndOperations};
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
          .takes_the_location = false,
          .operands = {kBits}};
    case BuiltinFn::kBitVecExtend:
      return {
          .name = "bitvec_extend",
          .declaration = Method{"Extend"},
          .takes_the_location = false,
          .mutates_receiver = true,
          .operands = {kMachine, kBits}};
    case BuiltinFn::kBitVecAppend:
      return {
          .name = "bitvec_append",
          .declaration = Method{"Append"},
          .takes_the_location = false,
          .mutates_receiver = true,
          .operands = {kMachine, kMachine}};
    case BuiltinFn::kBitVecInsertBits:
      return {
          .name = "bitvec_insert_bits",
          .declaration = Method{"InsertBits"},
          .takes_the_location = false,
          .mutates_receiver = true,
          .operands = {kMachine, kMachine, kMachine}};
    case BuiltinFn::kBitVecReverseBlocks:
      return {
          .name = "bitvec_reverse_blocks",
          .declaration = Method{"ReverseBlocks"},
          .takes_the_location = false,
          .operands = {kMachine, kMachine}};
    case BuiltinFn::kBitVecLen:
      return {
          .name = "bitvec_len",
          .declaration = Method{"Len"},
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kBitVecExtractBits:
      return {
          .name = "bitvec_extract_bits",
          .declaration = Method{"ExtractBits"},
          .takes_the_location = false,
          .operands = {kMachine, kMachine, kMachine}};
    case BuiltinFn::kBitVecLoad:
      return {
          .name = "bitvec_load",
          .declaration = Method{"Load"},
          .takes_a_type_argument = true,
          .takes_the_location = false,
          .operands = {kMachine, kMachine},
          .answer_told = AnswerTold::kIntegralExtent};
    case BuiltinFn::kBitVecPaddedTo:
      return {
          .name = "bitvec_padded_to",
          .declaration = Method{"PaddedTo"},
          .takes_the_location = false,
          .can_unwind = false,
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
          .takes_the_location = false,
          .mutates_receiver = true};
    case BuiltinFn::kDeleteIndex:
      return {
          .name = "delete_index",
          .declaration = Method{"DeleteIndex"},
          .takes_the_location = false,
          .mutates_receiver = true,
          .operands = {kMachine, kPosition}};
    case BuiltinFn::kAssocDeleteIndex:
      return {
          .name = "assoc_delete_index",
          .declaration = Method{"DeleteIndex"},
          .takes_the_location = false,
          .mutates_receiver = true,
          .operands = {kMachine, kKey}};
    case BuiltinFn::kReverse:
      return {
          .name = "reverse",
          .declaration = Method{"Reverse"},
          .takes_the_location = false,
          .mutates_receiver = true};
    case BuiltinFn::kSortOrder:
      return {
          .name = "sort_order",
          .declaration = StaticFactory{"SortOrder"},
          .takes_the_location = false,
          .answer_told = AnswerTold::kBuiltArray};
    case BuiltinFn::kApplyOrder:
      return {
          .name = "apply_order",
          .declaration = Method{"ApplyOrder"},
          .takes_the_location = false,
          .mutates_receiver = true};
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
          .takes_the_location = false,
          .mutates_receiver = true};
    case BuiltinFn::kPopBack:
      return {
          .name = "pop_back",
          .declaration = Method{"PopBack"},
          .takes_the_location = false,
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
          .takes_the_location = false,
          .operands = {kMachine, kKey}};
    // The traversal is answered by where the index visited lies, which the
    // use writes back (LRM 7.9.4 -- 7.9.8).
    case BuiltinFn::kAssocFirst:
      return {
          .name = "assoc_first",
          .declaration = Method{"First"},
          .takes_the_location = false,
          .writes_the_index_back = true};
    case BuiltinFn::kAssocLast:
      return {
          .name = "assoc_last",
          .declaration = Method{"Last"},
          .takes_the_location = false,
          .writes_the_index_back = true};
    case BuiltinFn::kAssocNext:
      return {
          .name = "assoc_next",
          .declaration = Method{"Next"},
          .takes_the_location = false,
          .writes_the_index_back = true,
          .operands = {kMachine, kKey}};
    case BuiltinFn::kAssocPrev:
      return {
          .name = "assoc_prev",
          .declaration = Method{"Prev"},
          .takes_the_location = false,
          .writes_the_index_back = true,
          .operands = {kMachine, kKey}};
    // Read off the array's first or last index where the query is written.
    case BuiltinFn::kAssocMinIndex:
      return {
          .name = "assoc_min_index",
          .declaration = Method{"MinIndex"},
          .takes_the_location = false};
    case BuiltinFn::kAssocMaxIndex:
      return {
          .name = "assoc_max_index",
          .declaration = Method{"MaxIndex"},
          .takes_the_location = false};
    case BuiltinFn::kAssocStatedDefault:
      return {
          .name = "assoc_stated_default",
          .declaration = Method{"StatedDefault"},
          .takes_the_location = false};
    case BuiltinFn::kAssocSetStatedDefault:
      return {
          .name = "assoc_set_stated_default",
          .declaration = Method{"SetStatedDefault"},
          .takes_the_location = false,
          .mutates_receiver = true,
          .operands = {kMachine, kHeld}};
    case BuiltinFn::kGetc:
      return {
          .name = "getc",
          .declaration = Method{"Getc"},
          .takes_the_location = false,
          .operands = {kMachine, kPosition}};
    case BuiltinFn::kPutc:
      return {
          .name = "putc",
          .declaration = Method{"Putc"},
          .takes_the_location = false,
          .mutates_receiver = true,
          .operands = {kMachine, kPosition, kNumber}};
    case BuiltinFn::kToupper:
      return {
          .name = "toupper",
          .declaration = Method{"Toupper"},
          .takes_the_location = false};
    case BuiltinFn::kTolower:
      return {
          .name = "tolower",
          .declaration = Method{"Tolower"},
          .takes_the_location = false};
    case BuiltinFn::kCompare:
      return {
          .name = "compare",
          .declaration = Method{"Compare"},
          .takes_the_location = false};
    case BuiltinFn::kIcompare:
      return {
          .name = "icompare",
          .declaration = Method{"Icompare"},
          .takes_the_location = false};
    case BuiltinFn::kSubstr:
      return {
          .name = "substr",
          .declaration = Method{"Substr"},
          .takes_the_location = false,
          .operands = {kMachine, kPosition, kPosition}};
    case BuiltinFn::kAtoi:
      return {
          .name = "atoi",
          .declaration = Method{"Atoi"},
          .takes_the_location = false};
    case BuiltinFn::kAtohex:
      return {
          .name = "atohex",
          .declaration = Method{"Atohex"},
          .takes_the_location = false};
    case BuiltinFn::kAtooct:
      return {
          .name = "atooct",
          .declaration = Method{"Atooct"},
          .takes_the_location = false};
    case BuiltinFn::kAtobin:
      return {
          .name = "atobin",
          .declaration = Method{"Atobin"},
          .takes_the_location = false};
    case BuiltinFn::kAtoreal:
      return {
          .name = "atoreal",
          .declaration = Method{"Atoreal"},
          .takes_the_location = false};
    case BuiltinFn::kItoa:
      return {
          .name = "itoa",
          .declaration = Method{"Itoa"},
          .takes_the_location = false,
          .can_unwind = false,
          .mutates_receiver = true,
          .operands = {kMachine, kNumber}};
    case BuiltinFn::kHextoa:
      return {
          .name = "hextoa",
          .declaration = Method{"Hextoa"},
          .takes_the_location = false,
          .mutates_receiver = true,
          .operands = {kMachine, kNumber}};
    case BuiltinFn::kOcttoa:
      return {
          .name = "octtoa",
          .declaration = Method{"Octtoa"},
          .takes_the_location = false,
          .mutates_receiver = true,
          .operands = {kMachine, kNumber}};
    case BuiltinFn::kBintoa:
      return {
          .name = "bintoa",
          .declaration = Method{"Bintoa"},
          .takes_the_location = false,
          .mutates_receiver = true,
          .operands = {kMachine, kNumber}};
    case BuiltinFn::kRealtoa:
      return {
          .name = "realtoa",
          .declaration = Method{"Realtoa"},
          .takes_the_location = false,
          .mutates_receiver = true};
    case BuiltinFn::kEventNew:
      return {
          .name = "event_new",
          .declaration = RuntimeFunction("EventNew"),
          .takes_the_location = false};
    case BuiltinFn::kTrigger:
      return {
          .name = "trigger",
          .declaration = RuntimeFunction("EventTrigger"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kTriggered:
      return {
          .name = "triggered",
          .declaration = RuntimeFunction("EventTriggered"),
          .takes_the_runtime_handle = true,
          .takes_the_read_report = true,
          .takes_the_location = false};
    case BuiltinFn::kEventOccurrences:
      return {
          .name = "event_occurrences",
          .declaration = RuntimeFunction("EventOccurrences"),
          .takes_the_read_report = true,
          .takes_the_location = false};
    case BuiltinFn::kEventReportsTo:
      return {
          .name = "event_reports_to",
          .declaration = RuntimeFunction("EventReportsTo"),
          .takes_the_location = false};
    case BuiltinFn::kSampledHistoryInstall:
      return {
          .name = "sampled_history_install",
          .declaration = Method{"Install"},
          .takes_the_location = false,
          .operands = {kMachine, kHeld}};
    case BuiltinFn::kSampledHistoryPush:
      return {
          .name = "sampled_history_push",
          .declaration = Method{"Push"},
          .takes_the_location = false,
          .operands = {kMachine, kHeld}};
    case BuiltinFn::kSampledHistoryAt:
      return {
          .name = "sampled_history_at",
          .declaration = Method{"At"},
          .takes_the_location = false};
    case BuiltinFn::kEvaluationAttemptsInstall:
      return {
          .name = "evaluation_attempts_install",
          .declaration = Method{"Install"},
          .takes_the_location = false,
          .callables = {kEffectFnPastTheCaller}};
    case BuiltinFn::kEvaluationAttemptsAllocate:
      return {
          .name = "evaluation_attempts_allocate",
          .declaration = AllocationFunction{},
          .takes_the_location = false};
    case BuiltinFn::kEvaluationAttemptsCommit:
      return {
          .name = "evaluation_attempts_commit",
          .declaration = Method{"Commit"},
          .takes_the_location = false,
          .callables = {kAttemptClosureFnMutPastTheCaller}};
    case BuiltinFn::kEvaluationAttemptsTick:
      return {
          .name = "evaluation_attempts_tick",
          .declaration = Method{"Tick"},
          .takes_the_location = false};
    case BuiltinFn::kEvaluationAttemptsRun:
      return {
          .name = "evaluation_attempts_run",
          .declaration = Method{"Run"},
          .takes_the_location = false};
    case BuiltinFn::kEvaluationAttemptsTestDisable:
      return {
          .name = "evaluation_attempts_test_disable",
          .declaration = Method{"TestDisable"},
          .takes_the_location = false};
    case BuiltinFn::kEvaluationAttemptsLiveWord:
      return {
          .name = "evaluation_attempts_live_word",
          .declaration = Method{"LiveWord"},
          .takes_the_location = false};
    case BuiltinFn::kEvaluationAttemptsAnswer:
      return {
          .name = "evaluation_attempts_answer",
          .declaration = Method{"Answer"},
          .takes_the_location = false};
    case BuiltinFn::kEvaluationAttemptsActionRuns:
      return {
          .name = "evaluation_attempts_action_runs",
          .declaration = Method{"ActionRuns"},
          .takes_the_location = false};
    case BuiltinFn::kEvaluationAttemptSharedWord:
      return {
          .name = "evaluation_attempt_shared_word",
          .declaration = Method{"SharedWord"},
          .takes_the_location = false};
    case BuiltinFn::kEvaluationAttemptLiveWord:
      return {
          .name = "evaluation_attempt_live_word",
          .declaration = Method{"LiveWord"},
          .takes_the_location = false};
    case BuiltinFn::kEvaluationAttemptNextUnstepped:
      return {
          .name = "evaluation_attempt_next_unstepped",
          .declaration = Method{"NextUnstepped"},
          .takes_the_location = false};
    case BuiltinFn::kEvaluationAttemptBitsAt:
      return {
          .name = "evaluation_attempt_bits_at",
          .declaration = Method{"BitsAt"},
          .takes_the_location = false};
    case BuiltinFn::kEvaluationAttemptSetWord:
      return {
          .name = "evaluation_attempt_set_word",
          .declaration = Method{"SetWord"},
          .takes_the_location = false};
    case BuiltinFn::kEvaluationAttemptStep:
      return {
          .name = "evaluation_attempt_step",
          .declaration = Method{"Step"},
          .takes_the_location = false};
    case BuiltinFn::kEvaluationAttemptSeedWord:
      return {
          .name = "evaluation_attempt_seed_word",
          .declaration = Method{"SeedWord"},
          .takes_the_location = false};
    case BuiltinFn::kEvaluationAttemptSeed:
      return {
          .name = "evaluation_attempt_seed",
          .declaration = Method{"Seed"},
          .takes_the_location = false};
    case BuiltinFn::kEvaluationAttemptCount:
      return {
          .name = "evaluation_attempt_count",
          .declaration = Method{"Count"},
          .takes_the_location = false};
    case BuiltinFn::kEvaluationAttemptPartOf:
      return {
          .name = "evaluation_attempt_part_of",
          .declaration = Method{"PartOf"},
          .takes_the_location = false};
    case BuiltinFn::kEvaluationAttemptImplicationAnswer:
      return {
          .name = "evaluation_attempt_implication_answer",
          .declaration = Method{"ImplicationAnswer"},
          .takes_the_location = false};
    case BuiltinFn::kEvaluationAttemptImplicationNonvacuous:
      return {
          .name = "evaluation_attempt_implication_nonvacuous",
          .declaration = Method{"ImplicationNonvacuous"},
          .takes_the_location = false};
    case BuiltinFn::kEvaluationAttemptResolve:
      return {
          .name = "evaluation_attempt_resolve",
          .declaration = Method{"Resolve"},
          .takes_the_location = false};
    case BuiltinFn::kWaitOnPropertyEvaluation:
      return {
          .name = "wait_on_property_evaluation",
          .declaration = RuntimeFunction("WaitOnPropertyEvaluation"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kAttemptsOf:
      return {
          .name = "attempts_of",
          .declaration = RuntimeFunction("AttemptsOf"),
          .takes_the_location = false};
    case BuiltinFn::kBindPropertyEvaluation:
      return {
          .name = "bind_property_evaluation",
          .declaration = RuntimeFunction("BindPropertyEvaluation"),
          .takes_the_location = false,
          .callables = {
              kEffectFnWhileItsAnswerIsHeld,
              kAttemptClosureFnMutWhileItsAnswerIsHeld}};
    case BuiltinFn::kTimerBind:
      return {
          .name = "timer_bind",
          .declaration = Method{"Bind"},
          .takes_the_location = false,
          .callables = {kEffectFnPastTheCaller}};
    case BuiltinFn::kTimerArm:
      return {
          .name = "timer_arm",
          .declaration = Method{"Arm"},
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kCountBits:
      return {
          .name = "count_bits",
          .declaration = Method{"CountBits"},
          .can_unwind = false,
          .operands = {kMachine, kHeld},
          .over_integral_values = BuiltinFn::kIntegralCountBits};
    case BuiltinFn::kIntegralCountBits:
      return IntegralEntry(IntegralOp::kCountBits, Method{"CountBits"});
    case BuiltinFn::kBitIdentical:
      return {
          .name = "bit_identical",
          .declaration = Method{"IsBitIdentical"},
          .takes_the_location = false,
          .over_integral_values = BuiltinFn::kIntegralBitIdentical};
    case BuiltinFn::kIntegralBitIdentical:
      return IntegralEntry(IntegralOp::kBitIdentical, Method{"IsBitIdentical"});
    case BuiltinFn::kHasUnknown:
      return {
          .name = "has_unknown",
          .declaration = Method{"HasUnknown"},
          .takes_the_location = false,
          .over_integral_values = BuiltinFn::kIntegralHasUnknown};
    case BuiltinFn::kIntegralHasUnknown:
      return IntegralEntry(IntegralOp::kHasUnknown, Method{"HasUnknown"});
    case BuiltinFn::kDeclaredDefault:
      return {
          .name = "declared_default",
          .declaration = StaticFactory{"DeclaredDefault"},
          .takes_the_location = false};
    case BuiltinFn::kToBoundFree:
      return {
          .name = "to_bound_free",
          .declaration = Method{"ToBoundFree"},
          .takes_a_type_argument = true};
    case BuiltinFn::kFromBoundFree:
      return {
          .name = "from_bound_free",
          .declaration = StaticFactory{"FromBoundFree"}};
    case BuiltinFn::kPositionCount:
      return {
          .name = "position_count",
          .declaration = StaticFactory{"PositionCount"},
          .takes_the_location = false};
    case BuiltinFn::kTakePositions:
      return {
          .name = "take_positions",
          .declaration = Method{"TakePositions"},
          .takes_the_location = false,
          .over_integral_values = BuiltinFn::kIntegralTakePositions};
    case BuiltinFn::kIntegralTakePositions:
      return IntegralEntry(
          IntegralOp::kTakePositions, ValueFunction("TakePositions"));
    case BuiltinFn::kToPositions:
      return {
          .name = "to_positions",
          .declaration = Method{"ToPositions"},
          .takes_the_location = false};
    case BuiltinFn::kWritePositions:
      return {
          .name = "write_positions",
          .declaration = Method{"WritePositions"},
          .takes_the_location = false};
    case BuiltinFn::kFromPositions:
      return {
          .name = "from_positions",
          .declaration = StaticFactory{"FromPositions"},
          .takes_the_location = false,
          .operands = {kBits}};
    case BuiltinFn::kIntegralWithPositions:
      return IntegralEntry(
          IntegralOp::kWithPositions, ValueFunction("WithPositions"));
    case BuiltinFn::kClog2:
      return IntegralEntry(IntegralOp::kCeilLog2, Method{"Clog2"});
    case BuiltinFn::kLn:
      return {
          .name = "ln",
          .declaration = Method{"Ln"},
          .takes_the_location = false};
    case BuiltinFn::kLog10:
      return {
          .name = "log10",
          .declaration = Method{"Log10"},
          .takes_the_location = false};
    case BuiltinFn::kExp:
      return {
          .name = "exp",
          .declaration = Method{"Exp"},
          .takes_the_location = false};
    case BuiltinFn::kSqrt:
      return {
          .name = "sqrt",
          .declaration = Method{"Sqrt"},
          .takes_the_location = false};
    case BuiltinFn::kFloor:
      return {
          .name = "floor",
          .declaration = Method{"Floor"},
          .takes_the_location = false};
    case BuiltinFn::kCeil:
      return {
          .name = "ceil",
          .declaration = Method{"Ceil"},
          .takes_the_location = false};
    case BuiltinFn::kSin:
      return {
          .name = "sin",
          .declaration = Method{"Sin"},
          .takes_the_location = false};
    case BuiltinFn::kCos:
      return {
          .name = "cos",
          .declaration = Method{"Cos"},
          .takes_the_location = false};
    case BuiltinFn::kTan:
      return {
          .name = "tan",
          .declaration = Method{"Tan"},
          .takes_the_location = false};
    case BuiltinFn::kAsin:
      return {
          .name = "asin",
          .declaration = Method{"Asin"},
          .takes_the_location = false};
    case BuiltinFn::kAcos:
      return {
          .name = "acos",
          .declaration = Method{"Acos"},
          .takes_the_location = false};
    case BuiltinFn::kAtan:
      return {
          .name = "atan",
          .declaration = Method{"Atan"},
          .takes_the_location = false};
    case BuiltinFn::kAtan2:
      return {
          .name = "atan2",
          .declaration = Method{"Atan2"},
          .takes_the_location = false};
    case BuiltinFn::kHypot:
      return {
          .name = "hypot",
          .declaration = Method{"Hypot"},
          .takes_the_location = false};
    case BuiltinFn::kSinh:
      return {
          .name = "sinh",
          .declaration = Method{"Sinh"},
          .takes_the_location = false};
    case BuiltinFn::kCosh:
      return {
          .name = "cosh",
          .declaration = Method{"Cosh"},
          .takes_the_location = false};
    case BuiltinFn::kTanh:
      return {
          .name = "tanh",
          .declaration = Method{"Tanh"},
          .takes_the_location = false};
    case BuiltinFn::kAsinh:
      return {
          .name = "asinh",
          .declaration = Method{"Asinh"},
          .takes_the_location = false};
    case BuiltinFn::kAcosh:
      return {
          .name = "acosh",
          .declaration = Method{"Acosh"},
          .takes_the_location = false};
    case BuiltinFn::kAtanh:
      return {
          .name = "atanh",
          .declaration = Method{"Atanh"},
          .takes_the_location = false};
    case BuiltinFn::kInitialize:
      return {
          .name = "initialize",
          .declaration = Method{"Initialize"},
          .takes_the_location = false,
          .can_unwind = false,
          .operands = {kMachine, kHeld}};
    case BuiltinFn::kNetInitializeTriState:
      return {
          .name = "net_initialize_tri_state",
          .declaration = Method{"InitializeTriState"},
          .takes_the_location = false};
    case BuiltinFn::kNetInitializeWiredAnd:
      return {
          .name = "net_initialize_wired_and",
          .declaration = Method{"InitializeWiredAnd"},
          .takes_the_location = false};
    case BuiltinFn::kNetInitializeWiredOr:
      return {
          .name = "net_initialize_wired_or",
          .declaration = Method{"InitializeWiredOr"},
          .takes_the_location = false};
    case BuiltinFn::kNetInitializeRetaining:
      return {
          .name = "net_initialize_retaining",
          .declaration = Method{"InitializeRetaining"},
          .takes_the_location = false};
    case BuiltinFn::kUnresolvedNetInitialize:
      return {
          .name = "unresolved_net_initialize",
          .declaration = Method{"InitializeUnresolved"},
          .takes_the_location = false,
          .operands = {kMachine, kHeld}};
    case BuiltinFn::kGet:
      return {
          .name = "get",
          .declaration = Method{"Get"},
          .takes_the_location = false,
          .answer = EntryAnswer::kReferenceToContents};
    case BuiltinFn::kSet:
      return {
          .name = "set",
          .declaration = Method{"Set"},
          .takes_the_location = false,
          .operands = {kMachine, kHeld}};
    case BuiltinFn::kSampledGet:
      return {
          .name = "sampled_get",
          .declaration = Method{"SampledGet"},
          .answer = EntryAnswer::kReferenceToSampledContents};
    case BuiltinFn::kArmSampling:
      return {
          .name = "arm_sampling",
          .declaration = Method{"ArmSampling"},
          .takes_the_location = false};
    case BuiltinFn::kOpenForWrite:
      return {
          .name = "open_for_write",
          .declaration = Method{"Mutate"},
          .takes_the_location = false};
    case BuiltinFn::kDesignateWhole:
      return {
          .name = "designate_whole",
          .declaration = Method{"WholeRef"},
          .takes_the_location = false};
    case BuiltinFn::kDesignateElement:
      return {
          .name = "designate_element",
          .declaration = Method{"DesignateElement"},
          .takes_the_location = false,
          .selects = PartSelection::kElement,
          .operands = {kMachine, kPosition},
          .over_queues = BuiltinFn::kQueueDesignateElement,
          .over_dynamic_arrays = BuiltinFn::kDynamicArrayDesignateElement};
    case BuiltinFn::kDynamicArrayDesignateElement:
      return {
          .name = "dynamic_array_designate_element",
          .declaration = Method{"DesignateElement"},
          .takes_the_location = false,
          .selects = PartSelection::kElement,
          .operands = {kMachine, kPosition}};
    case BuiltinFn::kQueueDesignateElement:
      return {
          .name = "queue_designate_element",
          .declaration = Method{"DesignateElement"},
          .selects = PartSelection::kElement,
          .operands = {kMachine, kPosition}};
    case BuiltinFn::kAssocDesignateElement:
      return {
          .name = "assoc_designate_element",
          .declaration = Method{"DesignateElement"},
          .selects = PartSelection::kElement,
          .operands = {kMachine, kKey}};
    case BuiltinFn::kDesignateComponent:
      return {
          .name = "designate_component",
          .declaration = Method{"DesignateComponent"},
          .takes_the_location = false,
          .selects = PartSelection::kComponent};
    case BuiltinFn::kDesignateMember:
      return {
          .name = "designate_member",
          .declaration = Method{"DesignateMember"},
          .takes_the_location = false,
          .selects = PartSelection::kComponent};
    case BuiltinFn::kReferElement:
      return {
          .name = "refer_element",
          .declaration = Method{"ReferElement"},
          .selects = PartSelection::kElement,
          .operands = {kMachine, kPosition},
          .over_queues = BuiltinFn::kQueueReferElement,
          .over_dynamic_arrays = BuiltinFn::kDynamicArrayReferElement};
    case BuiltinFn::kDynamicArrayReferElement:
      return {
          .name = "dynamic_array_refer_element",
          .declaration = Method{"ReferElement"},
          .selects = PartSelection::kElement,
          .operands = {kMachine, kPosition}};
    case BuiltinFn::kQueueReferElement:
      return {
          .name = "queue_refer_element",
          .declaration = Method{"ReferElement"},
          .selects = PartSelection::kElement,
          .operands = {kMachine, kPosition}};
    case BuiltinFn::kAssocReferElement:
      return {
          .name = "assoc_refer_element",
          .declaration = Method{"ReferElement"},
          .selects = PartSelection::kElement,
          .operands = {kMachine, kKey}};
    case BuiltinFn::kReferComponent:
      return {
          .name = "refer_component",
          .declaration = Method{"ReferComponent"},
          .takes_the_location = false,
          .selects = PartSelection::kComponent};
    case BuiltinFn::kReferProperty:
      return {
          .name = "refer_property",
          .declaration = RuntimeFunction("ReferProperty"),
          .acts_on_object_as = TheClassReached{},
          .takes_the_location = false};
    case BuiltinFn::kLeaseObject:
      return {
          .name = "lease_object",
          .declaration = RuntimeFunction("LeaseObject"),
          .takes_the_location = false};
    case BuiltinFn::kReferElementLeased:
      return {
          .name = "dynamic_array_refer_element_leased",
          .declaration = Method{"ReferElementLeased"},
          .selects = PartSelection::kElement,
          .operands = {kMachine, kPosition},
          .over_queues = BuiltinFn::kQueueReferElementLeased};
    case BuiltinFn::kQueueReferElementLeased:
      return {
          .name = "queue_refer_element_leased",
          .declaration = Method{"ReferElementLeased"},
          .selects = PartSelection::kElement,
          .operands = {kMachine, kPosition}};
    case BuiltinFn::kAssocReferElementLeased:
      return {
          .name = "assoc_refer_element_leased",
          .declaration = Method{"ReferElementLeased"},
          .selects = PartSelection::kElement,
          .operands = {kMachine, kKey}};
    case BuiltinFn::kLeasedReference:
      return {
          .name = "leased_reference",
          .declaration = Method{"Reference"},
          .takes_a_type_argument = true,
          .takes_the_location = false};
    case BuiltinFn::kReportsTo:
      return {
          .name = "reports_to",
          .declaration = Method{"ReportsTo"},
          .takes_the_location = false};
    case BuiltinFn::kBindMember:
      return {
          .name = "bind_member",
          .declaration = RuntimeFunction("BindMember"),
          .takes_the_location = false};
    case BuiltinFn::kBindAlias:
      return {
          .name = "bind_alias",
          .declaration = RuntimeFunction("BindAlias"),
          .takes_the_location = false};
    case BuiltinFn::kBindReference:
      return {
          .name = "bind_reference",
          .declaration = RuntimeFunction("BindReference"),
          .takes_the_location = false};
    case BuiltinFn::kDrivesContinuously:
      return {
          .name = "drives_continuously",
          .declaration = RuntimeFunction("DrivesContinuously"),
          .takes_the_location = false};
    case BuiltinFn::kAttachDriver:
      return {
          .name = "attach_driver",
          .declaration = Method{"AttachDriver"},
          .takes_a_type_argument = true,
          .takes_the_location = true};
    case BuiltinFn::kAttachWholeDriver:
      return {
          .name = "attach_whole_driver",
          .declaration = Method{"AttachWholeDriver"},
          .takes_the_location = true};
    case BuiltinFn::kDriveFromVariable:
      return {
          .name = "drive_from_variable",
          .declaration = Method{"DriveFrom"},
          .takes_the_location = false};
    case BuiltinFn::kDriveFromNet:
      return {
          .name = "drive_from_net",
          .declaration = Method{"DriveFrom"},
          .takes_the_location = false};
    case BuiltinFn::kDriveFromReference:
      return {
          .name = "drive_from_reference",
          .declaration = Method{"DriveFrom"},
          .takes_the_location = false};
    case BuiltinFn::kShowSource:
      return {
          .name = "show_source",
          .declaration = Method{"ShowSource"},
          .takes_the_location = false};
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
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kNbaEffects:
      return {
          .name = "nba_effects",
          .declaration = Method{"NbaEffects"},
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kNbaEffectsAfter:
      return {
          .name = "nba_effects_after",
          .declaration = Method{"NbaEffectsAfter"},
          .takes_the_runtime_handle = true,
          .takes_the_location = false,
          .operands = {kMachine, kNumber}};
    case BuiltinFn::kNbaEffectsAfterReal:
      return {
          .name = "nba_effects_after_real",
          .declaration = Method{"NbaEffectsAfterReal"},
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kRegionEffectsAfter:
      return {
          .name = "region_effects_after",
          .declaration = Method{"RegionEffectsAfter"},
          .takes_the_runtime_handle = true,
          .takes_the_location = false,
          .operands = {kMachine, kMachine, kNumber}};
    case BuiltinFn::kRegionEffectsAfterReal:
      return {
          .name = "region_effects_after_real",
          .declaration = Method{"RegionEffectsAfterReal"},
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kPassEndEffects:
      return {
          .name = "pass_end_effects",
          .declaration = Method{"PassEndEffects"},
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kRunDetached:
      return {
          .name = "run_detached",
          .declaration = Method{"RunDetached"},
          .takes_the_runtime_handle = true,
          .takes_the_location = false,
          .callables = {kStartedPastTheCaller}};
    case BuiltinFn::kRunObserving:
      return {
          .name = "run_observing",
          .declaration = Method{"RunObserving"},
          .takes_the_runtime_handle = true,
          .takes_the_location = false,
          .callables = {kStartedPastTheCaller}};
    case BuiltinFn::kResumeInNbaRegion:
      return {
          .name = "resume_in_nba_region",
          .declaration = RuntimeFunction("ResumeInNbaRegion"),
          .takes_the_location = false};
    case BuiltinFn::kResumeInRegionAfter:
      return {
          .name = "resume_in_region_after",
          .declaration = RuntimeFunction("ResumeInRegionAfter"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false,
          .operands = {kMachine, kMachine, kNumber}};
    case BuiltinFn::kResumeInRegionAfterReal:
      return {
          .name = "resume_in_region_after_real",
          .declaration = RuntimeFunction("ResumeInRegionAfterReal"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kPostponedEffects:
      return {
          .name = "postponed_effects",
          .declaration = Method{"PostponedEffects"},
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kSubmitAtRunEnd:
      return {
          .name = "submit_at_run_end",
          .declaration = Method{"SubmitAtRunEnd"},
          .takes_the_runtime_handle = true,
          .takes_the_location = false,
          .callables = {kEffectFnOncePastTheCaller}};
    case BuiltinFn::kObservedEffects:
      return {
          .name = "observed_effects",
          .declaration = Method{"ObservedEffects"},
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kDeferredEffectsAllocate:
      return {
          .name = "deferred_effects_allocate",
          .declaration = AllocationFunction{},
          .takes_the_location = false};
    case BuiltinFn::kDeferredEffectsCommit:
      return {
          .name = "deferred_effects_commit",
          .declaration = Method{"Commit"},
          .takes_the_location = false,
          .callables = {kEffectFnOncePastTheCaller}};
    case BuiltinFn::kAssertionControlAllocate:
      return {
          .name = "assertion_control_allocate",
          .declaration = AllocationFunction{},
          .takes_the_location = false};
    case BuiltinFn::kAssertionControlCommit:
      return {
          .name = "assertion_control_commit",
          .declaration = Method{"Commit"},
          .takes_the_location = false,
          .callables = {kEffectFnOncePastTheCaller}};
    case BuiltinFn::kAssertionControlDescribe:
      return {
          .name = "assertion_control_describe",
          .declaration = Method{"Describe"},
          .takes_the_location = false};
    case BuiltinFn::kAssertionControlRegisterIn:
      return {
          .name = "assertion_control_register_in",
          .declaration = Method{"RegisterIn"},
          .takes_the_location = false};
    case BuiltinFn::kAssertionControlRegister:
      return {
          .name = "assertion_control_register",
          .declaration = Method{"Register"},
          .takes_the_location = false};
    case BuiltinFn::kAssertionControlRegisterWithin:
      return {
          .name = "assertion_control_register_within",
          .declaration = Method{"RegisterWithin"},
          .takes_the_location = false};
    case BuiltinFn::kAssertionControlAllowed:
      return {
          .name = "assertion_control_allowed",
          .declaration = Method{"Allowed"},
          .takes_the_location = false};
    case BuiltinFn::kAssertControlOfDesign:
      return {
          .name = "assert_control_of_design",
          .declaration = Method{"AssertControlOfDesign"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kAssertControlOfScope:
      return {
          .name = "assert_control_of_scope",
          .declaration = Method{"AssertControlOfScope"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kAssertControlOfBlock:
      return {
          .name = "assert_control_of_block",
          .declaration = Method{"AssertControlOfBlock"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kAssertControlOfLoop:
      return {
          .name = "assert_control_of_loop",
          .declaration = Method{"AssertControlOfLoop"},
          .takes_the_runtime_handle = true};
    case BuiltinFn::kAssertionControlSubmitObserved:
      return {
          .name = "assertion_control_submit_observed",
          .declaration = Method{"SubmitObserved"},
          .takes_the_location = false,
          .callables = {kStartedPastTheCaller}};
    case BuiltinFn::kAssertionControlSubmitFinal:
      return {
          .name = "assertion_control_submit_final",
          .declaration = Method{"SubmitFinal"},
          .takes_the_location = false,
          .callables = {kStartedPastTheCaller}};
    case BuiltinFn::kAssertionControlRecordCoverage:
      return {
          .name = "assertion_control_record_coverage",
          .declaration = Method{"RecordCoverage"},
          .takes_the_location = false};
    case BuiltinFn::kFiles:
      return {
          .name = "files",
          .declaration = Method{"Files"},
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kCancellationFor:
      return {
          .name = "cancellation_for",
          .declaration = Method{"CancellationFor"},
          .takes_the_location = false};
    case BuiltinFn::kIsCancelled:
      return {
          .name = "is_cancelled",
          .declaration = Method{"IsCancelled"},
          .takes_the_location = false};
    case BuiltinFn::kFormat:
      return {.name = "format", .declaration = ValueFunction("Format")};
    case BuiltinFn::kFormatRuntime:
      return {
          .name = "format_runtime",
          .declaration = ValueFunction("FormatRuntime")};
    case BuiltinFn::kRenderedFormatArgMake:
      return {
          .name = "rendered_format_arg_make",
          .declaration = StaticFactory{"Rendered"},
          .takes_the_location = false};
    case BuiltinFn::kPatternedFormatArgMake:
      return {
          .name = "patterned_format_arg_make",
          .declaration = StaticFactory{"Patterned"},
          .takes_the_location = false,
          .operands = {kNumber}};
    case BuiltinFn::kTextPrintItemMake:
      return {
          .name = "text_print_item_make",
          .declaration = StaticFactory{"Text"},
          .takes_the_location = false};
    case BuiltinFn::kWrite:
      return {
          .name = "write",
          .declaration = Method{"Write"},
          .takes_the_location = false};
    case BuiltinFn::kWriteln:
      return {
          .name = "writeln",
          .declaration = Method{"Writeln"},
          .takes_the_location = false};
    case BuiltinFn::kDiagnostic:
      return {
          .name = "diagnostic",
          .declaration = Method{"Diagnostic"},
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kEmitInfo:
      return {.name = "emit_info", .declaration = Method{"EmitInfo"}};
    case BuiltinFn::kEmitWarning:
      return {.name = "emit_warning", .declaration = Method{"EmitWarning"}};
    case BuiltinFn::kEmitError:
      return {.name = "emit_error", .declaration = Method{"EmitError"}};
    case BuiltinFn::kEmitFatal:
      return {.name = "emit_fatal", .declaration = Method{"EmitFatal"}};
    case BuiltinFn::kEmitInfoAtCaller:
      return {
          .name = "emit_info_at_caller",
          .declaration = Method{"EmitInfoAtCaller"}};
    case BuiltinFn::kEmitWarningAtCaller:
      return {
          .name = "emit_warning_at_caller",
          .declaration = Method{"EmitWarningAtCaller"}};
    case BuiltinFn::kEmitErrorAtCaller:
      return {
          .name = "emit_error_at_caller",
          .declaration = Method{"EmitErrorAtCaller"}};
    case BuiltinFn::kEmitFatalAtCaller:
      return {
          .name = "emit_fatal_at_caller",
          .declaration = Method{"EmitFatalAtCaller"}};
    case BuiltinFn::kTimeFormat:
      return {
          .name = "time_format",
          .declaration = Method{"TimeFormat"},
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kSetTimeFormat:
      return {
          .name = "set_time_format",
          .declaration = Method{"SetTimeFormat"},
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kResetTimeFormat:
      return {
          .name = "reset_time_format",
          .declaration = Method{"ResetTimeFormat"},
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    // The input, the format, and the value each conversion starts from, a
    // tuple crossing with its type (LRM 21.3.4.3).
    case BuiltinFn::kScanString:
      return {
          .name = "scan_string",
          .declaration = ValueFunction("ScanString"),
          .operands = {kMachine, kMachine, kTyped},
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kFileOpen:
      return {
          .name = "file_open",
          .declaration = Method{"Open"},
          .takes_the_location = false};
    case BuiltinFn::kFileOpenMode:
      return {
          .name = "file_open_mode",
          .declaration = Method{"OpenWithMode"},
          .takes_the_location = false};
    case BuiltinFn::kFileClose:
      return {
          .name = "file_close",
          .declaration = Method{"Close"},
          .takes_the_location = false};
    case BuiltinFn::kFileGetc:
      return {
          .name = "file_getc",
          .declaration = Method{"Getc"},
          .takes_the_location = false};
    case BuiltinFn::kFileUngetc:
      return {
          .name = "file_ungetc",
          .declaration = Method{"Ungetc"},
          .takes_the_location = false};
    case BuiltinFn::kFileGets:
      return {
          .name = "file_gets",
          .declaration = Method{"Gets"},
          .takes_the_location = false,
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
          .takes_the_location = false,
          .operands = {kMachine, kNumber},
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kFileReadWords:
      return {
          .name = "file_read_words",
          .declaration = Method{"ReadWords"},
          .takes_the_location = false};
    case BuiltinFn::kFileSeek:
      return {
          .name = "file_seek",
          .declaration = Method{"Seek"},
          .takes_the_location = false};
    case BuiltinFn::kFileRewind:
      return {
          .name = "file_rewind",
          .declaration = Method{"Rewind"},
          .takes_the_location = false};
    case BuiltinFn::kFileTell:
      return {
          .name = "file_tell",
          .declaration = Method{"Tell"},
          .takes_the_location = false};
    case BuiltinFn::kFileEof:
      return {
          .name = "file_eof",
          .declaration = Method{"Eof"},
          .takes_the_location = false};
    case BuiltinFn::kFileError:
      return {
          .name = "file_error",
          .declaration = Method{"Error"},
          .takes_the_location = false,
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kFileFlush:
      return {
          .name = "file_flush",
          .declaration = Method{"Flush"},
          .takes_the_location = false};
    case BuiltinFn::kFileFlushAll:
      return {
          .name = "file_flush_all",
          .declaration = Method{"FlushAll"},
          .takes_the_location = false};
    case BuiltinFn::kDumpFile:
      return {
          .name = "dump_file",
          .declaration = RuntimeFunction("DumpFile"),
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
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kDumpOn:
      return {
          .name = "dump_on",
          .declaration = RuntimeFunction("DumpOn"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kDumpAll:
      return {
          .name = "dump_all",
          .declaration = RuntimeFunction("DumpAll"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kDumpLimit:
      return {
          .name = "dump_limit",
          .declaration = RuntimeFunction("DumpLimit"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kDumpFlush:
      return {
          .name = "dump_flush",
          .declaration = RuntimeFunction("DumpFlush"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kSerializeBits:
      return {
          .name = "value_serializer_bits",
          .declaration = Method{"Bits"},
          .takes_the_location = false,
          .operands = {kMachine, kBits}};
    case BuiltinFn::kSerializeReal:
      return {
          .name = "value_serializer_real",
          .declaration = Method{"Real"},
          .takes_the_location = false};
    case BuiltinFn::kSerializeText:
      return {
          .name = "value_serializer_text",
          .declaration = Method{"SerializeText"},
          .takes_the_location = false};
    case BuiltinFn::kSerializeObject:
      return {
          .name = "value_serializer_object",
          .declaration = Method{"SerializeObject"},
          .takes_the_location = false};
    case BuiltinFn::kSerializeSize:
      return {
          .name = "value_serializer_size",
          .declaration = Method{"SerializeSize"},
          .takes_the_location = false};
    case BuiltinFn::kSerializeKey:
      return {
          .name = "value_serializer_key",
          .declaration = Method{"Key"},
          .takes_the_location = false,
          .operands = {kMachine, kTyped}};
    case BuiltinFn::kSelectionDepth:
      return {
          .name = "selection_depth",
          .declaration = Method{"Depth"},
          .takes_the_location = false};
    case BuiltinFn::kSelectionNumber:
      return {
          .name = "selection_number",
          .declaration = Method{"Number"},
          .takes_the_location = false};
    case BuiltinFn::kSelectionKey:
      return {
          .name = "selection_key",
          .declaration = Method{"Key"},
          .takes_a_type_argument = true,
          .takes_the_location = false,
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kSettleKey:
      return {
          .name = "selection_settler_settle_key",
          .declaration = Method{"Key"},
          .takes_the_location = false,
          .operands = {kMachine, kMachine, kTyped}};
    case BuiltinFn::kSettlePositions:
      return {
          .name = "selection_settler_settle_positions",
          .declaration = Method{"SettlePositions"},
          .takes_the_location = false};
    case BuiltinFn::kSettleUnfixed:
      return {
          .name = "selection_settler_settle_unfixed",
          .declaration = Method{"SettleUnfixed"},
          .takes_the_location = false};
    case BuiltinFn::kSettleWatchedStep:
      return {
          .name = "selection_settler_settle_watched_step",
          .declaration = Method{"SettleWatchedStep"},
          .takes_the_location = false};
    case BuiltinFn::kSettleUnwatched:
      return {
          .name = "selection_settler_settle_unwatched",
          .declaration = Method{"SettleUnwatched"},
          .takes_the_location = false};
    case BuiltinFn::kSettledFixed:
      return {
          .name = "selection_settler_fixed",
          .declaration = Method{"Fixed"},
          .takes_the_location = false};
    case BuiltinFn::kSettledPositions:
      return {
          .name = "selection_settler_positions",
          .declaration = Method{"Positions"},
          .takes_the_location = false};
    case BuiltinFn::kDeserializeBits:
      return {
          .name = "value_deserializer_bits",
          .declaration = Method{"Bits"},
          .takes_the_location = false};
    case BuiltinFn::kDeserializeReal:
      return {
          .name = "value_deserializer_real",
          .declaration = Method{"Real"},
          .takes_the_location = false};
    case BuiltinFn::kDeserializeText:
      return {
          .name = "value_deserializer_text",
          .declaration = Method{"Text"},
          .takes_the_location = false};
    case BuiltinFn::kSubscribe:
      return {
          .name = "subscriber_subscribe",
          .declaration = Method{"Subscribe"},
          .takes_the_location = false};
    case BuiltinFn::kDumpPortsOf:
      return {
          .name = "dump_ports_of",
          .declaration = RuntimeFunction("DumpPortsOf"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kDumpPortsOff:
      return {
          .name = "dump_ports_off",
          .declaration = RuntimeFunction("DumpPortsOff"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kDumpPortsOn:
      return {
          .name = "dump_ports_on",
          .declaration = RuntimeFunction("DumpPortsOn"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kDumpPortsAll:
      return {
          .name = "dump_ports_all",
          .declaration = RuntimeFunction("DumpPortsAll"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kDumpPortsLimit:
      return {
          .name = "dump_ports_limit",
          .declaration = RuntimeFunction("DumpPortsLimit"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kDumpPortsFlush:
      return {
          .name = "dump_ports_flush",
          .declaration = RuntimeFunction("DumpPortsFlush"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kTestPlusargs:
      return {
          .name = "test_plusargs",
          .declaration = RuntimeFunction("TestPlusargs"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kValuePlusargs:
      return {
          .name = "integral_value_plusargs",
          .declaration = RuntimeFunction("ValuePlusargs"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false,
          .operands = {kMachine, kMachine, kNumber},
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kValuePlusargsString:
      return {
          .name = "string_value_plusargs",
          .declaration = RuntimeFunction("ValuePlusargs"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false,
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kRunHostCommand:
      return {
          .name = "run_host_command",
          .declaration = RuntimeFunction("RunHostCommand"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kRunNullHostCommand:
      return {
          .name = "run_null_host_command",
          .declaration = RuntimeFunction("RunNullHostCommand"),
          .takes_the_location = false};
    case BuiltinFn::kReadMem:
      return {
          .name = "read_mem",
          .declaration = RuntimeFunction("ReadMem"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kReadMemWithin:
      return {
          .name = "read_mem_within",
          .declaration = RuntimeFunction("ReadMemWithin"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kReadEnumeratedMem:
      return {
          .name = "read_enumerated_mem",
          .declaration = RuntimeFunction("ReadMem"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kReadEnumeratedMemWithin:
      return {
          .name = "read_enumerated_mem_within",
          .declaration = RuntimeFunction("ReadMemWithin"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kWriteMem:
      return {
          .name = "write_mem",
          .declaration = RuntimeFunction("WriteMem"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kWriteMemWithin:
      return {
          .name = "write_mem_within",
          .declaration = RuntimeFunction("WriteMemWithin"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kMemoryWordsReadGrid:
      return {
          .name = "memory_words_read_grid",
          .declaration = Method{"ReadGrid"},
          .mutates_receiver = true};
    case BuiltinFn::kMemoryWordsReadDense:
      return {
          .name = "memory_words_read_dense",
          .declaration = Method{"ReadDense"},
          .mutates_receiver = true};
    case BuiltinFn::kMemoryWordsReadKeyed:
      return {
          .name = "memory_words_read_keyed",
          .declaration = Method{"ReadKeyed"},
          .mutates_receiver = true};
    case BuiltinFn::kMemoryWordsPlanGrid:
      return {
          .name = "memory_words_plan_grid",
          .declaration = Method{"PlanGrid"},
          .takes_the_location = false,
          .mutates_receiver = true};
    case BuiltinFn::kMemoryWordsPlanDense:
      return {
          .name = "memory_words_plan_dense",
          .declaration = Method{"PlanDense"},
          .takes_the_location = false,
          .mutates_receiver = true};
    case BuiltinFn::kMemoryWordsCount:
      return {
          .name = "memory_words_count",
          .declaration = Method{"Count"},
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kMemoryWordsOrdinal:
      return {
          .name = "memory_words_ordinal",
          .declaration = Method{"Ordinal"},
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kMemoryWordsKey:
      return {
          .name = "memory_words_key",
          .declaration = Method{"Key"},
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kMemoryWordsBytesRead:
      return {
          .name = "memory_words_bytes_read",
          .declaration = Method{"BytesRead"},
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kMemoryWordsWord:
      return {
          .name = "memory_words_word",
          .declaration = Method{"Word"},
          .takes_a_type_argument = true,
          .takes_the_location = false,
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kMemoryWordsPut:
      return {
          .name = "memory_words_put",
          .declaration = Method{"Put"},
          .takes_the_location = false,
          .mutates_receiver = true,
          .operands = {kMachine, kMachine, kTyped}};
    case BuiltinFn::kMemoryWordsPutKeyed:
      return {
          .name = "memory_words_put_keyed",
          .declaration = Method{"PutKeyed"},
          .takes_the_location = false,
          .mutates_receiver = true,
          .operands = {kMachine, kMachine, kTyped}};
    case BuiltinFn::kMemoryWordsCommit:
      return {
          .name = "memory_words_commit",
          .declaration = Method{"Commit"},
          .mutates_receiver = true};
    case BuiltinFn::kLoadWords:
      return {
          .name = "load_words",
          .declaration = Method{"LoadWords"},
          .mutates_receiver = true};
    case BuiltinFn::kStoreWords:
      return {.name = "store_words", .declaration = Method{"StoreWords"}};
    case BuiltinFn::kToImage:
      return {
          .name = "to_image",
          .declaration = Method{"ToImage"},
          .takes_the_location = false};
    case BuiltinFn::kFromImage:
      return {
          .name = "from_image",
          .declaration = Method{"FromImage"},
          .takes_the_location = false,
          .mutates_receiver = true};
    case BuiltinFn::kWalkRandom:
      return {.name = "walk_random", .declaration = Method{"WalkRandom"}};
    case BuiltinFn::kDelay:
      return {
          .name = "delay",
          .declaration = RuntimeFunction("Delay"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false,
          .operands = {kMachine, kNumber}};
    case BuiltinFn::kDelayReal:
      return {
          .name = "delay_real",
          .declaration = RuntimeFunction("DelayReal"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kDelayTicks:
      return {
          .name = "delay_ticks",
          .declaration = RuntimeFunction("DelayTicks"),
          .takes_the_location = false,
          .operands = {kNumber}};
    case BuiltinFn::kDelayTicksReal:
      return {
          .name = "delay_ticks_real",
          .declaration = RuntimeFunction("DelayTicksReal"),
          .takes_the_location = false};
    case BuiltinFn::kScalarPropagationDelay:
      return {
          .name = "scalar_propagation_delay",
          .declaration = RuntimeFunction("ScalarPropagationDelay"),
          .takes_the_location = false,
          .operands = {kBits}};
    case BuiltinFn::kVectorPropagationDelay:
      return {
          .name = "vector_propagation_delay",
          .declaration = RuntimeFunction("VectorPropagationDelay"),
          .takes_the_location = false,
          .operands = {kBits}};
    case BuiltinFn::kObservationOnReaching:
      return {
          .name = "observation_on_reaching",
          .declaration = StaticFactory{"OnReaching"},
          .takes_the_location = false};
    case BuiltinFn::kObservationOfWatch:
      return {
          .name = "observation_of_watch",
          .declaration = StaticFactory{"OfWatch"},
          .takes_the_location = false,
          .callables = {kWatchFnMutWhileItsAnswerIsHeld}};
    case BuiltinFn::kObservationOfWatchQualified:
      return {
          .name = "observation_of_watch_qualified",
          .declaration = StaticFactory{"OfWatchQualified"},
          .takes_the_location = false,
          .callables = {
              kWatchFnMutWhileItsAnswerIsHeld,
              kConditionFnWhileItsAnswerIsHeld}};
    case BuiltinFn::kObservationFires:
      return {
          .name = "observation_fires",
          .declaration = Method{"Fires"},
          .takes_the_location = false};
    case BuiltinFn::kWaitRecollecting:
      return {
          .name = "wait_recollecting",
          .declaration = RuntimeFunction("WaitRecollecting"),
          .takes_the_location = false};
    case BuiltinFn::kWaitUntil:
      return {
          .name = "wait_until",
          .declaration = RuntimeFunction("WaitUntil"),
          .takes_the_location = false};
    case BuiltinFn::kWaitOn:
      return {
          .name = "wait_on",
          .declaration = RuntimeFunction("WaitOn"),
          .takes_the_location = false};
    case BuiltinFn::kWaitPerChangeOnImplicitList:
      return {
          .name = "wait_per_change_on_implicit_list",
          .declaration = RuntimeFunction("WaitPerChangeOnImplicitList"),
          .takes_the_location = false};
    case BuiltinFn::kWaitPerIntervalOnImplicitList:
      return {
          .name = "wait_per_interval_on_implicit_list",
          .declaration = RuntimeFunction("WaitPerIntervalOnImplicitList"),
          .takes_the_location = false};
    case BuiltinFn::kWaitContinuouslyOn:
      return {
          .name = "wait_continuously_on",
          .declaration = RuntimeFunction("WaitContinuouslyOn"),
          .takes_the_location = false};
    case BuiltinFn::kParkAt:
      return {
          .name = "park_at",
          .declaration = RuntimeFunction("ParkAt"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kReadReportEmpty:
      return {
          .name = "read_report_empty",
          .declaration = StaticFactory{"Empty"},
          .takes_the_location = false};
    case BuiltinFn::kReadReportForImplicitList:
      return {
          .name = "read_report_for_implicit_list",
          .declaration = StaticFactory{"ForImplicitList"},
          .takes_the_location = false};
    case BuiltinFn::kReadReportAdd:
      return {
          .name = "read_report_add",
          .declaration = Method{"Add"},
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kReadReportAddObject:
      return {
          .name = "read_report_add_object",
          .declaration = Method{"AddObject"},
          .acts_on_object_as = LibraryDecl::kObjectRoot,
          .takes_the_location = false};
    case BuiltinFn::kReadReportAddWrite:
      return {
          .name = "read_report_add_write",
          .declaration = Method{"AddWrite"},
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kReadReportSettleAsImplicitList:
      return {
          .name = "read_report_settle_as_implicit_list",
          .declaration = Method{"SettleAsImplicitList"},
          .takes_the_location = false};
    case BuiltinFn::kReadReportEnter:
      return {
          .name = "read_report_enter",
          .declaration = Method{"Enter"},
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kReadReportLeave:
      return {
          .name = "read_report_leave",
          .declaration = Method{"Leave"},
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kReadReportRunsTheBody:
      return {
          .name = "read_report_runs_the_body",
          .declaration = Method{"RunsTheBody"},
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kReadReportBeginEvaluation:
      return {
          .name = "read_report_begin_evaluation",
          .declaration = Method{"BeginEvaluation"},
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kReadReportEndEvaluation:
      return {
          .name = "read_report_end_evaluation",
          .declaration = Method{"EndEvaluation"},
          .takes_the_runtime_handle = true,
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kEvaluationInProgress:
      return {
          .name = "evaluation_in_progress",
          .declaration = RuntimeFunction("EvaluationInProgress"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kRefuseReport:
      return {
          .name = "refuse_report",
          .declaration = RuntimeFunction("RefuseReport")};
    case BuiltinFn::kSimTime:
      return {
          .name = "sim_time",
          .declaration = RuntimeFunction("SimTimeInUnit"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kSTime:
      return {
          .name = "stime",
          .declaration = RuntimeFunction("STimeInUnit"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kRealTime:
      return {
          .name = "realtime",
          .declaration = RuntimeFunction("RealTimeInUnit"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kUrandom:
      return {
          .name = "urandom",
          .declaration = RuntimeFunction("Urandom"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kUrandomSeeded:
      return {
          .name = "urandom_seeded",
          .declaration = RuntimeFunction("UrandomSeeded"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kUrandomRange:
      return {
          .name = "urandom_range",
          .declaration = RuntimeFunction("UrandomRange"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kRandom:
      return {
          .name = "random",
          .declaration = RuntimeFunction("Random"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kDistUniform:
      return {
          .name = "dist_uniform",
          .declaration = RuntimeFunction("DistUniform"),
          .takes_the_location = false,
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kDistNormal:
      return {
          .name = "dist_normal",
          .declaration = RuntimeFunction("DistNormal"),
          .takes_the_location = false,
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
          .returns = false};
    case BuiltinFn::kStop:
      return {
          .name = "stop",
          .declaration = RuntimeFunction("Stop"),
          .takes_the_runtime_handle = true,
          .returns = false};
    case BuiltinFn::kExit:
      return {
          .name = "exit",
          .declaration = RuntimeFunction("Exit"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kEnclosingScope:
      return {
          .name = "enclosing_scope",
          .declaration = Method{"EnclosingScope"},
          .takes_the_location = false};
    case BuiltinFn::kLoopBlocksAdd:
      return {
          .name = "loop_blocks_add",
          .declaration = Method{"Add"},
          .takes_the_location = false};
    case BuiltinFn::kLoopBlocksAt:
      return {
          .name = "loop_blocks_at",
          .declaration = Method{"At"},
          .takes_the_location = false};
    case BuiltinFn::kSetScopePlace:
      return {
          .name = "set_scope_place",
          .declaration = Method{"SetPlace"},
          .takes_the_location = false};
    case BuiltinFn::kAttachScope:
      return {
          .name = "attach_scope",
          .declaration =
              FreeFunction{
                  .scope = "lyra::runtime::Scope", .identifier = "Attach"},
          .takes_the_location = false};
    case BuiltinFn::kInstantiationEntry:
      return {
          .name = "instantiation_entry",
          .declaration = RuntimeFunction("InstantiationEntry"),
          .takes_the_location = false};
    case BuiltinFn::kInstantiationTable:
      return {
          .name = "instantiation_table",
          .declaration = RuntimeFunction("InstantiationTable"),
          .takes_the_location = false};
    case BuiltinFn::kBlockEntry:
      return {
          .name = "block_entry",
          .declaration = RuntimeFunction("BlockEntry"),
          .takes_the_location = false};
    case BuiltinFn::kBlockTable:
      return {
          .name = "block_table",
          .declaration = RuntimeFunction("BlockTable"),
          .takes_the_location = false};
    case BuiltinFn::kExtendSequence:
      return {
          .name = "sequence_extend",
          .declaration = RuntimeFunction("ExtendSequence"),
          .takes_the_location = false};
    case BuiltinFn::kObjectOf:
      return {
          .name = "object_of",
          .declaration = Method{"Object"},
          .takes_a_type_argument = true};
    case BuiltinFn::kDynamicCast:
      return {
          .name = "dynamic_cast",
          .declaration = RuntimeFunction("DynamicCast"),
          .takes_the_location = false};
    case BuiltinFn::kIsOfInterfaceOf:
      return {
          .name = "is_of_interface_of",
          .declaration = RuntimeFunction("IsOfInterfaceOf"),
          .takes_the_location = false};
    case BuiltinFn::kPtrRead:
      return {
          .name = "ptr_read",
          .declaration =
              FreeFunction{.scope = "lyra::value", .identifier = "PtrRead"},
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kDefaultPlacementNew:
      return {
          .name = "default_placement_new",
          .declaration = ReservedPlacementForm{},
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kRecyclerAllocate:
      return {
          .name = "recycler_allocate",
          .declaration =
              FreeFunction{
                  .scope = "lyra::value", .identifier = "RecyclerAllocate"},
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kElementPlace:
      return {.name = "element_place", .declaration = Method{"ElementPlace"}};
    case BuiltinFn::kElementDefault:
      return {
          .name = "element_default",
          .declaration =
              FreeFunction{
                  .scope = "lyra::value", .identifier = "ElementDefault"},
          .takes_a_type_argument = true,
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kDiscardedWrite:
      return {
          .name = "discarded_write",
          .declaration =
              FreeFunction{
                  .scope = "lyra::value", .identifier = "DiscardedWrite"},
          .takes_a_type_argument = true,
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kTypeRecordOf:
      return {
          .name = "type_record_of",
          .declaration =
              FreeFunction{
                  .scope = "lyra::value", .identifier = "TypeRecordOf"},
          .takes_a_type_argument = true,
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kUnsize:
      return {
          .name = "unsize",
          .declaration = RuntimeFunction("Unsize"),
          .takes_a_type_argument = true,
          .type_argument_named = TypeArgumentNamed::kTheCallSignature,
          .takes_the_location = false};
    case BuiltinFn::kAnyMake:
      return {
          .name = "any_make",
          .declaration = RuntimeFunction("MakeAny"),
          .takes_the_location = false,
          .operands = {kTyped, kMachine}};
    case BuiltinFn::kSeeAsAny:
      return {
          .name = "see_as_any",
          .declaration = RuntimeFunction("SeeAsAny"),
          .takes_a_type_argument = true,
          .takes_the_location = false,
          .operands = {kMachine, kMachine, kMachine},
          .answer_told = AnswerTold::kTypeCalledAt,
          .callables = {kEffectFnWhileItsAnswerIsHeld}};
    case BuiltinFn::kReceiveAny:
      return {
          .name = "receive_any",
          .declaration = RuntimeFunction("ReceiveAny"),
          .takes_the_location = false,
          .operands = {kMachine, kMachine}};
    case BuiltinFn::kObjectEventSource:
      return {
          .name = "object_event_source",
          .declaration = RuntimeFunction("EventSourceOf"),
          .acts_on_object_as = LibraryDecl::kObjectRoot,
          .takes_the_location = false};
    case BuiltinFn::kOpenObjectWrite:
      return {
          .name = "open_object_write",
          .declaration = RuntimeFunction("ErasedObjectWrite"),
          .acts_on_object_as = TheClassReached{},
          .takes_the_location = false};
    case BuiltinFn::kWrittenObject:
      return {
          .name = "written_object",
          .declaration = Method{"Object"},
          .takes_the_location = false};
    case BuiltinFn::kForkWaitAll:
      return {
          .name = "fork_wait_all",
          .declaration = RuntimeFunction("ForkWaitAll"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false,
          .callables = {kStartedWhileItsAnswerIsHeld}};
    case BuiltinFn::kForkWaitFirst:
      return {
          .name = "fork_wait_first",
          .declaration = RuntimeFunction("ForkWaitFirst"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false,
          .callables = {kStartedPastTheCaller}};
    case BuiltinFn::kSpawnAll:
      return {
          .name = "spawn_all",
          .declaration = RuntimeFunction("SpawnAll"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false,
          .callables = {kStartedPastTheCaller}};
    case BuiltinFn::kWaitFork:
      return {
          .name = "wait_fork",
          .declaration = RuntimeFunction("WaitFork"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kDisableFork:
      return {
          .name = "disable_fork",
          .declaration = RuntimeFunction("DisableFork"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kRequireCallerMayScheduleAfterReturn:
      return {
          .name = "require_caller_may_schedule_after_return",
          .declaration = RuntimeFunction("RequireCallerMayScheduleAfterReturn"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kTakeNoThreadContext:
      return {
          .name = "take_no_thread_context",
          .declaration = RuntimeFunction("TakeNoThreadContext"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kEnterNoThread:
      return {
          .name = "enter_no_thread",
          .declaration = Method{"Enter"},
          .takes_the_location = false};
    case BuiltinFn::kDisable:
      return {
          .name = "disable",
          .declaration = RuntimeFunction("Disable"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kEnterTarget:
      return {
          .name = "enter_target",
          .declaration = RuntimeFunction("EnterCancellationTarget"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kLeaveTarget:
      return {
          .name = "leave_target",
          .declaration = RuntimeFunction("LeaveCancellationTarget"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kRegionLandsEffect:
      return {
          .name = "region_lands_effect",
          .declaration = RuntimeFunction("RegionLandsEffect"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kReceiveDeparture:
      return {
          .name = "receive_departure",
          .declaration = RuntimeFunction("ReceiveDeparture"),
          .takes_the_location = false};
    case BuiltinFn::kProcessSelf:
      return {
          .name = "process_self",
          .declaration = RuntimeFunction("ProcessSelf"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
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
    case BuiltinFn::kWeakDowngrade:
      return {
          .name = "weak_downgrade",
          .declaration = RuntimeFunction("Downgrade"),
          .takes_the_location = false};
    case BuiltinFn::kWeakUpgrade:
      return {
          .name = "weak_upgrade",
          .declaration = RuntimeFunction("Upgrade"),
          .takes_the_read_report = true,
          .takes_the_location = false};
    case BuiltinFn::kWeakRefersTo:
      return {
          .name = "weak_refers_to",
          .declaration = RuntimeFunction("WeakRefersTo"),
          .takes_the_location = false};
    case BuiltinFn::kObjectIdentity:
      return {
          .name = "object_identity",
          .declaration = RuntimeFunction("ObjectIdentity"),
          .takes_the_location = false};
    case BuiltinFn::kBuiltinPreRandomize:
      return {
          .name = "builtin_pre_randomize",
          .declaration = RuntimeFunction("BuiltinPreRandomize"),
          .takes_the_location = false};
    case BuiltinFn::kBuiltinPostRandomize:
      return {
          .name = "builtin_post_randomize",
          .declaration = RuntimeFunction("BuiltinPostRandomize"),
          .takes_the_location = false};
    case BuiltinFn::kObjectPreRandomize:
      return {
          .name = "object_pre_randomize",
          .declaration = RuntimeFunction("ObjectPreRandomize"),
          .acts_on_object_as = LibraryDecl::kObjectRoot,
          .takes_the_location = false};
    case BuiltinFn::kObjectPostRandomize:
      return {
          .name = "object_post_randomize",
          .declaration = RuntimeFunction("ObjectPostRandomize"),
          .acts_on_object_as = LibraryDecl::kObjectRoot,
          .takes_the_location = false};
    case BuiltinFn::kObjectSrandom:
      return {
          .name = "object_srandom",
          .declaration = RuntimeFunction("ObjectSrandom"),
          .acts_on_object_as = LibraryDecl::kClassObject,
          .takes_the_location = false,
          .operands = {kMachine, kNumber}};
    case BuiltinFn::kObjectGetRandstate:
      return {
          .name = "object_get_randstate",
          .declaration = RuntimeFunction("ObjectGetRandstate"),
          .acts_on_object_as = LibraryDecl::kClassObject,
          .takes_the_location = false};
    case BuiltinFn::kObjectSetRandstate:
      return {
          .name = "object_set_randstate",
          .declaration = RuntimeFunction("ObjectSetRandstate"),
          .acts_on_object_as = LibraryDecl::kClassObject};
    case BuiltinFn::kRandomizeObject:
      return {
          .name = "randomize_object",
          .declaration = RuntimeFunction("RandomizeObject"),
          .acts_on_object_as = LibraryDecl::kClassObject,
          .takes_the_runtime_handle = true};
    case BuiltinFn::kRandomizeObjectListed:
      return {
          .name = "randomize_object_listed",
          .declaration = RuntimeFunction("RandomizeObjectListed"),
          .acts_on_object_as = LibraryDecl::kClassObject,
          .takes_the_runtime_handle = true};
    case BuiltinFn::kConstraintBlockEnabled:
      return {
          .name = "constraint_block_enabled",
          .declaration = RuntimeFunction("ConstraintBlockEnabled"),
          .acts_on_object_as = LibraryDecl::kObjectRoot,
          .takes_the_location = false};
    case BuiltinFn::kObjectRandModeSet:
      return {
          .name = "object_rand_mode_set",
          .declaration = RuntimeFunction("ObjectRandModeSet"),
          .acts_on_object_as = LibraryDecl::kObjectRoot,
          .takes_the_location = false};
    case BuiltinFn::kConstraintModeSet:
      return {
          .name = "constraint_mode_set",
          .declaration = RuntimeFunction("ConstraintModeSet"),
          .acts_on_object_as = LibraryDecl::kObjectRoot,
          .takes_the_location = false};
    case BuiltinFn::kConstraintModeGet:
      return {
          .name = "constraint_mode_get",
          .declaration = RuntimeFunction("ConstraintModeGet"),
          .acts_on_object_as = LibraryDecl::kObjectRoot,
          .takes_the_location = false};
    case BuiltinFn::kObjectConstraintModeSet:
      return {
          .name = "object_constraint_mode_set",
          .declaration = RuntimeFunction("ObjectConstraintModeSet"),
          .acts_on_object_as = LibraryDecl::kObjectRoot,
          .takes_the_location = false};
    case BuiltinFn::kRandomizeScope:
      return {
          .name = "randomize_scope",
          .declaration = RuntimeFunction("RandomizeScope"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kRandomizeList:
      return {
          .name = "randomize_list",
          .declaration = RuntimeFunction("RandomizeList"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintMembers:
      return {
          .name = "constraint_members",
          .declaration = RuntimeFunction("ConstraintMembers"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintRand:
      return {
          .name = "constraint_rand",
          .declaration = RuntimeFunction("ConstraintRand"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintRandc:
      return {
          .name = "constraint_randc",
          .declaration = RuntimeFunction("ConstraintRandc"),
          .takes_the_location = false};
    case BuiltinFn::kRandomWalkFixedArray:
      return {
          .name = "random_walk_fixed_array",
          .declaration = RuntimeFunction("RandomWalkFixedArray"),
          .takes_the_location = false};
    case BuiltinFn::kRandomWalkResizableArray:
      return {
          .name = "random_walk_resizable_array",
          .declaration = RuntimeFunction("RandomWalkResizableArray"),
          .takes_the_location = false};
    case BuiltinFn::kRandomWalkEnter:
      return {
          .name = "random_walk_enter",
          .declaration = RuntimeFunction("RandomWalkEnter"),
          .takes_the_location = false};
    case BuiltinFn::kRandomWalkLeave:
      return {
          .name = "random_walk_leave",
          .declaration = RuntimeFunction("RandomWalkLeave"),
          .takes_the_location = false};
    case BuiltinFn::kRandomWalkLeaf:
      return {
          .name = "random_walk_leaf",
          .declaration = RuntimeFunction("RandomWalkLeaf"),
          .takes_the_location = false};
    case BuiltinFn::kRandomWalkEnd:
      return {
          .name = "random_walk_end",
          .declaration = RuntimeFunction("RandomWalkEnd"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintVariable:
      return {
          .name = "constraint_variable",
          .declaration = RuntimeFunction("ConstraintVariable"),
          .takes_the_location = false,
          .operands = {kMachine, kMachine, kTyped}};
    case BuiltinFn::kConstraintConstant:
      return {
          .name = "constraint_constant",
          .declaration = RuntimeFunction("ConstraintConstant"),
          .takes_the_location = false,
          .operands = {kMachine, kTyped}};
    case BuiltinFn::kConstraintNegate:
      return {
          .name = "constraint_negate",
          .declaration = RuntimeFunction("ConstraintNegate"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintBitwiseNot:
      return {
          .name = "constraint_bitwise_not",
          .declaration = RuntimeFunction("ConstraintBitwiseNot"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintReduceAnd:
      return {
          .name = "constraint_reduce_and",
          .declaration = RuntimeFunction("ConstraintReduceAnd"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintReduceOr:
      return {
          .name = "constraint_reduce_or",
          .declaration = RuntimeFunction("ConstraintReduceOr"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintReduceXor:
      return {
          .name = "constraint_reduce_xor",
          .declaration = RuntimeFunction("ConstraintReduceXor"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintLogicalNot:
      return {
          .name = "constraint_logical_not",
          .declaration = RuntimeFunction("ConstraintLogicalNot"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintAdd:
      return {
          .name = "constraint_add",
          .declaration = RuntimeFunction("ConstraintAdd"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintSubtract:
      return {
          .name = "constraint_subtract",
          .declaration = RuntimeFunction("ConstraintSubtract"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintMultiply:
      return {
          .name = "constraint_multiply",
          .declaration = RuntimeFunction("ConstraintMultiply"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintDivide:
      return {
          .name = "constraint_divide",
          .declaration = RuntimeFunction("ConstraintDivide"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintRemainder:
      return {
          .name = "constraint_remainder",
          .declaration = RuntimeFunction("ConstraintRemainder"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintBitwiseAnd:
      return {
          .name = "constraint_bitwise_and",
          .declaration = RuntimeFunction("ConstraintBitwiseAnd"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintBitwiseOr:
      return {
          .name = "constraint_bitwise_or",
          .declaration = RuntimeFunction("ConstraintBitwiseOr"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintBitwiseXor:
      return {
          .name = "constraint_bitwise_xor",
          .declaration = RuntimeFunction("ConstraintBitwiseXor"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintShiftLeft:
      return {
          .name = "constraint_shift_left",
          .declaration = RuntimeFunction("ConstraintShiftLeft"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintShiftRight:
      return {
          .name = "constraint_shift_right",
          .declaration = RuntimeFunction("ConstraintShiftRight"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintEqual:
      return {
          .name = "constraint_equal",
          .declaration = RuntimeFunction("ConstraintEqual"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintLess:
      return {
          .name = "constraint_less",
          .declaration = RuntimeFunction("ConstraintLess"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintLogicalAnd:
      return {
          .name = "constraint_logical_and",
          .declaration = RuntimeFunction("ConstraintLogicalAnd"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintLogicalOr:
      return {
          .name = "constraint_logical_or",
          .declaration = RuntimeFunction("ConstraintLogicalOr"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintConcatenate:
      return {
          .name = "constraint_concatenate",
          .declaration = RuntimeFunction("ConstraintConcatenate"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintSelect:
      return {
          .name = "constraint_select",
          .declaration = RuntimeFunction("ConstraintSelect"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintResize:
      return {
          .name = "constraint_resize",
          .declaration = RuntimeFunction("ConstraintResize"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintSlice:
      return {
          .name = "constraint_slice",
          .declaration = RuntimeFunction("ConstraintSlice"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintRequire:
      return {
          .name = "constraint_require",
          .declaration = RuntimeFunction("ConstraintRequire"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintPrefer:
      return {
          .name = "constraint_prefer",
          .declaration = RuntimeFunction("ConstraintPrefer"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintDisableSoft:
      return {
          .name = "constraint_disable_soft",
          .declaration = RuntimeFunction("ConstraintDisableSoft"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintSolveBefore:
      return {
          .name = "constraint_solve_before",
          .declaration = RuntimeFunction("ConstraintSolveBefore"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintDistribution:
      return {
          .name = "constraint_distribution",
          .declaration = RuntimeFunction("ConstraintDistribution"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintDistributionItem:
      return {
          .name = "constraint_distribution_item",
          .declaration = RuntimeFunction("ConstraintDistributionItem"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintDistributionDefault:
      return {
          .name = "constraint_distribution_default",
          .declaration = RuntimeFunction("ConstraintDistributionDefault"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintUniqueSet:
      return {
          .name = "constraint_unique_set",
          .declaration = RuntimeFunction("ConstraintUniqueSet"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintUniqueMember:
      return {
          .name = "constraint_unique_member",
          .declaration = RuntimeFunction("ConstraintUniqueMember"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintUniqueArray:
      return {
          .name = "constraint_unique_array",
          .declaration = RuntimeFunction("ConstraintUniqueArray"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintUniqueRequire:
      return {
          .name = "constraint_unique_require",
          .declaration = RuntimeFunction("ConstraintUniqueRequire"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintArraySize:
      return {
          .name = "constraint_array_size",
          .declaration = RuntimeFunction("ConstraintArraySize"),
          .takes_the_location = false};
    case BuiltinFn::kRandomizeSettleFirst:
      return {
          .name = "randomize_settle_first",
          .declaration = RuntimeFunction("RandomizeSettleFirst"),
          .takes_the_location = false};
    case BuiltinFn::kConstraintWant:
      return {
          .name = "constraint_want",
          .declaration = RuntimeFunction("ConstraintWant"),
          .takes_the_location = false};
    case BuiltinFn::kRandomizeConstrain:
      return {
          .name = "randomize_constrain",
          .declaration = RuntimeFunction("RandomizeConstrain"),
          .takes_the_location = false};
    case BuiltinFn::kRandomizeFinish:
      return {
          .name = "randomize_finish",
          .declaration = RuntimeFunction("RandomizeFinish"),
          .takes_the_location = false};
    case BuiltinFn::kRandomizeNested:
      return {
          .name = "randomize_nested",
          .declaration = RuntimeFunction("RandomizeNested"),
          .takes_the_location = false};
    case BuiltinFn::kStaticConstraintBlockEnabled:
      return {
          .name = "static_constraint_block_enabled",
          .declaration = RuntimeFunction("StaticConstraintBlockEnabled"),
          .takes_the_location = false};
    case BuiltinFn::kRandModeSet:
      return {
          .name = "rand_mode_set",
          .declaration = RuntimeFunction("RandModeSet"),
          .takes_the_location = false};
    case BuiltinFn::kRandModeGet:
      return {
          .name = "rand_mode_get",
          .declaration = RuntimeFunction("RandModeGet"),
          .takes_the_location = false};
    case BuiltinFn::kStaticConstraintModeSet:
      return {
          .name = "static_constraint_mode_set",
          .declaration = RuntimeFunction("StaticConstraintModeSet"),
          .takes_the_location = false};
    case BuiltinFn::kStaticConstraintModeGet:
      return {
          .name = "static_constraint_mode_get",
          .declaration = RuntimeFunction("StaticConstraintModeGet"),
          .takes_the_location = false};
    case BuiltinFn::kRegisterCoroutine:
      return {
          .name = "register_coroutine",
          .declaration = RuntimeFunction("RegisterCoroutine"),
          .takes_the_location = false,
          .callables = {kStartedPastTheCaller},
          .hands_the_run_a_body = HandsTheRunABody{
              .body_at = 4, .as_a_reaction = BuiltinFn::kRegisterReaction}};
    case BuiltinFn::kRegisterReaction:
      return {
          .name = "register_reaction",
          .declaration = RuntimeFunction("RegisterReaction"),
          .takes_the_location = false};
    case BuiltinFn::kRegisterFinal:
      return {
          .name = "register_final",
          .declaration = RuntimeFunction("RegisterFinalProcedure"),
          .takes_the_location = false,
          .callables = {kEffectFnOncePastTheCaller}};
    case BuiltinFn::kRunAssertionAction:
      return {
          .name = "run_assertion_action",
          .declaration = RuntimeFunction("RunAssertionAction"),
          .takes_the_location = false,
          .callables = {kStartedPastTheCaller}};
    case BuiltinFn::kEnterScopeStaticInit:
      return {
          .name = "enter_scope_static_init",
          .declaration = RuntimeFunction("EnterScopeStaticInit"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kEnterNamespaceStaticInit:
      return {
          .name = "enter_namespace_static_init",
          .declaration = RuntimeFunction("EnterNamespaceStaticInit"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kLeaveStaticInit:
      return {
          .name = "leave_static_init",
          .declaration = RuntimeFunction("LeaveStaticInit"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false,
          .can_unwind = false};
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
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kExportedTaskReturnValue:
      return {
          .name = "exported_task_return_value",
          .declaration = RuntimeFunction("ExportedTaskReturnValue"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kContextImportTaskReturned:
      return {
          .name = "context_import_task_returned",
          .declaration = RuntimeFunction("ContextImportTaskReturned"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kContextImportFunctionReturned:
      return {
          .name = "context_import_function_returned",
          .declaration = RuntimeFunction("ContextImportFunctionReturned"),
          .takes_the_runtime_handle = true};
    case BuiltinFn::kCheckExportReachable:
      return {
          .name = "check_export_reachable",
          .declaration = RuntimeFunction("CheckExportReachable"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kBeforeForeignCodeRuns:
      return {
          .name = "before_foreign_code_runs",
          .declaration = RuntimeFunction("BeforeForeignCodeRuns"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kTakeDepartureIfDue:
      return {
          .name = "take_departure_if_due",
          .declaration = RuntimeFunction("TakeDepartureIfDue"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kClaimNamespaceInitialize:
      return {
          .name = "claim_namespace_initialize",
          .declaration = RuntimeFunction("ClaimNamespaceInitialization"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kRegisterNamespace:
      return {
          .name = "register_namespace",
          .declaration = RuntimeFunction("RegisterNamespace"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kToInt64:
      return IntegralEntry(IntegralOp::kToInt64, Method{"ToInt64"});
    case BuiltinFn::kToIntegral:
      return {
          .name = "to_integral",
          .declaration = Method{"ToIntegral"},
          .takes_a_type_argument = true,
          .takes_the_location = false,
          .answer_told = AnswerTold::kIntegralExtent};
    case BuiltinFn::kTruncate:
      return {
          .name = "truncate",
          .declaration = Method{"Truncate"},
          .takes_the_location = false};
    case BuiltinFn::kToBits:
      return {
          .name = "to_bits",
          .declaration = Method{"ToBits"},
          .takes_the_location = false};
    case BuiltinFn::kFromBits:
      return {
          .name = "from_bits",
          .declaration = StaticFactory{"FromBits"},
          .takes_a_type_argument = true,
          .takes_the_location = false};
    case BuiltinFn::kStringCStr:
      return {
          .name = "string_cstr",
          .declaration = Method{"CStr"},
          .takes_the_location = false};
    case BuiltinFn::kHandStringToForeignCaller:
      return {
          .name = "hand_string_to_foreign_caller",
          .declaration = RuntimeFunction("HandStringToForeignCaller"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false};
    case BuiltinFn::kToSvLogic:
      return {
          .name = "to_sv_logic",
          .declaration = ValueFunction("ToSvLogic"),
          .takes_the_location = false,
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
          .takes_the_location = false,
          .operands = {kMachine, kBits}};
    case BuiltinFn::kWriteCanonicalLogicVec:
      return {
          .name = "write_canonical_logic_vec",
          .declaration = ValueFunction("WriteCanonicalLogicVec"),
          .takes_the_location = false,
          .operands = {kMachine, kBits}};
    case BuiltinFn::kDpiBitBufferData:
      return {
          .name = "dpi_bit_buffer_data",
          .declaration = Method{"Data"},
          .takes_the_location = false};
    case BuiltinFn::kDpiLogicBufferData:
      return {
          .name = "dpi_logic_buffer_data",
          .declaration = Method{"Data"},
          .takes_the_location = false};
    case BuiltinFn::kDpiOpenArrayHandle:
      return {
          .name = "dpi_open_array_handle",
          .declaration = Method{"Handle"},
          .takes_the_location = false};
    case BuiltinFn::kDpiOpenArrayElementCount:
      return {
          .name = "dpi_open_array_element_count",
          .declaration = Method{"ElementCount"},
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kDpiOpenArrayOrdinal:
      return {
          .name = "dpi_open_array_ordinal",
          .declaration = Method{"Ordinal"},
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kDpiOpenArrayPut:
      return {
          .name = "dpi_open_array_put",
          .declaration = Method{"Put"},
          .takes_the_location = false,
          .mutates_receiver = true,
          .operands = {kMachine, kMachine, kTyped}};
    case BuiltinFn::kDpiOpenArrayWord:
      return {
          .name = "dpi_open_array_word",
          .declaration = Method{"Word"},
          .takes_a_type_argument = true,
          .takes_the_location = false,
          .answer_told = AnswerTold::kAnswerType};
    case BuiltinFn::kRunForeignTaskOnFiber:
      return {
          .name = "run_foreign_task_on_fiber",
          .declaration = RuntimeFunction("RunForeignTaskOnFiber"),
          .takes_the_runtime_handle = true,
          .takes_the_location = false,
          .callables = {kEffectFnOnceWhileItsAnswerIsHeld}};
    case BuiltinFn::kRunExportedTaskToCompletion:
      return {
          .name = "run_exported_task_to_completion",
          .declaration = RuntimeFunction("RunExportedTaskToCompletion"),
          .takes_the_location = false,
          .callables = {kStartedForTheCall}};
    case BuiltinFn::kCurrentExportScope:
      return {
          .name = "current_export_scope",
          .declaration = RuntimeFunction("CurrentExportScope"),
          .takes_the_location = false};
    case BuiltinFn::kFindExportEntry:
      return {
          .name = "find_export_entry",
          .declaration = RuntimeFunction("FindExportEntry"),
          .takes_the_location = false};
    case BuiltinFn::kFromIntegral:
      return {
          .name = "from_integral",
          .declaration = StaticFactory{"ConvertFromIntegral"},
          .takes_a_type_argument = true,
          .takes_the_location = false,
          .can_unwind = false,
          .operands = {kNumber}};
    case BuiltinFn::kIntegralFromInt:
      return IntegralEntry(IntegralOp::kFromInt, StaticFactory{"FromInt"});
    case BuiltinFn::kConvertFrom:
      return {
          .name = "convert_from",
          .declaration = ValueFunction("Convert"),
          .takes_a_type_argument = true,
          .takes_the_location = false};
    case BuiltinFn::kIntegralConvert:
      return IntegralEntry(IntegralOp::kConvert, ValueFunction("Convert"));
    case BuiltinFn::kToPosition:
      return IntegralEntry(
          IntegralOp::kToPosition, ValueFunction("ToPosition"));
    case BuiltinFn::kStringFromBits:
      return {
          .name = "string_from_bits",
          .declaration = StaticFactory{"FromIntegral"},
          .takes_the_location = false,
          .operands = {kBits}};
    case BuiltinFn::kFromByteArray:
      return {
          .name = "from_byte_array",
          .declaration = StaticFactory{"FromByteArray"},
          .takes_the_location = false,
          .operands = {kTyped}};
    case BuiltinFn::kIntegralFromString:
      return IntegralEntry(IntegralOp::kFromText, ValueFunction("FromString"));
    case BuiltinFn::kByteArrayFromString:
      return {
          .name = "byte_array_from_string",
          .declaration = StaticFactory{"FromString"},
          .takes_the_location = false,
          .answer_told = AnswerTold::kBuiltArray};
    case BuiltinFn::kByteArrayFromBits:
      return {
          .name = "byte_array_from_bits",
          .declaration = StaticFactory{"FromIntegral"},
          .takes_the_location = false,
          .operands = {kBits},
          .answer_told = AnswerTold::kBuiltArray};
    case BuiltinFn::kDynamicArrayFromArray:
      return {
          .name = "dynamic_array_from_array",
          .declaration = StaticFactory{"FromArray"},
          .takes_the_location = false,
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
    case BuiltinFn::kDynamicArrayNew:
      return {
          .name = "dynamic_array_new",
          .declaration = StaticFactory{"New"},
          .answer_told = AnswerTold::kBuiltArray};
    case BuiltinFn::kDynamicArrayNewCopy:
      return {
          .name = "dynamic_array_new_copy",
          .declaration = StaticFactory{"NewCopy"},
          .answer_told = AnswerTold::kBuiltArray};
    // The core of a dynamic array runs no code of the design's, and storage
    // that cannot be had ends the run, so none of its entries reports where
    // it was asked for.
    case BuiltinFn::kDynamicArrayGeneration:
      return {
          .name = "dynamic_array_generation",
          .declaration =
              FreeFunction{
                  .scope = "lyra::value",
                  .identifier = "DynamicArrayGeneration"},
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kDynamicArrayPlace:
      return {
          .name = "dynamic_array_place",
          .declaration = AllocationFunction{},
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kDynamicArrayAdopt:
      return {
          .name = "dynamic_array_adopt",
          .declaration =
              FreeFunction{
                  .scope = "lyra::value", .identifier = "DynamicArrayAdopt"},
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kDynamicArrayLetGo:
      return {
          .name = "dynamic_array_let_go",
          .declaration =
              FreeFunction{
                  .scope = "lyra::value", .identifier = "DynamicArrayLetGo"},
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kDynamicArrayFree:
      return {
          .name = "dynamic_array_free",
          .declaration =
              FreeFunction{
                  .scope = "lyra::value", .identifier = "DynamicArrayFree"},
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kGetUnchecked:
      return {
          .name = "get_unchecked",
          .declaration = Method{"GetUnchecked"},
          .takes_the_location = false,
          .can_unwind = false,
          .answer = EntryAnswer::kReadOnlyReferenceIntoTheReceiver,
          .selects = PartSelection::kElement,
          .operands = {kMachine, kMachine}};
    case BuiltinFn::kGetUncheckedMut:
      return {
          .name = "get_unchecked_mut",
          .declaration = Method{"GetUnchecked"},
          .takes_the_location = false,
          .can_unwind = false,
          .answer = EntryAnswer::kMutableReferenceIntoTheReceiver,
          .selects = PartSelection::kElement,
          .operands = {kMachine, kMachine}};
    case BuiltinFn::kResized:
      return {.name = "resized", .declaration = Method{"Resized"}};
    case BuiltinFn::kQueueConstruct:
      return {
          .name = "queue_construct",
          .declaration =
              FreeFunction{
                  .scope = "lyra::value", .identifier = "QueueConstruct"},
          .takes_a_type_argument = true,
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kDynamicArrayConstruct:
      return {
          .name = "dynamic_array_construct",
          .declaration =
              FreeFunction{
                  .scope = "lyra::value",
                  .identifier = "DynamicArrayConstruct"},
          .takes_a_type_argument = true,
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kAssociativeArrayConstruct:
      return {
          .name = "assoc_construct",
          .declaration =
              FreeFunction{
                  .scope = "lyra::value",
                  .identifier = "AssociativeArrayConstruct"},
          .takes_a_type_argument = true,
          .takes_the_location = false,
          .can_unwind = false};
    // The core of an associative array runs no code of the design's but the
    // ending of an entry through its types' records, which unwinds out of
    // nothing.
    case BuiltinFn::kAssociativeArrayFind:
      return AssociativeArrayCoreEntry("assoc_find", "AssociativeArrayFind");
    case BuiltinFn::kAssociativeArrayPrepare:
      return AssociativeArrayCoreEntry(
          "assoc_prepare", "AssociativeArrayPrepare");
    case BuiltinFn::kAssociativeArrayIndexOf:
      return AssociativeArrayCoreEntry(
          "assoc_index_of", "AssociativeArrayIndexOf");
    case BuiltinFn::kAssociativeArrayCommit:
      return AssociativeArrayCoreEntry(
          "assoc_commit", "AssociativeArrayCommit");
    case BuiltinFn::kAssociativeArrayRemove:
      return AssociativeArrayCoreEntry(
          "assoc_remove", "AssociativeArrayRemove");
    case BuiltinFn::kAssociativeArrayClear:
      return AssociativeArrayCoreEntry("assoc_clear", "AssociativeArrayClear");
    case BuiltinFn::kAssociativeArrayFirst:
      return AssociativeArrayCoreEntry(
          "assoc_core_first", "AssociativeArrayFirst");
    case BuiltinFn::kAssociativeArrayLast:
      return AssociativeArrayCoreEntry(
          "assoc_core_last", "AssociativeArrayLast");
    case BuiltinFn::kAssociativeArrayNext:
      return AssociativeArrayCoreEntry(
          "assoc_core_next", "AssociativeArrayNext");
    case BuiltinFn::kAssociativeArrayPrevious:
      return AssociativeArrayCoreEntry(
          "assoc_core_previous", "AssociativeArrayPrevious");
    case BuiltinFn::kAssociativeArrayAfter:
      return AssociativeArrayCoreEntry("assoc_after", "AssociativeArrayAfter");
    case BuiltinFn::kAssociativeArrayBefore:
      return AssociativeArrayCoreEntry(
          "assoc_before", "AssociativeArrayBefore");
    case BuiltinFn::kAssociativeArrayPrepareDefault:
      return AssociativeArrayCoreEntry(
          "assoc_prepare_default", "AssociativeArrayPrepareDefault");
    case BuiltinFn::kAssociativeArraySetDefault:
      return AssociativeArrayCoreEntry(
          "assoc_set_default", "AssociativeArraySetDefault");
    case BuiltinFn::kAssociativeArrayStatedDefault:
      return AssociativeArrayCoreEntry(
          "assoc_core_stated_default", "AssociativeArrayStatedDefault");
    case BuiltinFn::kAssociativeArrayEnd:
      return AssociativeArrayCoreEntry("assoc_end", "AssociativeArrayEnd");
    case BuiltinFn::kAssociativeArrayAdopt:
      return AssociativeArrayCoreEntry("assoc_adopt", "AssociativeArrayAdopt");
    case BuiltinFn::kWildcardIndexHasUnknown:
      return {
          .name = "wildcard_index_has_unknown",
          .declaration =
              FreeFunction{
                  .scope = "lyra::value",
                  .identifier = "WildcardIndexHasUnknown"},
          .takes_the_location = false,
          .can_unwind = false};
    // The core of a queue runs no code of the design's either.
    case BuiltinFn::kQueueReserve:
      return {
          .name = "queue_reserve",
          .declaration =
              FreeFunction{
                  .scope = "lyra::value", .identifier = "QueueReserve"},
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kQueueCommit:
      return {
          .name = "queue_commit",
          .declaration =
              FreeFunction{.scope = "lyra::value", .identifier = "QueueCommit"},
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kQueueHoldToBound:
      return {
          .name = "queue_hold_to_bound", .declaration = Method{"HoldToBound"}};
    case BuiltinFn::kQueueGrewPastItsBound:
      return {
          .name = "queue_grew_past_its_bound",
          .declaration =
              FreeFunction{
                  .scope = "lyra::value",
                  .identifier = "ReportQueueGrewPastItsBound"},
          .can_unwind = false};
    case BuiltinFn::kQueueWrittenAtAnInvalidIndex:
      return {
          .name = "queue_written_at_an_invalid_index",
          .declaration =
              FreeFunction{
                  .scope = "lyra::value",
                  .identifier = "ReportQueueWrittenAtAnInvalidIndex"},
          .can_unwind = false};
    case BuiltinFn::kAssociativeArrayReadAtAnInvalidIndex:
      return {
          .name = "assoc_read_at_an_invalid_index",
          .declaration =
              FreeFunction{
                  .scope = "lyra::value",
                  .identifier = "ReportAssociativeArrayReadAtAnInvalidIndex"},
          .can_unwind = false};
    case BuiltinFn::kAssociativeArrayReadAtAMissingEntry:
      return {
          .name = "assoc_read_at_a_missing_entry",
          .declaration =
              FreeFunction{
                  .scope = "lyra::value",
                  .identifier = "ReportAssociativeArrayReadAtAMissingEntry"},
          .can_unwind = false};
    case BuiltinFn::kAssociativeArrayWrittenAtAnInvalidIndex:
      return {
          .name = "assoc_written_at_an_invalid_index",
          .declaration =
              FreeFunction{
                  .scope = "lyra::value",
                  .identifier =
                      "ReportAssociativeArrayWrittenAtAnInvalidIndex"},
          .can_unwind = false};
    case BuiltinFn::kQueueFreeRing:
      return {
          .name = "queue_free_ring",
          .declaration =
              FreeFunction{
                  .scope = "lyra::value", .identifier = "QueueFreeRing"},
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kQueueAdopt:
      return {
          .name = "queue_adopt",
          .declaration =
              FreeFunction{.scope = "lyra::value", .identifier = "QueueAdopt"},
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kQueueReverse:
      return {
          .name = "queue_reverse",
          .declaration =
              FreeFunction{
                  .scope = "lyra::value", .identifier = "QueueReverse"},
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kQueueUnlink:
      return {
          .name = "queue_unlink",
          .declaration =
              FreeFunction{.scope = "lyra::value", .identifier = "QueueUnlink"},
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kQueueApplyOrder:
      return {
          .name = "queue_apply_order",
          .declaration =
              FreeFunction{
                  .scope = "lyra::value", .identifier = "QueueApplyOrder"},
          .takes_the_location = false,
          .can_unwind = false};
    // A value a type starts as is the library's value of that type built out
    // of nothing, as a C++ class's is its value-initialized object.
    case BuiltinFn::kDefault:
      return {
          .name = "default",
          .declaration = ValueFunction("DefaultOf"),
          .takes_a_type_argument = true,
          .takes_the_location = false,
          .can_unwind = false};
    case BuiltinFn::kWeakReferenceDefault:
      return {
          .name = "weak_reference_default",
          .declaration = ValueFunction("DefaultOf"),
          .takes_a_type_argument = true,
          .takes_the_location = false};
    case BuiltinFn::kStringFromText:
      return {
          .name = "from_text",
          .declaration = Constructor{},
          .takes_the_location = false};
    // A wildcard index and the image of an open array are each built over a
    // value whose type nothing else states (LRM 7.8.1, 35.5.6.1).
    case BuiltinFn::kWildcardIndexMake:
      return {
          .name = "wildcard_index_make",
          .declaration = Constructor{},
          .takes_the_location = false,
          .operands = {kTyped}};
    case BuiltinFn::kHierarchySegmentMake:
      return {
          .name = "hierarchy_segment_make",
          .declaration = Constructor{},
          .takes_the_location = false};
    case BuiltinFn::kTriggerMake:
      return {
          .name = "trigger_make",
          .declaration = Constructor{},
          .takes_the_location = false};
    case BuiltinFn::kFormatSpecMake:
      return {
          .name = "format_spec_make",
          .declaration = Constructor{},
          .takes_the_location = false};
    // What a value formats as is the value's own answer, so each is realized
    // once per kind of value it is built over, which it is handed a reference
    // to, and once for every integral value, which it reads as a number (LRM
    // 21.2.1).
    case BuiltinFn::kPrintItemMake:
      return {
          .name = "print_item_make",
          .declaration = Constructor{},
          .takes_the_location = false};
    case BuiltinFn::kIntegralPrintItemMake:
      return {
          .name = "integral_print_item_make",
          .declaration = Constructor{},
          .takes_the_location = false,
          .operands = {kNumber}};
    case BuiltinFn::kFormatArgMake:
      return {
          .name = "format_arg_make",
          .declaration = Constructor{},
          .takes_the_location = false};
    case BuiltinFn::kIntegralFormatArgMake:
      return {
          .name = "integral_format_arg_make",
          .declaration = Constructor{},
          .takes_the_location = false,
          .operands = {kNumber}};
    case BuiltinFn::kDpiBitBufferMake:
      return {
          .name = "dpi_bit_buffer_make",
          .declaration = Constructor{},
          .takes_the_location = false,
          .operands = {kBits}};
    case BuiltinFn::kDpiLogicBufferMake:
      return {
          .name = "dpi_logic_buffer_make",
          .declaration = Constructor{},
          .takes_the_location = false,
          .operands = {kBits}};
    case BuiltinFn::kDpiOpenArrayMake:
      return {
          .name = "dpi_open_array_make",
          .declaration = Constructor{},
          .takes_the_location = false};
    case BuiltinFn::kConcatStrings:
      return {
          .name = "concat",
          .declaration = StaticFactory{"Concat"},
          .takes_the_location = false};
    case BuiltinFn::kReplicateBits:
      return IntegralEntry(IntegralOp::kReplicate, ValueFunction("Replicate"));
    case BuiltinFn::kReplicateString:
      return {
          .name = "replicate_string",
          .declaration = Method{"Replicate"},
          .takes_the_location = false};
    case BuiltinFn::kPow:
      return {
          .name = "pow",
          .declaration = Method{"Pow"},
          .takes_the_location = false,
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
          .takes_the_location = false,
          .can_unwind = false,
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
          .takes_the_location = false,
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
          .takes_the_location = false,
          .operands = {kMachine, kBits}};
    case BuiltinFn::kEnumerationName:
      return {
          .name = "enumeration_name",
          .declaration = Method{"Name"},
          .takes_the_location = false,
          .operands = {kMachine, kBits}};
    case BuiltinFn::kEnumerationNext:
      return {
          .name = "enumeration_next",
          .declaration = Method{"Next"},
          .takes_the_location = false,
          .operands = {kMachine, kBits}};
    case BuiltinFn::kEnumerationPrev:
      return {
          .name = "enumeration_prev",
          .declaration = Method{"Prev"},
          .takes_the_location = false,
          .operands = {kMachine, kBits}};
    case BuiltinFn::kSelfHandle:
      return {
          .name = "self_handle",
          .declaration = RuntimeFunction("SelfHandle"),
          .takes_the_location = false};
    case BuiltinFn::kHierarchicalPathOf:
      return {
          .name = "hierarchical_path_of",
          .declaration = Method{"HierarchicalPathOf"},
          .takes_the_location = false};
  }
  throw InternalError("RuntimeEntryOf: unknown BuiltinFn");
}

auto HandsOutStorage(const EntryDeclaration& declared) -> bool {
  return std::visit(
      Overloaded{
          [](const FreeFunction&) { return false; },
          [](const Method&) { return false; },
          [](const StaticFactory&) { return false; },
          [](const AllocationFunction&) { return true; },
          [](const ReservedPlacementForm&) { return true; },
          [](const Constructor&) { return false; }},
      declared);
}

auto PlacesAtItsArgument(const EntryDeclaration& declared) -> bool {
  return std::visit(
      Overloaded{
          [](const FreeFunction&) { return false; },
          [](const Method&) { return false; },
          [](const StaticFactory&) { return false; },
          [](const AllocationFunction&) { return false; },
          [](const ReservedPlacementForm&) { return true; },
          [](const Constructor&) { return false; }},
      declared);
}

auto ActsOnAnObject(const EntryDeclaration& declared) -> bool {
  return std::visit(
      Overloaded{
          [](const FreeFunction&) { return true; },
          [](const Method&) { return true; },
          [](const StaticFactory&) { return false; },
          [](const AllocationFunction&) { return false; },
          [](const ReservedPlacementForm&) { return false; },
          [](const Constructor&) { return true; }},
      declared);
}

}  // namespace lyra::support
