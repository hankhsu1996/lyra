#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/common/deferred_assertion_abi.hpp"
#include "lyra/common/format.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/common/severity.hpp"
#include "lyra/common/system_tf.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/semantic/decision.hpp"

namespace lyra::mir {

// A single format operation within a display/write call.
// Either a literal string or a formatted value.
struct FormatOp {
  FormatKind kind;
  std::optional<Operand> value;  // nullopt for kLiteral
  std::string literal;           // only valid when kind == kLiteral
  TypeId type;           // for width/signedness lookup (invalid for kLiteral)
  FormatModifiers mods;  // width, precision, flags
  int8_t module_timeunit_power =
      -9;  // For kTime: unit of value (from scope's timescale)
};

// DisplayEffect represents a $display/$write/$fdisplay/$fwrite family system
// task. When descriptor is absent, output goes to stdout.
struct DisplayEffect {
  PrintKind print_kind;
  std::vector<FormatOp> ops;
  std::optional<Operand> descriptor;  // nullopt = stdout
};

// MIR semantic report intent. Distinct from runtime::ReportKind --
// this is IR-level classification, mapped to runtime kind at codegen.
enum class ReportIntent : uint8_t {
  kUserSeverity,
  kAssertionFailure,
};

// Post-report continuation. Mapped to runtime::ReportAction at codegen.
enum class ReportContinuation : uint8_t {
  kContinue,
  kFinish,
};

// ReportEffect: unified semantic reporting for severity tasks and assertion
// failures. Replaces SeverityEffect. Producers fill intent + severity +
// optional origin + format ops; codegen materializes to a single runtime
// report call.
struct ReportEffect {
  ReportIntent intent;
  Severity severity;
  std::optional<common::OriginId> origin;
  std::vector<FormatOp> ops;
  ReportContinuation continuation = ReportContinuation::kContinue;
};

// MemIOEffect represents $readmemh/$readmemb/$writememh/$writememb system
// tasks. These read/write memory files to/from unpacked array variables.
struct MemIOEffect {
  bool is_read = false;  // true = readmem, false = writemem
  bool is_hex = false;   // true = hex, false = binary
  PlaceId target;        // The memory array (written for read, read for write)
  TypeId target_type;    // Array type (for element width/count)
  TypedOperand
      filename;  // String operand (typed for packed-to-string coercion)
  std::optional<Operand> start_addr;
  std::optional<Operand> end_addr;
};

// SystemTfEffect: Generic effect-only system TFs.
// Covers simple system TFs where payload is just opcode + operands.
// Operands are stored here (Effect pattern).
struct SystemTfEffect {
  SystemTfOpcode opcode;
  std::vector<Operand> args;
};

// StrobeEffect: $strobe system task (IEEE 1800-2023 21.2.2).
// Unlike $display (immediate output), $strobe defers printing to the Postponed
// region and re-evaluates expressions using end-of-timestep values.
// The program is a synthetic MIR function that reads fresh values and prints.
struct StrobeEffect {
  // Synthetic function that performs the print
  FunctionId program;
};

// $timeformat: sets global time format for %t.
// All values are compile-time constants resolved in AST->HIR.
struct TimeFormatEffect {
  int8_t units = 127;  // Time unit power (127 = use global precision)
  int precision = 0;   // Decimal digits
  std::string suffix;  // Appended string
  int min_width = 20;  // Minimum field width
};

// $monitor: persistent change-triggered display (IEEE 1800 21.2.3).
// The setup program performs initial print and registers the check program with
// runtime. The check_program FunctionId is passed to runtime at registration,
// not stored in MIR.
struct MonitorEffect {
  FunctionId setup_program;
  FunctionId check_program;
  PrintKind print_kind;              // For print end call
  std::vector<FormatOp> format_ops;  // For LLVM comparison code generation
  std::vector<uint32_t> offsets;     // Per-operand offset into prev_buffer
  std::vector<uint32_t> byte_sizes;  // Per-operand byte size
  uint32_t prev_buffer_size = 0;     // Total size of prev_values buffer
};

// $monitoron/$monitoroff: control monitor enabled state.
// No-op if no active monitor (graceful handling).
struct MonitorControlEffect {
  bool enable;  // true=$monitoron, false=$monitoroff
};

// Fill strategy for FillPackedEffect.
// Decided at HIR->MIR lowering from source-level syntax (unbased-unsized vs
// sized literal). Backends must not re-derive this from value width.
enum class FillKind : uint8_t {
  // Fill every leaf bit with a single bit value (with unknown mask if 4-state).
  // Used for unbased-unsized literals ('0, '1, 'x, 'z).
  // fill_value must be 1-bit.
  kBitFill,

  // Fill outer elements with an element-typed value.
  // For nested packed arrays, backends should recurse into element types.
  // Used for sized literals (e.g., 8'hAA for [N:0][7:0] arrays).
  // fill_value width must match target's element width.
  kElementFill,
};

// FillPacked: fill a packed container with a replicated value.
//
// Contract:
// - target: PlaceId of an Integral or PackedArray type
// - fill_value: 1-bit for kBitFill, element-width for kElementFill
// - kind: explicit fill strategy (set by HIR->MIR, not inferred by backends)
//
// Output invariants (for interpreter):
// - RuntimeIntegral.bit_width == PackedBitWidth(target_type)
// - RuntimeIntegral.value.size() == ceil(bit_width / 64)
// - RuntimeIntegral.unknown.size() == ceil(bit_width / 64)
// - Padding bits in top word are masked to zero
struct FillPackedEffect {
  PlaceId target;
  Operand fill_value;
  uint32_t unit_bits = 0;
  uint32_t count = 0;
  uint32_t total_bits = 0;
};

// Record a decision observation with compile-time-constant payload.
// Used on the lazy path (kPriority) where each recording edge knows the exact
// match class and selected arm statically.
struct RecordDecisionObservation {
  semantic::DecisionId id;
  semantic::MatchClass match_class_const{};
  semantic::DecisionSelectedKind selected_kind_const{};
  semantic::DecisionArmIndex selected_arm_const;
};

// Record a decision observation with dynamic payload from straight-line
// select logic. Used on the eager path (kUnique/kUnique0) where match class
// and selected arm are computed at runtime from all pre-evaluated conditions.
struct RecordDecisionObservationDynamic {
  semantic::DecisionId id;
  Operand match_class;    // i8: MatchClass enum value
  Operand selected_kind;  // i8: DecisionSelectedKind enum value
  Operand selected_arm;   // i16: arm index (valid when kArm)
};

// Dense index for an immediate cover site. Allocated during HIR-to-MIR
// lowering and carried through MIR to codegen. Used to index into the
// runtime hit-count array.
struct CoverSiteId {
  uint32_t value = UINT32_MAX;

  explicit operator bool() const {
    return value != UINT32_MAX;
  }
  [[nodiscard]] auto Index() const -> uint32_t {
    return value;
  }
  auto operator==(const CoverSiteId&) const -> bool = default;
};

// Record a hit for an immediate cover statement (LRM 16.3).
// Emitted on the true arm of the cover condition. site_id is body-local
// (0-based per body). The backend applies a per-body base offset to
// produce the design-global runtime index.
struct CoverHitEffect {
  CoverSiteId site_id;
};

// Dense index for a deferred immediate assertion site. Body-local
// (0-based per body). The backend applies a per-body base offset to
// produce the design-global runtime index.
struct DeferredAssertionSiteId {
  uint32_t value = UINT32_MAX;

  explicit operator bool() const {
    return value != UINT32_MAX;
  }
  [[nodiscard]] auto Index() const -> uint32_t {
    return value;
  }
  auto operator==(const DeferredAssertionSiteId&) const -> bool = default;
};

// Which disposition path a deferred assertion enqueue represents.
// Values match DeferredAssertionDispositionAbi by construction (shared ABI).
enum class DeferredAssertionDisposition : uint8_t {
  kDefaultFailReport = 0,
  kFailAction = 1,
  kPassAction = 2,
  kCoverHit = 3,
};

// Compile-time enforcement that MIR and ABI disposition values match.
static_assert(
    static_cast<uint8_t>(DeferredAssertionDisposition::kDefaultFailReport) ==
    static_cast<uint8_t>(DeferredAssertionDispositionAbi::kDefaultFailReport));
static_assert(
    static_cast<uint8_t>(DeferredAssertionDisposition::kFailAction) ==
    static_cast<uint8_t>(DeferredAssertionDispositionAbi::kFailAction));
static_assert(
    static_cast<uint8_t>(DeferredAssertionDisposition::kPassAction) ==
    static_cast<uint8_t>(DeferredAssertionDispositionAbi::kPassAction));
static_assert(
    static_cast<uint8_t>(DeferredAssertionDisposition::kCoverHit) ==
    static_cast<uint8_t>(DeferredAssertionDispositionAbi::kCoverHit));

// Enqueue a pending observed deferred assertion record (LRM 16.4).
// Emitted by deferred assertion lowering. Condition has already been
// evaluated; this effect captures by-value arguments and enqueues a
// pending record into per-process deferred state.
//
// At runtime, the record is stamped with the current flush generation
// and matured at settle boundary.
struct EnqueueDeferredAssertionEffect {
  DeferredAssertionSiteId site_id;
  DeferredAssertionDisposition disposition;
  // Evaluated snapshot values for by-value actuals, in semantic actual
  // order (skipping ref actuals). Backend derives payload field order
  // from the semantic action. Empty for dispositions with no thunk
  // (kDefaultFailReport, kCoverHit).
  std::vector<Operand> snapshot_values;
};

// Canonical traversal of read operands for EnqueueDeferredAssertionEffect.
// All MIR passes that reason about effect operands must use this helper
// instead of open-coding their own traversal.
template <typename F>
void ForEachReadOperand(const EnqueueDeferredAssertionEffect& e, const F& f) {
  for (const auto& op : e.snapshot_values) {
    f(op);
  }
}

// Zero-initialize the entire storage of a place.
// Used for types where memberwise MIR initialization is not possible
// (e.g., unpacked unions whose members overlap in storage).
// The backend emits a memset(0) for the place's allocated storage.
struct ZeroInitStorageEffect {
  PlaceId target;
  TypeId target_type;
};

// EffectOp is the variant of all effect operations.
// Effect operations produce side effects but no value.
// Note: Builtin methods are now unified as Rvalue (kBuiltinCall), not Effect.
using EffectOp = std::variant<
    DisplayEffect, ReportEffect, MemIOEffect, TimeFormatEffect, SystemTfEffect,
    StrobeEffect, MonitorEffect, MonitorControlEffect, FillPackedEffect,
    RecordDecisionObservation, RecordDecisionObservationDynamic, CoverHitEffect,
    EnqueueDeferredAssertionEffect, ZeroInitStorageEffect>;

// Body execution requirement: what a callable body needs to execute correctly.
// Computed once from body contents at metadata formation time, stored on
// mir::Function. Two consumers read the stored value (verifier, call lowering);
// neither re-walks the body.
enum class BodyExecutionRequirement : uint8_t {
  kGenericCallable,             // Only needs design + engine
  kDeferredCheckOwnerRequired,  // Requires active decision owner context
};

// Classify an effect's execution context requirement.
// RecordDecisionObservation* requires an active decision owner;
// all other effects only need generic callable context.
auto GetEffectExecutionRequirement(const EffectOp& op)
    -> BodyExecutionRequirement;

}  // namespace lyra::mir
