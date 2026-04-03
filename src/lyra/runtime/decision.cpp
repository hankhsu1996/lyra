#include <cstdint>

// No-op stub for decision observation recording.
// The real implementation (Cut 3+) will write to ProcessDecisionState
// and enqueue the frame for settle-complete validation.
// For now, this stub allows the LLVM-emitted calls to link.
extern "C" void LyraRecordDecisionObservation(
    void* /*raw_frame*/, uint32_t /*decision_id_raw*/,
    uint8_t /*match_class_raw*/, uint8_t /*selected_kind_raw*/,
    uint16_t /*selected_arm_raw*/) {
  // No-op until Cut 3 wires up ProcessDecisionState.
}
