#pragma once

#include <optional>
#include <span>

#include "lyra/value/format.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

class RuntimeServices;

// Runtime entry points for LRM 21.3 file I/O system tasks. The backend emits
// these calls from MIR RuntimeFileOpenCall / RuntimeFileCloseCall /
// RuntimePrintCall-with-descriptor; the runtime owns the descriptor table
// (see FileTable) and dispatches per LRM 21.3.1 MCD/FD bit encoding.
//
// Descriptors arrive and leave as 32-bit signed PackedArray values so the
// emitted code can route them through SV's `int` carrier without
// per-call-site conversion code. Internally the runtime narrows to int32.

// $fopen(name [, mode]). Returns descriptor per LRM 21.3.1 or 0 on failure.
auto LyraFOpen(
    RuntimeServices& services, const value::String& name,
    std::optional<value::String> mode) -> value::PackedArray;

// $fclose(descriptor). No-op for 0 / pre-bound stdin/stdout/stderr; for an
// MCD closes every set-bit channel.
void LyraFClose(
    RuntimeServices& services, const value::PackedArray& descriptor);

// $fdisplay / $fwrite with descriptor. Iterates set bits per LRM 21.3.1.
// Bit 0 of an MCD (or the pre-bound STDOUT FD 32'h8000_0001) routes through
// RuntimeServices::Stream() so test-harness stdout matching stays
// consistent with $display output ordering; other bits go directly to
// FileTable's owned FILE* handles. Invalid bits silently no-op.
void LyraFPrint(
    RuntimeServices& services, value::PrintKind kind,
    const value::PackedArray& descriptor,
    std::span<const value::PrintItem> items);

}  // namespace lyra::runtime
