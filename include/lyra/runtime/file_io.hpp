#pragma once

#include <functional>
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

// LRM 21.2.2 + 21.3.2 $fstrobe-family runtime entry. Defers `print_action`
// to the postponed region and wires up LRM 21.3.2 implicit cancel:
// acquires a ChannelCancellation for `descriptor` at submit time; the
// wrapped action short-circuits if any participating channel is closed
// before the postponed region fires. Channel-reuse is safe -- a dead
// submission's observer keeps seeing the old (permanently stopped) state
// regardless of what the integer descriptor value later points at.
void LyraSubmitFStrobe(
    RuntimeServices& services, const value::PackedArray& descriptor,
    std::function<void()> print_action);

// LRM 21.3.4.1 $fgetc(fd). Returns the next byte as an int32 PackedArray, or
// -1 on EOF / error. Errors stamp FileTable's per-fd error slot.
auto LyraFGetc(RuntimeServices& services, const value::PackedArray& fd)
    -> value::PackedArray;

// LRM 21.3.4.1 $ungetc(c, fd). Pushes the low byte of `c` back onto fd's
// input buffer. Returns 0 on success or -1 on error.
auto LyraFUngetc(
    RuntimeServices& services, const value::PackedArray& c,
    const value::PackedArray& fd) -> value::PackedArray;

// LRM 21.3.4.2 $fgets(str, fd). Reads bytes into `dest` up to and including
// the next newline or until EOF. Trailing newline (when present) is kept.
// Returns the number of bytes written, or 0 on error.
auto LyraFGets(
    RuntimeServices& services, value::String& dest,
    const value::PackedArray& fd) -> value::PackedArray;

// LRM 21.3.4.4 $fread(integral_var, fd). Reads ceil(BitWidth/8) bytes from
// fd in big-endian order into `dest`. The destination's bit_width /
// signedness / 4-state-ness are read from its current shape so the runtime
// can produce a value of the right shape. Returns the byte count, 0 on
// error. Widths > 64 bits are not supported (returns 0).
auto LyraFRead(
    RuntimeServices& services, value::PackedArray& dest,
    const value::PackedArray& fd) -> value::PackedArray;

// LRM 21.3.5 $fseek(fd, offset, operation). `operation` is 0/1/2 for
// SEEK_SET/SEEK_CUR/SEEK_END. Returns 0 on success or -1 on error.
auto LyraFSeek(
    RuntimeServices& services, const value::PackedArray& fd,
    const value::PackedArray& offset, const value::PackedArray& operation)
    -> value::PackedArray;

// LRM 21.3.5 $rewind(fd). Equivalent to $fseek(fd, 0, 0).
auto LyraFRewind(RuntimeServices& services, const value::PackedArray& fd)
    -> value::PackedArray;

// LRM 21.3.5 $ftell(fd). Returns the current position or -1 on error.
auto LyraFTell(RuntimeServices& services, const value::PackedArray& fd)
    -> value::PackedArray;

// LRM 21.3.8 $feof(fd). Returns a nonzero value once an EOF has been
// observed on fd, zero otherwise.
auto LyraFEof(RuntimeServices& services, const value::PackedArray& fd)
    -> value::PackedArray;

// LRM 21.3.7 $ferror(fd, str). Returns the most recent errno stamped on fd
// and writes the textual message into `dest`. Cleared after the call.
auto LyraFError(
    RuntimeServices& services, const value::PackedArray& fd,
    value::String& dest) -> value::PackedArray;

// LRM 21.3.6 $fflush(descriptor). When `descriptor` is nullopt the runtime
// flushes every open file; otherwise it flushes the addressed channels (MCD
// fan-out or single FD).
void LyraFFlush(
    RuntimeServices& services, std::optional<value::PackedArray> descriptor);

}  // namespace lyra::runtime
