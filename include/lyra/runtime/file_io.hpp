#pragma once

#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/unpacked_array.hpp"

namespace lyra::runtime {

class RuntimeServices;

// Runtime entry points for LRM 21.3 file I/O system tasks. The backend emits
// these as ordinary calls from MIR (a generic CallExpr whose first argument is
// the engine handle); the runtime owns the descriptor table (see FileTable)
// and dispatches per LRM 21.3.1 MCD/FD bit encoding.
//
// Descriptors arrive and leave as 32-bit signed PackedArray values so the
// emitted code can route them through SV's `int` carrier without
// per-call-site conversion code. Internally the runtime narrows to int32.

// $fopen(name). LRM 21.3.1 MCD form: returns a multichannel descriptor or 0
// on failure.
auto LyraFOpen(RuntimeServices& services, const value::String& name)
    -> value::PackedArray;

// $fopen(name, mode). LRM 21.3.1 FD form: returns a single file descriptor or
// 0 on failure.
auto LyraFOpen(
    RuntimeServices& services, const value::String& name,
    const value::String& mode) -> value::PackedArray;

// $fclose(descriptor). No-op for 0 / pre-bound stdin/stdout/stderr; for an
// MCD closes every set-bit channel.
void LyraFClose(
    RuntimeServices& services, const value::PackedArray& descriptor);

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

// LRM 21.3.4.4 $fread into a packed destination. Reads (BitWidth+7)/8 bytes
// big-endian (first byte fills MSBs); the destination's existing shape
// drives the result's width / sign / 4-state. Returns byte count, 0 on
// error.
auto LyraFRead(
    RuntimeServices& services, value::PackedArray& dest,
    const value::PackedArray& fd) -> value::PackedArray;

// LRM 21.3.4.4 $fread into an unpacked destination. Iterates `dest` elements
// from SV index `sv_start` toward the highest numerical SV index (LRM "shall
// continue upward toward the highest address"), reading until EOF or the
// highest declared index. `declared_left` / `declared_right` are the
// destination's declared bounds, used to map an SV index to a storage offset
// (descending ranges walk storage backwards). EOF mid-element zero-pads that
// element's LSBs, matching the packed form's "as much as available". The
// caller always supplies `sv_start` (the lowest declared index when the SV
// call omits it).
auto LyraFRead(
    RuntimeServices& services, value::UnpackedArray<value::PackedArray>& dest,
    const value::PackedArray& fd, const value::PackedArray& declared_left,
    const value::PackedArray& declared_right,
    const value::PackedArray& sv_start) -> value::PackedArray;

// As above, loading at most `count` elements (LRM 21.3.4.4 explicit count).
auto LyraFRead(
    RuntimeServices& services, value::UnpackedArray<value::PackedArray>& dest,
    const value::PackedArray& fd, const value::PackedArray& declared_left,
    const value::PackedArray& declared_right,
    const value::PackedArray& sv_start, const value::PackedArray& count)
    -> value::PackedArray;

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

// LRM 21.3.6 $fflush(). Flushes every open file.
void LyraFFlush(RuntimeServices& services);

// LRM 21.3.6 $fflush(descriptor). Flushes the addressed channels (MCD fan-out
// or single FD).
void LyraFFlush(
    RuntimeServices& services, const value::PackedArray& descriptor);

}  // namespace lyra::runtime
