#pragma once

#include <optional>

#include "lyra/mir/expr_id.hpp"

namespace lyra::mir {

// LRM 21.3.1 $fopen. `mode` is absent for MCD form, present for FD form.
// Both operands are string-typed expressions; mode parses at runtime so a
// non-literal SV variable can flow through unchanged.
struct RuntimeFileOpenCall {
  ExprId name{};
  std::optional<ExprId> mode = std::nullopt;
};

// LRM 21.3.1 $fclose. `descriptor` is an int-typed expression; the runtime
// decides whether the value is an MCD (iterates set bits) or an FD (single
// descriptor) per the LRM bit-31 convention.
struct RuntimeFileCloseCall {
  ExprId descriptor{};
};

// LRM 21.3.4.1 $fgetc(fd). Returns the next byte (0..255) or EOF (-1).
struct RuntimeFileGetcCall {
  ExprId fd{};
};

// LRM 21.3.4.1 $ungetc(c, fd). Pushes one byte back onto fd; returns 0 on
// success or EOF on failure.
struct RuntimeFileUngetcCall {
  ExprId c{};
  ExprId fd{};
};

// LRM 21.3.4.2 $fgets(str, fd). Reads characters into `str_dest` (a
// string-typed lvalue temp) until newline or EOF; the trailing newline is
// kept. Returns the number of bytes written, or 0 on error. `str_dest`
// references a procedural-local temp produced by the statement-position
// desugaring; the writeback to the user's actual lvalue lives in the
// surrounding BlockStmt.
struct RuntimeFileGetsCall {
  ExprId str_dest{};
  ExprId fd{};
};

// LRM 21.3.4.4 $fread(integral_var, fd). Reads big-endian bytes into the
// integral-typed lvalue temp `int_dest`, filling the most significant bytes
// first. Returns the number of bytes read, 0 on error. The 4-state bit is
// always set to 0 (LRM "data ... are taken as 2-value data"). The
// memory-array form (`$fread(mem, fd[, start[, count]])`) is not modeled.
struct RuntimeFileReadCall {
  ExprId int_dest{};
  ExprId fd{};
};

// LRM 21.3.5 $fseek(fd, offset, operation). `operation` is 0/1/2 mapping to
// SEEK_SET / SEEK_CUR / SEEK_END. Returns 0 on success, -1 on error.
struct RuntimeFileSeekCall {
  ExprId fd{};
  ExprId offset{};
  ExprId operation{};
};

// LRM 21.3.5 $rewind(fd). Equivalent to $fseek(fd, 0, 0). Returns 0 on
// success, -1 on error.
struct RuntimeFileRewindCall {
  ExprId fd{};
};

// LRM 21.3.5 $ftell(fd). Returns the current read/write position or -1 on
// error.
struct RuntimeFileTellCall {
  ExprId fd{};
};

// LRM 21.3.8 $feof(fd). Returns nonzero once an EOF has been observed on fd,
// zero otherwise. Tracks the underlying fstream's eof flag.
struct RuntimeFileEofCall {
  ExprId fd{};
};

// LRM 21.3.7 $ferror(fd, str). Returns the errno of the most recent file
// I/O operation on fd (0 on no error) and writes a textual description into
// `str_dest`, a string-typed lvalue temp. Cleared after retrieval.
struct RuntimeFileErrorCall {
  ExprId fd{};
  ExprId str_dest{};
};

// LRM 21.3.6 $fflush([mcd|fd]). With no argument, flushes every open file;
// with one, flushes the addressed channels.
struct RuntimeFileFlushCall {
  std::optional<ExprId> descriptor = std::nullopt;
};

}  // namespace lyra::mir
