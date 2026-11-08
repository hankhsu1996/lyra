/*
 * The VPI routines of LRM clause 38 that take a variable argument list, as C
 * declares and reads one. Each reads its arguments and hands what it read to
 * the runtime, which does the rest.
 */

#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#include "vpi_user.h"

/* What the runtime is handed. Each takes only C's own types, and is declared
 * here because no header can serve both sides: the runtime's own C++ spells a
 * declaration in a form C cannot read. The runtime's definition is the same
 * prototype, spelled again there.
 *
 * `LyraVpiPrint` writes the `length` characters at `text` to the tool's output
 * and `LyraVpiPrintTo` to the channels `mcd` names (LRM 38.28 to 38.30, 38.41),
 * `text` being null where the format could not be applied; each answers the
 * number written or EOF. `LyraVpiControl` is `vpi_control` with the
 * diagnostic level `vpiStop` and `vpiFinish` are passed (LRM 38.4), answering
 * 1 or 0. */
PLI_INT32 LyraVpiPrint(const char* text, size_t length, const char* routine);
PLI_INT32 LyraVpiPrintTo(
    PLI_UINT32 mcd, const char* text, size_t length, const char* routine);
PLI_INT32 LyraVpiControl(PLI_INT32 operation, PLI_INT32 finish_number);

/* The text `format` makes of `arguments`, as `printf` makes it, with its
 * length, since a format may make a null character. Null where the format
 * cannot be applied; the caller frees it. */
static char* Formatted(const char* format, va_list arguments, size_t* length) {
  va_list measuring;
  va_copy(measuring, arguments);
  const int measured = vsnprintf(NULL, 0, format, measuring);
  va_end(measuring);
  if (measured < 0) {
    return NULL;
  }
  char* text = malloc((size_t)measured + 1);
  if (text == NULL) {
    return NULL;
  }
  vsnprintf(text, (size_t)measured + 1, format, arguments);
  *length = (size_t)measured;
  return text;
}

PLI_INT32 vpi_vprintf(PLI_BYTE8* format, va_list ap) {
  size_t length = 0;
  char* text = Formatted(format, ap, &length);
  const PLI_INT32 written = LyraVpiPrint(text, length, "vpi_vprintf");
  free(text);
  return written;
}

PLI_INT32 vpi_printf(PLI_BYTE8* format, ...) {
  va_list ap;
  va_start(ap, format);
  size_t length = 0;
  char* text = Formatted(format, ap, &length);
  va_end(ap);
  const PLI_INT32 written = LyraVpiPrint(text, length, "vpi_printf");
  free(text);
  return written;
}

PLI_INT32 vpi_mcd_vprintf(PLI_UINT32 mcd, PLI_BYTE8* format, va_list ap) {
  size_t length = 0;
  char* text = Formatted(format, ap, &length);
  const PLI_INT32 written =
      LyraVpiPrintTo(mcd, text, length, "vpi_mcd_vprintf");
  free(text);
  return written;
}

PLI_INT32 vpi_mcd_printf(PLI_UINT32 mcd, PLI_BYTE8* format, ...) {
  va_list ap;
  va_start(ap, format);
  size_t length = 0;
  char* text = Formatted(format, ap, &length);
  va_end(ap);
  const PLI_INT32 written = LyraVpiPrintTo(mcd, text, length, "vpi_mcd_printf");
  free(text);
  return written;
}

/* `vpiStop` and `vpiFinish` are passed the diagnostic level of the task they
 * ask for (LRM 38.4); the other operations are answered without reading what
 * they are passed. */
static PLI_INT32 Control(PLI_INT32 operation, va_list arguments) {
  PLI_INT32 finish_number = 0;
  if (operation == vpiStop || operation == vpiFinish) {
    finish_number = va_arg(arguments, PLI_INT32);
  }
  return LyraVpiControl(operation, finish_number);
}

PLI_INT32 vpi_control(PLI_INT32 operation, ...) {
  va_list ap;
  va_start(ap, operation);
  const PLI_INT32 answer = Control(operation, ap);
  va_end(ap);
  return answer;
}

/* The names an application compiled for a version of the standard calls
 * `vpi_control` by (LRM 36.12.2.1). No version changed what an operation
 * means, so each does what `vpi_control` does. */
PLI_INT32 vpi_control_1364v1995(PLI_INT32 operation, ...);
PLI_INT32 vpi_control_1364v2001(PLI_INT32 operation, ...);
PLI_INT32 vpi_control_1364v2005(PLI_INT32 operation, ...);
PLI_INT32 vpi_control_1800v2005(PLI_INT32 operation, ...);
PLI_INT32 vpi_control_1800v2009(PLI_INT32 operation, ...);
PLI_INT32 vpi_control_1800v2012(PLI_INT32 operation, ...);

PLI_INT32 vpi_control_1364v1995(PLI_INT32 operation, ...) {
  va_list ap;
  va_start(ap, operation);
  const PLI_INT32 answer = Control(operation, ap);
  va_end(ap);
  return answer;
}

PLI_INT32 vpi_control_1364v2001(PLI_INT32 operation, ...) {
  va_list ap;
  va_start(ap, operation);
  const PLI_INT32 answer = Control(operation, ap);
  va_end(ap);
  return answer;
}

PLI_INT32 vpi_control_1364v2005(PLI_INT32 operation, ...) {
  va_list ap;
  va_start(ap, operation);
  const PLI_INT32 answer = Control(operation, ap);
  va_end(ap);
  return answer;
}

PLI_INT32 vpi_control_1800v2005(PLI_INT32 operation, ...) {
  va_list ap;
  va_start(ap, operation);
  const PLI_INT32 answer = Control(operation, ap);
  va_end(ap);
  return answer;
}

PLI_INT32 vpi_control_1800v2009(PLI_INT32 operation, ...) {
  va_list ap;
  va_start(ap, operation);
  const PLI_INT32 answer = Control(operation, ap);
  va_end(ap);
  return answer;
}

PLI_INT32 vpi_control_1800v2012(PLI_INT32 operation, ...) {
  va_list ap;
  va_start(ap, operation);
  const PLI_INT32 answer = Control(operation, ap);
  va_end(ap);
  return answer;
}
