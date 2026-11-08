/* The routines of VPI that write the tool's output channel (LRM 38.5, 38.25,
   38.28, 38.30, 38.41). */
#include <stdarg.h>
#include <vpi_user.h>

int print(const char* text) {
  return vpi_printf("%s", text);
}

static int PrintFromAList(const char* format, ...) {
  va_list ap;
  va_start(ap, format);
  const int written = vpi_vprintf((PLI_BYTE8*)format, ap);
  va_end(ap);
  return written;
}

int print_from_a_list(const char* text) {
  return PrintFromAList("%s", text);
}

int print_to(int mcd, const char* text) {
  return vpi_mcd_printf((PLI_UINT32)mcd, "%s", text);
}

int flush_tool_output(void) {
  return vpi_flush();
}

int flush_channel(int mcd) {
  return vpi_mcd_flush((PLI_UINT32)mcd);
}
