/* The multichannel descriptor routines of VPI (LRM 38.24 to 38.29), each
   handed a descriptor the design holds as an int. */
#include <stdarg.h>
#include <stddef.h>
#include <string.h>
#include <vpi_user.h>

int open_channel(const char* name) {
  return (int)vpi_mcd_open((PLI_BYTE8*)name);
}

int write_channel(int mcd, const char* text) {
  return vpi_mcd_printf((PLI_UINT32)mcd, "%s", text);
}

static int WriteFromAList(PLI_UINT32 mcd, const char* format, ...) {
  va_list ap;
  va_start(ap, format);
  const int written = vpi_mcd_vprintf(mcd, (PLI_BYTE8*)format, ap);
  va_end(ap);
  return written;
}

int write_channel_from_a_list(int mcd, const char* text) {
  return WriteFromAList((PLI_UINT32)mcd, "%s", text);
}

/* A tool may answer a file's name as it was given or as the path it opened,
   so the name given only has to end what is answered. */
int ends_with_name(int cd, const char* name) {
  const char* answered = vpi_mcd_name((PLI_UINT32)cd);
  if (answered == NULL) return 0;
  const size_t length = strlen(answered);
  const size_t given = strlen(name);
  return length >= given && strcmp(answered + length - given, name) == 0;
}

int flush_channel(int mcd) {
  return vpi_mcd_flush((PLI_UINT32)mcd);
}

int close_channel(int mcd) {
  return (int)vpi_mcd_close((PLI_UINT32)mcd);
}
