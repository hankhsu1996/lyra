/* Compiled for the 1800v2017 compatibility mode (LRM 36.12.2.1). The design
   asks for nothing here, so the call is reached only where `now` is set, and
   what the file shows is that the name the mode binds is one the tool provides.
 */
#define VPI_COMPATIBILITY_VERSION_1800v2017 1
#include <vpi_user.h>

int finish_in_1800v2017_mode(int now) {
  return now != 0 ? vpi_control(vpiFinish, 0) : 0;
}
