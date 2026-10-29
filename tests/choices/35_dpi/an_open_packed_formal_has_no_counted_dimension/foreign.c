/* How many dimensions the open-array handle reports. */
#include <svdpi.h>

int32_t dimensions_of(const svOpenArrayHandle h) {
  return svDimensions(h);
}
