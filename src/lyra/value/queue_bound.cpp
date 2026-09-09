#include "lyra/value/queue_bound.hpp"

#include <iostream>

namespace lyra::value {

void ReportBoundOverflow() {
  std::cerr << "warning: bounded queue exceeded its declared bound; elements "
               "beyond the bound were discarded (LRM 7.10.5)\n";
}

}  // namespace lyra::value
