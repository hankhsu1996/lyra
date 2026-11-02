#pragma once

#include "lyra/base/source_location.hpp"

namespace lyra::test {

// Where a test's operation is written, which only a report names.
inline constexpr base::SourceLocation::Impl kWrittenAtImpl{
    .file = "test.sv", .line = 1, .column = 1};
inline constexpr base::SourceLocation kWrittenAt{&kWrittenAtImpl};

}  // namespace lyra::test
