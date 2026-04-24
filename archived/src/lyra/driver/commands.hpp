#pragma once

#include <argparse/argparse.hpp>

namespace lyra::driver {

auto InitCommand(const argparse::ArgumentParser& cmd) -> int;

}  // namespace lyra::driver
