#pragma once

#include <argparse/argparse.hpp>

namespace lyra::driver {

auto RunCommand(const argparse::ArgumentParser& cmd) -> int;
auto DumpCommand(const argparse::ArgumentParser& cmd) -> int;
auto CheckCommand(const argparse::ArgumentParser& cmd) -> int;
auto InitCommand(const argparse::ArgumentParser& cmd) -> int;

}  // namespace lyra::driver
