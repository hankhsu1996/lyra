#pragma once

#include <memory>
#include <string>
#include <vector>

namespace slang::ast {
class Compilation;
}

namespace lyra::frontend {

auto LoadCompilation(std::vector<std::string> files)
    -> std::unique_ptr<slang::ast::Compilation>;

}  // namespace lyra::frontend
