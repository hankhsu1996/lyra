#pragma once

#include <memory>
#include <string>
#include <vector>

namespace slang::ast {
class Compilation;
}

namespace volans::frontend {

auto LoadCompilation(std::vector<std::string> files)
    -> std::unique_ptr<slang::ast::Compilation>;

}  // namespace volans::frontend
