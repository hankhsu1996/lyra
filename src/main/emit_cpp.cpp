#include <fstream>
#include <iostream>
#include <sstream>

#include <slang/ast/Compilation.h>

#include "lyra/compiler/codegen.hpp"
#include "lyra/frontend/slang_frontend.hpp"
#include "lyra/lowering/ast_to_mir/ast_to_mir.hpp"

auto main(int argc, char* argv[]) -> int {
  if (argc < 2) {
    std::cerr << "Usage: emit_cpp <file.sv>\n";
    return 1;
  }

  lyra::frontend::SlangFrontend frontend;
  auto compilation = frontend.LoadFromFiles({argv[1]});
  if (!compilation) {
    std::cerr << "Failed to parse\n";
    return 1;
  }

  const auto& root = compilation->getRoot();
  auto mir = lyra::lowering::ast_to_mir::AstToMir(root);
  if (!mir) {
    std::cerr << "Failed to lower to MIR\n";
    return 1;
  }

  lyra::compiler::Codegen codegen;
  std::cout << codegen.Generate(*mir) << std::endl;
  return 0;
}
