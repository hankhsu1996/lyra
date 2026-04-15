#pragma once

#include <memory>
#include <string>
#include <variant>
#include <vector>

namespace lyra::projection::cpp {

struct ProjExpr;

struct ProjConstant {
  std::string text;
};

struct ProjVarRef {
  std::string name;
};

struct ProjUnary {
  std::string op_token;
  std::unique_ptr<ProjExpr> operand;
};

struct ProjBinary {
  std::string op_token;
  std::unique_ptr<ProjExpr> lhs;
  std::unique_ptr<ProjExpr> rhs;
};

using ProjExprData =
    std::variant<ProjConstant, ProjVarRef, ProjUnary, ProjBinary>;

struct ProjExpr {
  ProjExprData data;
};

struct ProjStmt;

struct ProjWrite {
  std::string target_name;
  std::unique_ptr<ProjExpr> value;
};

struct ProjBlock {
  std::vector<std::unique_ptr<ProjStmt>> children;
};

struct ProjIf {
  std::unique_ptr<ProjExpr> condition;
  std::unique_ptr<ProjStmt> then_branch;
  std::unique_ptr<ProjStmt> else_branch;
};

using ProjStmtData = std::variant<ProjWrite, ProjBlock, ProjIf>;

struct ProjStmt {
  ProjStmtData data;
};

struct ProjField {
  std::string type_text;
  std::string name;
  std::string initializer;
};

struct ProjProcess {
  std::string name;
  std::unique_ptr<ProjStmt> body;
};

struct PerCUProjection {
  std::string class_name;
  std::vector<ProjField> fields;
  std::vector<ProjProcess> processes;
};

// Host entry: trivial main that constructs the root object and runs it.
struct HostProjection {
  std::string cu_header_name;
  std::string qualified_class_name;
  std::string status_field_name;
  std::string entry_process_name;
};

}  // namespace lyra::projection::cpp
