// RISC-V ALU
// Supports basic RV32I operations

import riscv_pkg::*;

module alu (
    input  alu_op_t op,
    input  word_t   a,
    input  word_t   b,
    output word_t   result,
    output logic    zero
);

  always_comb begin
    case (op)
      ALU_ADD:  result = a + b;
      ALU_SUB:  result = a - b;
      ALU_SLL:  result = a << b[4:0];
      ALU_SLT:  result = {31'b0, $signed(a) < $signed(b)};
      ALU_SLTU: result = {31'b0, a < b};
      ALU_XOR:  result = a ^ b;
      ALU_SRL:  result = a >> b[4:0];
      ALU_SRA:  result = $unsigned($signed(a) >>> b[4:0]);
      ALU_OR:   result = a | b;
      ALU_AND:  result = a & b;
      default:  result = '0;
    endcase
  end

  assign zero = (result == '0);

endmodule
