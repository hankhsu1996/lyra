module execution_unit
  import riscv_package::*;
(
    input  opcode_t      opcode,
    input  alu_op_t      alu_op,
    input  logic         alu_source,
    input  address_t     program_counter,
    input  word_t        rs1_data,
    input  word_t        rs2_data,
    input  word_t        immediate,
    input  branch_cond_t branch_cond,
    output word_t        result,
    output logic         zero,
    output logic         branch_taken
);

  word_t operand_a;
  word_t operand_b;

  assign operand_a = (opcode == OP_AUIPC) ? program_counter : rs1_data;
  assign operand_b = alu_source ? immediate : rs2_data;

  always_comb begin
    case (alu_op)
      ALU_ADD:  result = operand_a + operand_b;
      ALU_SUB:  result = operand_a - operand_b;
      ALU_SLL:  result = operand_a << operand_b[4:0];
      ALU_SLT:  result = {31'b0, $signed(operand_a) < $signed(operand_b)};
      ALU_SLTU: result = {31'b0, operand_a < operand_b};
      ALU_XOR:  result = operand_a ^ operand_b;
      ALU_SRL:  result = operand_a >> operand_b[4:0];
      ALU_SRA:  result = $unsigned($signed(operand_a) >>> operand_b[4:0]);
      ALU_OR:   result = operand_a | operand_b;
      ALU_AND:  result = operand_a & operand_b;
      default:  result = '0;
    endcase
  end

  assign zero = (result == '0);

  always_comb begin
    case (branch_cond)
      BRANCH_EQ:  branch_taken = (rs1_data == rs2_data);
      BRANCH_NE:  branch_taken = (rs1_data != rs2_data);
      BRANCH_LT:  branch_taken = ($signed(rs1_data) < $signed(rs2_data));
      BRANCH_GE:  branch_taken = ($signed(rs1_data) >= $signed(rs2_data));
      BRANCH_LTU: branch_taken = (rs1_data < rs2_data);
      BRANCH_GEU: branch_taken = (rs1_data >= rs2_data);
      default:    branch_taken = 1'b0;
    endcase
  end

endmodule
