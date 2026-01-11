module instr_fetch
  import riscv_package::*;
(
    input  logic     clk,
    input  logic     rst_n,
    input  logic     jump,
    input  logic     branch_taken,
    input  opcode_t  opcode,
    input  word_t    immediate,
    input  word_t    rs1_data,
    output address_t pc,
    output address_t pc_plus_4
);

  address_t pc_next;

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n)
      pc <= '0;
    else
      pc <= pc_next;
  end

  assign pc_plus_4 = pc + 4;

  always_comb begin
    if (jump) begin
      if (opcode == OP_JAL)
        pc_next = pc + immediate;
      else
        pc_next = (rs1_data + immediate) & ~32'b1;
    end else if (branch_taken) begin
      pc_next = pc + immediate;
    end else begin
      pc_next = pc_plus_4;
    end
  end

endmodule
