module cpu
  import riscv_package::*;
#(
    parameter string PROGRAM = "programs/sum_1_to_10.hex"
)(
    input logic clk,
    input logic rst_n
);

  address_t             pc;
  address_t             pc_plus_4;
  word_t                instr;
  decoded_instruction_t decoded;
  control_signals_t     control;
  word_t                rs1_data;
  word_t                rs2_data;
  word_t                alu_result;
  logic                 alu_zero;
  logic                 branch_taken;
  word_t                mem_read_data;
  word_t                wb_data;

  instr_fetch if_stage (
      .clk          (clk),
      .rst_n        (rst_n),
      .jump         (control.jump),
      .branch_taken (control.branch && branch_taken),
      .opcode       (decoded.opcode),
      .immediate    (decoded.immediate),
      .rs1_data     (rs1_data),
      .pc           (pc),
      .pc_plus_4    (pc_plus_4)
  );

  instr_decode id_stage (
      .instr   (instr),
      .decoded (decoded),
      .control (control)
  );

  execution_unit ex_stage (
      .opcode          (decoded.opcode),
      .alu_op          (control.alu_op),
      .alu_source      (control.alu_source),
      .program_counter (pc),
      .rs1_data        (rs1_data),
      .rs2_data        (rs2_data),
      .immediate       (decoded.immediate),
      .branch_cond     (branch_cond_t'(decoded.funct3)),
      .result          (alu_result),
      .zero            (alu_zero),
      .branch_taken    (branch_taken)
  );

  instr_memory #(.PROGRAM(PROGRAM)) imem (
      .addr (pc),
      .data (instr)
  );

  data_memory dmem (
      .clk          (clk),
      .addr         (alu_result),
      .write_data   (rs2_data),
      .write_enable (control.memory_write),
      .read_data    (mem_read_data)
  );

  writeback wb_stage (
      .opcode             (decoded.opcode),
      .jump               (control.jump),
      .memory_to_register (control.memory_to_register),
      .pc_plus_4          (pc_plus_4),
      .memory_data        (mem_read_data),
      .immediate          (decoded.immediate),
      .alu_result         (alu_result),
      .write_data         (wb_data)
  );

  register_file regfile (
      .clk          (clk),
      .write_enable (control.register_write),
      .destination  (decoded.rd),
      .source1      (decoded.rs1),
      .source2      (decoded.rs2),
      .write_data   (wb_data),
      .read_data1   (rs1_data),
      .read_data2   (rs2_data)
  );

endmodule
