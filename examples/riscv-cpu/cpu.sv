// RISC-V Single-Cycle CPU
// RV32I subset: LUI, AUIPC, JAL, JALR, BEQ/BNE, LW, SW, ADD/SUB/AND/OR/XOR, ADDI

import riscv_pkg::*;

module cpu (
    input logic clk,
    input logic rst_n
);

  // ============================================================
  // Stage 1: Instruction Fetch (IF)
  // ============================================================
  addr_t pc;
  addr_t pc_next;
  word_t instruction;

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n)
      pc <= '0;
    else
      pc <= pc_next;
  end

  // ============================================================
  // Stage 2: Instruction Decode (ID)
  // ============================================================
  decoded_t decoded;
  control_t ctrl;
  word_t    rs1_data;
  word_t    rs2_data;

  // Decode fields
  always_comb begin
    decoded.opcode = opcode_t'(instruction[6:0]);
    decoded.rd     = instruction[11:7];
    decoded.rs1    = instruction[19:15];
    decoded.rs2    = instruction[24:20];
    decoded.funct3 = instruction[14:12];
    decoded.funct7 = instruction[31:25];

    // Immediate based on opcode
    case (decoded.opcode)
      OP_OPIMM, OP_LOAD, OP_JALR: decoded.imm = imm_i(instruction);
      OP_STORE:                   decoded.imm = imm_s(instruction);
      OP_BRANCH:                  decoded.imm = imm_b(instruction);
      OP_LUI, OP_AUIPC:           decoded.imm = imm_u(instruction);
      OP_JAL:                     decoded.imm = imm_j(instruction);
      default:                    decoded.imm = '0;
    endcase
  end

  // Control signal generation
  always_comb begin
    // Defaults
    ctrl = '{default: '0, alu_op: ALU_ADD};

    case (decoded.opcode)
      OP_LUI: begin
        ctrl.reg_write = 1'b1;
        ctrl.alu_src   = 1'b1;  // Use immediate
      end

      OP_AUIPC: begin
        ctrl.reg_write = 1'b1;
        ctrl.alu_src   = 1'b1;
      end

      OP_JAL: begin
        ctrl.reg_write = 1'b1;
        ctrl.jump      = 1'b1;
      end

      OP_JALR: begin
        ctrl.reg_write = 1'b1;
        ctrl.alu_src   = 1'b1;
        ctrl.jump      = 1'b1;
      end

      OP_BRANCH: begin
        ctrl.branch = 1'b1;
      end

      OP_LOAD: begin
        ctrl.reg_write  = 1'b1;
        ctrl.mem_read   = 1'b1;
        ctrl.alu_src    = 1'b1;
        ctrl.mem_to_reg = 1'b1;
      end

      OP_STORE: begin
        ctrl.mem_write = 1'b1;
        ctrl.alu_src   = 1'b1;
      end

      OP_OPIMM: begin
        ctrl.reg_write = 1'b1;
        ctrl.alu_src   = 1'b1;
        ctrl.alu_op    = alu_op_t'({1'b0, decoded.funct3});
      end

      OP_OP: begin
        ctrl.reg_write = 1'b1;
        ctrl.alu_op    = alu_op_t'({decoded.funct7[5], decoded.funct3});
      end

      default: ;
    endcase
  end

  // ============================================================
  // Stage 3: Execute (EX)
  // ============================================================
  word_t alu_a;
  word_t alu_b;
  word_t alu_result;
  logic  alu_zero;
  logic  branch_taken;

  // ALU input selection
  assign alu_a = (decoded.opcode == OP_AUIPC) ? pc : rs1_data;
  assign alu_b = ctrl.alu_src ? decoded.imm : rs2_data;

  // Branch condition
  always_comb begin
    case (branch_t'(decoded.funct3))
      BR_EQ:  branch_taken = (rs1_data == rs2_data);
      BR_NE:  branch_taken = (rs1_data != rs2_data);
      BR_LT:  branch_taken = ($signed(rs1_data) < $signed(rs2_data));
      BR_GE:  branch_taken = ($signed(rs1_data) >= $signed(rs2_data));
      BR_LTU: branch_taken = (rs1_data < rs2_data);
      BR_GEU: branch_taken = (rs1_data >= rs2_data);
      default: branch_taken = 1'b0;
    endcase
  end

  // ============================================================
  // Stage 4: Memory (MEM)
  // ============================================================
  word_t mem_rdata;
  addr_t mem_addr;

  assign mem_addr = alu_result;

  // ============================================================
  // Stage 5: Write Back (WB)
  // ============================================================
  word_t wb_data;

  always_comb begin
    if (ctrl.jump)
      wb_data = pc + 4;  // Return address for JAL/JALR
    else if (ctrl.mem_to_reg)
      wb_data = mem_rdata;
    else if (decoded.opcode == OP_LUI)
      wb_data = decoded.imm;
    else
      wb_data = alu_result;
  end

  // ============================================================
  // PC Update
  // ============================================================
  always_comb begin
    if (ctrl.jump) begin
      if (decoded.opcode == OP_JAL)
        pc_next = pc + decoded.imm;
      else  // JALR
        pc_next = (rs1_data + decoded.imm) & ~32'b1;
    end else if (ctrl.branch && branch_taken) begin
      pc_next = pc + decoded.imm;
    end else begin
      pc_next = pc + 4;
    end
  end

  // ============================================================
  // Sub-modules
  // ============================================================

  regfile u_regfile (
      .clk   (clk),
      .we    (ctrl.reg_write),
      .rd    (decoded.rd),
      .rs1   (decoded.rs1),
      .rs2   (decoded.rs2),
      .wdata (wb_data),
      .rdata1(rs1_data),
      .rdata2(rs2_data)
  );

  alu u_alu (
      .op    (ctrl.alu_op),
      .a     (alu_a),
      .b     (alu_b),
      .result(alu_result),
      .zero  (alu_zero)
  );

  memory u_memory (
      .clk  (clk),
      .iaddr(pc),
      .idata(instruction),
      .daddr(mem_addr),
      .wdata(rs2_data),
      .we   (ctrl.mem_write),
      .rdata(mem_rdata)
  );

endmodule
