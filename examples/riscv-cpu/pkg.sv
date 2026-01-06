// RISC-V CPU Package
// Defines types, opcodes, and instruction formats for RV32I subset

package riscv_pkg;

  // Basic types
  typedef logic [31:0] word_t;
  typedef logic [4:0]  reg_addr_t;
  typedef logic [31:0] addr_t;

  // Opcodes (bits [6:0] of instruction)
  typedef enum logic [6:0] {
    OP_LUI    = 7'b0110111,
    OP_AUIPC  = 7'b0010111,
    OP_JAL    = 7'b1101111,
    OP_JALR   = 7'b1100111,
    OP_BRANCH = 7'b1100011,
    OP_LOAD   = 7'b0000011,
    OP_STORE  = 7'b0100011,
    OP_OPIMM  = 7'b0010011,
    OP_OP     = 7'b0110011
  } opcode_t;

  // ALU operations
  typedef enum logic [3:0] {
    ALU_ADD  = 4'b0000,
    ALU_SUB  = 4'b1000,
    ALU_SLL  = 4'b0001,
    ALU_SLT  = 4'b0010,
    ALU_SLTU = 4'b0011,
    ALU_XOR  = 4'b0100,
    ALU_SRL  = 4'b0101,
    ALU_SRA  = 4'b1101,
    ALU_OR   = 4'b0110,
    ALU_AND  = 4'b0111
  } alu_op_t;

  // Branch conditions
  typedef enum logic [2:0] {
    BR_EQ  = 3'b000,
    BR_NE  = 3'b001,
    BR_LT  = 3'b100,
    BR_GE  = 3'b101,
    BR_LTU = 3'b110,
    BR_GEU = 3'b111
  } branch_t;

  // Decoded instruction struct
  typedef struct packed {
    opcode_t   opcode;
    reg_addr_t rd;
    reg_addr_t rs1;
    reg_addr_t rs2;
    logic [2:0] funct3;
    logic [6:0] funct7;
    word_t     imm;
  } decoded_t;

  // Control signals struct
  typedef struct packed {
    logic      reg_write;
    logic      mem_read;
    logic      mem_write;
    logic      alu_src;     // 0=rs2, 1=imm
    logic      mem_to_reg;  // 0=alu, 1=mem
    logic      branch;
    logic      jump;
    alu_op_t   alu_op;
  } control_t;

  // Immediate extraction functions
  function automatic word_t imm_i(input word_t inst);
    return {{20{inst[31]}}, inst[31:20]};
  endfunction

  function automatic word_t imm_s(input word_t inst);
    return {{20{inst[31]}}, inst[31:25], inst[11:7]};
  endfunction

  function automatic word_t imm_b(input word_t inst);
    return {{19{inst[31]}}, inst[31], inst[7], inst[30:25], inst[11:8], 1'b0};
  endfunction

  function automatic word_t imm_u(input word_t inst);
    return {inst[31:12], 12'b0};
  endfunction

  function automatic word_t imm_j(input word_t inst);
    return {{11{inst[31]}}, inst[31], inst[19:12], inst[20], inst[30:21], 1'b0};
  endfunction

endpackage
