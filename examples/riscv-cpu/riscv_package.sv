package riscv_package;

  typedef logic [31:0] word_t;
  typedef logic [4:0]  register_address_t;
  typedef logic [31:0] address_t;

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

  typedef enum logic [2:0] {
    BRANCH_EQ  = 3'b000,
    BRANCH_NE  = 3'b001,
    BRANCH_LT  = 3'b100,
    BRANCH_GE  = 3'b101,
    BRANCH_LTU = 3'b110,
    BRANCH_GEU = 3'b111
  } branch_cond_t;

  typedef struct packed {
    opcode_t            opcode;
    register_address_t  rd;
    register_address_t  rs1;
    register_address_t  rs2;
    logic [2:0]         funct3;
    logic [6:0]         funct7;
    word_t              immediate;
  } decoded_instruction_t;

  typedef struct packed {
    logic    register_write;
    logic    memory_read;
    logic    memory_write;
    logic    alu_source;
    logic    memory_to_register;
    logic    branch;
    logic    jump;
    alu_op_t alu_op;
  } control_signals_t;

  function automatic word_t immediate_i_type(input word_t instruction);
    return {{20{instruction[31]}}, instruction[31:20]};
  endfunction

  function automatic word_t immediate_s_type(input word_t instruction);
    return {{20{instruction[31]}}, instruction[31:25], instruction[11:7]};
  endfunction

  function automatic word_t immediate_b_type(input word_t instruction);
    return {{19{instruction[31]}}, instruction[31], instruction[7], instruction[30:25], instruction[11:8], 1'b0};
  endfunction

  function automatic word_t immediate_u_type(input word_t instruction);
    return {instruction[31:12], 12'b0};
  endfunction

  function automatic word_t immediate_j_type(input word_t instruction);
    return {{11{instruction[31]}}, instruction[31], instruction[19:12], instruction[20], instruction[30:21], 1'b0};
  endfunction

endpackage
