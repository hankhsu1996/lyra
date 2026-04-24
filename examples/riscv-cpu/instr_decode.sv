module instr_decode
  import riscv_package::*;
(
    input  word_t                instr,
    output decoded_instruction_t decoded,
    output control_signals_t     control
);

  decoded_instruction_t decoded_internal;
  control_signals_t     control_internal;

  assign decoded = decoded_internal;
  assign control = control_internal;

  always_comb begin
    decoded_internal.opcode    = opcode_t'(instr[6:0]);
    decoded_internal.rd        = instr[11:7];
    decoded_internal.rs1       = instr[19:15];
    decoded_internal.rs2       = instr[24:20];
    decoded_internal.funct3    = instr[14:12];
    decoded_internal.funct7    = instr[31:25];

    case (decoded_internal.opcode)
      OP_OPIMM, OP_LOAD, OP_JALR: decoded_internal.immediate = immediate_i_type(instr);
      OP_STORE:                   decoded_internal.immediate = immediate_s_type(instr);
      OP_BRANCH:                  decoded_internal.immediate = immediate_b_type(instr);
      OP_LUI, OP_AUIPC:           decoded_internal.immediate = immediate_u_type(instr);
      OP_JAL:                     decoded_internal.immediate = immediate_j_type(instr);
      default:                    decoded_internal.immediate = '0;
    endcase
  end

  always_comb begin
    control_internal = '{default: '0, alu_op: ALU_ADD};

    case (decoded_internal.opcode)
      OP_LUI: begin
        control_internal.register_write = 1'b1;
        control_internal.alu_source     = 1'b1;
      end

      OP_AUIPC: begin
        control_internal.register_write = 1'b1;
        control_internal.alu_source     = 1'b1;
      end

      OP_JAL: begin
        control_internal.register_write = 1'b1;
        control_internal.jump           = 1'b1;
      end

      OP_JALR: begin
        control_internal.register_write = 1'b1;
        control_internal.alu_source     = 1'b1;
        control_internal.jump           = 1'b1;
      end

      OP_BRANCH: begin
        control_internal.branch = 1'b1;
      end

      OP_LOAD: begin
        control_internal.register_write     = 1'b1;
        control_internal.memory_read        = 1'b1;
        control_internal.alu_source         = 1'b1;
        control_internal.memory_to_register = 1'b1;
      end

      OP_STORE: begin
        control_internal.memory_write = 1'b1;
        control_internal.alu_source   = 1'b1;
      end

      OP_OPIMM: begin
        control_internal.register_write = 1'b1;
        control_internal.alu_source     = 1'b1;
        control_internal.alu_op         = alu_op_t'({1'b0, decoded_internal.funct3});
      end

      OP_OP: begin
        control_internal.register_write = 1'b1;
        control_internal.alu_op         = alu_op_t'({decoded_internal.funct7[5], decoded_internal.funct3});
      end

      default: ;
    endcase
  end

endmodule
