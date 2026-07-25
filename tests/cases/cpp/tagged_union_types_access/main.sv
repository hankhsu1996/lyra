module Top;
  typedef union tagged {
    void Invalid;
    int  Valid;
  } vint_t;

  typedef struct packed {
    bit [4:0] r1;
    bit [4:0] r2;
    bit [4:0] rd;
  } add_fields_t;

  typedef union tagged {
    add_fields_t Add;
    bit [9:0]    Jmp;
  } instr_t;

  vint_t   v_default;
  vint_t   v_valid;
  vint_t   v_invalid;
  vint_t   v_copy;
  instr_t  i1;
  instr_t  i2;

  int      valid_read;
  int      copy_read;
  bit [4:0] add_r1_initial;
  bit [4:0] add_r2_after_write;
  bit [4:0] add_r1_after_compound;
  bit [9:0] jmp_read;

  // The same construction and member-access surface reached from a structural
  // expression rather than a procedural one: a declaration initializer and a
  // continuous assignment (LRM 10.3) both build and read tagged values.
  vint_t   v_struct_init = tagged Valid 42;
  vint_t   v_struct_assigned;
  assign   v_struct_assigned = tagged Valid 77;
  int      struct_init_read;
  assign   struct_init_read = v_struct_init.Valid;
  int      struct_assigned_read;
  assign   struct_assigned_read = v_struct_assigned.Valid;
  instr_t  i_struct_init = tagged Add '{5'd1, 5'd2, 5'd3};
  bit [4:0] struct_nested_read;
  assign   struct_nested_read = i_struct_init.Add.rd;

  initial begin
    v_valid   = tagged Valid (23 + 34);
    v_invalid = tagged Invalid;

    valid_read = v_valid.Valid;

    v_copy    = v_valid;
    copy_read = v_copy.Valid;

    i1 = tagged Add '{5'd7, 5'd11, 5'd3};
    add_r1_initial = i1.Add.r1;

    i1.Add.r2 = 5'd15;
    add_r2_after_write = i1.Add.r2;

    i1.Add.r1 += 5'd2;
    add_r1_after_compound = i1.Add.r1;

    i2 = tagged Jmp 10'h1F0;
    jmp_read = i2.Jmp;

    #1;
  end
endmodule
