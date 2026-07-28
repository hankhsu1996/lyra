module Top;
  typedef enum {A = 10, B = 20} val_t;
  // A packed enum used as an operand in a concatenation: LRM 6.19.3 auto-casts
  // it to its base integral, so it contributes its base bits like any packed
  // operand (the runtime enum value is-a packed array).
  typedef enum logic [6:0] {OP_A = 7'h33} op_t;
  val_t v;
  int result;
  logic [8:0] packed_result;
  initial begin
    v = A;
    result = v + 5;
    packed_result = {2'b10, OP_A};
  end
endmodule
