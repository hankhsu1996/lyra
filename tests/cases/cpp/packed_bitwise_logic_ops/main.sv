module Top;
  logic [3:0] not_;
  logic [3:0] and_;
  logic [3:0] or_;
  logic [3:0] xor_;
  logic [3:0] xnor_;

  initial begin
    logic [3:0] a;
    logic [3:0] b;
    a = 4'b10xz;
    b = 4'b1100;
    not_ = ~a;
    and_ = a & b;
    or_ = a | b;
    xor_ = a ^ b;
    xnor_ = a ~^ b;
  end
endmodule
