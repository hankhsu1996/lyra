module Top;
  logic [3:0] pos_;
  logic [3:0] neg_;
  logic [3:0] bitnot;
  logic lognot_nonzero;
  logic lognot_zero;
  logic [3:0] neg_xz;
  logic lognot_xz;
  initial begin
    logic [3:0] a;
    a = 4'b1010;
    pos_ = +a;
    neg_ = -a;
    bitnot = ~a;
    lognot_nonzero = !a;
    a = 4'b0000;
    lognot_zero = !a;
    a = 4'b00xx;
    neg_xz = -a;
    lognot_xz = !a;
  end
endmodule
