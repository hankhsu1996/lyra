module Top;
  bit [3:0] not_;
  bit [3:0] and_;
  bit [3:0] or_;
  bit [3:0] xor_;
  bit [3:0] xnor_;

  initial begin
    bit [3:0] a;
    bit [3:0] b;
    a = 4'b1010;
    b = 4'b1100;
    not_ = ~a;
    and_ = a & b;
    or_ = a | b;
    xor_ = a ^ b;
    xnor_ = a ~^ b;
  end
endmodule
