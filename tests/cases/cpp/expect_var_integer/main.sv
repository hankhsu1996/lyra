module Top;
  integer known_;
  integer neg_;
  integer xz_partial;
  integer xz_arith;
  initial begin
    integer a;
    integer b;
    a = 12345;
    b = -42;
    known_ = a;
    neg_ = b;
    b = 32'bxxxxxxxxxxxxxxxxxxxxxxxxxxxx00zz;
    xz_partial = b;
    xz_arith = a + b;
  end
endmodule
