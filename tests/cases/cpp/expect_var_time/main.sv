module Top;
  time known_;
  time big_;
  time xz_;
  initial begin
    time a;
    time b;
    a = 12345;
    known_ = a;
    a = 64'h1_0000_0000;
    big_ = a;
    b = 64'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx00zz;
    xz_ = b;
  end
endmodule
