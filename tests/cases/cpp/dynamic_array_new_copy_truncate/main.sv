module Top;
  int src [];
  int dst [];
  initial begin
    src = new[5];
    src[0] = 11;
    src[1] = 22;
    src[2] = 33;
    src[3] = 44;
    src[4] = 55;
    dst = new[2](src);
  end
endmodule
