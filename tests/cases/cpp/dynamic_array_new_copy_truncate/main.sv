module Top;
  int src [];
  int dst [];
  int d0, d1;
  initial begin
    src = new[5];
    src[0] = 11;
    src[1] = 22;
    src[2] = 33;
    src[3] = 44;
    src[4] = 55;
    dst = new[2](src);
    d0 = dst[0];
    d1 = dst[1];
  end
endmodule
