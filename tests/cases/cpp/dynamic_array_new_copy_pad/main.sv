module Top;
  int src [];
  int dst [];
  int d0, d1, d2, d3, d4;
  initial begin
    src = new[3];
    src[0] = 100;
    src[1] = 200;
    src[2] = 300;
    dst = new[5](src);
    d0 = dst[0];
    d1 = dst[1];
    d2 = dst[2];
    d3 = dst[3];
    d4 = dst[4];
  end
endmodule
