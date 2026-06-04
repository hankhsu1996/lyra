module Top;
  int src [];
  int dst [];
  initial begin
    src = new[3];
    src[0] = 100;
    src[1] = 200;
    src[2] = 300;
    dst = new[5](src);
  end
endmodule
