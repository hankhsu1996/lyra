module Top;
  event go;
  int r1, r2, r3;

  initial begin
    r1 = 0;
    @go;
    r1 = 11;
  end

  initial begin
    r2 = 0;
    @go;
    r2 = 22;
  end

  initial begin
    r3 = 0;
    @go;
    r3 = 33;
  end

  initial begin
    #5;
    -> go;
  end
endmodule
