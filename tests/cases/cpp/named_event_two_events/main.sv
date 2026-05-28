module Top;
  event e1, e2;
  int waited;
  int other;

  initial begin
    waited = 0;
    @e1;
    waited = 1;
  end

  initial begin
    other = 0;
    @e2;
    other = 1;
  end

  initial begin
    #5;
    -> e2;
    #5;
    -> e1;
  end
endmodule
