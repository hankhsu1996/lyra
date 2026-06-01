module Top;
  int a;
  int observed;
  initial begin
    a = 0;
    observed = 0;
  end
  always @(a) begin
    observed = a;
  end
  initial begin
    #1;
    a++;
    #1;
    ++a;
    #1;
    a--;
  end
endmodule
