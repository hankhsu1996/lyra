module Top;
  int a;
  int b;
  initial begin
    a = 5;
    b <= a++;
    #1;
  end
endmodule
