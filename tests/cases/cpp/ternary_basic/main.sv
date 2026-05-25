module Top;
  int a;
  int b;
  int larger;
  initial begin
    a = 7;
    b = 3;
    larger = (a > b) ? a : b;
  end
endmodule
