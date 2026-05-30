module Top;
  string base;
  int n;
  string s;

  initial begin
    base = "hello";
    n = 0;
    s = {n{base}};
  end
endmodule
