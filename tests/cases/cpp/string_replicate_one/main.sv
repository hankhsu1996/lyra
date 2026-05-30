module Top;
  string base;
  int n;
  string s;

  initial begin
    base = "hello";
    n = 1;
    s = {n{base}};
  end
endmodule
