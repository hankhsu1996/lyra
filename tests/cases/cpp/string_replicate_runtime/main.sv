module Top;
  string base;
  int n;
  string s;

  initial begin
    base = "xy";
    n = 4;
    s = {n{base}};
  end
endmodule
