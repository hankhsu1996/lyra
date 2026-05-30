module Top;
  string base;
  string s;

  initial begin
    base = "abc";
    s = {3{base}};
  end
endmodule
