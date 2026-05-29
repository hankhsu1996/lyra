module Top;
  string a;
  string b;
  bit r;

  initial begin
    a = "ab";
    b = "abc";
    r = (a < b);
  end
endmodule
