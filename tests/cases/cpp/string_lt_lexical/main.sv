module Top;
  string a;
  string b;
  bit r;

  initial begin
    a = "abc";
    b = "abd";
    r = (a < b);
  end
endmodule
