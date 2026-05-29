module Top;
  string a;
  string b;
  bit r;

  initial begin
    a = "foo";
    b = "foo";
    r = (a == b);
  end
endmodule
