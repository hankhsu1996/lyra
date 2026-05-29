module Top;
  string a;
  string b;
  bit r;

  initial begin
    a = "foo";
    b = "bar";
    r = (a != b);
  end
endmodule
