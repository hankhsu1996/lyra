module Top;
  string a;
  string b;
  string s;

  initial begin
    a = "hello";
    b = " world";
    s = {a, b};
  end
endmodule
