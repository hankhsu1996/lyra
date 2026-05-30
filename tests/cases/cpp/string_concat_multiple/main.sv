module Top;
  string a;
  string b;
  string c;
  string s;

  initial begin
    a = "one";
    b = "two";
    c = "three";
    s = {a, "-", b, "-", c};
  end
endmodule
