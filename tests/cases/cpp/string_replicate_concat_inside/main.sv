module Top;
  string a;
  string b;
  string s;

  initial begin
    a = "a";
    b = "b";
    s = {4{a, b}};
  end
endmodule
