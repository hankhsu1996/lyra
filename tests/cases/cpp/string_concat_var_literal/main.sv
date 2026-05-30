module Top;
  string a;
  string s;

  initial begin
    a = "hi";
    s = {a, " there"};
  end
endmodule
