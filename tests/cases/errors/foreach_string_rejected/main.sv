module Top;
  string s = "hello";
  initial begin
    foreach (s[i]) begin
      s[i] = "x";
    end
  end
endmodule
