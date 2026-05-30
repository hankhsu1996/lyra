module Top;
  string s;

  initial begin
    s.realtoa(3.14);
    $display("%s", s);
  end
endmodule
