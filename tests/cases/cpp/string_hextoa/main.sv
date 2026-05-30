module Top;
  string s_ff;
  string s_dead;

  initial begin
    s_ff.hextoa(255);
    s_dead.hextoa(32'hdeadbeef);
  end
endmodule
