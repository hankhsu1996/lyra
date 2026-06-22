module Top;
  string s;
  byte pre;
  byte post;

  initial begin
    s = "hello";
    // LRM 10.4.2: a nonblocking write to a string element defers the putc to
    // the NBA region, so the active-region read still sees the old byte.
    s[0] <= "H";
    pre = s[0];
    #1;
    post = s[0];
  end
endmodule
