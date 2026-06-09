module Top;
  // Two sibling blocks each declare a static `x`. They are distinct per-instance
  // locals that accumulate independently across calls (LRM 6.21), so the emitted
  // frame must keep them apart rather than collapsing them onto one member.
  function automatic int f();
    automatic int a;
    automatic int b;
    begin
      static int x = 0;
      x = x + 1;
      a = x;
    end
    begin
      static int x = 0;
      x = x + 10;
      b = x;
    end
    return a * 100 + b;
  endfunction
  initial begin
    $display("%0d", f());
    $display("%0d", f());
  end
endmodule
