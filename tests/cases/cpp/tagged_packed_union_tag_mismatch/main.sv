module Top;
  typedef union tagged packed {
    void Invalid;
    int  Valid;
  } vint_t;

  vint_t v;
  int    got;

  initial begin
    v = tagged Invalid;
    // LRM 11.9: reading a member inconsistent with the current tag is a
    // run-time error, so the simulation stops here.
    got = v.Valid;
    $display("unreachable");
  end
endmodule
