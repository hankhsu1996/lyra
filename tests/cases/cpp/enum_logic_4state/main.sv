module Top;
  // 4-state base: default initial value is all-X. name() on X returns "";
  // next() on a non-member returns the default initial value (all-X), which
  // is itself not a member, so subsequent name() still returns "".
  typedef enum logic [1:0] {IDLE = 2'b01, RUN = 2'b10, STOP = 2'b11} state_t;
  state_t s_default;
  state_t s_member;
  string n_default;
  string n_member;
  string n_x_next;
  initial begin
    // Variable default is all-X.
    n_default = s_default.name();
    // Member case.
    s_member = RUN;
    n_member = s_member.name();
    // next() on the X-valued default: returns default initial value (X),
    // whose name() is "".
    n_x_next = s_default.next().name();
    $display("[%s][%s][%s]", n_default, n_member, n_x_next);
  end
endmodule
