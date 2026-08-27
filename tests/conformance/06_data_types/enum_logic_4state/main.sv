// An enumeration's default initial value is the default initial value of its
// base type, so a variable of an enumeration over a 4-state type starts out
// all x. That value, and any other value the base type can hold that was not
// declared as a name, is no member of the enumeration: name() returns the
// empty string for it, and next() on it returns the enumeration's default
// initial value rather than one of the names
// (LRM 6.19.5.3, 6.19.5.6, Table 6-7).
module Top;
  typedef enum logic [1:0] {IDLE = 2'b01, RUN = 2'b10, STOP = 2'b11} state_t;

  state_t untouched;
  state_t member;
  state_t undeclared;

  string untouched_name = "unset";
  string member_name;
  string undeclared_name = "unset";
  logic [1:0] undeclared_next = 2'b01;

  initial begin
    untouched_name = untouched.name();

    member = RUN;
    member_name = member.name();

    undeclared = state_t'(2'b00);
    undeclared_name = undeclared.name();
    undeclared_next = undeclared.next();
  end

  final begin
    if (untouched !== 2'bxx)
      $fatal(1, "untouched was %b, expected xx", untouched);
    if (untouched_name !== "")
      $fatal(1, "untouched_name was '%s', expected the empty string",
             untouched_name);
    if (member !== 2'b10) $fatal(1, "member was %b, expected 10", member);
    if (member_name !== "RUN")
      $fatal(1, "member_name was '%s', expected RUN", member_name);
    if (undeclared_name !== "")
      $fatal(1, "undeclared_name was '%s', expected the empty string",
             undeclared_name);
    if (undeclared_next !== 2'bxx)
      $fatal(1, "undeclared_next was %b, expected xx", undeclared_next);
    $display("All checks passed");
  end
endmodule
