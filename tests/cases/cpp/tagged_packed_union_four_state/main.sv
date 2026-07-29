module Top;
  // A 4-state member makes the whole union 4-state (LRM 7.3.1), so both the
  // undefined gap between the tag and a narrow member's bits and an
  // uninitialized value's tag bits read as x.
  typedef union tagged packed {
    logic [7:0] Wide;
    logic [3:0] Narrow;
  } lt_t;

  lt_t        u;
  lt_t        fresh;
  logic [8:0] raw_narrow;
  logic [8:0] raw_fresh;
  logic [3:0] narrow_read;
  int         narrow_arm;
  int         fresh_arm;

  initial begin
    u = tagged Narrow 4'h5;
    raw_narrow  = u;
    narrow_read = u.Narrow;

    if (u matches tagged Narrow .n) narrow_arm = int'(n);
    else narrow_arm = -1;

    // LRM 11.9: an uninitialized tagged union is undefined including its tag
    // bits, so it is consistent with no member and matches no tagged pattern.
    raw_fresh = fresh;
    if (fresh matches tagged Wide .w) fresh_arm = int'(w);
    else fresh_arm = -1;
  end
endmodule
