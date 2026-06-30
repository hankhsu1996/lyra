module Top;
  // 2-state fields (one unsigned, one signed) sharing a packed struct with a
  // 4-state field. The `logic` member forces the struct's storage to 4-state,
  // so each `bit` field is a 2-state value living in a 4-state slot: a read
  // must collapse X to 0 and honour the field's signedness, and a write must
  // widen the 2-state value into the 4-state slot (LRM 7.2.1, 11.8.1). The
  // packed union and the nested struct exercise the same field path through
  // overlapping storage and through composed member access.
  typedef struct packed {
    bit [7:0] lo;
    bit signed [3:0] sgn;
    logic [7:0] hi;
  } mixed_t;

  typedef union packed {
    bit [7:0] u_lo;
    logic [7:0] u_hi;
  } mixed_union_t;

  typedef struct packed {
    bit [3:0] n_lo;
    logic [3:0] n_hi;
  } inner_t;

  typedef struct packed {
    inner_t inr;
    logic [7:0] top;
  } outer_t;

  mixed_t s;
  mixed_union_t u;
  outer_t o;
  bit [7:0] uninit_lo;
  int res_lo;
  int res_hi;
  int res_sgn;
  int res_u;
  int res_nested;
  bit [7:0] back_lo;

  initial begin
    uninit_lo = s.lo;
    s.lo = 8'd200;
    s.sgn = -4'sd3;
    s.hi = 8'hff;
    res_lo = s.lo;
    res_hi = s.hi;
    res_sgn = s.sgn;
    back_lo = s.lo;

    u.u_lo = 8'd42;
    res_u = u.u_lo;

    o.inr.n_lo = 4'd9;
    res_nested = o.inr.n_lo;
  end
endmodule
