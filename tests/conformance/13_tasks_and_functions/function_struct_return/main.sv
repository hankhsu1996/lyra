// A function may return a structure or a union. Inside such a function a name
// beginning with the function's own name selects a member of the return value,
// and at the call site a member may be selected from the value the call yields
// (LRM 13.4.1).
typedef struct packed {
  logic [3:0] hi;
  logic [3:0] lo;
} pair_t;

typedef union packed {
  logic [7:0] byte_view;
  pair_t pair_view;
} u_t;

module Top;
  logic [7:0] by_name;
  logic [7:0] by_pattern;
  logic [3:0] member;
  logic [7:0] from_union;

  function automatic pair_t mk_byname(input logic [3:0] a, input logic [3:0] b);
    mk_byname.hi = a;
    mk_byname.lo = b;
  endfunction

  function automatic pair_t mk_pattern(input logic [3:0] a,
                                       input logic [3:0] b);
    return '{hi: a, lo: b};
  endfunction

  function automatic u_t mk_union(input logic [7:0] v);
    mk_union.byte_view = v;
  endfunction

  initial begin
    by_name = mk_byname(4'hA, 4'h5);
    by_pattern = mk_pattern(4'hC, 4'h3);
    member = mk_byname(4'hA, 4'h5).hi;
    from_union = mk_union(8'h9F);
  end

  final begin
    if (by_name !== 8'hA5) $fatal(1, "by_name was %h, expected a5", by_name);
    if (by_pattern !== 8'hC3)
      $fatal(1, "by_pattern was %h, expected c3", by_pattern);
    if (member !== 4'hA) $fatal(1, "member was %h, expected a", member);
    if (from_union !== 8'h9F)
      $fatal(1, "from_union was %h, expected 9f", from_union);
    $display("All checks passed");
  end
endmodule
