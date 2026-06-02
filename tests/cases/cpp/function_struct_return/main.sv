typedef struct packed {
  logic [3:0] hi;
  logic [3:0] lo;
} pair_t;

typedef union packed {
  logic [7:0] byte_view;
  pair_t pair_view;
} u_t;

module Top;
  logic [7:0] r_byname;
  logic [7:0] r_explicit;
  logic [3:0] r_member;
  logic [7:0] r_union;

  function pair_t mk_byname(input logic [3:0] a, input logic [3:0] b);
    mk_byname.hi = a;
    mk_byname.lo = b;
  endfunction

  function pair_t mk_explicit(input logic [3:0] a, input logic [3:0] b);
    return '{hi: a, lo: b};
  endfunction

  function u_t mk_union(input logic [7:0] v);
    mk_union.byte_view = v;
  endfunction

  initial begin
    r_byname = mk_byname(4'hA, 4'h5);
    r_explicit = mk_explicit(4'hC, 4'h3);
    r_member = mk_byname(4'hA, 4'h5).hi;
    r_union = mk_union(8'h9F);
  end
endmodule
