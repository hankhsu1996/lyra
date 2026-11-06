// @rejected:
//
// What may be passed by reference is listed, and a member of an unpacked union
// is not on the list whether the union is tagged or not and whether the formal
// is 'ref' or 'const ref', so a conforming tool rejects this program (LRM
// 13.5.2, 7.3.2).
module Top;
  typedef union tagged { int a; int b; } t_t;
  t_t t;

  function automatic int get(const ref int r);
    return r;
  endfunction

  initial begin
    t = tagged a 3;
    if (get(t.a) == 3) $display("the program the standard rejects ran");
  end
endmodule
