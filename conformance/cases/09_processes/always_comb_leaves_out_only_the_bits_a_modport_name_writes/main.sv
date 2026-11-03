// An always_comb or always_latch procedure leaves out of its implicit list
// only what it also writes (LRM 9.2.2.2.1 b), and what a write through a name
// a modport defines writes is the bits of the declarations that name's
// expression reaches (LRM 25.5.4), narrowed by any select written over the
// name (LRM 11.5.3). So a procedure writing some bits of a declaration through
// such a name still wakes when other bits of that declaration change, and a
// procedure reading some bits of a name joining several declarations wakes
// only when those bits change, whichever of the declarations they lie in.
interface Bus;
  logic [7:0] data = 0;
  logic [7:0] middle = 0;
  logic [3:0] b1 = 0, b2 = 0;
  logic [3:0] g1 = 0, g2 = 0;
  logic [3:0] l1 = 0, l2 = 0;
  logic [3:0] d1 = 0, d2 = 0;
  logic [3:0] f1 = 0, f2 = 0;
  logic [3:0] c1 = 0, c2 = 0;
  logic [3:0] r1 = 0, r2 = 0;
  logic [3:0] q = 0;
  typedef struct packed { logic [3:0] a, b; } pair_t;
  pair_t      s = 0;

  modport ctrl(
    output .Low(data[3:0]),
    output .High(data[7:4]),
    output .Middle(middle[5:2]),
    output .Top(middle[7]),
    output .B({b1, b2}),
    output .G({g1, g2}),
    output .L({l1, l2}),
    output .D({d1, d2}),
    output .F({f1, f2}),
    output .C({c1, c2}),
    output .R({r1, r2}),
    output .Q(q[3:0]),
    output .S(s));
endinterface

module Body(Bus.ctrl p);
  logic       enable = 1;
  logic [3:0] read_f;
  int         f_runs = 0;
  int         at = 0;
  logic [3:0] source = 0;
  logic [3:0] read_r;
  logic       clk = 0;
  int         rises = 0;

  // A name standing for part of one declaration, written whole, beside a
  // read of the rest of that declaration.
  always_comb p.Low = p.High;

  // The same, written through a select over the name.
  always_comb p.Middle[3] = p.Top;

  // A name joining two declarations, written through a select that reaches
  // one of them, beside a read of the other.
  always_comb p.B[3:0] = p.B[7:4];

  // A select reaching across both declarations a name joins.
  always_comb p.G[5:2] = {2'b00, p.G[7:6]};

  always_latch if (enable) p.L[3:0] = p.L[7:4];

  // What a function the procedure calls writes is left out as well.
  function automatic void copy_d();
    p.D[3:0] = p.D[7:4];
  endfunction
  always_comb copy_d();

  // A read of one declaration through the name wakes on that one alone.
  always_comb begin
    f_runs++;
    read_f = p.F[7:4];
  end

  // A position the program computes.
  always_comb p.C[at +: 4] = source;

  // A read reaching across both declarations a name joins wakes on each.
  always_comb read_r = p.R[5:2];

  // A member of a name standing for a structure.
  always_comb p.S.a = p.S.b;

  // A sampled value of a name designating storage (LRM 16.9.3).
  always @(posedge clk) if ($rose(p.Q[0])) rises++;
endmodule

module Top;
  Bus  b ();
  Body body (b);

  initial begin
    #1 b.data[7:4] = 4'h7;
    #1 if (b.data !== 8'h77) $fatal(1, "a part of one declaration, written whole: %h", b.data);

    b.middle[7] = 1'b1;
    #1 if (b.middle !== 8'b1000_1000) $fatal(1, "a part of one declaration, written through a select: %b", b.middle);

    b.b1 = 4'h5;
    #1 if (b.b2 !== 4'h5) $fatal(1, "one of two joined declarations: %h", b.b2);

    b.g1[3:2] = 2'b11;
    #1 if (b.g1 !== 4'b1100 || b.g2 !== 4'b1100) $fatal(1, "a select across both: %b %b", b.g1, b.g2);

    b.l1 = 4'h9;
    #1 if (b.l2 !== 4'h9) $fatal(1, "an always_latch: %h", b.l2);

    b.d1 = 4'h3;
    #1 if (b.d2 !== 4'h3) $fatal(1, "a function the procedure calls: %h", b.d2);

    begin
      int runs;
      runs = body.f_runs;
      b.f2 = 4'hf;
      #1 if (body.f_runs !== runs) $fatal(1, "woken by the declaration it does not read");
      b.f1 = 4'h6;
      #1 if (body.f_runs !== runs + 1 || body.read_f !== 4'h6) $fatal(1, "a read of one joined declaration: %0d %h", body.f_runs - runs, body.read_f);
    end

    body.source = 4'ha;
    #1 if (b.c2 !== 4'ha) $fatal(1, "a computed position, value: %h", b.c2);
    body.at = 4;
    #1 if (b.c1 !== 4'ha) $fatal(1, "a computed position, moved: %h", b.c1);

    b.r1[0] = 1'b1;
    #1 if (body.read_r !== 4'b0100) $fatal(1, "a read across both, the more significant: %b", body.read_r);
    b.r2[3] = 1'b1;
    #1 if (body.read_r !== 4'b0110) $fatal(1, "a read across both, the less significant: %b", body.read_r);

    b.s.b = 4'h5;
    #1 if (b.s.a !== 4'h5) $fatal(1, "a member of a structure: %h", b.s.a);

    body.clk = 1;
    #1 body.clk = 0;
    b.q[0] = 1'b1;
    #1 body.clk = 1;
    #1 if (body.rises !== 1) $fatal(1, "a sampled value: %0d", body.rises);

    $display("All checks passed");
  end
endmodule
