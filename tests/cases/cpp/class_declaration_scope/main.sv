// A class declaration belongs to the scope that declares it (LRM 8.1), so two
// scopes may declare the same class name and the two are distinct types. Both
// scopes that can do it within one design are covered: two modules, whose
// declarations meet in one emitted program, and two sibling generate blocks of
// one module (LRM 27.6), whose declarations meet in one emitted unit. Each
// class carries a differently named property, so a design that conflated two of
// them would not compile rather than read the wrong value.
module Alpha (output int out);
  class Packet;
    int alpha_only = 11;
  endclass
  Packet p = new();
  initial out = p.alpha_only;
endmodule

module Beta (output int out);
  class Packet;
    int beta_only = 22;
  endclass
  Packet p = new();
  initial out = p.beta_only;
endmodule

module Top;
  int alpha_v;
  int beta_v;
  int g1_v;
  int g2_v;

  Alpha a (.out(alpha_v));
  Beta b (.out(beta_v));

  if (1) begin : g1
    class Packet;
      int g1_only = 33;
    endclass
    Packet p = new();
    initial g1_v = p.g1_only;
  end

  if (1) begin : g2
    class Packet;
      int g2_only = 44;
    endclass
    Packet p = new();
    initial g2_v = p.g2_only;
  end

  // Read after every time-zero initial has run, so the four values are
  // observed in one place regardless of process order.
  initial begin
    #1;
    $display("%0d %0d %0d %0d", alpha_v, beta_v, g1_v, g2_v);
  end
endmodule
