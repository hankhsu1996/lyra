// An interface port declared with a range stands for every instance of the
// array connected to it, and selecting an element of the port reaches that
// instance (LRM 25.3, 23.3.3.5). Where a defparam made one element of the
// connected array differ from the others (LRM 23.10.1), every way of reaching
// an element reaches the one the select names: a name through the port, the
// port handed on to another port carrying a range, part of an array connected
// to a port, a port and an array declared in opposite directions, paired left
// index to left index, an array an interface holds reached through a port, and
// that whole array handed on to another port. A virtual interface is not among
// them: an instance a defparam targets cannot be assigned to one (LRM 25.9).
interface Bus #(parameter int W = 8);
  logic [W-1:0] data;
  int width = $bits(data);
endinterface

interface Holder;
  Bus bank[2] ();
endinterface

module Reader (Bus q[2]);
  int w0, w1;
  initial begin
    #2;
    w0 = q[0].width;
    w1 = q[1].width;
  end
endmodule

module User (Bus p[2]);
  logic [15:0] seen0, seen1;
  Reader r (.q(p));
  initial begin
    p[0].data = '1;
    p[1].data = '1;
    #1;
    seen0 = 16'(p[0].data);
    seen1 = 16'(p[1].data);
  end
endmodule

module Flip (Bus f[0:1]);
  int w0, w1;
  initial begin
    #1;
    w0 = f[0].width;
    w1 = f[1].width;
  end
endmodule

module Inside (Holder h);
  int w0, w1;
  initial begin
    #1;
    w0 = h.bank[0].width;
    w1 = h.bank[1].width;
  end
endmodule

module Relay (Holder h);
  Reader r (.q(h.bank));
endmodule

module Top;
  Bus b[2] ();
  defparam b[1].W = 16;
  User u (.p(b));

  Bus c[3] ();
  defparam c[2].W = 12;
  Reader part (.q(c[1:2]));

  Bus d[1:0] ();
  defparam d[1].W = 16;
  Flip flip (.f(d));

  Holder h ();
  defparam h.bank[1].W = 16;
  Inside in (.h(h));
  Relay relay (.h(h));

  final begin
    if (u.seen0 !== 16'h00ff) $fatal(1, "p[0] read back %h", u.seen0);
    if (u.seen1 !== 16'hffff) $fatal(1, "p[1] read back %h", u.seen1);
    if (b[0].data !== 8'hff) $fatal(1, "b[0] holds %h", b[0].data);
    if (b[1].data !== 16'hffff) $fatal(1, "b[1] holds %h", b[1].data);
    if (u.r.w0 !== 8 || u.r.w1 !== 16)
      $fatal(1, "handed on, the port's elements are %0d and %0d bits",
             u.r.w0, u.r.w1);
    if (part.w0 !== 8 || part.w1 !== 12)
      $fatal(1, "a part of c is %0d and %0d bits", part.w0, part.w1);
    if (flip.w0 !== 16 || flip.w1 !== 8)
      $fatal(1, "paired left to left, f is %0d and %0d bits", flip.w0,
             flip.w1);
    if (in.w0 !== 8 || in.w1 !== 16)
      $fatal(1, "through the holder, bank is %0d and %0d bits", in.w0, in.w1);
    if (relay.r.w0 !== 8 || relay.r.w1 !== 16)
      $fatal(1, "bank handed on is %0d and %0d bits", relay.r.w0, relay.r.w1);
    $display("All checks passed");
  end
endmodule
