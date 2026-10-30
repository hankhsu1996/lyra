// A class declared in a module or a generate block is a type of each instance
// of the scope declaring it (LRM 6.22, 8.25), so an object of one instance's
// class is not of another's (LRM 8.16), and each instance runs the code its own
// text elaborated to, reading its own parameters and keeping its own static
// properties (LRM 8.9). That holds for its constructor, a virtual and a static
// method, a task, and a class of another module extending it through a type
// parameter (LRM 8.13, 8.15, 8.20), for a class whose declaration differs
// between the blocks of a loop, and for a covergroup each block declares (LRM
// 19.3), whose type's coverage counts its own block's instances alone (LRM
// 19.11). It holds too where instances of one module build different
// alternatives of a conditional generate (LRM 27.5) and only one of them
// builds the block declaring the classes, the covergroup and the types.
class Base;
endclass

module Extends #(type T = int);
  class D extends T;
    function new();
      super.new(100);
    endfunction
    virtual function int get();
      return super.get() + 1;
    endfunction
  endclass
  function automatic int through_base();
    T t;
    D d = new;
    t = d;
    return t.get();
  endfunction
endmodule

module Holds #(parameter int K = 1) (output int o[5]);
  class C;
    static int made = 0;
    int v;
    function new(int a);
      made++;
      v = a + K;
    endfunction
    virtual function int get();
      return v * K;
    endfunction
    static function int twice(int x);
      return 2 * x + K;
    endfunction
    function int ones();
      bit [K:0] w;
      w = '1;
      return int'(w);
    endfunction
    task later(output int r);
      #1 r = get();
    endtask
  endclass
  Extends #(.T(C)) e();
  C c;
  initial begin
    int r;
    c = new(10);
    c.later(r);
    o[0] = r;
    o[1] = C::twice(5);
    o[2] = c.ones();
    o[3] = e.through_base();
    o[4] = C::made;
  end
endmodule

module Blocks (output int o[3], output int same[3], output int width[3],
               output real cov[3], output real type_cov[3]);
  for (genvar i = 0; i < 3; i++) begin : g
    localparam int K = i * 7 + 1;
    int x;
    class Differs;
      static int made = 0;
      function int get();
        made++;
        return K + made;
      endfunction
    endclass
    class Alike extends Base;
      function int get();
        return i;
      endfunction
    endclass
    class Wide;
      bit [i:0] v;
      function int bits();
        return $bits(v);
      endfunction
    endclass
    covergroup cg;
      coverpoint x {
        bins low = {[0:K-1]};
        bins high = {[K:100]};
      }
    endgroup
    Differs d = new;
    Alike a = new;
    Wide w = new;
    cg c = new;
    function automatic int is_alike(Base b);
      Alike as_alike;
      return $cast(as_alike, b);
    endfunction
    initial begin
      o[i] = d.get();
      same[i] = a.get();
      width[i] = w.bits();
      if (i == 0) begin
        x = 0;
        c.sample();
      end
      x = K;
      c.sample();
      cov[i] = c.get_inst_coverage();
      type_cov[i] = c.get_coverage();
    end
  end
endmodule

module Chooses #(parameter int P = 0) (output int o);
  if (P == 0) begin : off
    typedef struct {
      int a;
      int b;
    } pair_t;
    typedef enum {
      RED,
      GREEN = 5
    } color_t;
    class Held extends Base;
      static int made = 0;
      virtual function int get();
        made++;
        return 10 + made;
      endfunction
    endclass
    class Local;
      function int get();
        return 3;
      endfunction
    endclass
    covergroup cg;
      coverpoint o;
    endgroup
    pair_t p;
    color_t k;
    Held h = new;
    cg c = new;
    initial begin
      Local l;
      l = new;
      p.a = 2;
      p.b = 3;
      k = GREEN;
      o = h.get() + l.get() + p.a + p.b + int'(k);
      c.sample();
    end
  end else begin : on
    initial o = P;
  end
endmodule

module Top;
  int narrow[5];
  int wide[5];
  int blocks[3];
  int same[3];
  int width[3];
  real cov[3];
  real type_cov[3];
  int chose[2];
  Holds #(.K(2)) n (.o(narrow));
  Holds #(.K(3)) w (.o(wide));
  Blocks b (
      .o(blocks), .same(same), .width(width), .cov(cov), .type_cov(type_cov));
  Chooses #(.P(0)) chose_off (.o(chose[0]));
  Chooses #(.P(1)) chose_on (.o(chose[1]));

  final begin
    if (narrow[0] !== 24 || wide[0] !== 39)
      $fatal(1, "a task gave %0d and %0d", narrow[0], wide[0]);
    if (narrow[1] !== 12 || wide[1] !== 13)
      $fatal(1, "a static method gave %0d and %0d", narrow[1], wide[1]);
    if (narrow[2] !== 7 || wide[2] !== 15)
      $fatal(1, "a method's own width gave %0d and %0d", narrow[2], wide[2]);
    if (narrow[3] !== 205 || wide[3] !== 310)
      $fatal(1, "a derived class gave %0d and %0d", narrow[3], wide[3]);
    if (narrow[4] !== 2 || wide[4] !== 2)
      $fatal(1, "a static property counted %0d and %0d", narrow[4], wide[4]);
    if (blocks[0] !== 2 || blocks[1] !== 9 || blocks[2] !== 16)
      $fatal(1, "blocks gave %0d %0d %0d", blocks[0], blocks[1], blocks[2]);
    if (same[0] !== 0 || same[1] !== 1 || same[2] !== 2)
      $fatal(1, "alike blocks gave %0d %0d %0d", same[0], same[1], same[2]);
    if (b.g[0].is_alike(b.g[0].a) !== 1 || b.g[1].is_alike(b.g[0].a) !== 0)
      $fatal(1, "a block's object is of another block's class");
    if (width[0] !== 1 || width[1] !== 2 || width[2] !== 3)
      $fatal(1, "widths gave %0d %0d %0d", width[0], width[1], width[2]);
    if (cov[0] != 100.0 || cov[1] != 50.0 || cov[2] != 50.0)
      $fatal(1, "covergroups gave %0f %0f %0f", cov[0], cov[1], cov[2]);
    if (type_cov[0] != 100.0 || type_cov[1] != 50.0 || type_cov[2] != 50.0)
      $fatal(1, "covergroup types gave %0f %0f %0f", type_cov[0], type_cov[1],
             type_cov[2]);
    if (chose[0] !== 24 || chose[1] !== 1)
      $fatal(1, "instances choosing blocks gave %0d and %0d", chose[0],
             chose[1]);
    $display("All checks passed");
  end
endmodule
