// Two packages may each declare a structure holding, by value, a structure the
// other declares (LRM 7.2, 26.2), so long as no structure ends up holding
// itself. A generic class of one package specialized on a structure of the
// other is a third place a structure may sit, and a structure declared inside
// that specialization holds the argument by value too (LRM 8.25). Every member
// is written and read back through the outermost value.
package leaf_pkg;
  typedef struct {
    int x;
  } Leaf;

  typedef struct {
    mid_pkg::Mid m;
    int y;
  } Top;

  class Wrap #(type T = int);
    typedef struct {
      T inner;
      int z;
    } Wrapped;
  endclass
endpackage

package mid_pkg;
  typedef struct {
    leaf_pkg::Leaf l;
    int w;
  } Mid;

  typedef struct {
    leaf_pkg::Wrap#(Mid)::Wrapped wrapped;
  } Outer;
endpackage

module Top;
  leaf_pkg::Top t;
  mid_pkg::Outer o;
  int t_sum = -1;
  int o_sum = -1;

  initial begin
    t.m.l.x = 1;
    t.m.w = 2;
    t.y = 3;
    t_sum = t.m.l.x + t.m.w + t.y;

    o.wrapped.inner.l.x = 10;
    o.wrapped.inner.w = 20;
    o.wrapped.z = 30;
    o_sum = o.wrapped.inner.l.x + o.wrapped.inner.w + o.wrapped.z;
  end

  final begin
    if (t_sum !== 6) $fatal(1, "t_sum was %0d, expected 6", t_sum);
    if (o_sum !== 60) $fatal(1, "o_sum was %0d, expected 60", o_sum);
    $display("All checks passed");
  end
endmodule
