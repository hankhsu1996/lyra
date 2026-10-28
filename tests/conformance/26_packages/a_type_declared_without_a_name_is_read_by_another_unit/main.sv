// A declaration may state an enumeration or an unpacked structure without a
// typedef naming it (LRM 6.19, 7.2), and the type is then the declaration's
// own. Another unit reaching the declaration -- a package's variable or
// parameter out of the package (LRM 26.3), an instance's by a hierarchical
// name (LRM 23.6) -- reads the type as the declaring unit declares it: each
// member's value and each field, the element of an array or a queue of one
// included.
package p;
  enum {A, B} state = B;
  parameter enum {X, Y} P = Y;
  struct {int count; logic [3:0] tag;} record = '{count: 3, tag: 4'h9};
  typedef enum {C, D} pair_t[2];
  pair_t pair = '{D, C};
  enum {E, F, G} queue[$] = '{G};
endpackage

module Child;
  parameter enum {H, J} Q = J;
  enum {K, L} mode = L;
  struct {logic [7:0] byte_value;} held = '{byte_value: 8'h5a};
endmodule

module Top;
  Child c();

  initial begin
    #1;
    if (p::state !== p::B) $fatal(1, "state was %0d", p::state);
    if (p::B !== 1) $fatal(1, "B was %0d", p::B);
    if (p::P !== p::Y) $fatal(1, "P was %0d", p::P);
    if (p::record.count !== 3 || p::record.tag !== 4'h9)
      $fatal(1, "record was %0d %0h", p::record.count, p::record.tag);
    if (p::pair[0] !== p::D || p::pair[1] !== p::C)
      $fatal(1, "pair was %0d %0d", p::pair[0], p::pair[1]);
    if (p::queue[0] !== p::G) $fatal(1, "queue held %0d", p::queue[0]);
    if (c.Q !== c.J) $fatal(1, "Q was %0d", c.Q);
    if (c.mode !== c.L) $fatal(1, "mode was %0d", c.mode);
    if (c.held.byte_value !== 8'h5a)
      $fatal(1, "held was %0h", c.held.byte_value);
    $display("All checks passed");
  end
endmodule
