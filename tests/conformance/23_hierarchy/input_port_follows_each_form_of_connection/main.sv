// An input port declared with a variable data type is continuously assigned
// from what it is connected to, whatever that is: a variable of the port's own
// type, an expression, a part of a variable, a variable reached by a
// hierarchical name, or an input of the instantiating module handed on (LRM
// 23.3.3, 23.3.3.2). Left unconnected it holds its data type's default initial
// value, and omitted it takes the default the port declares (LRM 23.2.2.4).
// The data type may be any that crosses a port: an unpacked structure, a vector
// wider than a machine word, an unpacked array. A change to the source is a
// change to the port, which an event control on the port sees once.
module Probe (
    input logic [7:0] in
);
  int changes;

  always @(in) changes++;
endmodule

module Handed (
    input logic [7:0] in
);
  Probe below (.in(in));
endmodule

module Forms (
    input logic [7:0] same,
    input logic [7:0] computed,
    input logic [3:0] part,
    input logic [7:0] elsewhere,
    input logic [7:0] open,
    input int         open_int,
    input int         defaulted = 17
);
endmodule

typedef struct {
  logic [3:0] tag;
  int         count;
} record_t;

module Shapes (
    input record_t          record,
    input logic    [99:0]   wide,
    input int               list  [3]
);
endmodule

module Other;
  logic [7:0] kept = 8'd77;
endmodule

module Top (
    input logic [7:0] from_nothing
);
  logic [7:0] src;
  record_t record;
  logic [99:0] wide;
  int list[3];

  Other other ();
  Forms forms (
      .same(src),
      .computed(src + 8'd1),
      .part(src[3:0]),
      .elsewhere(other.kept),
      .open(),
      .open_int()
  );
  Shapes shapes (
      .record(record),
      .wide(wide),
      .list(list)
  );
  Handed handed (.in(src));
  Probe each[2] (.in(src));
  for (genvar i = 0; i < 2; i++) begin : lane
    Probe probe (.in(src));
  end
  Probe at_top (.in(from_nothing));

  initial begin
    src = 8'd9;
    record = '{tag: 4'd5, count: 100};
    wide = {4'hA, 96'd3};
    list = '{1, 2, 3};
    #1;
    if (forms.same !== 8'd9) $fatal(1, "same was %0d, expected 9", forms.same);
    if (forms.computed !== 8'd10)
      $fatal(1, "computed was %0d, expected 10", forms.computed);
    if (forms.part !== 4'd9) $fatal(1, "part was %0d, expected 9", forms.part);
    if (forms.elsewhere !== 8'd77)
      $fatal(1, "elsewhere was %0d, expected 77", forms.elsewhere);
    if (forms.open !== 8'bx) $fatal(1, "open was %b, expected all x", forms.open);
    if (forms.open_int !== 0)
      $fatal(1, "open_int was %0d, expected 0", forms.open_int);
    if (forms.defaulted !== 17)
      $fatal(1, "defaulted was %0d, expected 17", forms.defaulted);
    if (shapes.record.tag !== 4'd5 || shapes.record.count !== 100)
      $fatal(1, "record was %0d / %0d, expected 5 / 100", shapes.record.tag,
             shapes.record.count);
    if (shapes.wide !== {4'hA, 96'd3})
      $fatal(1, "wide was %h, expected a then 3", shapes.wide);
    if (shapes.list[2] !== 3)
      $fatal(1, "list[2] was %0d, expected 3", shapes.list[2]);
    if (handed.below.in !== 8'd9)
      $fatal(1, "a handed-on input was %0d, expected 9", handed.below.in);
    if (each[1].in !== 8'd9)
      $fatal(1, "each[1].in was %0d, expected 9", each[1].in);
    if (lane[1].probe.in !== 8'd9)
      $fatal(1, "lane[1].probe.in was %0d, expected 9", lane[1].probe.in);
    if (at_top.in !== 8'bx)
      $fatal(1, "an input of the top was %b, expected all x", at_top.in);

    handed.below.changes = 0;
    each[1].changes = 0;
    lane[1].probe.changes = 0;
    src = 8'd20;
    other.kept = 8'd78;
    record.count = 101;
    wide[0] = 1'b0;
    list[2] = 30;
    #1;
    if (forms.same !== 8'd20) $fatal(1, "same was %0d, expected 20", forms.same);
    if (forms.computed !== 8'd21)
      $fatal(1, "computed was %0d, expected 21", forms.computed);
    if (forms.part !== 4'd4) $fatal(1, "part was %0d, expected 4", forms.part);
    if (forms.elsewhere !== 8'd78)
      $fatal(1, "elsewhere was %0d, expected 78", forms.elsewhere);
    if (shapes.record.count !== 101)
      $fatal(1, "record.count was %0d, expected 101", shapes.record.count);
    if (shapes.wide !== {4'hA, 96'd2})
      $fatal(1, "wide was %h, expected a then 2", shapes.wide);
    if (shapes.list[2] !== 30)
      $fatal(1, "list[2] was %0d, expected 30", shapes.list[2]);
    if (handed.below.in !== 8'd20)
      $fatal(1, "a handed-on input was %0d, expected 20", handed.below.in);
    if (handed.below.changes !== 1)
      $fatal(1, "a handed-on input saw %0d changes, expected 1",
             handed.below.changes);
    if (each[1].changes !== 1)
      $fatal(1, "each[1] saw %0d changes, expected 1", each[1].changes);
    if (lane[1].probe.changes !== 1)
      $fatal(1, "lane[1].probe saw %0d changes, expected 1",
             lane[1].probe.changes);

    $display("All checks passed");
  end
endmodule
