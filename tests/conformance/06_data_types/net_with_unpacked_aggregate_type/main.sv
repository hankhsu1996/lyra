// The data type of a net may be a fixed-size unpacked array, structure or
// union whose every element is itself a valid data type for a net (LRM 6.7.1).
// Such a net is one net whose bits each resolve rather than a collection of
// separate ones: where nothing drives it, it holds the high-impedance value
// every undriven net holds (LRM 6.6), it takes a driver per connection or per
// continuous assignment, and it reads element-wise or member-wise like any
// other aggregate. Independent elements may be driven by separate continuous
// assignments (LRM 6.5), which is how a structure gets one driver per member.
typedef struct {
  logic [3:0] tag;
  logic [7:0] payload;
} frame_t;

typedef union {
  logic [3:0] narrow;
  logic [7:0] wide;
} slot_t;

module Child (
    input wire logic request [0:1],
    input wire logic [7:0] operand [0:1],
    output wire logic acknowledge [0:1],
    output wire logic [7:0] total
);
  assign acknowledge = '{request[1], request[0]};
  assign total = operand[0] + operand[1];
endmodule

module Floating (input wire logic probe [0:1]);
endmodule

module Top;
  logic request [0:1];
  logic [7:0] operand [0:1];
  logic acknowledge [0:1];
  wire logic [7:0] total;

  wire frame_t whole;
  wire frame_t split;
  wire slot_t chosen;

  assign whole = '{4'h6, 8'hc3};
  assign split.tag = 4'h9;
  assign split.payload = 8'h5a;
  assign chosen.wide = 8'he1;

  Child child (
      .request(request),
      .operand(operand),
      .acknowledge(acknowledge),
      .total(total)
  );
  Floating floating ();

  logic first_acknowledge;
  logic second_acknowledge;
  logic [7:0] seen_total;
  logic first_probe;
  logic second_probe;

  initial begin
    request[0] = 1'b1;
    request[1] = 1'b0;
    operand[0] = 8'd11;
    operand[1] = 8'd22;
    #1;
    first_acknowledge = acknowledge[0];
    second_acknowledge = acknowledge[1];
    seen_total = total;
    first_probe = floating.probe[0];
    second_probe = floating.probe[1];
  end

  final begin
    if (first_acknowledge !== 1'b0)
      $fatal(1, "first_acknowledge was %b, expected 0", first_acknowledge);
    if (second_acknowledge !== 1'b1)
      $fatal(1, "second_acknowledge was %b, expected 1", second_acknowledge);
    if (seen_total !== 8'd33)
      $fatal(1, "seen_total was %0d, expected 33", seen_total);
    if (whole.tag !== 4'h6)
      $fatal(1, "whole.tag was %h, expected 6", whole.tag);
    if (whole.payload !== 8'hc3)
      $fatal(1, "whole.payload was %h, expected c3", whole.payload);
    if (split.tag !== 4'h9)
      $fatal(1, "split.tag was %h, expected 9", split.tag);
    if (split.payload !== 8'h5a)
      $fatal(1, "split.payload was %h, expected 5a", split.payload);
    if (chosen.wide !== 8'he1)
      $fatal(1, "chosen.wide was %h, expected e1", chosen.wide);
    if (first_probe !== 1'bz)
      $fatal(1, "first_probe was %b, expected z", first_probe);
    if (second_probe !== 1'bz)
      $fatal(1, "second_probe was %b, expected z", second_probe);
    $display("All checks passed");
  end
endmodule
