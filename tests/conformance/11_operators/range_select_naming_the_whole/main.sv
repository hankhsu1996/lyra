// A constant part-select whose bounds are the vector's own declared range
// names every bit of it, so reading it reads the whole vector and writing it
// writes the whole vector, in whichever direction the range was declared and
// however many packed dimensions it has. The select is still unsigned whatever
// the vector's signedness, so it extends with zeros where the vector would
// extend with its sign (LRM 11.5.1, 11.8.1). A write through it is a write of
// the vector, which a process waiting on the vector sees, whether the write is
// blocking, nonblocking or continuous (LRM 9.4.2, 10.3.2). A slice of an
// unpacked array taking every element is the array in the same way (LRM
// 7.4.6). A select one bit short of the whole names only the bits it names.
module Flop #(parameter int W = 8) (
    input logic clk,
    input logic [W-1:0] din,
    output logic [W-1:0] dout
);
  always_ff @(posedge clk) dout[W-1:0] <= din[W-1:0];
endmodule

// `K` is read only as an indexed select's base, so each instance may be built
// with its own value and the select's start is not known to the module.
module Window #(parameter int K = 0) (
    input logic [7:0] v,
    output logic [7:0] w
);
  assign w = v[K+:8];
endmodule

module Top;
  logic [7:0] descending;
  logic [0:7] ascending;
  logic signed [7:0] negative;
  logic [127:0] wide;
  logic [3:0][7:0] matrix;

  logic [7:0] read_descending;
  logic [0:7] read_ascending;
  int signed_read_whole;
  int unsigned_read_select;
  logic [127:0] read_wide;
  logic [31:0] read_matrix;
  logic [3:0] read_unknown;
  logic [3:0] unknown;
  logic [7:0] initialized = 8'h5C;
  logic [7:0] read_initialized = initialized[7:0];

  logic [7:0] blocking_target;
  logic [7:0] nonblocking_target;
  logic signed [7:0] signed_target;
  logic [7:0] compound_target;
  wire [7:0] continuous_target;
  logic [7:0] continuous_source;
  assign continuous_target[7:0] = continuous_source;

  logic [7:0] short_low;
  logic [7:0] short_high;

  int array_source[0:3];
  int array_read[0:3];
  int array_written[0:3];
  int array_deferred[0:3];
  int array_expected[0:3] = '{10, 20, 30, 40};

  logic clk;
  logic [15:0] flop_in;
  logic [15:0] flop_out;
  Flop #(.W(16)) flop (.clk(clk), .din(flop_in), .dout(flop_out));

  logic [7:0] window_in;
  logic [7:0] window_whole;
  Window #(.K(0)) window (.v(window_in), .w(window_whole));

  logic [0:0] edge_bit;
  int edges;
  initial begin
    edges = 0;
    forever begin
      @(posedge edge_bit[0:0]);
      edges++;
    end
  end

  int wakeups;
  initial begin
    wakeups = 0;
    forever begin
      @(blocking_target);
      wakeups++;
    end
  end

  int select_wakeups;
  initial begin
    select_wakeups = 0;
    forever begin
      @(compound_target[7:0]);
      select_wakeups++;
    end
  end

  function automatic logic [7:0] passed_through(logic [7:0] value);
    return value;
  endfunction

  logic [7:0] read_passed;

  initial begin
    descending = 8'hA5;
    ascending = 8'h3C;
    negative = -8'sd1;
    wide = 128'h0123_4567_89AB_CDEF_FEDC_BA98_7654_3210;
    matrix = 32'hDEAD_BEEF;
    unknown = 4'b1x0z;
    edge_bit = 0;
    array_source = array_expected;

    read_descending = descending[7:0];
    read_ascending = ascending[0:7];
    signed_read_whole = negative;
    unsigned_read_select = negative[7:0];
    read_wide = wide[127:0];
    read_matrix = matrix[3:0];
    read_unknown = unknown[3:0];
    read_passed = passed_through(descending[7:0]);
    array_read = array_source[0:3];
    array_written[0:3] = array_source;
    array_deferred[0:3] <= array_source[0:3];

    signed_target[7:0] = 8'hF0;

    short_low = 8'hFF;
    short_high = 8'hFF;
    short_low[6:0] = 7'h00;
    short_high[7:1] = 7'h00;

    #1 blocking_target[7:0] = 8'h11;
    #1 blocking_target[7:0] = 8'h22;
    #1 nonblocking_target[7:0] <= 8'h33;
    continuous_source = 8'h44;

    compound_target = 8'h01;
    #1 compound_target[7:0] += 8'h01;
    #1 compound_target[7:0] += 8'h01;

    window_in = 8'h96;
    #1 edge_bit[0:0] = 1;
    #1 edge_bit[0:0] = 0;
    #1 edge_bit[0:0] = 1;

    clk = 0;
    flop_in = 16'hBEEF;
    #1 clk = 1;
    #1;
  end

  final begin
    if (read_descending !== 8'hA5)
      $fatal(1, "descending[7:0] was %h, expected a5", read_descending);
    if (read_ascending !== 8'h3C)
      $fatal(1, "ascending[0:7] was %h, expected 3c", read_ascending);
    if (signed_read_whole !== -1)
      $fatal(1, "negative read whole was %0d, expected -1",
             signed_read_whole);
    if (unsigned_read_select !== 255)
      $fatal(1, "negative[7:0] was %0d, expected 255", unsigned_read_select);
    if (read_wide !== 128'h0123_4567_89AB_CDEF_FEDC_BA98_7654_3210)
      $fatal(1, "wide[127:0] was %h", read_wide);
    if (read_matrix !== 32'hDEAD_BEEF)
      $fatal(1, "matrix[3:0] was %h, expected deadbeef", read_matrix);
    if (read_unknown !== 4'b1x0z)
      $fatal(1, "unknown[3:0] was %b, expected 1x0z", read_unknown);
    if (read_initialized !== 8'h5C)
      $fatal(1, "initialized[7:0] in a declaration was %h, expected 5c",
             read_initialized);
    if (read_passed !== 8'hA5)
      $fatal(1, "descending[7:0] passed was %h, expected a5", read_passed);
    if (signed_target !== -8'sd16)
      $fatal(1, "signed_target was %0d, expected -16", signed_target);
    if (short_low !== 8'h80)
      $fatal(1, "short_low was %h, expected 80", short_low);
    if (short_high !== 8'h01)
      $fatal(1, "short_high was %h, expected 01", short_high);
    if (array_read != array_expected)
      $fatal(1, "array_source[0:3] read was not the array");
    if (array_written != array_expected)
      $fatal(1, "array_written[0:3] written was not the array");
    if (array_deferred != array_expected)
      $fatal(1, "array_deferred[0:3] written later was not the array");
    if (blocking_target !== 8'h22)
      $fatal(1, "blocking_target was %h, expected 22", blocking_target);
    if (wakeups != 2)
      $fatal(1, "a wait on the vector woke %0d times, expected 2", wakeups);
    if (compound_target !== 8'h03)
      $fatal(1, "compound_target was %h, expected 03", compound_target);
    if (select_wakeups != 3)
      $fatal(1, "a wait on the whole select woke %0d times, expected 3",
             select_wakeups);
    if (edges != 2)
      $fatal(1, "posedge of a one-bit whole select fired %0d times", edges);
    if (nonblocking_target !== 8'h33)
      $fatal(1, "nonblocking_target was %h, expected 33", nonblocking_target);
    if (continuous_target !== 8'h44)
      $fatal(1, "continuous_target was %h, expected 44", continuous_target);
    if (window_whole !== 8'h96)
      $fatal(1, "window_whole was %h, expected 96", window_whole);
    if (flop_out !== 16'hBEEF)
      $fatal(1, "flop_out was %h, expected beef", flop_out);
    $display("All checks passed");
  end
endmodule
