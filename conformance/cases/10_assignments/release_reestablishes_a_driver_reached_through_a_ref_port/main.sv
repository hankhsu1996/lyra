// Releasing a variable that a continuous assignment drives reestablishes that
// assignment (LRM 10.6.2), so the variable then shows what its driver holds at
// the release, a change the driver made while the force covered it included.
// A module reaches a variable connected to its ref port as the variable itself
// (LRM 23.3.3.2), so a continuous assignment it makes to the port drives the
// variable like any other, to the whole of it or one assignment to each part,
// and whatever the variable's width. The target of a force is a singular
// variable (LRM 10.6.2), which a vector of any width is (LRM 6.4).
module DrivesWhole #(parameter int W = 8) (ref logic [W-1:0] target,
                                           input logic [7:0] in);
  assign target = {{(W - 8){1'b0}}, in};
endmodule

module DrivesParts #(parameter int W = 16) (ref logic [W-1:0] target,
                                            input logic [7:0] in);
  assign target[7:0] = in;
  assign target[W-1:8] = '1;
endmodule

module Top;
  logic [7:0] src;
  logic [15:0] narrow_whole;
  logic [15:0] narrow_parts;
  logic [99:0] wide_whole;
  logic [99:0] wide_parts;
  logic [2047:0] huge_whole;

  DrivesWhole #(16) narrow_whole_driver(.target(narrow_whole), .in(src));
  DrivesParts #(16) narrow_parts_driver(.target(narrow_parts), .in(src));
  DrivesWhole #(100) wide_whole_driver(.target(wide_whole), .in(src));
  DrivesParts #(100) wide_parts_driver(.target(wide_parts), .in(src));
  DrivesWhole #(2048) huge_whole_driver(.target(huge_whole), .in(src));

  initial begin
    src = 8'd7;
    #1;
    if (narrow_whole !== 16'd7 || narrow_parts !== 16'hFF07)
      $fatal(1, "before any force the narrow variables were %h and %h",
             narrow_whole, narrow_parts);
    if (wide_whole !== 100'd7 || wide_parts !== {{92{1'b1}}, 8'd7})
      $fatal(1, "before any force the wide variables were %h and %h",
             wide_whole, wide_parts);
    if (huge_whole !== 2048'd7)
      $fatal(1, "before any force the widest variable was %h", huge_whole);

    force narrow_whole = 16'd55;
    force narrow_parts = 16'd55;
    force wide_whole = 100'd55;
    force wide_parts = 100'd55;
    force huge_whole = 2048'd55;
    #1;
    src = 8'd9;
    #1;
    if (narrow_whole !== 16'd55 || narrow_parts !== 16'd55 ||
        wide_whole !== 100'd55 || wide_parts !== 100'd55 ||
        huge_whole !== 2048'd55)
      $fatal(1, "a driver outranked a force: %h %h %h %h", narrow_whole,
             narrow_parts, wide_whole, wide_parts);

    release narrow_whole;
    release narrow_parts;
    release wide_whole;
    release wide_parts;
    release huge_whole;
    #1;
    if (narrow_whole !== 16'd9)
      $fatal(1, "narrow_whole was %h once released, expected 0009",
             narrow_whole);
    if (narrow_parts !== 16'hFF09)
      $fatal(1, "narrow_parts was %h once released, expected ff09",
             narrow_parts);
    if (wide_whole !== 100'd9)
      $fatal(1, "wide_whole was %h once released, expected 9", wide_whole);
    if (huge_whole !== 2048'd9)
      $fatal(1, "huge_whole was %h once released, expected 9", huge_whole);
    if (wide_parts !== {{92{1'b1}}, 8'd9})
      $fatal(1, "wide_parts was %h once released, expected its low byte 09",
             wide_parts);

    $display("All checks passed");
  end
endmodule
