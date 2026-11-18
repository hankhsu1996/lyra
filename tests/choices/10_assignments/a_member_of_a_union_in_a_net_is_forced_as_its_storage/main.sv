// A `force` names a net, a constant bit-select or part-select of a vector net,
// or a concatenation of these (LRM 10.6.2), so a force of a member of a union
// in a net, or of a part of one, is no target the list names. Lyra takes one
// as it takes a force of a structure's member: the member is a view of the
// union's storage from its first bit (LRM 7.3), so the force holds the bits
// of the storage the member reaches, overriding every driver there, whichever
// member those drivers drive through, until it is released.
module Top;
  typedef union { logic [3:0] narrow; logic [15:0] wide; } overlay_t;
  logic [3:0] a;
  wire overlay_t held;
  assign held.wide = {12'h4ff, a};
  wire overlay_t shared;
  assign shared.narrow = a;

  initial begin
    a = 4'h9;
    #1;
    force held.wide[7:4] = 4'h0;
    #1;
    if (held.wide !== 16'h4f09)
      $fatal(1, "a forced part of a union member: %h, expected 4f09",
             held.wide);
    release held.wide[7:4];
    #1;
    if (held.wide !== 16'h4ff9)
      $fatal(1, "a released part of a union member: %h, expected 4ff9",
             held.wide);

    force shared.wide = 16'ha5a5;
    a = 4'h3;
    #1;
    if (shared.wide !== 16'ha5a5)
      $fatal(1, "a driver of another member moved a forced member: %h",
             shared.wide);
    release shared.wide;
    #1;
    if (shared.narrow !== 4'h3)
      $fatal(1, "a released member's driver: %h, expected 3", shared.narrow);
    $display("All checks passed");
  end
endmodule
