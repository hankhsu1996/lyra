// The left-hand side of a procedural assign may be a concatenation of
// variables (LRM 10.6.1), and that of a force a concatenation of variables or
// nets (LRM 10.6.2). Each member is then held to its share of the right-hand
// side, first member most significant, and follows the right-hand side while
// the assignment is in effect (LRM 10.6). A procedural assignment to a member
// does not show while it is held. Deassign and release take the same form and
// end the hold on every member: a variable keeps the value it had until it is
// next assigned.
module Top;
  logic [7:0] source;

  logic [3:0] assigned_high, assigned_low;
  logic [3:0] forced_high, forced_low;
  wire [3:0] net_high, net_low;
  assign net_high = 4'h1;
  assign net_low = 4'h2;

  logic [7:0] assigned_first, assigned_followed, assigned_overridden;
  logic [7:0] assigned_kept, assigned_free;
  logic [7:0] forced_first, forced_followed, forced_kept, forced_free;
  logic [7:0] net_forced, net_followed, net_released;

  initial begin
    source = 8'h5C;
    {assigned_high, assigned_low} = 8'h00;
    {forced_high, forced_low} = 8'h00;
    #1;

    assign {assigned_high, assigned_low} = source;
    force {forced_high, forced_low} = ~source;
    force {net_high, net_low} = source;
    #1;
    assigned_first = {assigned_high, assigned_low};
    forced_first = {forced_high, forced_low};
    net_forced = {net_high, net_low};

    source = 8'hE1;
    #1;
    assigned_followed = {assigned_high, assigned_low};
    forced_followed = {forced_high, forced_low};
    net_followed = {net_high, net_low};

    assigned_low = 4'h7;
    #1;
    assigned_overridden = {assigned_high, assigned_low};

    deassign {assigned_high, assigned_low};
    release {forced_high, forced_low};
    release {net_high, net_low};
    #1;
    assigned_kept = {assigned_high, assigned_low};
    forced_kept = {forced_high, forced_low};
    net_released = {net_high, net_low};

    assigned_low = 4'h7;
    forced_high = 4'h9;
    #1;
    assigned_free = {assigned_high, assigned_low};
    forced_free = {forced_high, forced_low};
  end

  final begin
    if (assigned_first !== 8'h5C)
      $fatal(1, "assign first held %h, expected 5c", assigned_first);
    if (forced_first !== 8'hA3)
      $fatal(1, "force first held %h, expected a3", forced_first);
    if (net_forced !== 8'h5C)
      $fatal(1, "forced nets held %h, expected 5c", net_forced);

    if (assigned_followed !== 8'hE1)
      $fatal(1, "assign followed to %h, expected e1", assigned_followed);
    if (forced_followed !== 8'h1E)
      $fatal(1, "force followed to %h, expected 1e", forced_followed);
    if (net_followed !== 8'hE1)
      $fatal(1, "forced nets followed to %h, expected e1", net_followed);

    if (assigned_overridden !== 8'hE1)
      $fatal(1, "a held member showed %h, expected e1", assigned_overridden);

    if (assigned_kept !== 8'hE1)
      $fatal(1, "after deassign held %h, expected e1", assigned_kept);
    if (forced_kept !== 8'h1E)
      $fatal(1, "after release held %h, expected 1e", forced_kept);
    if (net_released !== 8'h12)
      $fatal(1, "released nets held %h, expected 12", net_released);

    if (assigned_free !== 8'hE7)
      $fatal(1, "after deassign a write left %h, expected e7", assigned_free);
    if (forced_free !== 8'h9E)
      $fatal(1, "after release a write left %h, expected 9e", forced_free);
    $display("All checks passed");
  end
endmodule
