// An interface port names the instance the connection bound it to (LRM 25.3),
// while a hierarchical name names the instance it spells (LRM 23.6). One module
// may use both, and then the two reach the same instance only where the
// connection happens to have bound that one: the port follows its binding into
// every instantiation, and the name does not. This holds for a member read
// through either and for a subroutine enabled through either, because both are
// references and what separates them is the route rather than the target.
interface Bus;
  logic [7:0] data;

  function automatic void Write(input logic [7:0] value);
    data = value;
  endfunction
endinterface

module Dual (
    Bus b
);
  logic [7:0] through_port = 8'h00;
  logic [7:0] by_name = 8'h00;

  initial #1 b.Write(8'h33);
  initial #2 Top.first.Write(8'h44);
  initial #3 begin
    through_port = b.data;
    by_name = Top.first.data;
  end
endmodule

module Top;
  Bus first ();
  Bus second ();

  // One specialization, two bindings: what the port reaches differs between
  // them and what the hierarchical name reaches does not.
  Dual on_first (first);
  Dual on_second (second);

  final begin
    if (first.data !== 8'h44)
      $fatal(1, "first.data was %h, expected 44", first.data);
    if (second.data !== 8'h33)
      $fatal(1, "second.data was %h, expected 33", second.data);
    if (on_second.through_port !== 8'h33)
      $fatal(
          1, "on_second read %h through its port, expected 33",
          on_second.through_port);
    if (on_second.by_name !== 8'h44)
      $fatal(
          1, "on_second read %h by name, expected 44", on_second.by_name);
    $display("All checks passed");
  end
endmodule
