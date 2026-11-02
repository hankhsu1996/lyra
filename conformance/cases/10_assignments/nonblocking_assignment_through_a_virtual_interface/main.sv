// A nonblocking assignment whose target is reached through a virtual interface
// writes the member of the instance the handle held when the statement ran,
// and writes it at the end of the time step as every nonblocking assignment
// does: a virtual interface reference the target needs is evaluated at the
// same time as the right-hand side (LRM 10.4.2), and every component of the
// instance is available through the handle in a procedural statement
// (LRM 25.9). The handle may be a variable of a module, a class property --
// the standard's own transactor (LRM 25.9) -- or a subroutine argument, it may
// select a modport, the target may be a whole variable, an element or a part
// of one, and the assignment may carry an event control (LRM 9.4.5).
interface Bus;
  logic [7:0] data;
  logic [7:0] lanes [2];
  logic       req;
  modport ctrl(output req);
endinterface

class Transactor;
  virtual Bus bus;

  function new(virtual Bus b);
    bus = b;
  endfunction

  task request();
    bus.req <= 1'b1;
  endtask
endclass

module Top;
  Bus one ();
  Bus two ();
  virtual Bus held;
  virtual Bus.ctrl view;
  Transactor t;
  event go;

  logic [7:0] read_before_update = 8'h00;
  logic [7:0] first_update = 8'h00;
  logic [7:0] lane_before_update = 8'h00;
  logic [7:0] low_after_update = 8'h00;

  task automatic drive(virtual Bus b, logic [7:0] v);
    b.data <= v;
  endtask

  initial begin
    one.data = 8'h01;
    two.data = 8'h02;
    two.lanes[1] = 8'h12;
    one.req = 1'b0;
    two.req = 1'b0;

    // The handle is evaluated where the statement ran: moving it before the
    // update lands does not move the update, and the update is deferred.
    held = one;
    held.data <= 8'h21;
    held = two;
    read_before_update = one.data;
    #1;
    first_update = one.data;

    // An element and a part of a member, through the instance held now.
    held.lanes[1] <= 8'h31;
    held.data[3:0] <= 4'ha;
    lane_before_update = two.lanes[1];
    #1;
    low_after_update = two.data;

    // A modport view, a subroutine argument, and the transactor's property.
    view = one;
    view.req <= 1'b1;
    drive(two, 8'h41);
    t = new(two);
    t.request();
    #1;

    // An event control: the handle is still read where the statement ran.
    held = one;
    held.data <= @(go) 8'h51;
    held = two;
    #1;
    ->go;
    #1;
  end

  final begin
    if (read_before_update !== 8'h01)
      $fatal(1, "read_before_update was %h, expected 01", read_before_update);
    if (first_update !== 8'h21)
      $fatal(1, "first_update was %h, expected 21", first_update);
    if (lane_before_update !== 8'h12)
      $fatal(1, "lane_before_update was %h, expected 12", lane_before_update);
    if (low_after_update !== 8'h0a)
      $fatal(1, "low_after_update was %h, expected 0a", low_after_update);
    if (one.data !== 8'h51) $fatal(1, "one.data was %h, expected 51", one.data);
    if (two.data !== 8'h41) $fatal(1, "two.data was %h, expected 41", two.data);
    if (two.lanes[1] !== 8'h31)
      $fatal(1, "two.lanes[1] was %h, expected 31", two.lanes[1]);
    if (one.req !== 1'b1) $fatal(1, "one.req was %b, expected 1", one.req);
    if (two.req !== 1'b1) $fatal(1, "two.req was %b, expected 1", two.req);
    $display("All checks passed");
  end
endmodule
