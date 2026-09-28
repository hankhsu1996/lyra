// Once a virtual interface holds an instance, every component of that instance
// is available through it (LRM 25.9): a variable, a name a modport defines by
// an expression -- storage it designates, or a value it computes (LRM 25.5.4)
// -- and an interface the instance itself instantiates, whose own components
// and subroutines are reached through it and which is itself a value a virtual
// interface of its type can hold. The instance may be one named directly, one
// reached through an interface port, or one element of an array of instances.
// A virtual interface may be the result of a function and a member of a
// structure, and one whose parameters no instance has is a legal declaration
// that can only ever hold null.
interface Inner;
  logic [7:0] x;
  function automatic void Bump();
    x = x + 8'd1;
  endfunction
endinterface

interface Bus #(
    parameter int W = 8
);
  logic [W-1:0] data;
  logic [3:0]   flags;
  Inner         sub ();
  modport view(output .low(data[3:0]), input .plus(data + 8'd1), input flags);
endinterface

module User (
    Bus b
);
  virtual Bus from_port;
  initial begin
    from_port = b;
    from_port.data = 8'h21;
  end
endmodule

typedef struct {
  virtual Bus bus;
  int         id;
} Binding;

module Top;
  Bus one ();
  Bus many [2] ();
  User u (one);

  virtual Bus held;
  virtual Bus.view viewed;
  virtual Bus #(.W(16)) never_built;
  virtual Inner inner;
  Binding binding;

  logic [7:0] seen_plus = 8'h00;
  logic [3:0] seen_flags = 4'h0;
  bit never_built_is_null = 1'b0;

  function automatic virtual Bus Pick(int i);
    if (i == 0) return many[0];
    return many[1];
  endfunction

  initial begin
    #1;
    held = many[1];
    held.data = 8'h31;

    // A name the view defines over storage is written through the handle, and
    // one it computes is read through it.
    viewed = one;
    viewed.low = 4'h5;
    one.flags = 4'h9;
    seen_plus = viewed.plus;
    seen_flags = viewed.flags;

    held.sub.x = 8'h41;
    held = Pick(0);
    held.data = 8'h51;
    inner = held.sub;
    inner.x = 8'h71;
    held.sub.Bump();

    binding.bus = one;
    binding.id = 7;
    binding.bus.sub.x = 8'h61;

    never_built_is_null = (never_built == null);
  end

  final begin
    // Written 21 through the port's handle, then its low nibble 5 through the
    // view.
    if (one.data !== 8'h25) $fatal(1, "one.data was %h, expected 25", one.data);
    if (seen_plus !== 8'h26) $fatal(1, "seen_plus was %h, expected 26", seen_plus);
    if (seen_flags !== 4'h9) $fatal(1, "seen_flags was %h, expected 9", seen_flags);
    if (many[1].data !== 8'h31) $fatal(1, "many[1].data was %h, expected 31", many[1].data);
    if (many[1].sub.x !== 8'h41) $fatal(1, "many[1].sub.x was %h, expected 41", many[1].sub.x);
    if (many[0].data !== 8'h51) $fatal(1, "many[0].data was %h, expected 51", many[0].data);
    // Written 71 through the nested instance held as a value, then bumped by
    // its own function called through the outer handle.
    if (many[0].sub.x !== 8'h72) $fatal(1, "many[0].sub.x was %h, expected 72", many[0].sub.x);
    if (one.sub.x !== 8'h61) $fatal(1, "one.sub.x was %h, expected 61", one.sub.x);
    if (never_built_is_null !== 1'b1) $fatal(1, "a virtual interface never assigned was not null");
    $display("All checks passed");
  end
endmodule
