// An identifier is a sequence of letters, digits, dollar signs and
// underscores, or an escaped identifier carrying any printable character other
// than white space, and nothing in the language reserves a spelling to the
// implementation (LRM 5.6, 5.6.1). So a declaration keeps its own meaning
// whatever word it is spelled with, including words another language reserves,
// and it keeps it at every position a name can be declared at: a design
// element, a variable, a port, a signal a view offers, an instance, a class and
// its members, a subroutine and its arguments and locals. A name reached from
// another compilation unit (LRM 26.3) is the same declaration as the one that
// unit declared, so both ends denote one object.
package pkg;
  int delete;
  int \a+b ;

  function automatic int typename(int explicit);
    int noexcept;
    noexcept = explicit + 1;
    return noexcept;
  endfunction

  function automatic int \f+g (int x);
    return x + 2;
  endfunction

  class template;
    int public;

    function new();
      public = 7;
    endfunction

    function int private();
      return public;
    endfunction
  endclass
endpackage

interface Iface;
  logic [7:0] throw;
  modport catch(input throw);
endinterface

module inline(input logic [7:0] register, output logic [7:0] goto);
  assign goto = register + 8'd1;
endmodule

module Top;
  int operator;
  int delete;
  int bitand;
  int xor_eq;
  logic [7:0] register;
  logic [7:0] goto;

  int from_a_package_variable;
  int from_an_escaped_package_variable;
  int from_a_package_subroutine;
  int from_an_escaped_package_subroutine;
  int from_a_class_member;
  int through_a_view;
  int through_a_port;
  int from_a_named_block;

  Iface namespace ();
  inline friend (.register(register), .goto(goto));

  initial begin : using
    pkg::template decltype;
    int mutable;

    mutable = 8;
    operator = 1;
    delete = 2;
    bitand = 3;
    xor_eq = 4;
    register = 8'd40;

    pkg::delete = 5;
    pkg::\a+b  = 6;

    decltype = new();

    from_a_package_variable = pkg::delete;
    from_an_escaped_package_variable = pkg::\a+b ;
    from_a_package_subroutine = pkg::typename(10);
    from_an_escaped_package_subroutine = pkg::\f+g (10);
    from_a_class_member = decltype.private();

    namespace.throw = 8'd9;
  end

  final begin
    // Each value is distinct, so two declarations that collapsed into one
    // would answer with the other's value rather than with nothing.
    if (operator !== 1) $fatal(1, "operator was %0d, expected 1", operator);
    if (delete !== 2) $fatal(1, "delete was %0d, expected 2", delete);
    if (bitand !== 3) $fatal(1, "bitand was %0d, expected 3", bitand);
    if (xor_eq !== 4) $fatal(1, "xor_eq was %0d, expected 4", xor_eq);
    if (from_a_package_variable !== 5)
      $fatal(1, "from_a_package_variable was %0d, expected 5",
             from_a_package_variable);
    if (from_an_escaped_package_variable !== 6)
      $fatal(1, "from_an_escaped_package_variable was %0d, expected 6",
             from_an_escaped_package_variable);
    if (from_a_package_subroutine !== 11)
      $fatal(1, "from_a_package_subroutine was %0d, expected 11",
             from_a_package_subroutine);
    if (from_an_escaped_package_subroutine !== 12)
      $fatal(1, "from_an_escaped_package_subroutine was %0d, expected 12",
             from_an_escaped_package_subroutine);
    if (from_a_class_member !== 7)
      $fatal(1, "from_a_class_member was %0d, expected 7", from_a_class_member);

    // A port carries the value across the instance, and a signal a view offers
    // is the interface's own, so both are read back where they were written.
    through_a_port = goto;
    through_a_view = namespace.throw;
    from_a_named_block = using.mutable;

    if (through_a_port !== 41)
      $fatal(1, "through_a_port was %0d, expected 41", through_a_port);
    if (through_a_view !== 9)
      $fatal(1, "through_a_view was %0d, expected 9", through_a_view);
    if (from_a_named_block !== 8)
      $fatal(1, "from_a_named_block was %0d, expected 8", from_a_named_block);
    $display("All checks passed");
  end
endmodule
