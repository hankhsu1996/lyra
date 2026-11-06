// Case equality and case inequality on a virtual interface answer as equality
// and inequality do. Table 11-1 gives === and !== any operand but a real,
// while LRM 25.9 lists only == and != among the comparisons a virtual
// interface takes, so the standard does not settle whether the text is
// allowed; LRM 6.14, 8.4 and 15.5.5.3 give case equality to every other kind
// of handle, with the same semantics as == and !=. The front end admits the
// form and evaluates it as ==, and Lyra keeps that reading. A virtual
// interface holds an instance or null and never an x or a z, so the two
// operators agree on every value. This case holds Lyra to that.
interface I;
endinterface

module Top;
  I first ();
  I second ();
  virtual I v;
  virtual I w;

  initial begin
    if (!(v === null)) $fatal(1, "an unset virtual interface is not === null");
    if (v !== w) $fatal(1, "two unset virtual interfaces differ under !==");
    v = first;
    if (!(v === first)) $fatal(1, "a virtual interface is not === the instance it holds");
    if (v === second) $fatal(1, "a virtual interface is === an instance it does not hold");
    if (!(v !== null)) $fatal(1, "a set virtual interface is not !== null");
    w = second;
    if (v === w) $fatal(1, "virtual interfaces holding different instances are ===");
    w = first;
    if (v !== w) $fatal(1, "virtual interfaces holding one instance differ under !==");
    $display("All checks passed");
  end
endmodule
