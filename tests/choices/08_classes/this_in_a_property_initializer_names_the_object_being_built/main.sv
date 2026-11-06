// `this` in a class property's initializer names the object being
// constructed, and reads it as the unqualified name would. LRM 8.11 allows
// `this` only within non-static methods, constraints and embedded
// covergroups, so the text is an error the standard requires; the front end
// accepts it all the same, and Lyra runs it as the front end analyzed it. LRM
// 8.7 initializes the base class first, then each property, and leaves a
// property's value before its initialization undefined. Lyra initializes a
// class's own properties in declaration order, each starting at its type's
// value, so a property declared earlier is read with its initializer applied,
// one declared later with its type's start value, and every property of the
// base with the base's construction complete. This case holds Lyra to that.
module Top;
  class Base;
    int from_base = 7;
  endclass

  class C extends Base;
    int earlier = 1;
    int reads_earlier = this.earlier;
    int reads_later = this.later;
    int later = 4;
    int reads_base = this.from_base;
  endclass

  initial begin
    static C c = new;
    if (c.reads_earlier !== 1) $fatal(1, "an earlier property read %0d, expected 1", c.reads_earlier);
    if (c.reads_later !== 0) $fatal(1, "a later property read %0d, expected 0", c.reads_later);
    if (c.reads_base !== 7) $fatal(1, "a base property read %0d, expected 7", c.reads_base);
    $display("All checks passed");
  end
endmodule
