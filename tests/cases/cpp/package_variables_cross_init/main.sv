// A package variable initializer that reads another package's variable (LRM
// 26.2 / 10.5): the design root must initialize the read package first so the
// dependent package's initializer observes an initialized cell. The dependency
// is a cross-unit reference the referring package records, and the root orders
// its initializer calls by it, regardless of the order the packages are declared
// or lowered in.
package pb;
  int b = 10;
endpackage

package pa;
  int a = pb::b + 1;
endpackage

module Top;
  int got;
  initial got = pa::a;
endmodule
