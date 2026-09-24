// A member of an enumeration is the whole value its declaration gives it. Over a
// 4-state base a member may hold x or z bits, and a base may be wider than any
// machine word, so every method that asks which member a value is -- name(),
// next(), prev(), and whether $cast may assign it -- compares all of the value,
// both its known bits and its unknown ones (LRM 6.19, 6.19.5, 6.24.2).
module Top;
  typedef enum logic [1:0] {A = 2'b00, B = 2'bx1, C = 2'b1z} unknown_t;
  typedef enum logic [99:0] {
    LOW = 100'h0,
    HIGH = 100'h40_0000_0000_0000_0000,
    BOTH = 100'h40_0000_0000_0000_0001
  } wide_t;
  typedef struct {unknown_t u; wide_t w;} pair_t;

  unknown_t u;
  wide_t w;
  pair_t p;
  logic [1:0] raw_unknown;
  logic [99:0] raw_wide;

  string unknown_name;
  string after_unknown_name;
  string before_unknown_name;
  logic [1:0] after_unknown;
  string high_name;
  string after_high_name;
  string before_high_name;
  string both_name;
  int cast_unknown = -1;
  int cast_wide = -1;
  int cast_near_wide = -1;
  string unknown_first;
  string wide_last;
  string printed;

  initial begin
    u = B;
    unknown_name = u.name();
    after_unknown_name = u.next().name();
    before_unknown_name = u.prev().name();
    after_unknown = u.next();

    w = HIGH;
    high_name = w.name();
    after_high_name = w.next().name();
    before_high_name = w.prev().name();
    w = BOTH;
    both_name = w.name();

    raw_unknown = 2'b1z;
    cast_unknown = $cast(u, raw_unknown);
    raw_wide = 100'h40_0000_0000_0000_0001;
    cast_wide = $cast(w, raw_wide);
    raw_wide = 100'h1;
    cast_near_wide = $cast(w, raw_wide);

    u = u.first();
    unknown_first = u.name();
    w = w.last();
    wide_last = w.name();

    p.u = B;
    p.w = HIGH;
    printed = $sformatf("%p", p);
  end

  final begin
    if (unknown_name != "B")
      $fatal(1, "unknown_name was '%s', expected B", unknown_name);
    if (after_unknown_name != "C")
      $fatal(1, "after_unknown_name was '%s', expected C", after_unknown_name);
    if (before_unknown_name != "A")
      $fatal(1, "before_unknown_name was '%s', expected A",
             before_unknown_name);
    if (after_unknown !== 2'b1z)
      $fatal(1, "after_unknown was %b, expected 1z", after_unknown);
    if (high_name != "HIGH")
      $fatal(1, "high_name was '%s', expected HIGH", high_name);
    if (after_high_name != "BOTH")
      $fatal(1, "after_high_name was '%s', expected BOTH", after_high_name);
    if (before_high_name != "LOW")
      $fatal(1, "before_high_name was '%s', expected LOW", before_high_name);
    if (both_name != "BOTH")
      $fatal(1, "both_name was '%s', expected BOTH", both_name);
    if (cast_unknown !== 1)
      $fatal(1, "cast_unknown was %0d, expected 1", cast_unknown);
    if (cast_wide !== 1)
      $fatal(1, "cast_wide was %0d, expected 1", cast_wide);
    if (cast_near_wide !== 0)
      $fatal(1, "cast_near_wide was %0d, expected 0", cast_near_wide);
    if (unknown_first != "A")
      $fatal(1, "unknown_first was '%s', expected A", unknown_first);
    if (wide_last != "BOTH")
      $fatal(1, "wide_last was '%s', expected BOTH", wide_last);
    if (printed != "'{u:B, w:HIGH}")
      $fatal(1, "printed was '%s', expected '{u:B, w:HIGH}", printed);
    $display("All checks passed");
  end
endmodule
