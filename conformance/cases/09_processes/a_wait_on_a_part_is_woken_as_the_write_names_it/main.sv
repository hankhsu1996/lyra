// A wait on part of a variable is woken by a write that changes that part and
// by no other (LRM 9.4.2) -- a write of a member of a structure wakes an
// always_comb reading the whole structure, and a write of one member of a
// packed union a wait on another, the union standing alone or as a member of a
// structure, since its members are one vector's bits (LRM 7.3.1) -- and an
// always_comb leaves out of its list what it also writes (LRM 9.2.2.2.1), for
// every way a part is selected: an element of an unpacked array (LRM 7.4), a
// member of a structure (LRM 7.2), a member of an unpacked union (LRM 7.3), a
// character of a string (LRM 6.16) and an entry of an associative array,
// whatever its index type (LRM 7.8).
//
// Each always_comb of a part of its own reads the part and then gives it an
// input's value by a nonblocking assignment, which lands after the procedure
// has stopped, and counts its runs: a write of its own that it did not leave
// out would run it again. The one reading an entry of an associative array
// writes the entry before it reads it, and runs once on each change of its
// input.
module Top;
  typedef struct {
    int a;
    int b;
  } pair_t;
  typedef union {
    int a;
    int b;
  } overlay_t;
  typedef union packed {
    logic [31:0] a;
    logic [31:0] b;
  } shared_t;
  typedef struct {
    shared_t u;
    int c;
  } holder_t;

  int in;

  int arr[4];
  pair_t pair;
  overlay_t overlay;
  shared_t shared;
  holder_t holder;
  string text = "abcd";
  int keyed[logic signed [63:0]];
  int named[string];

  int element_woke, member_woke, union_woke, character_woke, keyed_woke;
  int named_woke, whole_woke, alias_woke, nested_alias_woke;
  pair_t pair_copy;
  always_comb begin
    pair_copy = pair;
    whole_woke++;
  end
  always @(shared.b) alias_woke++;
  always @(holder.u.b) nested_alias_woke++;
  always @(arr[2]) element_woke++;
  always @(pair.b) member_woke++;
  always @(overlay.a) union_woke++;
  always @(text[1]) character_woke++;
  always @(keyed[5]) keyed_woke++;
  always @(named["x"]) named_woke++;

  int own_arr[4];
  pair_t own_pair;
  overlay_t own_overlay;
  int own_keyed[logic signed [63:0]];
  int element_out, member_out, union_out, keyed_out;
  int element_runs, member_runs, union_runs, keyed_runs;
  always_comb begin
    element_out = own_arr[3];
    own_arr[3] <= in;
    element_runs++;
  end
  always_comb begin
    member_out = own_pair.a;
    own_pair.a <= in;
    member_runs++;
  end
  always_comb begin
    union_out = own_overlay.b;
    own_overlay.b <= in;
    union_runs++;
  end
  always_comb begin
    own_keyed[7] = in;
    keyed_out = own_keyed[7];
    keyed_runs++;
  end

  int element_before, member_before, union_before, keyed_before;
  int whole_before;

  initial begin
    #1;
    whole_before = whole_woke;
    arr[1] = 10;
    pair.a = 10;
    text[0] = "z";
    keyed[3] = 10;
    named["y"] = 10;
    #1;
    if (element_woke !== 0)
      $fatal(1, "a sibling element woke a wait on arr[2]");
    if (character_woke !== 0)
      $fatal(1, "another character woke a wait on text[1]");
    if (keyed_woke !== 0) $fatal(1, "another entry woke a wait on keyed[5]");
    if (named_woke !== 0)
      $fatal(1, "another entry woke a wait on named[\"x\"]");
    if (whole_woke !== whole_before + 1)
      $fatal(1, "a write of pair.a ran a procedure reading pair %0d times",
             whole_woke - whole_before);
    arr[2] = 11;
    pair.b = 11;
    overlay.a = 11;
    shared.a = 11;
    holder.u.a = 11;
    text[1] = "y";
    keyed[5] = 11;
    named["x"] = 11;
    #1;
    if (element_woke !== 1)
      $fatal(1, "a wait on arr[2] woke %0d times, expected 1", element_woke);
    if (member_woke !== 1)
      $fatal(1, "a wait on pair.b woke %0d times, expected 1", member_woke);
    if (union_woke !== 1)
      $fatal(1, "a wait on overlay.a woke %0d times, expected 1", union_woke);
    if (alias_woke !== 1)
      $fatal(1, "a write of shared.a woke a wait on shared.b %0d times",
             alias_woke);
    if (nested_alias_woke !== 1)
      $fatal(1, "a write of holder.u.a woke a wait on holder.u.b %0d times",
             nested_alias_woke);
    if (whole_woke !== whole_before + 2)
      $fatal(1, "a write of pair.b ran a procedure reading pair %0d times",
             whole_woke - whole_before - 1);
    if (character_woke !== 1)
      $fatal(1, "a wait on text[1] woke %0d times, expected 1",
             character_woke);
    if (keyed_woke !== 1)
      $fatal(1, "a wait on keyed[5] woke %0d times, expected 1", keyed_woke);
    if (named_woke !== 1)
      $fatal(1, "a wait on named[\"x\"] woke %0d times, expected 1",
             named_woke);

    element_before = element_runs;
    member_before = member_runs;
    union_before = union_runs;
    keyed_before = keyed_runs;
    in = 5;
    #1;
    if (element_runs !== element_before + 1)
      $fatal(1, "the element procedure ran %0d times on one change",
             element_runs - element_before);
    if (member_runs !== member_before + 1)
      $fatal(1, "the member procedure ran %0d times on one change",
             member_runs - member_before);
    if (union_runs !== union_before + 1)
      $fatal(1, "the union procedure ran %0d times on one change",
             union_runs - union_before);
    if (keyed_out !== 5 || keyed_runs !== keyed_before + 1)
      $fatal(1, "the entry procedure ran %0d times on one change",
             keyed_runs - keyed_before);
    $display("All checks passed");
    $finish;
  end
endmodule
