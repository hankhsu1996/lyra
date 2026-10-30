// A parallel statement of a fork-join block names the variables of the scope
// enclosing the fork, not copies of them, and no process the fork spawns
// starts executing until the parent blocks or terminates (LRM 9.3.2). What a
// branch reads is therefore the value the parent left behind after running on
// past the fork, and what a branch writes is what the parent reads later.
// Neither depends on the lifetime of the enclosing declaration (LRM 6.21), nor
// on the variable's type: a structure, a string and a 4-state vector the
// declaration left at its default (LRM Table 6-7) are shared the same way.
module Top;
  typedef struct {
    int count;
    string tag;
  } record_t;

  int shared_static = 1;
  int branch_saw_static;
  int branch_saw_automatic;
  bit branch_saw_unknown;
  int parent_saw;
  int parent_saw_time;
  record_t parent_saw_record;
  string parent_saw_text;

  initial begin
    automatic int enclosing_automatic = 7;
    automatic record_t enclosing_record = '{1, "a"};
    automatic string enclosing_text = "x";
    automatic logic [3:0] enclosing_undriven;
    fork
      begin
        branch_saw_static = shared_static;
        branch_saw_automatic = enclosing_automatic;
        branch_saw_unknown = $isunknown(enclosing_undriven);
        #10 shared_static = 42;
        enclosing_record.count += enclosing_automatic;
        enclosing_record.tag = {enclosing_record.tag, "b"};
        enclosing_text = {enclosing_text, "y"};
      end
    join_none
    shared_static = 2;
    enclosing_automatic = 99;
    enclosing_record.count = 3;
    #20;
    parent_saw = shared_static;
    parent_saw_time = $time;
    parent_saw_record = enclosing_record;
    parent_saw_text = enclosing_text;
  end

  final begin
    if (branch_saw_static !== 2)
      $fatal(1, "branch_saw_static was %0d, expected 2", branch_saw_static);
    if (branch_saw_automatic !== 99)
      $fatal(1, "branch_saw_automatic was %0d, expected 99",
             branch_saw_automatic);
    if (branch_saw_unknown !== 1'b1)
      $fatal(1, "an automatic left at its default was not all x");
    if (parent_saw !== 42)
      $fatal(1, "parent_saw was %0d, expected 42", parent_saw);
    if (parent_saw_time !== 20)
      $fatal(1, "parent_saw_time was %0d, expected 20", parent_saw_time);
    if (parent_saw_record != record_t'{102, "ab"})
      $fatal(1, "parent_saw_record was '{%0d, %s}, expected '{102, ab}",
             parent_saw_record.count, parent_saw_record.tag);
    if (parent_saw_text != "xy")
      $fatal(1, "parent_saw_text was %s, expected xy", parent_saw_text);
    $display("All checks passed");
  end
endmodule
