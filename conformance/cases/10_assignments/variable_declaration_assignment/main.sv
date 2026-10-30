// An assignment written as part of a variable declaration is a variable
// initialization rather than a continuous assignment: it places the value in
// the variable before any initial or always procedure starts, and it has no
// duration, so the variable holds that value until the next assignment to it
// (LRM 10.5). The initialized object may be an aggregate as readily as a
// scalar.
//
// The assignment is made in the statement that declares the variable, so the
// initializer may name the variable it initializes, and reads the value the
// variable already holds: a static one its default (LRM 6.8), an automatic one
// the default it takes on each entry to its scope (LRM 6.21, 13.3), a class
// property the default it takes as its object is made (LRM 8.7). A default is
// the type's, so a 4-state value reads x, a string the empty string, and a
// structure the member defaults it declares (LRM 6.16, 7.2.2), wherever the
// variable lives -- in a procedure, a function, a task, a class method, an
// object, or past its scope in a branch that outlives it. An enumeration's
// methods are called on such a variable as on any of its type.
module Top;
  typedef enum {RED, GREEN, BLUE} color_e;
  typedef struct {
    int a = 5;
    logic [3:0] b;
  } member_default_t;

  class Holder;
    member_default_t member = member;
    member_default_t members[2] = members;
  endclass

  class Colors #(type T = int);
    static function T last_of();
      T e = e.last();
      return e;
    endfunction
  endclass

  function automatic int counts_from_its_default();
    int count = count + 1;
    return count;
  endfunction

  function automatic logic [3:0] four_state_in_a_function();
    logic [3:0] v = v;
    return v;
  endfunction

  function automatic int member_default_in_a_function();
    member_default_t v = v;
    return v.a;
  endfunction

  task automatic member_default_in_a_task(output int a);
    member_default_t v = v;
    a = v.a;
  endtask

  int scalar = 7;
  logic [7:0] vector = 8'hA5;
  int elements [3] = '{1, 2, 3};

  int scalar_at_start;
  logic [7:0] vector_at_start;
  int element_sum;
  int scalar_after_write;
  logic [3:0] automatic_from_itself = 4'b0000;
  logic [3:0] static_from_itself = 4'b0000;
  logic [3:0] outliving_from_itself = 4'b0000;
  logic [3:0] function_four_state = 4'b0000;
  int procedure_member_default = -1;
  int function_member_default = -1;
  int task_member_default = -1;
  int function_count = -1;
  color_e method_on_itself = RED;
  color_e method_through_a_parameter = RED;
  string string_from_itself = "unset";
  int property_member_default = -1;
  int properties_member_default = -1;

  initial begin
    scalar_at_start = scalar;
    vector_at_start = vector;
    element_sum = elements[0] + elements[1] + elements[2];
    scalar = 20;
    scalar_after_write = scalar;
  end

  initial begin
    automatic logic [3:0] a = a;
    static logic [3:0] s = s;
    automatic member_default_t m = m;
    automatic logic [3:0] kept = kept;
    automatic color_e e = e.last();
    automatic string t = {t, "a"};
    automatic Holder holder = new;
    automatic_from_itself = a;
    static_from_itself = s;
    string_from_itself = t;
    property_member_default = holder.member.a;
    properties_member_default = holder.members[1].a;
    procedure_member_default = m.a;
    fork
      #1 outliving_from_itself = kept;
    join_none
    function_four_state = four_state_in_a_function();
    function_member_default = member_default_in_a_function();
    member_default_in_a_task(task_member_default);
    void'(counts_from_its_default());
    function_count = counts_from_its_default();
    method_on_itself = e;
    method_through_a_parameter = Colors#(color_e)::last_of();
  end

  final begin
    if (scalar_at_start !== 7)
      $fatal(1, "scalar_at_start was %0d, expected 7", scalar_at_start);
    if (vector_at_start !== 8'hA5)
      $fatal(1, "vector_at_start was %h, expected a5", vector_at_start);
    if (element_sum !== 6)
      $fatal(1, "element_sum was %0d, expected 6", element_sum);
    if (scalar_after_write !== 20)
      $fatal(1, "scalar_after_write was %0d, expected 20",
             scalar_after_write);
    if (scalar !== 20) $fatal(1, "scalar was %0d, expected 20", scalar);
    if (automatic_from_itself !== 4'bxxxx)
      $fatal(1, "automatic_from_itself was %b, expected xxxx",
             automatic_from_itself);
    if (static_from_itself !== 4'bxxxx)
      $fatal(1, "static_from_itself was %b, expected xxxx",
             static_from_itself);
    if (outliving_from_itself !== 4'bxxxx)
      $fatal(1, "outliving_from_itself was %b, expected xxxx",
             outliving_from_itself);
    if (function_four_state !== 4'bxxxx)
      $fatal(1, "function_four_state was %b, expected xxxx",
             function_four_state);
    if (procedure_member_default !== 5)
      $fatal(1, "procedure_member_default was %0d, expected 5",
             procedure_member_default);
    if (function_member_default !== 5)
      $fatal(1, "function_member_default was %0d, expected 5",
             function_member_default);
    if (task_member_default !== 5)
      $fatal(1, "task_member_default was %0d, expected 5",
             task_member_default);
    if (function_count !== 1)
      $fatal(1, "function_count was %0d, expected 1", function_count);
    if (method_on_itself !== BLUE)
      $fatal(1, "method_on_itself was %s, expected BLUE",
             method_on_itself.name());
    if (method_through_a_parameter !== BLUE)
      $fatal(1, "method_through_a_parameter was %s, expected BLUE",
             method_through_a_parameter.name());
    if (string_from_itself != "a")
      $fatal(1, "string_from_itself was '%s', expected 'a'",
             string_from_itself);
    if (property_member_default !== 5)
      $fatal(1, "property_member_default was %0d, expected 5",
             property_member_default);
    if (properties_member_default !== 5)
      $fatal(1, "properties_member_default was %0d, expected 5",
             properties_member_default);
    $display("All checks passed");
  end
endmodule
