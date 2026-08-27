// $typename returns a string standing for the resolved type of its argument.
// A typedef of an equivalent type resolves back to the type it names, default
// signing is dropped, a "$" stands in for the name of an anonymous unpacked
// array, and an expression operand reports its self-determined type without
// being evaluated. A user-defined name is prefixed by the scope that declares
// it and an enumeration carries its members' encoded values (LRM 20.6.1).
module Top;
  typedef bit node;
  typedef enum bit [1:0] {A, B, C = 3} step;

  node unpacked_vector[2:0];
  int elements[];
  int signed explicitly_signed;
  string text;
  step state;
  int i;

  int probe_calls;

  string name_of_unpacked;
  string name_of_dynamic;
  string name_of_signed;
  string name_of_string;
  string name_of_enum;
  string name_of_expression;
  string name_of_call;

  function automatic int probe();
    probe_calls = probe_calls + 1;
    return 0;
  endfunction

  initial begin
    probe_calls = 0;
    name_of_unpacked = $typename(unpacked_vector);
    name_of_dynamic = $typename(elements);
    name_of_signed = $typename(explicitly_signed);
    name_of_string = $typename(text);
    name_of_enum = $typename(state);
    name_of_expression = $typename(i + i);
    name_of_call = $typename(probe());
  end

  final begin
    if (name_of_unpacked != "bit$[2:0]")
      $fatal(1, "the anonymous unpacked array was '%s', expected 'bit$[2:0]'",
             name_of_unpacked);
    if (name_of_dynamic != "int$[]")
      $fatal(1, "the dynamic array was '%s', expected 'int$[]'",
             name_of_dynamic);
    if (name_of_signed != "int")
      $fatal(1, "an explicitly signed int was '%s', expected 'int'",
             name_of_signed);
    if (name_of_string != "string")
      $fatal(1, "a string was '%s', expected 'string'", name_of_string);
    if (name_of_enum != "enum{A=2'd0,B=2'd1,C=2'd3}Top.step")
      $fatal(1, "the enumeration was '%s'", name_of_enum);
    if (name_of_expression != "int")
      $fatal(1, "the sum of two ints was '%s', expected 'int'",
             name_of_expression);
    if (name_of_call != "int")
      $fatal(1, "an int function call was '%s', expected 'int'",
             name_of_call);
    if (probe_calls !== 0)
      $fatal(1, "$typename evaluated its operand %0d times, expected none",
             probe_calls);
    $display("All checks passed");
  end
endmodule
