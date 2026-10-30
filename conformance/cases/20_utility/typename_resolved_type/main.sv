// $typename returns a string standing for the resolved type of its argument.
// A typedef of an equivalent type resolves back to the type it names, default
// signing is dropped, a "$" stands in for the name of an anonymous unpacked
// array, and an expression operand reports its self-determined type without
// being evaluated. A user-defined name is prefixed by the scope that declares
// it and an enumeration carries its members' encoded values (LRM 20.6.1). How
// a value is spelled the standard shows only by example, so each is read as
// some text after its member's `=`.
// An enumeration written in place as a member's type is given a name the
// system generates, whose number is the system's to choose, and the name of a
// type is a constant.
module Top;
  typedef bit node;
  typedef enum bit [1:0] {A, B, C = 3} step;
  typedef struct packed {
    enum logic [1:0] {IDLE, BUSY} mode;
    logic valid;
  } status;

  localparam string kNameOfStatus = $typename(status);

  node unpacked_vector[2:0];
  int elements[];
  int signed explicitly_signed;
  string text;
  step state;
  status flags;
  int i;

  int probe_calls;

  string name_of_unpacked;
  string name_of_dynamic;
  string name_of_signed;
  string name_of_string;
  string name_of_enum;
  string name_of_status;
  string unvalued_status;
  string before_number;
  string after_number;
  string name_of_expression;
  string name_of_call;

  // The name with each encoded value, the text from an `=` to the next `,` or
  // `}`, replaced by V where it is there at all.
  function automatic string Unvalued(string name);
    string result = "";
    int i = 0;
    while (i < name.len()) begin
      result = {result, name.substr(i, i)};
      if (name[i] == "=") begin
        int start = i + 1;
        i = start;
        while (i < name.len() && name[i] != "," && name[i] != "}") i++;
        if (i > start) result = {result, "V"};
      end else begin
        i++;
      end
    end
    return result;
  endfunction

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
    name_of_status = $typename(flags);
    before_number = "struct packed{enum{IDLE=V,BUSY=V}Top.e$";
    after_number = " mode;logic valid;}Top.status";
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
    if (Unvalued(name_of_enum) != "enum{A=V,B=V,C=V}Top.step")
      $fatal(1, "the enumeration was '%s'", name_of_enum);
    unvalued_status = Unvalued(name_of_status);
    if (unvalued_status.len() <= before_number.len() + after_number.len()
        || unvalued_status.substr(0, before_number.len() - 1) != before_number
        || unvalued_status.substr(unvalued_status.len() - after_number.len(),
                                  unvalued_status.len() - 1) != after_number)
      $fatal(1, "the structure holding an enumeration was '%s'",
             name_of_status);
    if (kNameOfStatus != name_of_status)
      $fatal(1, "the constant name was '%s', the name where it runs '%s'",
             kNameOfStatus, name_of_status);
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
