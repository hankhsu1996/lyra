module Top;
  string r_return;
  string r_output;
  string r_inout;

  function string greet(input string name);
    return {"Hello, ", name};
  endfunction

  task fill(output string s);
    s = "filled";
  endtask

  task append_bang(inout string s);
    s = {s, "!"};
  endtask

  initial begin
    r_return = greet("World");
    fill(r_output);
    r_inout = "hi";
    append_bang(r_inout);
  end
endmodule
