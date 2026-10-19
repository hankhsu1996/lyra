// An enumeration of N members, asked its last member's name and its next.
// @each-adds: bytes
module Top #(parameter int N = 4);
  typedef enum { [[m{i}|, ]] } e_t;

  e_t e;
  string name;

  initial begin
    e = m{last};
    name = e.name();
    e = e.next();
  end
endmodule
