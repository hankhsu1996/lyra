// @rejected:
//
// A member of an unpacked structure and an element of an unpacked array may
// be passed by reference, and a member of an unpacked union may not (LRM
// 13.5.2). The clause does not say which governs a structure's member or an
// array's element that lies in a union's member: a union is one storage its
// members are alternative readings of, with no required representation (LRM
// 7.3), so what lies in a member has no storage of its own while another
// member may be what the union holds. Lyra takes the stricter reading, in the
// front end it pins, and refuses a part below a union's member as the member.
module Top;
  typedef struct { int x; int y; } s_t;
  typedef union { s_t s; int words [2]; } u_t;
  u_t v;

  task automatic set(ref int r);
    r = 3;
  endtask

  initial begin
    set(v.s.x);
    $display("the program the standard rejects ran");
  end
endmodule
