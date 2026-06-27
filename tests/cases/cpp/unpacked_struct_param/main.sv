module Top;
  typedef struct {
    int a;
    byte b;
  } pair_t;

  typedef struct {
    pair_t inner;
    int    tag;
  } nest_t;

  // A struct-typed parameter folds its constant value into a struct literal
  // (LRM 6.20). Reading a member substitutes the folded component.
  parameter pair_t P = '{a: 5, b: 8'd6};
  parameter nest_t Q = '{inner: '{a: 7, b: 8'd8}, tag: 9};

  int p_a;
  int p_b;
  int q_ia;
  int q_ib;
  int q_tag;

  initial begin
    p_a = P.a;
    p_b = P.b;
    q_ia = Q.inner.a;
    q_ib = Q.inner.b;
    q_tag = Q.tag;
  end
endmodule
