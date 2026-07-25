module Top;
  typedef struct {
    int x;
    int y;
  } pair_t;

  typedef union tagged {
    void   Zero;
    int    Simple;
    pair_t Pair;
  } val_t;

  typedef union tagged {
    int A;
    int B;
    int C;
  } trio_t;

  val_t  v_zero;
  val_t  v_simple;
  val_t  v_pair;
  trio_t t_a;
  trio_t t_b;
  trio_t t_c;

  int    case_zero;
  int    case_simple;
  int    case_pair;
  int    case_a;
  int    case_b;
  int    case_c;
  int    if_matched;
  int    if_missed;
  int    cond_matched;
  int    cond_missed;
  int    multi_clause;
  int    multi_clause_filter_fails;
  int    ternary_matched;
  int    ternary_missed;
  val_t  bound_whole;
  int    case_whole;
  val_t  cont_src;
  int    cont_matched;
  int    cont_filtered;

  // LRM 12.6.3: the same clause chain as a continuous assign's predicate. The
  // filter clause reads what the pattern to its left bound.
  assign cont_matched = cont_src matches tagged Simple .s ? s : -1;
  assign cont_filtered = cont_src matches tagged Simple .s &&& (s > 40) ? s : 0;

  initial begin
    v_zero   = tagged Zero;
    v_simple = tagged Simple 42;
    v_pair   = tagged Pair '{100, 200};
    cont_src = tagged Simple 42;

    case (v_zero) matches
      tagged Zero              : case_zero = -1;
      tagged Simple .n         : case_zero = n;
      tagged Pair '{.a, .b}    : case_zero = a + b;
    endcase

    case (v_simple) matches
      tagged Zero              : case_simple = -1;
      tagged Simple .n         : case_simple = n;
      tagged Pair '{.a, .b}    : case_simple = a + b;
    endcase

    case (v_pair) matches
      tagged Zero              : case_pair = -1;
      tagged Simple .n         : case_pair = n;
      tagged Pair '{.a, .b}    : case_pair = a + b;
    endcase

    t_a = tagged A 10;
    t_b = tagged B 20;
    t_c = tagged C 30;

    case (t_a) matches
      tagged A .x : case_a = x;
      tagged B .x : case_a = x * 2;
      tagged C .x : case_a = x * 3;
    endcase

    case (t_b) matches
      tagged A .x : case_b = x;
      tagged B .x : case_b = x * 2;
      tagged C .x : case_b = x * 3;
    endcase

    case (t_c) matches
      tagged A .x : case_c = x;
      tagged B .x : case_c = x * 2;
      tagged C .x : case_c = x * 3;
    endcase

    if (v_simple matches tagged Simple .n)
      if_matched = n;
    else
      if_matched = 0;

    if (v_zero matches tagged Simple .n)
      if_missed = n;
    else
      if_missed = 999;

    if (v_simple matches tagged Simple .m)
      cond_matched = m;
    else
      cond_matched = 0;
    if (v_zero matches tagged Simple .m)
      cond_missed = m;
    else
      cond_missed = 555;

    if (v_pair matches tagged Pair '{.a, .b} &&& (a < b))
      multi_clause = a + b;
    else
      multi_clause = 0;

    // The pattern matches but the filter clause fails, so the chain fails
    // below its outermost level -- the else arm still has to run.
    if (v_pair matches tagged Pair '{.a, .b} &&& (a > b))
      multi_clause_filter_fails = a + b;
    else
      multi_clause_filter_fails = 321;

    // LRM 12.6.3: the same clause chain as the predicate of a conditional
    // expression.
    ternary_matched = v_simple matches tagged Simple .q ? q : 0;
    ternary_missed = v_zero matches tagged Simple .q ? q : 888;

    // LRM 12.6 grammar: a bare `.identifier` is a pattern at any position,
    // including a case item's top level, where it always matches and binds
    // the whole subject.
    case (v_simple) matches
      .whole : bound_whole = whole;
    endcase
    if (bound_whole matches tagged Simple .w)
      case_whole = w;
    else
      case_whole = 0;
  end
endmodule
