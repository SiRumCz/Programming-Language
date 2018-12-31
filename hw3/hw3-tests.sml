use "hw3.sml";

fun tests_passed(t: string, tests: bool list) =
  let 
    fun toInt (b: bool) =
      if b then 1 else 0;
    val len = length tests
    val count = foldl op+ 0 (map toInt tests)
                      
    fun add_index(xs) =
      let
        fun add(i, xs) =
          case xs of
              [] => []
            | head::tail => (i,head) :: add(i+1, tail)
      in
        add(1,xs)
      end
          
    fun report_failed(tests_with_i) =
      let
        fun report_each(xs) =
          case xs of
              [] => []
           |  (i,x)::tail =>
              (if not x
               then print ("      Subtest  " ^
                             Int.toString(i) ^ " failed \n")
               else ();
               report_each(tail))
              (* (print "\n      Test "^ Int.toString(i) ^ " failed \n"; true) *)
      in
        print("\n**Test " ^ t ^ " failed!!!!!\n   "^ Int.toString(len -count) ^ " out of " ^ Int.toString(len) ^ " failed\n\n");
        List.length (report_each(tests_with_i)) = 0
      end
        
  in
    if (count = len)
    then 
      (print ("\n**Test " ^ t ^ " passed\n   "^ Int.toString(count) ^ " out of " ^ Int.toString(len) ^ "\n\n");
       count = len)
    else
      (report_failed(add_index(tests)) before print ("\n\n"))
  end;


val words = ["This","the","A","Hello","World","not"];

val test_only_capitals = 
    ("1. test_only_capitals",[
      only_capitals(words) = ["This","A","Hello","World"],
      only_capitals ["A","B","C"] =  ["A","B","C"],
      only_capitals [] =  [],
      only_capitals(["Mexico","Ottawa"]) = ["Mexico", "Ottawa"],
      only_capitals(["vancouver","Ottawa","Victoria"]) = ["Ottawa", "Victoria"]
    ]);

val t1 = tests_passed test_only_capitals;

val lwords = ["This","the","A","Hello","World","not","long string","loooong string"]
val swords = ["the","not","cat","dog"] (* check first/last etc *)
                 
val test_longest_string1 =
    ("2. test_longest_string1",[
      longest_string1(lwords) = "loooong string",
      longest_string1(swords) = "the",
      longest_string1 ["A","bc","C"] = "bc",
      longest_string1 ["A","bc","C", "de", "x"] = "bc",
      longest_string1(["Mexico","Ottawa"]) = "Mexico",
      longest_string1(["vancouver","Ottawa","Victoria"]) = "vancouver",
      longest_string1([]) = ""
    ]);

val t2 = tests_passed test_longest_string1;

val test_longest_string2 =
    ("3. test_longest_string2",[
      longest_string2(lwords) = "loooong string",
      longest_string2([]) = "",
      longest_string2 ["A","bc","C", "de"] = "de",
      longest_string2 ["A","bc","C", "de", "x"] = "de",
      longest_string2(["Mexico","Ottawa"]) = "Ottawa",
      longest_string2(swords) = "dog"
    ]);

val t3 = tests_passed test_longest_string2;

val test_longest_string3 =
    ("4-parta. test_longest_string3",[
          longest_string3(lwords) = "loooong string",
          longest_string3(swords) = "the",
          longest_string3 ["A","bc","C"] = "bc",
          longest_string3 ["A","bc","C", "de", "x"] = "bc",
          longest_string3(["Mexico","Ottawa"]) = "Mexico",
          longest_string3(["vancouver","Ottawa","Victoria"]) = "vancouver",
          longest_string3([]) = ""
        ]);

val t4a = tests_passed test_longest_string3


val test_longest_string4a =
    ("4-partb. test longest_string4",[
      longest_string4(lwords) = "loooong string",
      longest_string4([]) = "",
      longest_string4 ["A","bc","C", "de"] = "de",
      longest_string4 ["A","bc","C", "de", "x"] = "de",
      longest_string4(["Mexico","Ottawa"]) = "Ottawa",
      longest_string4(swords) = "dog"
    ]);

val t4b = tests_passed test_longest_string4a;

val t4 = t4a andalso t4b;

val test_longest_cap =
  let
    val words = ["This","the","A","Hello","World","not","long string","loooong string"]
  in
    ("5. test longest_capitalized",[
      longest_capitalized words = "Hello",
      longest_capitalized([]) = "",
      longest_capitalized(["vancouver","Ottawa","Victoria","Guatemala"]) = "Guatemala",
      longest_capitalized ["then","The","To","testing"] = "The",
      longest_capitalized ["then","The","To","Cat","testing"] = "The",
      longest_capitalized ["then","the","to","testing"] = ""
    ])
  end;

val t5 = tests_passed test_longest_cap;

val test_rev_string =
    ("6. test rev_string", [
      rev_string "Hello World!" = "!dlroW olleH",
      rev_string "canada" = "adanac",
      rev_string "anna" = "anna",
      rev_string "" = ""
    ]);
        
val t6 = tests_passed test_rev_string;

val test_first_answer =
    ("7. test first_answer", [
      first_answer (fn x => if (x mod 2) = 0 then SOME x else NONE) [1,1,4,3] = 4,
      first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4,
      first_answer (fn x => SOME x) [1,2,3,4,5] = 1,
      first_answer (fn x => NONE) [1,2,3,4,5] = 1 handle NoAnswer  => true,
      first_answer (fn x => if (x mod 2) = 0 then SOME x else NONE) [1,1,5,3] = 10 handle NoAnswer => true,
      first_answer (fn x => if String.size(x) = 3 then SOME x else NONE) ["this", "is", "the", "end", "of", "the", "world"] = "the"
    ]);
        
val t7 = tests_passed test_first_answer;

(*
val test_first_answer_exc =
    test("first_answer_exception",[
          (first_answer(fn x => if (x mod 2) = 0 then SOME x else NONE) [1,1,5,3]; false)
           handle NoAnswer => true = true,
          (first_answer (fn x => if x > 3 then SOME x else NONE) [];false)
           handle NoAnswer => true = true
        ])
*)
val test_all_answers =
    ("8. all_answers",
     [all_answers (fn x => if (x < 0) then NONE else SOME ([1,2,3])) [1,2,3] = SOME [1,2,3,1,2,3,1,2,3],
      all_answers (fn x => if (x < 0) then NONE else SOME ([1])) [1,2,3] = SOME [1,1,1],
      all_answers (fn x => if (x < 0) then NONE else SOME ([x])) [1,2,3] = SOME [1,2,3],
      all_answers (fn x  => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE,
      all_answers (fn x => if x = 1 then SOME [x] else NONE) [1,1,1,1,1] = SOME [1,1,1,1,1],
      all_answers (fn x => if x = 1 then SOME [x] else NONE) [1,2,1,2,1,2] = NONE,
      all_answers (fn x => if (x < 0) then NONE else SOME ([1])) [] = SOME []]
    );

val t8 = tests_passed test_all_answers;

val test_count_wildcards  =
    ("9.b count_wildcards",
    [count_wildcards(Wildcard) = 1,
     count_wildcards(UnitP) = 0,
     count_wildcards(ConstP 1) = 0,
     count_wildcards(TupleP []) =  0,
     count_wildcards(TupleP [Wildcard]) = 1,
     count_wildcards(TupleP [Wildcard, UnitP, Variable "x"]) = 1,
     count_wildcards(TupleP [Wildcard, Wildcard, Variable "xy"]) = 2,
     count_wildcards(TupleP [TupleP [Wildcard, Wildcard]]) = 2,
     count_wildcards(TupleP [ConstructorP ("a", Wildcard), TupleP [Wildcard]]) = 2]);

val t9b = tests_passed test_count_wildcards;

val test_count_wildcards_and_var_len  =
    ("9.c test_count_wildcards_and_variable_lengths",
    [count_wild_and_variable_lengths(Wildcard) = 1,
     count_wild_and_variable_lengths(UnitP) = 0,
     count_wild_and_variable_lengths(ConstP 1) = 0,
     count_wild_and_variable_lengths(TupleP []) =  0,
     count_wild_and_variable_lengths(TupleP [Wildcard]) = 1,
     count_wild_and_variable_lengths(TupleP [Wildcard, UnitP, Variable "abc"]) = 4,
     count_wild_and_variable_lengths(TupleP [Wildcard, Wildcard, Variable "xy"]) = 4,
     count_wild_and_variable_lengths(TupleP [TupleP [Wildcard, Wildcard, Variable "This is the sea"]]) = 17,
     count_wild_and_variable_lengths(TupleP [ConstructorP ("a", Wildcard), TupleP [Wildcard]]) = 2]);

val t9c = tests_passed test_count_wildcards_and_var_len;


val test_count_some_var =
  ("9.d count_wild_and_variable_lengths",
   [count_some_var("test",
                   TupleP [TupleP [Wildcard, UnitP, Variable "test",Variable "test"]]) = 2,
    count_some_var("test",TupleP [TupleP [Wildcard, UnitP,
                                          Variable "te"]]) = 0,
    count_some_var ("test",
                    TupleP [TupleP [Wildcard, UnitP, Variable "test"]]) = 1
  ]);

val t9d = tests_passed test_count_some_var;

val test_check_pat =
    ("10. check_pat", [
      check_pat (TupleP [Wildcard,Variable "cat",
                         Variable "pp",TupleP[Variable "tt"],
                         Wildcard,ConstP 3,
                         ConstructorP("cony",Variable "pp")]) = false,
      check_pat (TupleP [Variable "cat",
                         ConstructorP("cat",Wildcard)]) = true,
      check_pat (TupleP [Wildcard,Variable "cat",
                         Variable "pp",TupleP[Variable "tt"],
                         Wildcard,ConstP 3,
                         ConstructorP("tt",Variable "pq")]) = true,
      check_pat (TupleP [Wildcard,Variable "cat",
                         Variable "pp",TupleP[Variable "tt"],
                         Wildcard,ConstP 3,
                         ConstructorP("cony",Variable "test")]) = true
    ]);

val t10 = tests_passed test_check_pat;

val test_match =
    ("test_match",
         [match(Unit, UnitP) = SOME [],
          match(Unit, Variable "cat") = SOME [("cat", Unit)],
          match(Unit, ConstP 3) = NONE,
          match(Tuple [], TupleP [Wildcard]) = NONE,
          match(Tuple [Unit], TupleP []) = NONE,
          match(Tuple [Unit], Variable "cat") = SOME [("cat", Tuple [Unit])],
          match(Tuple [Unit], TupleP [Variable "cat"]) = SOME [("cat", Unit)],
          match(Tuple [Unit, Const 8], TupleP [Variable "cat", ConstP 3]) = NONE,
          match(Tuple [Unit, Const 8], TupleP [Variable "cat", Variable "dog"]) = SOME [("cat", Unit),("dog", Const 8)],
          match(Tuple [Unit, Tuple [Unit, Unit]],
                        TupleP [Variable "cat", TupleP [Variable "dog", Variable "rat"]]) = SOME [("cat", Unit), ("dog", Unit),  ("rat", Unit)],
          match(Constructor ("mat", Unit), ConstructorP ("hat", Variable "cat")) = NONE,
          match(Constructor ("dog", Unit), ConstructorP ("dog", Variable "cat")) = SOME [("cat", Unit)],
          match(Tuple[Const 17, Unit, Const 7,
                              Constructor ("zoe", Const 7),
                              Constructor ("zoe", (Constructor ("zoe", Const 7)))],
                        TupleP[Wildcard,Wildcard]) = NONE,
          match(Const 7, Wildcard ) = SOME [],
          match(Unit, Wildcard ) = SOME [],
          match(Tuple[Const 7], Wildcard ) = SOME [],
          match(Constructor("cat", Const 7), Wildcard ) = SOME [],
          match(Const 7, Variable "Zoe" ) = SOME [("Zoe", Const 7)],
          match(Unit, Variable "chopsticks" ) = SOME [("chopsticks", Unit)],
          match(Unit, UnitP ) = SOME [],
          match(Const 7, UnitP ) = NONE,
          match(Const 7, ConstP 7 ) = SOME [],
          match(Const 7, ConstP 8 ) = NONE,
          match(Constructor("Cat", Const 7), ConstructorP("Cat", Wildcard)) =  SOME[],
          match(Constructor("Dog", Const 7), ConstructorP("Cat", Wildcard)) =  NONE,
          match(Constructor("Cat", Const 7), ConstructorP("Cat", UnitP)) =  NONE,
          match(Constructor("Cat", Const 7), ConstructorP("Cat", Variable "dog"))  =  SOME [("dog", Const 7)],
          match(Tuple[Const 7], TupleP[ConstP 7]) =  SOME [],
          match(Tuple[Const 7], TupleP[ConstP 7,ConstP 7]) =  NONE,
          match(Tuple[Const 7, Const 6, Unit, Const 8],
                        TupleP[ConstP 7, Variable "cat",Wildcard, ConstP 8]) = SOME [("cat",Const 6)],
          match(Tuple[Const 7, Const 6, Unit, Const 7],
                        TupleP[
                          Variable "a",
                          Variable "ab",
                          Variable "abc",
                          Variable "abcd"]) = SOME [("a",Const 7), ("ab",Const 6), ("abc",Unit), ("abcd",Const 7)
          ]
    ]);

val t11 = tests_passed test_match;

val test_first_match =
    ("test_first_match",
         [
           first_match Unit [UnitP] = SOME [],
           first_match Unit [Variable "cat"] = SOME [("cat", Unit)],
           first_match Unit [ConstP 3] = NONE,
           first_match (Tuple []) [TupleP [Wildcard]] = NONE,
           first_match (Tuple [Unit]) [TupleP []] = NONE,
           first_match (Tuple [Unit]) [TupleP [Variable "cat"]] = SOME [("cat", Unit)],
           first_match (Tuple [Unit, Const 8]) [TupleP [Variable "cat", Variable "dog"]] = SOME [("cat", Unit),("dog", Const 8)],
           first_match (Constructor ("mat", Unit)) [ConstructorP ("hat",Variable "cat")] = NONE,
           first_match (Constructor ("dog", Unit)) [ConstructorP("dog",Variable "cat")] = SOME [("cat", Unit)],
           first_match (Const 7) [Wildcard ] = SOME [],
           first_match Unit [Wildcard ] = SOME [],
           first_match (Tuple[Const 7]) [Wildcard ] = SOME [],
           first_match Unit [Variable "chopsticks" ] = SOME [("chopsticks", Unit)],
           first_match Unit [UnitP ] = SOME [],
           first_match (Const 7) [UnitP ] = NONE,
           first_match (Const 7) [ConstP 7 ] = SOME [],
           first_match (Const 7) [ConstP 8 ] = NONE,
           first_match (Constructor("Cat", Const 7)) [ConstructorP("Cat",Variable "dog")]  =  SOME [("dog",Const 7)],
           first_match (Tuple[Const 7]) [TupleP[ConstP 7]] =  SOME [],
           first_match (Tuple[Const 7]) [TupleP[ConstP 7,ConstP 7]] =  NONE,
           first_match (Tuple[Const 7, Const 6, Unit, Const 8])
                               [TupleP[ConstP 7, Variable "cat",Wildcard, ConstP 8]] = SOME [("cat",Const 6)],
           first_match (Tuple[Const 7, Const 6, Unit, Const 7])
                               [TupleP[Variable "a", Variable "ab",
                                       Variable "abc", Variable "abcd"]] = SOME [
              ("a",Const 7),
              ("ab",Const 6),
              ("abc",Unit),
              ("abcd",Const 7)
           ]
    ]);

val t12 = tests_passed test_first_match;

val total = [t1, t2, t3, t4a, t4b, t5, t6, t7, t8, t9b, t9c, t9d, t10, t11, t12];

val totalPassed = tests_passed("**Overall tests: ", total);
