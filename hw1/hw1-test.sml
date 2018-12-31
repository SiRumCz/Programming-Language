(* Assignment 1 Simple Test *)

(* These are basic test cases.  *)

(* loads the bindings from the file with your solutions *)

(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw1.sml";

(* Some bindings to avoid repetition *)

val feb28_2012 = (2012, 2, 28);
val feb12_2011 = (2011, 2, 12);
val dec1_2013 = (2013,  12,  1);
val march31_2011 = (2011,  3,  31);
val april28_2011 = ( 2011,  4,  28);
val june1_2013 = (2013,  6,  1);

val test1 = is_older(( 1,  2,  3),( 2,  3,  4)) = true;
val test1a = is_older(feb28_2012,dec1_2013) = true;
val test1b = is_older(dec1_2013, feb28_2012) = false;
val test1c = is_older(dec1_2013, dec1_2013) = false;

val test2 = number_in_month([feb28_2012,dec1_2013],2) = 1;
val test2a = number_in_month([feb28_2012,dec1_2013],3) = 0;
val test2b = number_in_month([feb28_2012,dec1_2013,march31_2011,april28_2011],3) = 1;
val test2b = number_in_month([feb28_2012,dec1_2013,feb12_2011,march31_2011,april28_2011],2) = 2;

val test3a = number_in_months([feb28_2012,dec1_2013,march31_2011,april28_2011],[2,3,4]) = 3;

val test4  = dates_in_month([feb28_2012,dec1_2013],2) = [feb28_2012];
val test4a = dates_in_month([feb28_2012,dec1_2013],12) = [dec1_2013];
val test4b = dates_in_month([feb28_2012,dec1_2013],3) = [];
val test4c = dates_in_month([feb28_2012,feb12_2011,dec1_2013],2) = [feb28_2012,feb12_2011];

val test5a = dates_in_months([feb28_2012,dec1_2013,march31_2011,april28_2011],[2,3,4]) = [feb28_2012,march31_2011,april28_2011];
                                                                                             
val test5d = dates_in_months([feb28_2012,dec1_2013,march31_2011,april28_2011],[5,7]) = [];

val test6 = get_nth(["hi", "there", "how", "are", "you"], 2) = "there";
val test6a = get_nth(["hi", "there", "how", "are", "you"], 7) = "never" handle InvalidParameter => true;
val test6b = get_nth(["hi", "there", "how", "are", "you"], 0) = "never" handle InvalidParameter => true;
val test6c = get_nth([], 0) = "never" handle InvalidParameter => true;

val test7 = date_to_string(june1_2013) = "June 1, 2013";

val test7a = date_to_string(april28_2011) = "April 28, 2011";

val test8 = number_before_reaching_sum(10, [1,2,3,4,5]) = 3
val test8a = number_before_reaching_sum(10, [11,1,2,3,4,5]) = 0
val test8b = number_before_reaching_sum(12, [11,1,2,3,4,5]) = 1
val test8c = number_before_reaching_sum(1, [1,2,3,4,5]) = 0;
val test8d = number_before_reaching_sum(6, [1,2,3,4,5]) = 2;

val test9  = what_month(70) = 3;
val test9a = what_month(31) = 1;
val test9b = what_month(32) = 2;
val test9c = what_month(360) = 12;
val test91 = what_month(70) = 3;
val test92 = what_month(30) = 1;
val test93 = what_month(1) = 1;
val test94 = what_month(32) = 2;
val test95 = what_month(365) = 12;
val test96 = what_month(364) = 12;

val test10 = month_range(31, 34) = [1,2,2,2];
val test10a = month_range(360, 365) = [12,12,12,12,12,12];
val test10b = month_range(31,31 + 28 +1) = [1,
                                           2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                           3];
val test10c = month_range(35, 34) = [];
val test10d = month_range(35, 35) = [2];
val test10e = month_range(31+29, 31+29) = [3];

val test11 = oldest([feb28_2012,march31_2011,april28_2011]) = SOME march31_2011;
val test11a = oldest([april28_2011]) = SOME april28_2011;
val test11b = oldest([]) = NONE;

val test12 = reasonable_date( 2014,  12,  31);
val test12a = not (reasonable_date ( 2015,  2,  29));

val test12b =      reasonable_date( 2012,  2,  29);
val test12c = not (reasonable_date( 2014,  0,  31));
val test12d = not (reasonable_date( 2014,  13,  31));
val test12e = not (reasonable_date( 2013,  2,  29));
val test12e =     (reasonable_date( 2013,  3,  31));
val test12f = not (reasonable_date( 0,  1,  10));
val test12g = not (reasonable_date(~10,  1,  10));
val test12i =     reasonable_date (2014, 10, 14);
val test12j = not ( reasonable_date( 2000,  1,  32));
val test12k = not (reasonable_date( 2000,  2,  31));

val test12l =     (reasonable_date( 2000,  1,  31));
val test12m = not (reasonable_date( 2000,  2,  31));
val test12n =     reasonable_date( 2000,  3,  31);
val test12o = not (reasonable_date( 2000,  4,  31));
val test12p =     (reasonable_date( 2000,  5,  31));
val test12r = not (reasonable_date( 2000,  6,  31));
val test12s =     reasonable_date( 2000,  7,  31);
val test12t =     reasonable_date( 2000,  8,  31);
val test12u = not (reasonable_date( 2000,  9,  31));
val test12v =     reasonable_date( 2000, 10,  31);
val test12x = not (reasonable_date( 2000, 11,  31));
val test12y =     reasonable_date( 2000, 12,  31);

val test13j = not (reasonable_date( 2000,  1,  32));
val test13m = not (reasonable_date( 2000,  2,  32));
val test13n = not (reasonable_date( 2000,  3,  32));
val test13o = not (reasonable_date( 2000,  4,  32));
val test13p = not (reasonable_date( 2000,  5,  32));
val test13r = not (reasonable_date( 2000,  6,  32));
val test13s = not (reasonable_date( 2000,  7,  32));
val test13t = not (reasonable_date( 2000,  8,  32));
val test13u = not (reasonable_date( 2000,  9,  32));
val test13v = not (reasonable_date( 2000, 10,  32));
val test13x = not (reasonable_date( 2000, 11,  32));
val test13y = not (reasonable_date( 2000, 12,  32));

val test14a = not (reasonable_date(2000, 1, 0));
val test14b = not (reasonable_date(2000, ~1, 1));
val test14c = not (reasonable_date(~2000, 1, 1));
val test14d = not (reasonable_date(2000, 1, ~1));


val test15a = not (reasonable_date(1900, 2, 29));
val test15b =     reasonable_date(2000, 2, 29);
val test15c = not (reasonable_date(2001, 2, 29));
val test15d =     reasonable_date(2008, 2, 29);

val tests1 = [test1, test1a, test1b, test1c];
val tests2 = [test2, test2a,  test2b,  test2b];
val tests3 = [test3a];
val tests4 = [test4, test4a,  test4b,  test4c] ;
val tests5 = [test5a,  test5d ];
val tests6 = [test6, test6a,  test6b,  test6c ];
val tests7 = [ test7,  test7a];
val tests8 = [test8, test8a,  test8b,  test8c,  test8d];
val tests9 = [test9, test9a,  test9b,  test9c,  test91,  test92,  test93,  test94,  test95,  test96 ];
val tests10 = [test10, test10a,  test10b, test10c,  test10d,  test10e];
val tests11 = [test11, test11a,  test11b ];

val tests12 = [test12, test12a,  test12b, test12c,  test12d,  test12e,  test12e,  test12f,  test12g,  test12i,  test12j,  test12k,  test12l,  test12m,  test12n,  test12o,  test12p,  test12r,  test12s,  test12t,  test12u,  test12v,  test12x,  test12y];

val tests13 = [test13j, test13m,  test13n,  test13o,  test13p,  test13r,  test13s,  test13t,  test13u,  test13v,  test13x,  test13y ];
val tests14 = [test14a, test14b, test14c,  test14d ];
val tests15 = [test15a, test15b, test15c,  test15d ];

fun toInt (b: bool) =
  if b then 1 else 0;

fun sum(a:int, b:int) =
  a + b;

fun tests_passed(t: string, tests: bool list) =
  let 
    val len = length tests
    val count = foldl sum 0 (map toInt tests)
  in
    print ("\n**Test " ^ t ^ " "^ (if count = len then "passed" else "failed")
           ^ ":  "^ Int.toString(count) ^ " out of " ^ Int.toString(len) ^ "\n");
    count = len
  end;

val total = tests_passed("**Overal",[
  tests_passed("1", tests1),
  tests_passed("2", tests2),
  tests_passed("3", tests3),
  tests_passed("4", tests4),
  tests_passed("5", tests5),
  tests_passed("6", tests6),
  tests_passed("7", tests7),
  tests_passed("8", tests8),
  tests_passed("9", tests9),
  tests_passed("10", tests10),
  tests_passed("11", tests11),
  tests_passed("12", tests12),
  tests_passed("13", tests13),
  tests_passed("14", tests14),
  tests_passed("15", tests15)]);


