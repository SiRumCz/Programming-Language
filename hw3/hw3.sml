(*  
	CSC330 Fall2018 Assignment 3
	Zhe(Kevin) Chen 
*)
(* Assign 03 Provided Code *)

(*  Version 1.0 *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

(* Description of g:
	Problem 9 a)
		function g takes in two functions f1 and f2 and one pattern(datatype)
		and f1 and f2 gives function g the choice to do staff on either Wildcard or Variable.
		if the pattern is the TupleP then it will compute on every pattern in the TupleP
		and output the total count.
*)

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end


(**** put all your code after this line ****)
(* 1  filter f l *)
fun only_capitals ls = 
	List.filter (fn str => Char.isUpper(String.sub(str, 0))) ls
	
(* 2  foldl f init [x1, x2, ..., xn] *)
(* f(xn,...,f(x2, f(x1, init))...) *)
fun longest_string1 ls =
	List.foldl (fn (x,y) => 
					if String.size(x) > String.size(y) (* if same length keep the earlier one *)
					then 
						x 
					else 
						y
				) "" ls

(* 3 *)	
fun longest_string2 ls =
	List.foldl (fn (x,y) => 
					if String.size(x) < String.size(y) (* if same length keep the later one *)
					then 
						y 
					else 
						x
				) "" ls
	
(* 4 *)
fun longest_string_helper f ls =
	List.foldl f "" ls

val longest_string3 = 
	longest_string_helper (fn (x,y) => 
								if String.size(x) > String.size(y) 
								then 
									x 
								else 
									y
							) 
							
val longest_string4 = 
	longest_string_helper (fn (x,y) => 
								if String.size(x) < String.size(y) 
								then 
									y 
								else 
								x
							) 
	
(* 5 *)
val longest_capitalized = 
	longest_string3 o only_capitals (* val-binding *)
	
(* 6 *)
(* http://sml-family.org/Basis/string.html for String references *)
val rev_string = String.implode o List.rev o String.explode
	
(* 7 *)
fun first_answer f ls = 
	case ls of 
		[] => raise NoAnswer (*  the first argument returns NONE for all list elements *)
		|hd::tlList => case f hd of 
							NONE => first_answer f tlList
							|SOME v => v (* first time so v be the result *)	

(* 8 *)
fun all_answers f ls = 
	let
		fun aux f = 
			List.foldl (fn (x, acc) => case f x of 
						SOME lst => acc@lst) (* fold every SOME lists into one list *)
	in
		case ls of 
			[] => SOME [] (* base case *)
			|_ => case List.exists (fn x => f x = NONE) ls of (* exists f l *)
					true => NONE (* there exists NONE result *)
					|false => SOME (aux f [] ls)
	end

(* 9 *)
(* b) count_wildcards *)
fun count_wildcards p = 
	g (fn x => 1) (fn y => 0) p
	
(* c) count_wild_and_variable_lengths *)	
fun count_wild_and_variable_lengths p = 
	g (fn x => 1) (fn y => String.size y) p
	
(* d) count_some_var: number of times the string appears as a variable in the pattern *)
fun count_some_var (str,p) = 
	g (fn x => 0) (fn y => case str = y of true => 1 |false => 0) p
	
(* 10 *)
fun check_pat p = 
	let
		fun aux1 p = (* get the list of all Variable *)
			case p of 
				Variable x => [x] (* only count for Variable *)
				|TupleP ps => List.foldl(fn (pt, acc) => aux1(pt)@acc) [] ps
				|ConstructorP(_, p) => aux1(p)
				|_ => [] (* do nothing *)

		fun aux2 ls =  (* check if the list is distinct *)
			case ls of 
				[] => true (* base case *)
				|hd::tlList => case List.exists (fn x => x = hd) tlList of 
									true => false
									|false => aux2 tlList
	in
		aux2(aux1(p))
	end

(* 11 *)
(* Listpair.zip http://sml-family.org/Basis/list-pair.html *)
fun match (v, p) = 
	case (v, p) of 
		(_, Wildcard) => SOME [] (* Wildcard matches everything and produces the empty list of bindings *)
		|(_, Variable s) => SOME [(s, v)] (* one-element list holding (s,v) *)
		|(Unit, UnitP) => SOME [] (* produces the empty list of bindings *)
		|(Const x, ConstP y) => if x = y then SOME [] else NONE
		|(Tuple vs, TupleP ps) => if List.length ps = List.length vs 
								  then all_answers match(ListPair.zip(vs, ps))
								  else NONE (* s all the lists from the nested pattern matches appended together *)
		|(Constructor(s2, v), ConstructorP(s1, p)) => if s1 = s2 
													  then match(v, p)
													  else NONE 
		|(_, _) => NONE (* Nothing else matches *)

(* 12 *)
fun first_match v ps = 
	SOME (first_answer (fn x => match(v, x)) ps) handle NoAnswer => NONE