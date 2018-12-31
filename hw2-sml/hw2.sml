(*  
	CSC330 Fall2018 Assignment 2
	Zhe(Kevin) Chen 
*)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid some warning regarding polymorphic comparison  *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for Part 1 here *)

(* 1 *)
fun all_except_option(str, ls) = 
	case ls of 
		[] => NONE (* base case *)
		|head::tail => case same_string(str, head) of 
					  true => SOME tail (* head == str *)
					  |false => case all_except_option(str, tail) of 
									NONE => NONE (* if NONE means not found, keep NONE *)
									|SOME li => SOME (head::li) 
									(* if prev result is some list, append the head to the list *)

(* 2 *)								
fun get_substitutions1(lls, s) = 
	case lls of 
		[] => [] (* base case *)
		|headlist::taillistlist => case all_except_option(s, headlist) of 
										NONE => get_substitutions1(taillistlist, s) (* matches nothing *)
										|SOME li => li@get_substitutions1(taillistlist, s) 
										(* do list append to the remain possible list *)
							
(* 3 *)
(* auxiliary function(local helper) with accumator *)
fun get_substitutions2(lls, s) = 
	let 
		fun aux(lls, str, acc) = 
			case lls of 
				[] => acc (* base case *)
				|headlist::taillistlist => case all_except_option(str, headlist) of 
										NONE => aux(taillistlist, str, acc)
										|SOME li => aux(taillistlist, str, acc@li) (* tail-recursive *)
										(* if use li@acc, the first will goes to last because first list will be the acc. *)
	in
		aux(lls, s, [])
	end

(* 4 *)
(* auxiliary function(local helper) with accumator to form the full name type of the list *)
fun similar_names(lls, fullname) = 
	let
		fun aux(ls, fname, acc) = 
			case ls of 
			[] => acc (* base case *)
			|head::taillist => case fname of (* type fullname = {firstname:string, last:string, middle:string} *)
									{first, middle, last} => 
									aux(taillist, fname, acc@[{first=head, last=last, middle=middle}]) 
									(* print format: {first, last, middle}*)
	in
		case fullname of 
			{first, middle, last} => aux(first::get_substitutions2(lls, first), fullname, [])
			(* get the list then call aux() to reformat *)
	end
	
(************************************************************************)
(* Game *)

(* you may assume that Num is always used with valid values 2, 3, ..., 10 *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw


exception IllegalMove

(* put your solutions for Part 2 here *)

(* 5 *)
fun card_color(cd) = 
	case cd of 
		(Clubs, _) => Black
		|(Diamonds, _) => Red
		|(Hearts, _) => Red
		|(Spades, _) => Black
		
(* 6 *)
fun card_value(cd) = 
	case cd of 
		(_, Num n) => n
		|(_, Ace) => 11
		|_ => 10

(* 7 *)
(* takes a list of cards cs, a card c, and an exception e *)
fun remove_card(cs, c, e) = 
	case cs of 
		[] => raise e (* base case *)
		|head::taillist => case (c = head) of
							true => taillist
							|false => head::remove_card(taillist, c, e)

(* 8 *)
fun all_same_color(cdlist) = 
	case cdlist of 
		[] => true (* empty list return true *)
		|_::[] => true (* list with only one card return true *)
		|a::(b::taillist) => case card_color(a)=card_color(b) of 
								true => all_same_color(b::taillist)
								|false => false

(* 9 *)
fun sum_cards(cdlist) = 
	let
		fun aux(clist, acc) = 
			case clist of 
				[] => acc (* base case *)
				|a::taillist => aux(taillist, card_value(a)+acc)
	in 
		aux(cdlist, 0)
	end		

(* 10 *)
fun score(cdlist, goal) = 
	let
		val sum = sum_cards(cdlist); (* sum of held-cards *)
		(* preliminary score based on rules *)
		val perlim_score = 
			case sum > goal of 
				true => 2 * (sum - goal)
				|false => goal - sum
	in	
		(* final score *)
		case all_same_color(cdlist) of 
			true => perlim_score div 2
			|false => perlim_score
	end

(* 11 *)
fun officiate(cdlist, mvlist, goal) = 
	let 
		fun aux(cdlist, mvlist, goal, heldcards) = 
			case mvlist of 
				[] => score(heldcards, goal) (* base case where movelist is empty *)
				|currmv::restmoves => 
					case cdlist of 
						[] => score(heldcards, goal) (* card list is empty, game over *)
						|currcd::restcards => 
							case currmv of (* player makes a move *)
								Discard cd => aux(cdlist, restmoves, goal, remove_card(heldcards, cd, IllegalMove))
								|Draw => case (card_value(currcd)+sum_cards(heldcards) > goal) of 
												true => score(currcd::heldcards, goal) (* drawing causes sum to exceed the goal, game over *)
												|false => aux(restcards, restmoves, goal, currcd::heldcards) (* continue *)
	in 
		aux(cdlist, mvlist, goal, []) (* The game starts with the held-cards being the empty list *)
	end

(************************************************************************)
(* Section 3 Tests *)

(*1*)val test1_0=all_except_option("tom",["jerry","bob","kevin", "tom"]) = SOME ["jerry","bob","kevin"];
(*2*)val test2_0=get_substitutions1([["Liam","Noah"],["Noah","James","Elijah"],["Benjamin","Noah","Mason"],["Henry"]],
									"Noah")
				= ["Liam","James","Elijah","Benjamin","Mason"];
(*3*)val test3_0=get_substitutions1([["Liam","Noah"],["Noah","James","Elijah"],["Benjamin","Noah","Mason"],["Henry"]],
									"Noah")
				= ["Liam","James","Elijah","Benjamin","Mason"];
(*4*)val test4_0=similar_names([["Tina","Kevin"],
                             ["Kevin","James","Elijah"],
                             ["Benjamin","Kevin","Mason"],
							 ["Henry"]], 
							 {first="Kevin", middle="Z", last="Chen"}) =
            [{first="Kevin",last="Chen",middle="Z"},
			 {first="Tina",last="Chen",middle="Z"},
             {first="James",last="Chen",middle="Z"},
             {first="Elijah",last="Chen",middle="Z"},
             {first="Benjamin",last="Chen",middle="Z"},
             {first="Mason",last="Chen",middle="Z"}];
(*5*)val test5_0= card_color((Diamonds,Jack)) = Red;
(*6*)val test6_0= card_value(Clubs, Queen) = 10;
exception notFound
(*7*)val cards0 = [(Clubs, Queen), (Clubs, Jack), (Clubs, Ace), (Clubs, Num 7), (Clubs, Num 8)];
	 val test7_0 = remove_card(cards0, (Clubs, Ace), notFound) = [(Clubs, Queen), (Clubs, Jack), (Clubs, Num 7), (Clubs, Num 8)];
(*8*)val test8_0 = all_same_color(cards0) = true;
(*9*)val test9_0 = sum_cards(cards0) = 46;
(*10*)val test10_0 = score(cards0, 47) = 1 div 2;
(*11*)val test11_0 = officiate(cards0, [Draw, Draw, Draw, Discard (Spades, Num 5)], 18) = 2 handle IllegalMove => true;

































