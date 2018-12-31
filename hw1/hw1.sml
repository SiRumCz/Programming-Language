(*  
	CSC330 Fall2018 Assignment 1
	Zhe(Kevin) Chen  
*)

(* #1 year, #2 month, #3 day *)
type DATE = (int * int * int)
exception InvalidParameter

(* 1 *)
fun is_older(d1: DATE, d2: DATE): bool =
	if (#1 d1 = #1 d2)
	then 
		if (#2 d1 = #2 d2)
		then 
			if (#3 d1 = #3 d2)
			then 
				false (* if it gets here means same date *)
			else
				#3 d1 < #3 d2
		else
			#2 d1 < #2 d2
	else
		#1 d1 < #1 d2

(* 2 *)		
fun number_in_month(dateList: DATE list, target: int): int = 
	if null dateList
	then 
		0 (* base case, empty DATE list *)
	else
		if (#2 (hd dateList) = target)
		then 
			1+number_in_month(tl dateList, target)
		else
			number_in_month(tl dateList, target)
			
(* 3 *)			
fun number_in_months(dateList: DATE list, monthList: int list): int = 
	if null monthList orelse null dateList
	then 
		0 (* base case, empty month list *)
	else
		number_in_month(dateList, hd monthList)+number_in_months(dateList, tl monthList)
		
(* 4 *)
fun dates_in_month(dateList: DATE list, target: int): DATE list =
	if null dateList
	then 
		[] (* base case, empty date list return nil *)
	else
		if (#2 (hd dateList) = target)
		then
			(hd dateList)::dates_in_month(tl dateList, target)
		else
			dates_in_month(tl dateList, target)

(* 5 *)			
fun dates_in_months(dateList: DATE list, monthList: int list): DATE list = 
	if null monthList orelse null dateList
	then
		[] (* base case, empty month list return nil *)
	else
		dates_in_month(dateList, hd monthList)
		@dates_in_months(dateList, tl monthList) (* ML's list-append operator: @ *)

(* 6 *)
fun get_nth(stringList: string list, pos: int): string = 
	if null stringList orelse 0 = pos
	then
		raise InvalidParameter (* base case, when empty or pos 0 *)
	else
		if (1 = pos)
		then
			hd stringList (* hits *)
		else
			get_nth(tl stringList, pos-1)
			
(* 7 *)
val monthNameList = ["January", "February", "March", "April", "May", "June", "July", 
"August", "September", "October", "November", "December"];
(* 
	operator ˆ for concatenating strings.
	library function Int.toString for converting an int to a string.
*)
fun date_to_string(date: DATE): string = 
	get_nth(monthNameList, #2 date)^" "^Int.toString(#3 date)^", "^Int.toString(#1 date) (* format: "month day, year" *)

(* 8 *)
fun number_before_reaching_sum(sum: int, ls: int list): int = 
	let
		val num = hd ls;
	in
		if null ls orelse 0 > sum
		then
			0 (* base case, when sum is less than 0 or list is empty *)
		else
			if (1 > sum-num) (* base case, when reach or exceed the sum, counts 0 *)
			then
				0
			else
				1+number_before_reaching_sum(sum-num, tl ls)
	end

(* 9 *)
val daysInMonthList = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] (* according to http://www.howmanydaysin.com/2018/ *)
fun what_month(day: int): int = 
	1+number_before_reaching_sum(day, daysInMonthList) (* nbrs()+1 *)
	
(* 10 *)	
fun month_range(day1: int, day2: int): int list = 
	if (day1 > day2)
	then
		[] (* when day1 is bigger than day2 *)
	else
		if (day1 = day2)
		then 
			what_month(day1)::[]
		else
			what_month(day1)::month_range(day1+1, day2)
	
(* 11 *)
fun oldest(dateList: DATE list): DATE option = 
	if null dateList
	then
		NONE (* base case, empty list *)
	else
		let
			val d1 = hd dateList;
			val tlOldest = oldest(tl dateList); (* get oldest date in rest of the list *)
		in
			if isSome tlOldest andalso is_older(valOf tlOldest, d1)
			then 
				tlOldest
			else
				SOME d1
		end

(* 12 *)
fun  reasonable_date(date: DATE): bool = 			
	let
		val daysInLeapYearList = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
		val year = #1 date;
		val month = #2 date;
		val day = #3 date;
		fun get_month_day(ls: int list, pos: int): int = 
		(* already checked if date is vaild *)
			if (1 = pos)
				then
					hd ls (* hits *)
				else
					get_month_day(tl ls, pos-1)
	in 
		if (1> year) orelse (1 > month) orelse (12 < month) orelse (1 > day) (* check valid date *)
		then 
			false
		else
			if (0 = year mod 400) orelse ((0 = year mod 4) andalso (0 <> year mod 100)) (* check leap year with 366 days *)
			then
				day < 1+get_month_day(daysInLeapYearList, month) (* returns true if within the day *)
			else
				day < 1+get_month_day(daysInMonthList, month)
	end	







