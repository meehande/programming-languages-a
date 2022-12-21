(*
Homework for Week 2 of Programming Languages Course [here](https://www.coursera.org/learn/programming-languages/peer/bplyW/homework-1)

*)

(*
1. is_older - true if arg1 date  < arg2 date

- if the year is less, true
- elif year equal, compare months

2023, 1, 1 - 2022, 10, 1

2022, 1, 1 - 2022, 10, 1

2021, 1, 1 - 2022, 10, 1

if year > false
else

1 - <=
2 -

if 

y<y -> t
y==y&m<m -> t
y==y&m==m&d<d -> t
else false


y>y -> false
y==y&m>m -> false




 
*)
	

fun is_older(dt1: int*int*int, dt2: int*int*int) =
    if (#1 (dt1) < #1 (dt2)) orelse ((#1 (dt1) = #1 (dt2)) andalso (#2 (dt1) < #2 (dt2))) orelse (#1 dt1 = #1 dt2 andalso #2 dt1 = #2 dt2 andalso #3 dt1 <  #3 dt2)
    then true
    else false


(*
2. Number in month
	
*)
			 
	     
fun number_in_month(dts: (int*int*int) list, month: int) =
    if null dts
    then 0
    else 
	let
	    val count = 0
			    
	    fun is_in_month(dt: (int*int*int)) =
		if #2 dt = month
		then count + 1
		else count
			 
	in
	    is_in_month(hd dts) + number_in_month(tl dts, month)	
	end

	    (*
3. number_in_months
	    
	    *)


fun number_in_months(dts: (int*int*int) list, months: int list) =
    if null months
    then 0
    else
	number_in_month(dts, hd months) + number_in_months(dts, tl months)

(*4*)
fun dates_in_month(dts: (int*int*int) list, month: int) =
    if null dts
    then []
    else
	let
	    val dt = hd dts
	    val rest = dates_in_month(tl dts, month)
	in 
		    
	    if #2 dt = month
	    then dt::rest
	    else rest
	end



fun append(l1: (int*int*int) list, l2: (int*int*int) list) =
    if null l1
    then l2
    else
	hd (l1) :: append(tl l1, l2)
			 
    
(*5*)	    
fun dates_in_months(dts: (int*int*int) list, months: int list) =
    if null months
    then []
    else
	append(dates_in_month(dts, hd months), dates_in_months(dts, tl months))
	
	
(*6*)
fun get_nth(strs: string list, n: int) =
    let
	val count = 1
	fun move(strs: string list, i: int) =
	    if i = n
	    then hd strs
	    else
		move(tl strs, i+1)
		
    in
	move(strs, count)
	
    end


	(*7*)
fun date_to_string(dt: (int*int*int)) =
    let
	val months = ["January","February","March","April","May","June","July","August","September","October","November","December"]
	val month = get_nth(months, #2 dt)
	val strdate = month ^ " "  ^ Int.toString(#3 dt) ^ ", " ^ Int.toString(#1 dt)
    in
	strdate
    end

	(*8*)
fun number_before_reaching_sum(maxsum: int, positive_n: int list) =
    let
	fun sumwhile(values: int list, currsum: int, n: int) =
	    if currsum < maxsum
	    then
		sumwhile(tl values, currsum + hd (values), n+1)
	    else
		n - 1	
		    
    in
	sumwhile(positive_n, 0, 0)
    end

	(*9*)
fun what_month(day: int) =
    let
	val month_counts = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	number_before_reaching_sum(day, month_counts) + 1
    end

	(*10*)
fun month_range(day1: int, day2: int) =
    if day1 > day2
    then []
    else
	what_month(day1) :: month_range(day1+1, day2)
	

fun max_date(dts: (int*int*int) list, curr_max: (int*int*int)) =
    if null dts
    then curr_max
    else
	if is_older(curr_max, hd dts)
	then
	    max_date(tl dts, hd dts)
	else
	    max_date(tl dts, curr_max)

		    
fun oldest(dts: (int*int*int) list) =
    let	
	fun max_date(dts: (int*int*int) list, curr_max: (int*int*int))=
	    if null dts
	    then curr_max
	    else
		if is_older(curr_max, hd dts)
		then
		    max_date(tl dts, hd dts)
		else
		    max_date(tl dts, curr_max)

	
    in
	if null dts
	then NONE
	else
	    SOME(max_date(tl dts, hd dts))
    end
	

	       
	
