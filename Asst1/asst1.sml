
(* Takes two dates, date1 and date2. Returns true if date1 > date2, 
   otherwise false *) 
fun is_older( date1 : (int * int * int), date2 : (int * int * int) ) =
    (* if year1 > year2 or
          month1 > month2 or
          dayOfMonth1 > dayOfMonth2
       then date1 > date2  *)
    (#1 date1) > (#1 date2)
    orelse (#2 date1) > (#2 date2)
    orelse (#3 date1) > (#3 date2)   

(* Takes a list of dates, and returns the number of dates
   that match the given month *)
fun number_in_month 
( 
   dates : (int * int * int) list
 , hasMonth : int 
) =
    let
	fun inMonth ( date : (int * int * int) ) =
            if (#2 date) = hasMonth then 1 else 0
    in
        if null dates
        then 0
        else inMonth( (hd dates) ) + number_in_month( tl dates, hasMonth)
    end

(* Takes a list of dates and list of months, returning the number
   of dates that have a month that match any value in the list of
   months *)
fun number_in_months 
( 
   dates  : (int * int * int) list
 , months : (int)             list 
) =
    let
	fun inMonth ( month : int ) =
             number_in_month(dates, month) 
    in
        if null months
        then 0
	else inMonth( (hd months) ) + number_in_months( dates, tl months)
    end

(* Takes a list of dates and a month, returning the list of dates
   that have a matching month. 
   Order is preserved 
  *)
   
fun dates_in_month
( 
   dates    : (int * int * int) list
 , hasMonth : int 
) =
    if null dates
    then []
    else
	let val tailDates = dates_in_month(tl dates, hasMonth)
        in
            if  (#2 (hd dates)) = hasMonth
	    then hd dates::tailDates
	    else tailDates
	end

(* Takes a lits of dates and a list of months, returning the list of
   dates that have any date with a month in the months list.
   Order preservation is not defined.
 *)
fun dates_in_months
( 
   dates  : (int * int * int) list
 , months : (int)             list 
) =
    if null months
    then []
    else
	let val datesInMonth     = dates_in_month(dates, hd months)
            val tailDatesInMonth = dates_in_months(dates, tl months) 	
        in
	    let fun appendList ( l1 : (int * int * int) list, l2 : (int * int * int) list ) =
		    if null l1
		    then l2
                    else hd l1 :: appendList(tl l1, l2)
            in
		if null datesInMonth
		then tailDatesInMonth
		else appendList (datesInMonth,tailDatesInMonth)
	    end
        end 

(* Returns the nth string in a list of strings.
   Does not consider a string list of a size smaller than n.
  
   Uses a 1-starting-index -based list, where {string << index} is:
     {String1 << 1, String2 << 2, ..., StringN << N} 
 *)

fun get_nth(
  values : string list
 ,n      : int
) =
    if null values
    then hd [] (* As per assignment description, 
                  apply hd or tl to the empty list in this case, which is okay *)
    else if n = 1
    then hd values
    else get_nth(tl values, n-1)

(* List of months, in order, used for converting a month index
   into a string value. *)
val Months = 
    [ "January"
     ,"February"
     ,"March"
     ,"April"
     ,"May"
     ,"June"
     ,"July"
     ,"August"
     ,"September"
     ,"October"
     ,"November"
     ,"December"
    ];

(* Converts a date into a string, of format "MMM dd, YYYY".

   eg: (2013,1,20) ==> January 20, 2013

 *)
fun date_to_string( date : (int * int * int) ) =
    get_nth(Months,(#2 date))^" "^(Int.toString (#3 date))^", "^(Int.toString (#1 date))
    
(* Returns an int, n, such that the first n elements of the list l
   add to less than sum, but the first n+1 elements of the list, l, add to sum
   or more. 

   sum -- Assumed to be >= 0
   l   -- Each value assumed to be >= 0
       -- Sum(l) > parameter sum

 *)
fun number_before_reaching_sum( sum : int, l : int list ) =    
    let fun headSum( hlist : int list, hsum : int, index : int) =
	    if null hlist
	    then hd [] (* unsupported case *)
            else
		let val activeSum = hsum + hd hlist
                    val nextSum   = activeSum + (hd (tl hlist))
                in
                     (* check running sum + head < sum
                        and   running sum + head + head+1 >= sum
                        if so... return the index, n *)
		    if ( (activeSum < sum) andalso (nextSum >= sum))
		    then index
                      (* otherwise continue to the next head
                         and increment the index *)
		    else headSum(tl hlist, activeSum, index + 1)
                end
    in
        (* prime headSum with
           full list:l
           starting some of all head values:0
           starting index (n):1
         *)
	headSum(l, 0, 1)
    end

val DaysOfYear = 
    [ 0  (* all dates between 1 and 31 are in january, the following dates follow each month*)
     ,31 (* jan *)
     ,28 (* feb *)
     ,31 (* mar *)
     ,30 (* apr *)
     ,31 (* may *)
     ,30 (* jun *)
     ,31 (* jul *)
     ,31 (* aug *)
     ,30 (* sep *)
     ,31 (* oct *)
     ,30 (* nov *)
     ,31 (* dec *)]
   

(* Rreturns what month a given day of the year is (1 for January, 2 for February, etc.) *)
fun what_month ( dayOfYear : int ) = 
    get_nth(Months, number_before_reaching_sum(dayOfYear, DaysOfYear))


(* Take two days of the year, day1 and day 2, returning an int list 
   of the months between each date 

   Result length is day2 - day1 + 1
   When day1 > day2, returns 0
*)
fun month_range ( day1 : int, day2 : int ) =
    if day1 > day2
    then []
    else number_before_reaching_sum(day1, DaysOfYear)
         :: month_range(day1 + 1, day2)
