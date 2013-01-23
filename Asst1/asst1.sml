
(* takes two dates, date1 and date2. Returns true if date1 > date2, 
   otherwise false *) 
fun is_older( date1 : (int * int * int), date2 : (int * int * int) ) =
    (* if year1 > year2 or
          month1 > month2 or
          dayOfMonth1 > dayOfMonth2
       then date1 > date2  *)
    (#1 date1) > (#1 date2)
    orelse (#2 date1) > (#2 date2)
    orelse (#3 date1) > (#3 date2)   

(* takes a list of dates, and returns the number of dates
   that match the given month *)
fun number_in_month ( dates : (int * int * int) list, hasMonth : int ) =
    let
	fun inMonth ( date : (int * int * int) ) =
            if (#2 date) = hasMonth then 1 else 0
    in
        if null dates
        then 0
        else inMonth( (hd dates) ) + number_in_month( tl dates, hasMonth)
    end

(* takes a list of dates and list of months, returning the number
   of dates that have a month that match any value in the list of
   months *)
fun number_in_months ( 
   dates  : (int * int * int) list
 , months : (int)             list ) =
    let
	fun inMonth ( month : int ) =
             number_in_month(dates, month) 
    in
        if null months
        then 0
	else inMonth( (hd months) ) + number_in_months( dates, tl months)
    end
