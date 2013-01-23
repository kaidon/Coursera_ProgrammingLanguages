
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

