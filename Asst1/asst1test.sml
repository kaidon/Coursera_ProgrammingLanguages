use "asst1.sml";

(**********************)
(* Tests for is_older *)
(**********************)
val is_older__d1_greater_d2 = if is_older( (2000,12,1), (2000,1,1) )
then "pass"
else "fail"

val is_older__d1_less_d2 = if is_older( (2000,1,1), (2000,12,1) )
then "fail"
else "pass"

val is_older__d1_greater_d2_yeardiff = if is_older( (2001,1,1), (2000,12,12) )
then "pass"
else "fail"

val is_older__d1_greater_d2_monthdiff = if is_older( (2000,2,1), (2000,1,1) )
then "pass"
else "fail"

val is_older__d1_greater_d2_daydiff = if is_older( (2000,1,31), (2000,1,10) )
then "pass"
else "fail"

(**********************)
(* Tests for number_in_month *)
(**********************)
val number_in_month__null_list = if number_in_month ( [], 10 ) = 0 
then "pass"
else "fail"

val number_in_month__match_none = if number_in_month
(
  [(2000,1,1), (2000,2,1), (2000,3,1)]
 ,4
) = 0
then "pass"
else "fail"

val number_in_month__match_all = if number_in_month
(
  [(2000,1,1), (2000,1,2), (2000,1,3)]
 ,1
) = 3
then "pass"
else "fail"

val number_in_month__match_some = if number_in_month
(
  [(2000,1,1), (2000,2,1), (2000,3,1)]
 ,2
) = 1
then "pass"
else "fail"

(**********************)
(* Tests for number_in_months *)
(**********************)
val number_in_months__null_date_list = if number_in_months
( 
  []
 ,[1,2,3] 
) = 0 
then "pass"
else "fail"

val number_in_months__null_month_list = if number_in_months
( 
  [(2000,1,1),(2000,2,1),(2000,3,1)]
 ,[] 
) = 0 
then "pass"
else "fail"

val number_in_months__null_both_list = if number_in_months ( [], [] ) = 0 
then "pass"
else "fail"

val number_in_months__match_none = if number_in_months
(
  [(2000,1,1), (2000,2,1), (2000,3,1)]
 ,[4,5,6]
) = 0
then "pass"
else "fail"

val number_in_months__match_all = if number_in_months
(
  [(2000,1,1), (2000,2,2), (2000,3,3)]
 ,[1,2,3,4,5,6]
) = 3
then "pass"
else "fail"

val number_in_months__match_some = if number_in_months
(
  [(2000,1,1), (2000,2,1), (2000,3,1)]
 ,[1,4,5,6,7]
) = 1
then "pass"
else "fail"

(**********************)
(* Tests for dates_in_month *)
(**********************)
val dates_in_month__null_list = if null (dates_in_month ( [], 10 ))
then "pass"
else "fail"

val dates_in_month__match_none = if null (dates_in_month
(
  [(2000,1,1), (2000,2,1), (2000,3,1)]
 , 4
))
then "pass"
else "fail"

val dates_in_month__match_all = if dates_in_month
(
  [(2000,1,1), (2000,1,2), (2000,1,3)]
 ,1
) = [(2000,1,1), (2000,1,2), (2000,1,3)]
then "pass"
else "fail"

val dates_in_month__match_some = if dates_in_month
(
  [(2000,1,1), (2000,2,1), (2000,3,1)]
 ,2
) = [(2000,2,1)]
then "pass"
else "fail"

(**********************)
(* Tests for dates_in_months *)
(**********************)
val dates_in_months__null_date_list = if null (dates_in_months
( 
  []
 ,[1,2,3] 
))
then "pass"
else "fail"

val dates_in_months__null_month_list = if null (dates_in_months
( 
  [(2000,1,1),(2000,2,1),(2000,3,1)]
 ,[] 
))
then "pass"
else "fail"

val dates_in_months__null_both_list = if null (dates_in_months ( [], [] )) 
then "pass"
else "fail"

val dates_in_months__match_none = if null (dates_in_months
(
  [(2000,1,1), (2000,2,1), (2000,3,1)]
 ,[4,5,6]
))
then "pass"
else "fail"

val dates_in_months__match_all = if dates_in_months
(
  [(2000,1,1), (2000,2,2), (2000,3,3)]
 ,[1,2,3,4,5,6]
) = [(2000,1,1), (2000,2,2), (2000,3,3)]
then "pass"
else "fail"

val dates_in_months__match_some = if dates_in_months
(
  [(2000,1,1), (2000,2,1), (2000,3,1)]
 ,[1,4,5,6,7]
) = [(2000,1,1)]
then "pass"
else "fail"

(**********************)
(* Tests for get_nth  *)
(**********************)

val get_nth__first = if get_nth(["a","b","c"],1) = "a"
then "pass"
else "fail"

val get_nth__last = if get_nth(["a","b","c"],3) = "c"
then "pass"
else "fail"

val get_nth__mid = if get_nth(["a","b","c"],2) = "b"
then "pass"
else "fail"

(**********************)
(* Tests for date_to_string  *)
(**********************)
val date_to_string__Jan1_2000 = if date_to_string((2000,1,1))="January 1, 2000"
then "pass"
else "fail"

val date_to_string__May20_1981 = if date_to_string((1981,5,20))="May 20, 1981"
then "pass"
else "fail" 

(**********************)
(* Tests for number_before_reaching_sum *)
(**********************)

(* 1->0 < 0 ... n = 1
   2->1 > 0
 *)
val number_before_reaching_sum__simple_case = 
    if number_before_reaching_sum( 1, [0,1] ) = 1
    then "pass"
    else " fail"

(* 1->0  < 4
   2->1  < 4
   3->3  < 4 ... n = 3
   4->6  > 4
   5->10 > 4
   6->15 > 4
 *)
val number_before_reaching_sum__more_complex = 
    if number_before_reaching_sum( 4, [0,1,2,3,4,5] ) = 3
    then "pass"
    else "fail"

(* 1->0  < 7
   2->1  < 7
   3->3  < 7 
   4->6  < 7 ... n = 4
   5->10 > 7
   6->15 > 7
 *)
val number_before_reaching_sum__more_complex2 = 
if number_before_reaching_sum( 7, [0,1,2,3,4,5] ) = 4
    then "pass"
    else "fail"

(**********************)
(* Tests for what_month *)
(**********************)
val what_month__low_bound =
if ["January","February","March","December"] =
   [what_month(1),what_month(32),what_month(60),what_month(335)]
   then "pass"
   else "fail"

val what_moth__high_bound =
if ["January","February","March","December"] =
   [what_month(31),what_month(59),what_month(90),what_month(365)]
   then "pass"
   else "fail"

val what_moth__mid_bound =
if ["January","February","March","December"] =
   [what_month(15),what_month(45),what_month(82),what_month(345)]
   then "pass"
   else "fail"

(**********************)
(* Tests for what_month *)
(**********************)
val month_range__spans_months =
if month_range(30,33) =
   [1,1,2,2] (* last 2 days of January, first 2 of February *)
   then "pass"
   else "fail"
