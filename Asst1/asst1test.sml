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
