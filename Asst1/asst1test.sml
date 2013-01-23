use "asst1.sml";

(* Tests for is_older *)
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


