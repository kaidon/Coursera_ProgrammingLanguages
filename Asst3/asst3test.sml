use "asst3.sml";

fun assertTrue (expr, failDesc) = 
    case expr() of
	true => "pass"
      | _  =>  "!FAIL! " ^ failDesc

fun assertEquals (l1,l2, failDesc) =
    case l1=l2 of
	true => "pass"
      | _ => "!FAIL! " ^ failDesc


(**********************)
(* Tests for only_capitals *)
(**********************)
val stringList = ["aa","Aa","Bb","BB","aA","bB","ZZZ","123","#@!$"];

assertEquals ([], only_capitals [], "only_capitals empty list");
assertEquals (["Aa","Bb","BB","ZZZ"], only_capitals(stringList)
	      , "only_capitals")
