use "asst3.sml";

fun assertTrue (expr, failDesc) = 
    case expr() of
	true => "pass"
      | _  =>  "!FAIL! " ^ failDesc

fun assertEquals (expect,result, failDesc) =
    case expect=result of
	true => "pass"
      | _ => "!FAIL! " ^ failDesc


(**********************)
(* Tests for only_capitals *)
(**********************)
val stringList = ["aa","Aa","Bb","BB","aA","bB","ZZZ","123","#@!$"];

assertEquals ([], only_capitals [], "only_capitals empty list");
assertEquals (["Aa","Bb","BB","ZZZ"], only_capitals(stringList)
	      , "only_capitals");

(**********************)
(* Tests for longest_string1 *)
(**********************)
val stringList2 = ["a", "bb", "ccc", "dddd"];
val stringList3 = ["dddd", "ccc", "bb", "a"];
val stringList4 = ["a", "bbb", "ccc", "d"];

assertEquals("",    longest_string1([]),          "longest_string empty");
assertEquals("dddd",longest_string1(stringList2), "longest_string1");
assertEquals("dddd",longest_string1(stringList3), "longest_string1");
assertEquals("bbb", longest_string1(stringList4), "longest_string1 dupes");

