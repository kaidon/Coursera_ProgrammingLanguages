use "asst3.sml";

fun assertTrue (expr, failDesc) = 
    case expr() of
	true => "pass"
      | _  =>  "!FAIL! " ^ failDesc

fun assertEquals (expect, result, failDesc) =
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
val stringList4 = ["a", "bbb", "ccc", "zzz", "d"];

assertEquals("",    longest_string1([]),          "longest_string1 empty");
assertEquals("dddd",longest_string1(stringList2), "longest_string1");
assertEquals("dddd",longest_string1(stringList3), "longest_string1");
assertEquals("bbb", longest_string1(stringList4), "longest_string1 dupes");

(**********************)
(* Tests for longest_string2 *)
(**********************)
assertEquals("",    longest_string2([]),          "longest_string2 empty");
assertEquals("dddd",longest_string2(stringList2), "longest_string2");
assertEquals("dddd",longest_string2(stringList3), "longest_string2");
assertEquals("zzz", longest_string2(stringList4), "longest_string2 dupes");

(**********************)
(* Tests for longest_string3 *)
(**********************)
assertEquals("",    longest_string3([]),          "longest_string3 empty");
assertEquals("dddd",longest_string3(stringList2), "longest_string3");
assertEquals("dddd",longest_string3(stringList3), "longest_string3");
assertEquals("bbb", longest_string3(stringList4), "longest_string3 dupes");

(**********************)
(* Tests for longest_string4 *)
(**********************)
assertEquals("",    longest_string4([]),          "longest_string4 empty");
assertEquals("dddd",longest_string4(stringList2), "longest_string4");
assertEquals("dddd",longest_string4(stringList3), "longest_string4");
assertEquals("zzz", longest_string4(stringList4), "longest_string4 dupes");

(**********************)
(* Tests for longest_capitalized *)
(**********************)
val stringList5 = ["a", "BBb", "CCc", "zzz", "d"];
assertEquals("",    longest_capitalized([]),          "longest_capitalized");
assertEquals("ZZZ", longest_capitalized(stringList),  "longest_capitalized");
assertEquals("BBb", longest_capitalized(stringList5), "longest_capitalized");

(**********************)
(* Tests for rev_string *)
(**********************)
assertEquals("", rev_string(""), "rev_string null");
assertEquals("edcba", rev_string("abcde"), "rev_string works");

(**********************)
(* Tests for first_answer*)
(**********************)
assertEquals(true
	    , (first_answer (fn v => SOME false) [] handle NoAnswer => true)
	    , "NoAnswer raised");

assertEquals(2
	    , (first_answer (fn v => if v>1 then SOME v else NONE)
			    [1,2,3,4,5,6])
	    , "first_answer exact match");

assertEquals(10
	    , (first_answer (fn v => if v>1 then SOME v else NONE)
			    [1,~1,~2] handle NoAnswer =>  10)
	    , "first_answer exact match");

(**********************)
(* Tests for all_answers*)
(**********************)
assertEquals(SOME[]
	    ,(all_answers (fn v => SOME[v,v,v]) [])
	    , "all_answers empty list");

fun intToList x = 
    case x of
	1 => SOME [1,2,3]
      | 2 => SOME [4,5]
      | _ => NONE;

assertEquals(SOME[1,2,3,4,5]
	    , all_answers intToList [1,2]
	    , "all_answers appends correctly");

assertEquals(NONE
	    , all_answers intToList [1,2,3,4]
	    , "all_answers has NONE");
