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

assertEquals(SOME[4,5,1,2,3]
	    , all_answers intToList [1,2]
	    , "all_answers appends correctly");

assertEquals(NONE
	    , all_answers intToList [1,2,3,4]
	    , "all_answers has NONE");

(**********************)
(* Tests for count_wildcards*)
(**********************)
val pat1 = Wildcard;
val pat2 = Variable "varA";
val pat3 = ConstP 10;
val pat4 = TupleP[Wildcard, Variable "varB", ConstP 99
		  , TupleP[Wildcard, Variable "varC"]
		  , ConstructorP("con1", Wildcard)];
val pat5 = ConstructorP("con2", Wildcard);

assertEquals(1, count_wildcards(pat1), "count_wildcards");
assertEquals(0, count_wildcards(pat2), "count_wildcards");
assertEquals(0, count_wildcards(pat3), "count_wildcards");
assertEquals(3, count_wildcards(pat4), "count_wildcards");
assertEquals(1, count_wildcards(pat5), "count_wildcards");

(**********************)
(* Tests for count_wild_and_variable_lengths*)
(**********************)
assertEquals(1, count_wild_and_variable_lengths(pat1), "count_wildcards");
assertEquals(4, count_wild_and_variable_lengths(pat2), "count_wildcards");
assertEquals(0, count_wild_and_variable_lengths(pat3), "count_wildcards");
assertEquals(11,count_wild_and_variable_lengths(pat4), "count_wildcards");
assertEquals(1, count_wild_and_variable_lengths(pat5), "count_wildcards");

(**********************)
(* Tests for count_some_var*)
(**********************)
assertEquals(0, count_some_var( ("var",pat1)), "count_some_var");
assertEquals(1, count_some_var( ("varA",pat2)), "count_some_var");
assertEquals(0, count_some_var( ("varB",pat2)), "count_some_var");
assertEquals(1, count_some_var( ("varC",pat4)), "count_some_var");
assertEquals(0, count_some_var( ("varZ",pat4)), "count_some_var");

(**********************)
(* Tests for check_pat*)
(**********************)
val pat6 = TupleP[Variable "varA", Variable "varB", Variable "varC"];
val pat7 = TupleP[Variable "varA", Variable "varB", Variable "varA"];

assertEquals(true, check_pat(pat1), "check_pat true for pattern with no vars");
assertEquals(true, check_pat(pat4), "check_pat with more complex pattern");
assertEquals(true, check_pat(pat6), "check_pat all distinct");
assertEquals(false, check_pat(pat7), "check_pat non distinct");

(**********************)
(*   Tests for match  *)
(**********************)
assertEquals(SOME [], match( Const(5), pat1),"match Wildcard");
assertEquals(SOME [], match( Unit, pat1),"match Wildcard");
assertEquals(SOME [], match( Tuple[Unit,Const(5)],pat1), "match Wildcard");
assertEquals(SOME [], match( Constructor("a",Const(5)),pat1), "match Wildcard");
assertEquals(SOME[("a", Unit)]
	    , match(Tuple[Const 4, Unit], TupleP[ConstP 4, Variable "a"])
	    , "match simple");
assertEquals(NONE
	    , match( Tuple [Const 3, Unit],TupleP [ConstP 4, Variable "a"])
	    , "match Const vals don't match");
assertEquals(SOME []
	    , match(Constructor("abc" , Const 1),ConstructorP("abc", ConstP 1))
	    , "match Contructor ids match");
assertEquals(NONE
	    , match(Constructor("DEF" , Const 1),ConstructorP("abc", ConstP 1))
	    , "match Contructor ids don't match");
assertEquals(SOME [ ("c", Tuple [Unit, Unit]), 
                    ("b", Constructor("foo", Const 2)),
                    ("a", Const 1) ]
	    , match(
		  Tuple [Const 1, Constructor("foo", Const 2), Tuple [Unit, Unit]],
 		  TupleP [Variable "a", Variable "b", Variable "c"])
	    , "match all variables");
assertEquals(SOME[("a", Unit)]
	    , match( Tuple [Const 1, Unit], TupleP [Wildcard, Variable "a"])
	    , "match Wildcard takes value");

(**********************)
(*   Tests for first_match *)
(**********************)
assertEquals(NONE
	    , first_match Unit []
	    , "first_match empty list");
assertEquals(SOME[]
	    , first_match Unit [ConstP 5, Wildcard, UnitP]
	    , "first_match has matches, but all empty");
assertEquals(SOME[("a", Unit)]
	    , first_match (Tuple[Const 4, Unit])
			  ([UnitP, ConstP 5, TupleP [ConstP 4, Variable "a"]])
	    , "first_match simple");

