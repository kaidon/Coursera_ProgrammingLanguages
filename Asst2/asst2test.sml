(* Coursera Programming Languages
   Sheldon Warkentin
   January 30, 2012
   
   Assignment 2: Tests
*)

use "asst2.sml";

(**********************)
(* Tests for all_except_option*)
(**********************)
val all_except_option__emptyIsNone = 
    case all_except_option("A",[]) of
	NONE => "pass"
      | _ => "FAIL"

val all_except_option__noMatch = 
    case all_except_option("A", ["B","C","D"]) of
	NONE => "pass"
     | _  => "FAIL"
			 
val all_except_option__match_start = 
    case all_except_option("A", ["A","B","C"]) of
	SOME ["B","C"] => "pass"
     | _ => "FAIL"

val all_except_option__match_end = 
    case all_except_option("C", ["A","B","C"]) of
	SOME ["A","B"] => "pass"
     | _ => "FAIL"

val all_except_option__dupes_unhandled = 
    case all_except_option("B", ["A","B","B","C"]) of
	SOME ["A","B","C"] => "pass"
     | _ => "FAIL"

(**********************)
(* Tests for get_substitutions1 *)
(**********************)
val get_substitutions1__emptyMap =
    case get_substitutions1([],"A") of
	[] => "pass"
    | _ => "FAIL"

val get_substitutions1__uniqueMatches = 
    case get_substitutions1(
	[
	  ["Fred","Fredrick"]
	 ,["Elizabeth","Betty"]
	 ,["Freddie","Fred","F"]
	]
       ,"Fred") of
	["Fredrick","Freddie","F"] => "pass"
     | _ => "FAIL"

val get_substitutions1__repeatMatches = 		      
    case    get_substitutions1(
	    [
	      ["Fred","Fredrick"]
	     ,["Jeff","Jeffrey"]
	     ,["Geoff","Jeff","Jeffrey"]
	    ]
	   ,"Jeff") of
	["Jeffrey","Geoff","Jeffrey"] => "pass"
      | _ => "FAIL"

