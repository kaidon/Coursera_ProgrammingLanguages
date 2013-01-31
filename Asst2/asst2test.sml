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

(**********************)
(* Tests for get_substitutions2 *)
(**********************)
val get_substitutions2__emptyMap =
    case get_substitutions2([],"A") of
	[] => "pass"
      | _ => "FAIL"

val get_substitutions2__uniqueMatches = 
    case get_substitutions2(
	    [
	      ["Fred","Fredrick"]
	     ,["Elizabeth","Betty"]
	     ,["Freddie","Fred","F"]
	    ]
	   ,"Fred") of
	["Fredrick","Freddie","F"] => "pass"
      | _ => "FAIL"

val get_substitutions2__repeatMatches = 		      
    case    get_substitutions2(
	    [
	      ["Fred","Fredrick"]
	     ,["Jeff","Jeffrey"]
	     ,["Geoff","Jeff","Jeffrey"]
	    ]
	   ,"Jeff") of
	["Jeffrey","Geoff","Jeffrey"] => "pass"
      | _ => "FAIL"

(**********************)
(* Tests for similar_names *)
(**********************)

val similar_names__originalOnly = 
    case similar_names([],{first="A",middle="B",last="C"}) of
	[{first="A",middle="B",last="C"}] => "pass"
	| _ => "FAIL"

val similar_names__no_match_originalOnly =
    case similar_names(
	    [["A","B"],["B","C"]]
	   ,{first="Z",middle="B",last="C"}) of
	[{first="Z",middle="B",last="C"}] => "pass"
      | _ => "FAIL"

val similar_names__many_match =    
    case similar_names(
	    [["A","B"],["B","C"],["B","D"]]
	   ,{first="B",middle="C",last="D"}) of
	[{first="B",middle="C",last="D"}
	,{first="A",middle="C",last="D"}
	,{first="C",middle="C",last="D"}
	,{first="D",middle="C",last="D"}] => "pass"
      | _ => "FAIL"

(**********************)
(* Tests for card_color *)
(**********************)
val card_color__hearts = 
    case card_color(Hearts,Ace) of Red => "pass" 
				|_  => "FAIL"

val card_color__diamonds = 
    case card_color(Diamonds,Ace) of Red => "pass" 
				|_  => "FAIL"

val card_color__spades = 
    case card_color(Spades,Ace) of Black => "pass" 
				|_  => "FAIL"

val card_color__clubs = 
    case card_color(Clubs,Ace) of Black => "pass" 
				|_  => "FAIL"

(**********************)
(* Tests for card_value *)
(**********************)

(* convert a list of ranks with a suit into a list of cards *)
fun toCard (rankList,asSuit) =
 case rankList of
     [] => []
	| x::xs' => (asSuit,x)::toCard(xs',asSuit)

(* exhaustive list of all supported ranks *)
val allRanks = [Num 1, Num 2, Num 3, Num 4, Num 5, Num 6, 
		Num 7, Num 8, Num 9, Num 10, 
		Jack, Queen, King, Ace];

(* calls card_value to get the rank number for each card *)
fun getRanks cardList =
    case cardList of
	[] => []
	   | x::xs' => card_value(x)::getRanks(xs')

(* the exact rank for each card *)
val expectedRanks = [1,2,3,4,5,6,7,8,9,10,10,10,10,11];

(* exhaustively perform every combination of rank and suit to check rank *)
val card_value__hearts = 
    case getRanks(toCard(allRanks,Hearts)) = expectedRanks of
	true => "pass"
      | _ => "FAIL"
val card_value__spades = 
    case getRanks(toCard(allRanks,Spades)) = expectedRanks of
	true => "pass"
      | _ => "FAIL"
val card_value__diamonds = 
    case getRanks(toCard(allRanks,Diamonds)) = expectedRanks of
	true => "pass"
      | _ => "FAIL"
val card_value__clubs = 
    case getRanks(toCard(allRanks,Clubs)) = expectedRanks of
	true => "pass"
      | _ => "FAIL"
