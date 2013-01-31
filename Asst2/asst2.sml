(* Coursera Programming Languages
   Sheldon Warkentin
   January 30, 2012
   
   Assignment 2: Questions 1-11
*)

(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* Takes a string and a string list. Return NONE if the
 string is not in the list, else return SOME lst where lst is
 identical to the argument list except the string is not in it *)
(* string * string list -> string list option *)
fun all_except_option (except, inList) =
      case inList of
	  [] => NONE
	| (x::xs') => case same_string(except, x) of
			 true => SOME xs' (* assumes no more matches now *)
		       | false => case all_except_option(except, xs') of
				      NONE => NONE
				    | SOME ys' => SOME (x::ys')


(* Takes a string list list (a list of list of strings, 
 the substitutions) and a string s and returns a string list. 
 The result has all the strings that are in some list in substitutions that 
 also has s, but s itself should not be in the result.*)
(* string string list * string -> string list *)		
fun get_substitutions1 (substSet, match) =
    case substSet of
	[] => []
	   | (hdList::tailLists) => 
	     let val toAppend = get_substitutions1(tailLists,match)
	     in 
		 case all_except_option(match,hdList) of
		     NONE => toAppend
		   | SOME matches =>  matches @ toAppend
	     end

(* tail recursive version of get_substitutions1 *)
fun get_substitutions2 (substSet, match) =
    case substSet of
	[] => []
	   | (hdList::tailLists) => 
	     let val toAppend = get_substitutions1(tailLists,match)
	     in 
		 case all_except_option(match,hdList) of
		     NONE => toAppend
		   | SOME matches =>  matches @ toAppend
	     end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove


