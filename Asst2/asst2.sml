(* Coursera Programming Languages
   Sheldon Warkentin
   January 30, 2012
   
   Assignment 2: Questions 1-11
*)

(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(****************************************)
(**************   PART 1 ****************)
(****************************************)

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
    let fun aux(matchSets,acc) =
	    case matchSets of
		[] => acc
	      | hdList::tlLists => 
		let val headMatch = all_except_option(match,hdList)
		in
		    case headMatch of
			NONE => aux(tlLists,acc)
		      | SOME headMatch => aux(tlLists,acc @ headMatch)
	      (* append the accumualtor on the match to keep order *)
		end
    in
	aux(substSet,[])
    end

(****************************************)
(**************   PART 2 ****************)
(****************************************)


(* Takes a list of substitutions, and returns a list of full names
  with only the first name substituted *)
fun similar_names(substSet,{first=f,middle=m,last=l}) = 
    let fun replaceFirst(fromNames) = 
	    case fromNames of
		[] => []
	      | x::xs' => {first=x,middle=m,last=l} :: replaceFirst(xs')
    in
	{first=f,middle=m,last=l}::replaceFirst(get_substitutions2(substSet,f))
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* Return color of a card -- based on suit *)
fun card_color card =
    case card of
	(Spades,_) => Black
     | (Clubs,_) => Black
     | (_,_) => Red

(* Returns the value of a card. NUmbered cards have their number, aces are 11, 
   and the rest are 10 *)
fun card_value card = 
    case card of
	(_,Ace) => 11
      | (_,Num x) => x (* Num int *)
      | (_,_) => 10
