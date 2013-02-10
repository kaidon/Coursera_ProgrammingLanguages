(* Coursera Programming Languages  
   January 30, 2012
   
   Assignment 3: Questions 1-12
*)

(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)


datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string;

(* Given a string list, returns string list with only strings 
   that start with captail letter. 

   Assumes all strings are size >= 1*)
fun only_capitals xs = 
    List.filter (fn x => Char.isUpper(String.sub(x,0))) xs

(* Given a string list, returns the longest string the the list.
   Returns "" for an empty list.
   If more than one match, returns element closest to start of list. *)
fun longest_string1 xs = 
    List.foldl (fn (x,last) => 
		   case String.size(x) > String.size(last) of
		       true => x 
		     | _ => last) 
	       "" xs

(* Same as longest_string1, but returns element closest to the end of
   the list in the event of another match *)
fun longest_string2 xs = 
    List.foldl (fn (x,last) => 
		   case String.size(x) >= String.size(last) of
		       true => x 
		     | _ => last) 
	       "" xs

fun longest_string_helper cond xs = 
        List.foldl (fn (x,last) => 
		   case cond(String.size(x), String.size(last)) of
		       true => x 
		     | _ => last) 
	       "" xs

(* Simplification of longest_string1 *)
val longest_string3 =
    longest_string_helper (fn (sizeX,sizeL) => sizeX > sizeL)

(* Simplication of longest_string_2 *)
val longest_string4 =
    longest_string_helper (fn (sizeX,sizeL) => sizeX >= sizeL)

(* returns the longest string in a list that starts with a capital letter.
   Returns "" for an empty list. *)
val longest_capitalized = longest_string_helper (fn (x,y) => x>y) 
			  o only_capitals;

(* reverse a string 
   String lib functions used: {explode,implode)
*)
fun rev_string s = (String.implode o rev o String.explode) s

(* The first argument should be applied to elements of the second argument 
   in order until the first time it returns SOME v for some v and then v 
   is the result of the call to first_answer.
   
   If the first argument returns NONE for all list elements, then 
   first_answer should raise the exception NoAnswer. *)

fun first_answer getAnswer lst =
    case lst of
	[] => raise NoAnswer
      | x::xs => case getAnswer(x) of
		    SOME v => v
		  | _ => first_answer getAnswer xs

(* The first argument should be applied to elements of the second
   argument.

  If it returns NONE for any element, then the result for all_answers is NONE. 
  Else the calls to the first argument will have produced SOME lst1, 
  SOME lst2, ... SOME lstn and the result of all_answers is SOME lst where 
  lst is lst1, lst2, ..., lstn appended together (order doesn't matter). *)


fun all_answers getAnswers lst =
    let fun appendAnswers ( allAns, lastAns, qs ) =
	    case lastAns of
		NONE => NONE
	      | SOME a =>  case qs of 
			  [] => SOME (a @ allAns) (* no, return ans *)
			    | q::qs' =>  (*append last answer, and get next *)
			      appendAnswers (a @ allAns
					    , getAnswers q
					    , qs')	
    in
	appendAnswers([], SOME [], lst)
    end
