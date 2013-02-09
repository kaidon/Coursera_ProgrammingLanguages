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
