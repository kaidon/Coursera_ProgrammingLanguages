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

(* Count all Wildcard values in a pattern *)
fun count_wildcards p = 
    g 
	(fn wildCard => 1)  (* Counting Wildcard, just return 1 *)
	(fn variable => 0)  (* Variables don't count *)
	p

(* Count all Wildcard occurances, as well as the sum of the string lengths
   of all variable name lengths *)
fun count_wild_and_variable_lengths p =
    g
	(fn wildCard => 1 ) (*Counting wildcard occurrence *)
	(fn variable => String.size(variable)) (* length of var name *)
	p
	
(* With a string and pattern, returns number of times the string appears
   as a variable in the pattern *)
fun count_some_var (matchVariable,p) =
    g
	(fn wildCard => 0) (* dont' care about wildcards *)
	(fn variable => if variable = matchVariable then 1 else 0)
	p

(* Checks if all variables appearing in a pattern are distinct from
   one another.
   Constructor names do not matter *)
fun check_pat p =
    let fun getAllVarNames p = (* find all variable strings *)
	    case p of
		Variable x => x::[]
	      | TupleP ps  => List.foldl
				  (fn (pat,acc) => getAllVarNames(pat)@acc)
				  []  ps
	      | ConstructorP(_,p) => getAllVarNames(p)@[]
	      | _                 => []
	fun hasDupes varNames = (* pop each variable off list, seeing if it *)
	    case varNames of    (* exists in the remainder of the list *)
		[] => false
	      | x::xs => List.exists
			     (fn inList => inList=x)
			     xs
			 orelse hasDupes(xs)				 
    in
	hasDupes(getAllVarNames(p)) = false (* negate it, no dupes = pass *)
    end   
	
(*  takes a valu*pattern, returning a (string*value) list option.
    NONE if pattern does not match.
    SOME lst, where lst is list of bindings if it does.

    Note: if value matches but pattern has no patterns of form Variable s, then
          result is SOME[]. *)

fun match valuPatternPair =
    case valuPatternPair of 
	(* Const matches exactly on int *)	
	(Const x, ConstP y) => 
	if x=y then SOME[] else NONE   

      (* Unit matches on constructor *)
      | (Unit, UnitP) => SOME[]

      (* Tuples match on size, and contents -- recursively *)
      | (Tuple tv, TupleP tp) => 
	if List.length(tv) = List.length(tp) 
	then all_answers match (ListPair.zip(tv,tp)) 
	else NONE
		 
      (* Constructor matches on name, then valu * pattern *)
      | (Constructor(sv,v), ConstructorP(sp,p)) =>
	if sv = sp then match(v,p) else NONE

      (* Variable matches on anything *)
      | (v,Variable x) => SOME[(x,v)]

      (* Wildcard matches on anything else *)
      | (_,Wildcard) => SOME[]

      (* The rest results in no matches *)
      | _  => NONE			 		      

(* takes a valu and list of patterns, returning (string * value) list  option
   NONE if no pattern in the list matches 
   SOME lst where lst is the list of bindings for the first pattern in 
   the list that matches. *)

fun first_match aValu patterns =
    SOME (first_answer (fn p => match(aValu,p)) patterns)
    handle NoAnswer => NONE
