(* Coursera Programming Languages, Homework 3, Solution Code *)

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
	     | Datatype of string

(**** you can put all your code here ****)

(* 1. Write a function only_capitals that takes a string list and returns a string list that has only
      the strings in the argument that start with an uppercase letter. Assume all strings have at least 1
      character. Use List.filter, Char.isUpper, and String.sub to make a 1-2 line solution. *)
fun only_capitals str_list = 
  List.filter (fn str => Char.isUpper (String.sub (str, 0))) str_list

(* 2. Write a function longest_string1 that takes a string list and returns the longest string in the
      list. If the list is empty, return "". In the case of a tie, return the string closest to the beginning of the
      list. Use foldl, String.size, and no recursion (other than the implementation of foldl is recursive). *)
fun longest_string1 str_list = 
    let val longest = ""
    in
    List.foldl (fn (str, longest) => if (String.size (longest) >= String.size (str)) 
                                    then longest else str)
               longest
               str_list
    end
(* 3.  Write a function longest_string2 that is exactly like longest_string1 except in the case of ties
       it returns the string closest to the end of the list. Your solution should be almost an exact copy of
       longest_string1. Still use foldl and String.size. *)
fun longest_string2 str_list = 
    let val longest = ""
    in
    List.foldl (fn (str, longest) => if (String.size (longest) > String.size
    (str)) 
                                    then longest else str)
               longest
               str_list
    end

(* 4. Write functions longest_string_helper, longest_string3, and longest_string4 such that:
        • longest_string3 has the same behavior as longest_string1 and longest_string4 has the
          same behavior as longest_string2.
        • longest_string_helper has type (int * int -> bool) -> string list -> string
          (notice the currying). This function will look a lot like longest_string1 and longest_string2
          but is more general because it takes a function as an argument.
        • If longest_string_helper is passed a function that behaves like > (so it returns true exactly
          when its first argument is stricly greater than its second), then the function returned has the same
          behavior as longest_string1.
        • longest_string3 and longest_string4 are defined with val-bindings and partial applications
          of longest_string_helper. *)
fun longest_string_helper f = fn str_list =>
    let val longest = ""
    in
      List.foldl (fn (str, longest) => if (f (String.size (longest), String.size
      (str)))
                                       then longest
                                       else str)
                longest 
                str_list
    end

val longest_string3 = longest_string_helper (fn (x, y) => x >= y)
val longest_string4 = longest_string_helper (fn (x, y) => x > y)

(* 5. Write a function longest_capitalized that takes a string list and returns the longest string in
      the list that begins with an uppercase letter, or "" if there are no such strings. Assume all strings
      have at least 1 character. Use a val-binding and the ML library’s o operator for composing functions.
      Resolve ties like in problem 2. *)
val longest_capitalized = fn str_list => (longest_string1 o only_capitals)
str_list

(* 6. Write a function rev_string that takes a string and returns the string that is the same characters in
      reverse order. Use ML’s o operator, the library function rev for reversing lists, and two library functions
      in the String module. (Browse the module documentation to find the most useful functions.) *)
fun rev_string str = (String.implode o List.rev o String.explode) str

(* 7. Write a function first_answer of type (’a -> ’b option) -> ’a list -> ’b
      (notice the 2 arguments are curried). The first argument should be applied to
      elements of the second argument in order until the first time it returns SOME
      v for some v and then v is the result of the call to first_answer. If the
      first argument returns NONE for all list elements, then first_answer should
      raise the exception NoAnswer. Hints: Sample solution is 5 lines and does
      nothing fancy. *)
fun first_answer f = fn arg_list =>
    case arg_list of
         [] => raise NoAnswer
       | x :: rest => case (f x) of
                           NONE => first_answer f (rest)
                         | SOME x => x

(* 8. Write a function all_answers of type (’a -> ’b list option) -> ’a list -> ’b list option
      (notice the 2 arguments are curried). The first argument should be applied to elements of the second
      argument. If it returns NONE for any element, then the result for all_answers is NONE. Else the
      calls to the first argument will have produced SOME lst1, SOME lst2, ... SOME lstn and the result of
      all_answers is SOME lst where lst is lst1, lst2, ..., lstn appended together (order doesn’t matter).
      Hints: The sample solution is 8 lines. It uses a helper function with an accumulator and uses @. Note
      all_answers f [] should evaluate to SOME []. *)
fun all_answers f = fn arg_list =>
    let 
        fun all_answers_helper (arg_list, acc) =
            case (arg_list, acc) of
                 ([], _) => acc
               | (x :: rest, SOME ans_list) => case (f x) of
                                                     NONE => NONE
                                                   | SOME [x] =>
                                                       all_answers_helper (rest,
                                                       SOME (ans_list @ [x]))
    in
    case arg_list of 
         [] => SOME []
       | x :: rest => case (f x) of
                           NONE => NONE
                         | _ => all_answers_helper (x :: rest, SOME [])
    end

(* 9(a). Use g to define a function count_wildcards that takes a pattern and
         returns how many Wildcard patterns it contains.  *)
fun count_wildcards p = 
    g (fn () => 1) (fn _ => 0) p































