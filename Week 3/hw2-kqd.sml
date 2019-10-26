(* Kevin Dam, Coursera PL, HW2 Solution Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* 1.  This problem involves using first-name substitutions to come up with alternate names. For example,
       Fredrick William Smith could also be Fred William Smith or Freddie William Smith. Only part (d) is
       specifically about this, but the other problems are helpful. *)

(* 1.(a) Write a function all_except_option, which takes a string and a string list. Return NONE if the
         string is not in the list, else return SOME lst where lst is identical to the argument list except the string
         is not in it. You may assume the string is in the list at most once. Use same_string, provided to you,
         to compare strings. Sample solution is around 8 lines. *)
fun all_except_option(s : string, los : string list) = (* string list option *)
    let 
      fun all_except_option_nonempty(s : string, los : string list) = (* string
        list *)
        case los of
             [] => [] (* should never happen *)
           | x :: [] => if same_string(x, s)
                        then []
                        else x :: []
           | x :: rest => if same_string(x, s)
                          then rest
                          else x :: all_except_option_nonempty(s, rest)
    in
      case los of
           [] => NONE
         | _ => SOME(all_except_option_nonempty(s, los))
    end

(* 1.(b) Write a function get_substitutions1, which takes a string list list (a list of list of strings, the
         substitutions) and a string s and returns a string list. The result has all the strings that are in
         some list in substitutions that also has s, but s itself should not be in the
         result. *)
fun contains(str:string, strs:string list) = (* bool *)
  case strs of
       [] => false
     | x :: rest => if same_string(x, str)
                    then true
                    else contains(str, rest)

fun get_substitutions1(subs : string list list, s: string) = (* string list *)
  case subs of
       [] => []
     | x :: [] => if contains(s, x)
                  then case all_except_option(s, x) of
                            NONE => []
                          | SOME subs_for_x => subs_for_x
                  else []
     | x :: rest => if contains(s, x)
                    then case all_except_option(s, x) of
                              NONE => get_substitutions1(rest, s)
                            | SOME subs_for_x => subs_for_x @
                            get_substitutions1(rest, s)
                    else get_substitutions1(rest, s)

(* 1.(c) Write a function get_substitutions2, which is like get_substitutions1 except it uses a tail-recursive
         local helper function. *)
fun get_substitutions2(subs: string list list, s: string) = (* string list *)
    let 
      fun get_substitutions2_helper(subs: string list list, s: string, acc: string
            list) = (* string list *)
        case subs of
             [] => acc
           | x :: [] => if contains(s, x)
                        then case all_except_option(s, x) of
                                  NONE => acc
                                | SOME subs_for_x => acc @ subs_for_x
                        else acc
           | x :: rest => if contains(s, x)
                          then case all_except_option(s, x) of
                                    NONE => get_substitutions2_helper(rest, s,
                                    acc)
                                  | SOME subs_for_x =>
                                    get_substitutions2_helper(rest, s,
                                    acc @ subs_for_x)
                          else get_substitutions2_helper(rest, s, acc)
    in
      get_substitutions2_helper(subs, s, [])
    end







(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
